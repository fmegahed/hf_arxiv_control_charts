# The filter engine. No Shiny here: a filter state is a plain list, and every
# tab of the app reads the same one.
#
# A state holds
#   track             track id, or NULL for all tracks
#   year_from/_to     integers or NULL
#   conditions        list of list(field, values, track, role)
#                       track  NULL for a field shared by all tracks, else the
#                              track the field belongs to
#                       role   "any" (primary or additional label) or "primary"
#   public_code, real_data, reviews_only, include_screened   logical switches
#   authors           names; a paper must list every one of them as an author
#   venue             text that the journal reference has to contain
#   category          an arXiv category, such as stat.ME
#   ids               arXiv ids; only these papers
#   title             words that the title has to contain
#   since             "YYYY-MM-DD"; submitted on or after this day
#   residual          the part of a question no field covers (ranked, not filtered)
#   keywords          terms of a keyword search (used when a question could
#                     not be translated)
#   sort              "newest", "oldest" or "relevance"
#
# Values inside one condition are alternatives (OR); conditions, switches and
# years all have to hold (AND). A condition on a track's own field leaves
# papers of other tracks untouched.

SORT_CHOICES <- c("newest", "oldest", "relevance")
AUTHOR_NAME_MAX_CHARS <- 80L
ARXIV_CATEGORY_PATTERN <- "^[a-z-]+([.][A-Za-z-]+)?$"
ARXIV_ID_PATTERN <- "^([0-9]{4}[.][0-9]{4,5}|[a-z-]+([.][A-Z]{2})?/[0-9]{7})$"
CONDITION_ROLES <- c("any", "primary")

new_filter_state <- function(track = NULL) {
  list(track = track, year_from = NULL, year_to = NULL, conditions = list(),
       public_code = FALSE, real_data = FALSE, reviews_only = FALSE, include_screened = FALSE,
       authors = character(0), venue = "", category = "", ids = character(0), title = "", since = NULL,
       residual = "", keywords = character(0), sort = "newest")
}

# A state always has every element, in the same order, so two states that
# mean the same are identical(). (Assigning NULL to a list element removes
# it; this puts it back.)
normalize_state <- function(state) {
  out <- new_filter_state()
  for (name in names(out)) {
    value <- state[[name]]
    if (!is.null(value)) out[[name]] <- value
  }
  for (name in c("year_from", "year_to")) if (!is.null(out[[name]])) out[[name]] <- as.integer(out[[name]])
  out
}

new_condition <- function(field, values, track = NULL, role = "any") {
  list(field = field, values = as.character(unlist(values)), track = track, role = role)
}

# The track a condition on `field` belongs to: NULL for shared fields,
# otherwise the given track, the current track, or the only track that has it.
condition_track <- function(spec, field, track = NULL, context_track = NULL) {
  if (field %in% spec_shared_fields(spec)) return(NULL)
  tracks <- spec_field_tracks(spec, field)
  for (candidate in list(track, context_track)) {
    if (!is.null(candidate) && candidate %in% tracks) return(candidate)
  }
  if (length(tracks) == 1L) tracks[[1]] else NA_character_
}

same_condition_slot <- function(a, field, track, role) {
  identical(a$field, field) && identical(a$track %||% "", track %||% "") && identical(a$role, role)
}

# Set (replace) the values of one condition; empty values remove it.
set_condition <- function(state, field, values, track = NULL, role = "any") {
  values <- unique(as.character(unlist(values)))
  values <- values[!is.na(values) & nzchar(values)]
  keep <- !vapply(state$conditions, same_condition_slot, logical(1), field = field, track = track, role = role)
  state$conditions <- state$conditions[keep]
  if (length(values) > 0L) {
    state$conditions <- c(state$conditions, list(new_condition(field, values, track, role)))
  }
  normalize_state(state)
}

get_condition_values <- function(state, field, track = NULL, role = "any") {
  for (cond in state$conditions) {
    if (same_condition_slot(cond, field, track, role)) return(cond$values)
  }
  character(0)
}

# ---- Masks -------------------------------------------------------------------

# Rows that count as papers: in scope, plus screened-out ones on request.
# Rows whose extraction failed are never shown as papers.
scope_mask <- function(papers, include_screened = FALSE) {
  keep <- papers$status == "ok"
  if (isTRUE(include_screened)) keep <- keep | papers$status == "out_of_scope"
  keep & !is.na(keep)
}

track_mask <- function(papers, track) {
  if (is.null(track)) rep(TRUE, nrow(papers)) else papers$track == track
}

# Every element of a delimited column with the row it came from. Splitting
# the column once keeps filters and charts fast on a few thousand papers.
long_values <- function(x) {
  x <- as.character(x)
  parts <- strsplit(x, LIST_SEP, fixed = TRUE)
  parts[is.na(x)] <- list(character(0))
  out <- data.frame(row = rep(seq_along(parts), lengths(parts)),
                    value = trimws(unlist(parts, use.names = FALSE)), stringsAsFactors = FALSE)
  out[nzchar(out$value), , drop = FALSE]
}

# TRUE where the cell contains any of `values` as a whole element: the same
# rule as has_value(), for several values and without a loop over cells.
has_any_value <- function(x, values) {
  long <- long_values(x)
  hit <- rep(FALSE, length(x))
  hit[unique(long$row[long$value %in% values])] <- TRUE
  hit
}

condition_mask <- function(papers, cond, spec) {
  field <- spec_field_any(spec, cond$field, cond$track)
  column <- if (identical(cond$role, "primary")) primary_column(field) else field$name
  if (!column %in% names(papers)) return(rep(FALSE, nrow(papers)))
  cells <- papers[[column]]
  if (identical(field$kind, "tristate")) {
    wanted <- c(TRUE, FALSE, NA)[match(cond$values, TRISTATE_VALUES)]
    hit <- rep(FALSE, nrow(papers))
    for (value in wanted) hit <- hit | (if (is.na(value)) is.na(cells) else (!is.na(cells) & cells == value))
  } else {
    hit <- has_any_value(cells, cond$values)
  }
  if (!is.null(cond$track)) hit <- hit | papers$track != cond$track
  hit
}

keyword_terms <- function(text) {
  if (is.null(text) || length(text) == 0L) return(character(0))
  words <- unlist(strsplit(tolower(paste(text, collapse = " ")), "[^a-z0-9-]+"))
  words <- words[nchar(words) >= 3L & !grepl("^[0-9]+$", words)]
  unique(setdiff(words, KEYWORD_STOPWORDS))
}

KEYWORD_STOPWORDS <- c(
  "the", "and", "for", "with", "that", "this", "from", "are", "was", "were", "which", "what", "who",
  "how", "does", "did", "use", "uses", "used", "using", "provide", "provides", "have", "has", "any",
  "all", "their", "them", "there", "about", "into", "than", "then", "also", "can", "papers", "paper",
  "arxiv", "submitted", "published", "study", "studies", "research", "work", "works", "show", "find",
  "list", "give", "between", "since", "before", "after", "during", "not", "but", "its", "our", "your",
  "methods", "method", "based", "new", "approach", "approaches")

# Share of the terms that occur in each paper's title, abstract or summary.
keyword_scores <- function(papers, terms) {
  if (length(terms) == 0L || nrow(papers) == 0L) return(rep(0, nrow(papers)))
  hits <- vapply(terms, function(term) grepl(term, papers$search_text, fixed = TRUE),
                 logical(nrow(papers)))
  if (is.null(dim(hits))) hits <- matrix(hits, nrow = nrow(papers))
  rowMeans(hits)
}

# Words of a person's name, in lower case, without initials and punctuation.
name_words <- function(name) {
  words <- strsplit(tolower(gsub("[^[:alpha:]' -]", " ", name)), "[[:space:]]+")[[1]]
  words[nchar(words) > 1L]
}

# Which papers list `name` as an author. Every word of the name has to occur
# as a whole word in one author entry, so "Fadel Megahed" finds
# "Fadel M. Megahed" and a family name alone finds everyone who has it.
author_mask <- function(authors, name) {
  wanted <- name_words(name)
  if (length(wanted) == 0L) return(rep(TRUE, length(authors)))
  vapply(authors, function(cell) {
    entries <- split_values(cell)
    any(vapply(entries, function(entry) all(wanted %in% name_words(entry)), logical(1)))
  }, logical(1), USE.NAMES = FALSE)
}

# Logical vector: which rows of `papers` the state selects.
filter_mask <- function(papers, state, spec, settings) {
  keep <- scope_mask(papers, state$include_screened) & track_mask(papers, state$track)
  if (!is.null(state$year_from)) keep <- keep & !is.na(papers$year) & papers$year >= state$year_from
  if (!is.null(state$year_to)) keep <- keep & !is.na(papers$year) & papers$year <= state$year_to
  for (cond in state$conditions) keep <- keep & condition_mask(papers, cond, spec)
  if (isTRUE(state$public_code)) keep <- keep & !is.na(papers$code_public) & papers$code_public
  if (isTRUE(state$real_data)) {
    keep <- keep & has_any_value(papers[[settings$data_field]], settings$real_data_values)
  }
  if (isTRUE(state$reviews_only)) {
    keep <- keep & has_any_value(papers[[settings$paper_type_field]], settings$review_paper_types)
  }
  for (name in state$authors) keep <- keep & author_mask(papers$authors, name)
  if (nzchar(state$venue %||% "")) {
    keep <- keep & !is.na(papers$journal_ref) & grepl(tolower(state$venue), tolower(papers$journal_ref), fixed = TRUE)
  }
  if (nzchar(state$category %||% "")) {
    keep <- keep & (has_any_value(papers$categories, state$category) | papers$primary_category %in% state$category)
  }
  if (length(state$ids) > 0L) keep <- keep & papers$paper_id %in% state$ids
  for (word in name_words(state$title %||% "")) {
    keep <- keep & grepl(paste0("(^|[^[:alpha:]])", word, "([^[:alpha:]]|$)"), tolower(papers$title))
  }
  if (!is.null(state$since)) keep <- keep & !is.na(papers$submitted_date) & papers$submitted_date >= as.Date(state$since)
  if (length(state$keywords) > 0L) keep <- keep & keyword_scores(papers, state$keywords) > 0
  keep
}

apply_filters <- function(papers, state, spec, settings) {
  papers[filter_mask(papers, state, spec, settings), , drop = FALSE]
}

# Counts behind the "Showing n of N" line, for the current track scope.
scope_counts <- function(papers, state, spec, settings) {
  in_track <- papers[track_mask(papers, state$track), , drop = FALSE]
  screened <- in_track[in_track$status == "out_of_scope" & !is.na(in_track$status), , drop = FALSE]
  category <- screened$scope_category
  category[is.na(category)] <- "reason_not_recorded"
  tally <- sort(table(category), decreasing = TRUE)
  reasons <- data.frame(category = names(tally), n = as.integer(tally), stringsAsFactors = FALSE)
  list(
    shown = sum(filter_mask(papers, state, spec, settings)),
    # papers the same filters would add if screened-out papers were included
    hidden_matches = if (isTRUE(state$include_screened)) 0L else
      sum(filter_mask(papers, utils::modifyList(state, list(include_screened = TRUE)), spec, settings) &
            papers$status %in% "out_of_scope"),
    in_scope = sum(in_track$status == "ok", na.rm = TRUE),
    screened_out = nrow(screened),
    failed = sum(in_track$status == "failed", na.rm = TRUE),
    total = sum(scope_mask(in_track, state$include_screened)),
    reasons = reasons
  )
}

sort_papers <- function(papers, sort = "newest", scores = NULL) {
  if (nrow(papers) == 0L) return(papers)
  if (identical(sort, "relevance") && !is.null(scores)) {
    score <- score_for(papers, scores)
    return(papers[order(-ifelse(is.na(score), -1, score), -as.numeric(papers$submitted_date)), , drop = FALSE])
  }
  decreasing <- !identical(sort, "oldest")
  papers[order(papers$submitted_date, decreasing = decreasing, na.last = TRUE), , drop = FALSE]
}

# ---- Chips -------------------------------------------------------------------

value_label <- function(value) ifelse(value == NONE_OF_LISTED, "Other (not in the list)", value)

# One chip per active part of the state, in reading order. `id` is what
# remove_chip() takes.
state_chips <- function(state, spec) {
  chips <- list()
  add <- function(id, label) chips[[length(chips) + 1L]] <<- list(id = id, label = label)
  if (!is.null(state$track)) add("track", paste0("Track: ", spec_track(spec, state$track)$short_label))
  if (!is.null(state$year_from) || !is.null(state$year_to)) {
    from <- state$year_from; to <- state$year_to
    label <- if (is.null(from)) paste0("up to ", to) else if (is.null(to)) paste0(from, " onwards")
             else if (from == to) as.character(from) else paste0(from, " to ", to)
    add("year", paste0("Year: ", label))
  }
  for (i in seq_along(state$conditions)) {
    cond <- state$conditions[[i]]
    field <- spec_field_any(spec, cond$field, cond$track)
    label <- field$label
    if (identical(cond$role, "primary") && identical(field$kind, "primary_additional")) {
      label <- paste0(label, " (primary)")
    }
    text <- paste0(label, ": ", paste(value_label(cond$values), collapse = " or "))
    if (!is.null(cond$track) && !identical(cond$track, state$track)) {
      text <- paste0(text, " (", spec_track(spec, cond$track)$short_label, " papers only)")
    }
    add(paste0("cond:", i), text)
  }
  for (i in seq_along(state$authors)) add(paste0("author:", i), paste0("Author: ", state$authors[i]))
  if (nzchar(state$venue %||% "")) add("venue", paste0("Journal reference contains: ", state$venue))
  if (nzchar(state$category %||% "")) add("category", paste0("arXiv category: ", state$category))
  if (length(state$ids) > 0L) add("ids", paste0("arXiv id: ", paste(state$ids, collapse = ", ")))
  if (nzchar(state$title %||% "")) add("title", paste0("Title contains: ", state$title))
  if (!is.null(state$since)) add("since", paste0("Submitted since: ", state$since))
  if (isTRUE(state$public_code)) add("code", "Code: public")
  if (isTRUE(state$real_data)) add("real", "Data: uses real data")
  if (isTRUE(state$reviews_only)) add("reviews", "Reviews and tutorials only")
  if (isTRUE(state$include_screened)) add("screened", "Including screened-out papers")
  if (length(state$keywords) > 0L) add("keywords", paste0("Keywords: ", paste(state$keywords, collapse = ", ")))
  criteria <- parse_criteria(state$residual)
  for (i in seq_along(criteria)) add(paste0("crit:", i), criterion_label(criteria[[i]]))
  chips
}

remove_chip <- function(state, id) {
  if (identical(id, "track")) state$track <- NULL
  else if (identical(id, "year")) state$year_from <- state$year_to <- NULL
  else if (startsWith(id, "cond:")) {
    index <- suppressWarnings(as.integer(sub("cond:", "", id, fixed = TRUE)))
    if (!is.na(index) && index >= 1L && index <= length(state$conditions)) {
      state$conditions <- state$conditions[-index]
    }
  }
  else if (startsWith(id, "author:")) {
    index <- suppressWarnings(as.integer(sub("author:", "", id, fixed = TRUE)))
    if (!is.na(index) && index >= 1L && index <= length(state$authors)) state$authors <- state$authors[-index]
  }
  else if (identical(id, "code")) state$public_code <- FALSE
  else if (identical(id, "real")) state$real_data <- FALSE
  else if (identical(id, "reviews")) state$reviews_only <- FALSE
  else if (identical(id, "screened")) state$include_screened <- FALSE
  else if (identical(id, "keywords")) state$keywords <- character(0)
  else if (identical(id, "residual")) state$residual <- ""
  else if (startsWith(id, "crit:")) {
    index <- suppressWarnings(as.integer(sub("crit:", "", id, fixed = TRUE)))
    criteria <- parse_criteria(state$residual)
    if (!is.na(index) && index >= 1L && index <= length(criteria)) state$residual <- format_criteria(criteria[-index])
  }
  else if (identical(id, "venue")) state$venue <- ""
  else if (identical(id, "category")) state$category <- ""
  else if (identical(id, "ids")) state$ids <- character(0)
  else if (identical(id, "title")) state$title <- ""
  else if (identical(id, "since")) state["since"] <- list(NULL)
  if (!nzchar(state$residual) && length(state$keywords) == 0L && identical(state$sort, "relevance")) {
    state$sort <- "newest"
  }
  normalize_state(state)
}

# "Clear all" keeps the track: leaving a track is done by removing its chip.
clear_filters <- function(state) new_filter_state(state$track)

has_active_filters <- function(state) {
  !identical(state[setdiff(names(state), c("track", "sort"))],
             new_filter_state()[setdiff(names(state), c("track", "sort"))])
}

# ---- Sanitizing --------------------------------------------------------------

# Check a state from outside (a model, a URL) against the spec. Unknown
# fields and values are dropped and years clamped; `dropped` says what went.
sanitize_state <- function(state, spec, year_range, context_track = NULL) {
  dropped <- character(0)
  clean <- new_filter_state()
  year_range <- as.integer(year_range)

  if (!is.null(state$track) && length(state$track) == 1L && !is.na(state$track)) {
    if (state$track %in% names(spec$tracks)) clean$track <- state$track
    else dropped <- c(dropped, paste0("unknown track '", state$track, "'"))
  }
  context <- clean$track %||% context_track

  year <- function(value, name) {
    value <- suppressWarnings(as.integer(value %||% NA))
    if (length(value) != 1L || is.na(value)) return(NULL)
    clamped <- min(max(value, year_range[1]), year_range[2])
    if (clamped != value) {
      dropped <<- c(dropped, paste0(name, " ", value, " is outside the data (", year_range[1], " to ",
                                    year_range[2], ")"))
    }
    clamped
  }
  clean$year_from <- year(state$year_from, "start year")
  clean$year_to <- year(state$year_to, "end year")
  if (!is.null(clean$year_from) && !is.null(clean$year_to) && clean$year_from > clean$year_to) {
    swap <- clean$year_from; clean$year_from <- clean$year_to; clean$year_to <- swap
  }
  if (identical(clean$year_from, year_range[1]) && identical(clean$year_to, year_range[2])) {
    clean$year_from <- clean$year_to <- NULL
  }

  for (cond in state$conditions) {
    field_name <- cond$field %||% ""
    tracks <- spec_field_tracks(spec, field_name)
    if (length(tracks) == 0L) {
      dropped <- c(dropped, paste0("unknown field '", field_name, "'"))
      next
    }
    track <- condition_track(spec, field_name, cond$track, context)
    if (!is.null(track) && is.na(track)) {
      dropped <- c(dropped, paste0("field '", field_name, "' needs a track"))
      next
    }
    field <- spec_field_any(spec, field_name, track)
    if (!isTRUE(field$filter %in% c("core", "more"))) {
      dropped <- c(dropped, paste0("field '", field_name, "' cannot be filtered"))
      next
    }
    values <- unique(as.character(unlist(cond$values)))
    allowed <- field_choices(field)
    bad <- setdiff(values, allowed)
    if (length(bad) > 0L) {
      dropped <- c(dropped, paste0("'", bad, "' is not a value of ", field$label))
    }
    values <- intersect(values, allowed)
    role <- if (isTRUE(cond$role %in% CONDITION_ROLES)) cond$role else "any"
    if (length(values) > 0L) clean <- set_condition(clean, field_name, values, track, role)
  }

  for (flag in c("public_code", "real_data", "reviews_only", "include_screened")) {
    clean[[flag]] <- isTRUE(as.logical(state[[flag]] %||% FALSE))
  }
  authors <- trimws(as.character(unlist(state$authors)))
  authors <- authors[!is.na(authors) & vapply(authors, function(a) length(name_words(a)) > 0L, logical(1))]
  clean$authors <- unique(substr(authors, 1L, AUTHOR_NAME_MAX_CHARS))
  one_text <- function(value, max_chars = AUTHOR_NAME_MAX_CHARS) {
    value <- as.character(unlist(value))
    if (length(value) != 1L || is.na(value)) return("")
    substr(trimws(gsub("[[:space:]]+", " ", value)), 1L, max_chars)
  }
  clean$venue <- one_text(state$venue)
  category <- one_text(state$category)
  clean$category <- if (grepl(ARXIV_CATEGORY_PATTERN, category)) category else ""
  ids <- arxiv_base_id(trimws(as.character(unlist(state$ids))))
  clean$ids <- unique(ids[!is.na(ids) & grepl(ARXIV_ID_PATTERN, ids)])
  clean$title <- one_text(state$title)
  since <- suppressWarnings(as.Date(one_text(state$since), format = "%Y-%m-%d"))
  if (!is.na(since)) clean$since <- format(since, "%Y-%m-%d")
  clean$residual <- format_criteria(parse_criteria(one_text(state$residual, 4L * CRITERION_MAX_CHARS)))
  keywords <- as.character(unlist(state$keywords))
  clean$keywords <- unique(keywords[!is.na(keywords) & nzchar(keywords)])
  can_rank <- nzchar(clean$residual) || length(clean$keywords) > 0L
  sort <- state$sort %||% "newest"
  clean$sort <- if (isTRUE(sort %in% SORT_CHOICES) && (sort != "relevance" || can_rank)) sort else "newest"

  list(state = normalize_state(clean), dropped = dropped)
}
