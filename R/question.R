# Turning a question into a filter state.
#
# A model reads the question and fills a structured form built from the
# factsheet specification: a track, years, values of filterable fields, three
# switches, and `residual`, the part of the question that no field covers. The
# form is then checked in code against the specification, so a value the
# specification does not list can never reach the filters. The model call is
# passed in as a function, which keeps this file free of network code.

QUESTION_MAX_CHARS <- 300L
QUESTION_RATE_LIMIT <- 10L
QUESTION_RATE_WINDOW_SEC <- 600L
QUESTION_MODEL_TRIES <- 2L

QUESTION_TRACK_SEP <- "__"

# What the question asks the app to do. Only the first three lead to a list
# of papers; the tab that opens depends on which.
QUESTION_INTENTS <- c("find_papers", "count_or_trend", "find_people", "definition", "cannot_answer")
QUESTION_INTENT_TABS <- c(find_papers = "explore", count_or_trend = "landscape", find_people = "authors")
QUESTION_MAX_DAYS_BACK <- 366L
QUESTION_ID_PATTERN <- "[0-9]{4}[.][0-9]{4,5}"

# Filterable fields the form offers. Shared fields appear once; a track's own
# fields are prefixed with the track id because names can repeat across tracks.
question_fields <- function(spec) {
  shared <- spec_shared_fields(spec)
  out <- list()
  for (track in names(spec$tracks)) {
    for (filter in spec_filters(spec, track)) {
      is_shared <- filter$column %in% shared
      key <- if (is_shared) filter$column else paste0(track, QUESTION_TRACK_SEP, filter$column)
      if (!is.null(out[[key]])) next
      out[[key]] <- list(key = key, field = spec_field(spec, track, filter$column),
                         track = if (is_shared) NULL else track)
    }
  }
  out
}

question_field_description <- function(entry, spec) {
  field <- entry$field
  scope <- if (is.null(entry$track)) "any track" else paste0(spec_track(spec, entry$track)$short_label, " papers")
  values <- field_values(field)
  options <- if (identical(field$kind, "tristate")) paste(values[1:2], collapse = " / ") else {
    options_block(values, field_definitions(field)[values])
  }
  paste0(field$label, " (", scope, "). ", field$question, "\nOptions:\n", options)
}

# The ellmer type of the form, generated from the specification.
build_question_type <- function(spec) {
  entries <- question_fields(spec)
  conditions <- lapply(entries, function(entry) {
    values <- if (identical(entry$field$kind, "tristate")) TRISTATE_VALUES[1:2] else field_choices(entry$field)
    ellmer::type_array(ellmer::type_enum(values), question_field_description(entry, spec), required = FALSE)
  })
  ellmer::type_object(
    interpretation = ellmer::type_string(
      "One plain sentence saying how you read the question. Fill this first."),
    intent = ellmer::type_enum(QUESTION_INTENTS, paste(
      "find_papers: the reader wants a list of papers.",
      "count_or_trend: the reader asks how many, how something changed over time, or what is rising or common.",
      "find_people: the reader asks who works on something.",
      "definition: the reader asks what a term means.",
      "cannot_answer: nothing in the question can be answered from this database.")),
    track = ellmer::type_enum(
      c(names(spec$tracks), ALL_TRACKS_KEY),
      paste0("The research track the question is about. Use '", ALL_TRACKS_KEY,
             "' when it names none or spans several.")),
    year_from = ellmer::type_integer("First submission year wanted. Leave empty if the question gives none.",
                                     required = FALSE),
    year_to = ellmer::type_integer("Last submission year wanted. Leave empty if the question gives none.",
                                   required = FALSE),
    public_code = ellmer::type_boolean(paste(
      "True only if the question asks for papers whose code is publicly available.",
      "Use this switch for that, not a condition on how the code is shared.")),
    real_data = ellmer::type_boolean("True only if the question asks for papers that analyse real data."),
    reviews_only = ellmer::type_boolean("True only if the question asks for reviews, surveys or tutorials."),
    authors = ellmer::type_array(
      ellmer::type_string("One person's name as written in the question."),
      "Names of people the question asks for as authors of the papers (for example 'papers by Jane Doe'). Leave empty when the question names no author. Never put a person's name into 'residual'."),
    journal = ellmer::type_string(
      "Name of a journal, when the question asks for papers published in it. Write it as in the question.",
      required = FALSE),
    arxiv_category = ellmer::type_string(
      "An arXiv category code such as stat.ME, only when the question gives one.", required = FALSE),
    arxiv_ids = ellmer::type_array(
      ellmer::type_string("An arXiv identifier such as 2501.01234."),
      "arXiv identifiers of specific papers the reader wants to see. Do not put the paper of a 'similar to' request here."),
    title_words = ellmer::type_string(
      "Words of a paper's title, only when the question asks for one paper by its title.", required = FALSE),
    submitted_within_days = ellmer::type_integer(
      "Days back from today, when the question asks for recent papers by a period shorter than a year (this week = 7, last month = 31). Use the year fields for anything else.",
      required = FALSE),
    similar_to = ellmer::type_string(
      "arXiv identifier of a paper, when the question asks for papers similar to that paper.", required = FALSE),
    conditions = do.call(ellmer::type_object, c(
      list(.description = paste(
        "Fill a field only when the question clearly asks for one of its listed options. Leave every other field out.",
        "Several options in one field are alternatives: a paper with any one of them is kept.",
        "When the question needs two things at once that belong to the same field, give the more specific one here and put the other into 'criteria'.")),
      conditions)),
    guessed = ellmer::type_array(
      ellmer::type_enum(names(entries)),
      paste("Every field under 'conditions' that you filled although the question does not name that option or a plain synonym of it.",
            "Such a filter is offered to the reader as a suggestion instead of being applied. Leave empty when you filled no field by inference.")),
    criteria = ellmer::type_array(
      ellmer::type_object(
        text = ellmer::type_string("A short noun phrase, for example 'wind turbines'."),
        exclude = ellmer::type_boolean("True when the question asks for papers WITHOUT this.")),
      paste("What the question asks for that no field above can express: a subject, an application, a method, a dataset or a reported result.",
            "One entry per separate requirement ('additive manufacturing' and 'small budgets' are two entries).",
            "Leave empty when the fields above cover the whole question.",
            "Never put a person's name, a journal, a date, a count or a ranking here,",
            "and never the question's own measure, such as 'rising', 'most common', 'interesting' or 'best'.")),
    definition_of = ellmer::type_string("The term, when the question asks what a term means.", required = FALSE),
    cannot_answer = ellmer::type_string(paste(
      "If a part of the question cannot be answered from the titles, authors, dates, journal references and factsheets of the papers",
      "(for example citation counts, impact, which paper is best, or a subject that has nothing to do with these papers),",
      "say which part and why in one short sentence. Otherwise use an empty string."))
  )
}

question_system_prompt <- function(spec, context_track = NULL, year_range = NULL, today = Sys.Date()) {
  tracks <- vapply(names(spec$tracks), function(id) {
    paste0("- ", id, ": ", spec$tracks[[id]]$label, ". ", spec$tracks[[id]]$description)
  }, character(1))
  paste0(
    "You translate a question about a database of arXiv papers in quality engineering into search filters. ",
    "You do not answer the question. Use only the options listed in the form. ",
    "Never guess a filter that the question does not ask for; put a subject the form cannot express into 'criteria', ",
    "and say in 'cannot_answer' what the database cannot tell. Today is ", format(today, "%Y-%m-%d"), ".\n\n",
    "Tracks:\n", paste(tracks, collapse = "\n"), "\n\n",
    if (!is.null(context_track)) paste0("The user is currently looking at the track '", context_track,
                                        "'. Keep that track unless the question names another one.\n")
    else "The user is looking at all tracks.\n",
    if (!is.null(year_range)) paste0("The database covers submission years ", year_range[1], " to ",
                                     year_range[2], ".\n") else ""
  )
}

# The live model call. Returns the form as a list, or stops with an error.
question_model_fn <- function(spec, provider = "openai") {
  type <- NULL
  function(system_prompt, question) {
    if (is.null(type)) type <<- build_question_type(spec)
    chat <- create_chat(provider, spec$models$question, system_prompt)
    # A person is waiting: one retry, then fall back to keyword search.
    withr::with_options(list(ellmer_max_tries = QUESTION_MODEL_TRIES),
                        chat$chat_structured(question, type = type))
  }
}

# Form (as returned by the model) -> unchecked filter state.
filter_spec_to_state <- function(raw, spec, today = Sys.Date()) {
  state <- new_filter_state()
  track <- raw$track
  if (!is.null(track) && length(track) == 1L && !identical(track, ALL_TRACKS_KEY)) state$track <- track
  state$year_from <- raw$year_from
  state$year_to <- raw$year_to
  for (key in names(raw$conditions)) {
    values <- as.character(unlist(raw$conditions[[key]]))
    if (length(values) == 0L) next
    parts <- strsplit(key, QUESTION_TRACK_SEP, fixed = TRUE)[[1]]
    cond <- if (length(parts) == 2L) new_condition(parts[2], values, parts[1]) else new_condition(key, values)
    state$conditions <- c(state$conditions, list(cond))
  }
  for (flag in c("public_code", "real_data", "reviews_only")) state[[flag]] <- isTRUE(raw[[flag]])
  state$authors <- as.character(unlist(raw$authors))
  text <- function(value) if (is.character(value) && length(value) == 1L && !is.na(value)) value else ""
  state$venue <- text(raw$journal)
  state$category <- text(raw$arxiv_category)
  state$ids <- as.character(unlist(raw$arxiv_ids))
  state$title <- text(raw$title_words)
  days <- suppressWarnings(as.integer(raw$submitted_within_days %||% NA))
  if (length(days) == 1L && !is.na(days) && days >= 1L && days <= QUESTION_MAX_DAYS_BACK) {
    state$since <- format(today - days, "%Y-%m-%d")
  }
  # `residual` is the older, single-phrase form of the criteria.
  # The model's array of criteria arrives as a data frame; tests pass a list.
  items <- raw$criteria
  if (is.data.frame(items)) {
    items <- lapply(seq_len(nrow(items)), function(i) list(text = items$text[i], exclude = items$exclude[i]))
  }
  criteria <- lapply(items, function(item) {
    new_criterion(clean_criterion_text(text(item$text)), isTRUE(item$exclude))
  })
  if (nzchar(trimws(text(raw$residual)))) {
    criteria <- c(criteria, list(new_criterion(clean_criterion_text(text(raw$residual)))))
  }
  similar <- arxiv_base_id(trimws(text(raw$similar_to)))
  if (grepl(paste0("^", QUESTION_ID_PATTERN, "$"), similar)) {
    criteria <- c(criteria, list(new_criterion(similar, similar_to = similar)))
    state$ids <- setdiff(arxiv_base_id(state$ids), similar)
  }
  state$residual <- format_criteria(Filter(function(criterion) nzchar(criterion$text), criteria))
  state
}

# Conditions the model marked as its own inference: not applied, only offered.
split_guessed_conditions <- function(raw) {
  guessed <- as.character(unlist(raw$guessed))
  keys <- names(raw$conditions) %||% character(0)
  applied <- raw
  applied$conditions <- raw$conditions[!keys %in% guessed]
  list(applied = applied, guessed = raw$conditions[keys %in% guessed])
}

# Definitions from the specification whose value name matches `term`.
lookup_definitions <- function(spec, term, track = NULL, max_found = 3L) {
  term <- tolower(trimws(term %||% ""))
  if (nchar(term) < 3L) return(character(0))
  tracks <- if (is.null(track)) names(spec$tracks) else track
  found <- character(0)
  for (id in tracks) {
    for (field in spec_fields(spec, id)) {
      definitions <- tryCatch(field_definitions(field), error = function(e) NULL)
      for (value in names(definitions)) {
        if (is.na(definitions[[value]])) next
        if (grepl(term, tolower(value), fixed = TRUE) || grepl(tolower(value), term, fixed = TRUE)) {
          found <- c(found, paste0(field$label, ", \"", value, "\": ", definitions[[value]]))
        }
      }
    }
  }
  utils::head(unique(found), max_found)
}

# The model sometimes sets the public-code switch and also lists every public
# way of sharing code. The two say the same thing, so the list is dropped.
drop_redundant_code_condition <- function(state) {
  if (!isTRUE(state$public_code)) return(state$conditions)
  Filter(function(cond) {
    !(identical(cond$field, "code_availability") && setequal(cond$values, PUBLIC_CODE_SOURCES))
  }, state$conditions)
}

# Check a filled form against the specification. Returns the clean state, what
# was dropped, and the model's one-sentence reading of the question.
validate_filter_spec <- function(raw, spec, year_range, context_track = NULL, today = Sys.Date(),
                                 question = "") {
  parts <- split_guessed_conditions(raw)
  raw <- parts$applied
  state <- filter_spec_to_state(raw, spec, today)
  # An arXiv id typed in the question is used even if the model missed it.
  typed <- regmatches(question, gregexpr(QUESTION_ID_PATTERN, question))[[1]]
  similar <- vapply(Filter(function(criterion) !is.null(criterion$similar_to), parse_criteria(state$residual)),
                    function(criterion) criterion$similar_to, character(1))
  state$ids <- setdiff(unique(c(arxiv_base_id(state$ids), typed)), similar)
  suggestions <- sanitize_state(
    filter_spec_to_state(list(conditions = parts$guessed), spec, today), spec, year_range,
    context_track = context_track)$state$conditions
  no_track_named <- is.null(raw$track) || identical(raw$track, ALL_TRACKS_KEY)
  checked <- sanitize_state(state, spec, year_range, context_track = context_track)
  clean <- checked$state
  # Inside a track, a question that names no track stays in that track.
  if (is.null(clean$track) && no_track_named && !is.null(context_track)) clean$track <- context_track
  if (nzchar(clean$residual)) clean$sort <- "relevance"
  clean$conditions <- drop_redundant_code_condition(clean)
  clean <- normalize_state(clean)
  one <- function(value) if (is.character(value) && length(value) == 1L && !is.na(value)) trimws(value) else ""
  intent <- if (isTRUE(raw$intent %in% QUESTION_INTENTS)) raw$intent else QUESTION_INTENTS[1]
  list(state = clean, dropped = checked$dropped, interpretation = one(raw$interpretation),
       intent = intent, suggestions = suggestions, cannot_answer = one(raw$cannot_answer),
       definitions = if (identical(intent, "definition")) lookup_definitions(spec, one(raw$definition_of), clean$track)
                     else character(0))
}

check_question <- function(question) {
  question <- trimws(question %||% "")
  if (!nzchar(question)) return("Type a question first.")
  if (nchar(question) > QUESTION_MAX_CHARS) {
    return(paste0("Questions are limited to ", QUESTION_MAX_CHARS, " characters; this one has ",
                  nchar(question), "."))
  }
  NULL
}

# TRUE when another question may be asked now. `times` are the times of the
# questions already asked in this session.
rate_limit_ok <- function(times, now = Sys.time(), limit = QUESTION_RATE_LIMIT,
                          window_sec = QUESTION_RATE_WINDOW_SEC) {
  sum(as.numeric(now) - as.numeric(times) < window_sec) < limit
}

# Without a model: keep the track, read years that appear in the question,
# and search the remaining words in title, abstract and summary.
keyword_fallback_state <- function(question, year_range, context_track = NULL) {
  state <- new_filter_state(context_track)
  years <- as.integer(regmatches(question, gregexpr("\\b(19|20)[0-9]{2}\\b", question))[[1]])
  years <- years[years >= year_range[1] & years <= year_range[2]]
  if (length(years) > 0L) {
    state$year_from <- min(years)
    state$year_to <- max(years)
  }
  state$ids <- regmatches(question, gregexpr(QUESTION_ID_PATTERN, question))[[1]]
  state$keywords <- if (length(state$ids) > 0L) character(0) else keyword_terms(question)
  if (length(state$keywords) > 0L) state$sort <- "relevance"
  normalize_state(state)
}

# Question -> list(ok, mode, state, message, dropped).
#   mode "model"    the model's form, validated
#   mode "keyword"  the model could not be reached; keyword search instead
interpret_question <- function(question, spec, year_range, context_track = NULL,
                               model_fn = question_model_fn(spec), today = Sys.Date(), check_fn = NULL) {
  problem <- check_question(question)
  if (!is.null(problem)) return(list(ok = FALSE, mode = "none", message = problem))
  question <- trimws(question)
  raw <- tryCatch(model_fn(question_system_prompt(spec, context_track, year_range, today), question),
                  error = function(e) e)
  if (inherits(raw, "error") || !is.list(raw)) {
    reason <- if (inherits(raw, "error")) classify_error(conditionMessage(raw)) else "parse_error"
    return(list(
      ok = TRUE, mode = "keyword", dropped = character(0), error_class = reason,
      state = keyword_fallback_state(question, year_range, context_track),
      message = paste0(
        "The question could not be translated into filters (", question_failure_text(reason),
        "). Showing a keyword search of titles, abstracts and summaries instead.")))
  }
  checked <- validate_filter_spec(raw, spec, year_range, context_track, today, question)
  # A second model says, for each filter, whether the question asks for it.
  # Filters it rejects are offered as suggestions instead of being applied.
  if (!is.null(check_fn) && length(checked$state$conditions) > 0L) {
    verdict <- tryCatch(check_fn(question, checked$state$conditions), error = function(e) NULL)
    if (!is.null(verdict) && isTRUE(verdict$checked)) {
      checked$state$conditions <- verdict$keep
      checked$state <- normalize_state(checked$state)
      checked$suggestions <- c(checked$suggestions, verdict$demote)
    }
  }
  list(ok = TRUE, mode = "model", state = checked$state, dropped = checked$dropped,
       message = checked$interpretation, intent = checked$intent, suggestions = checked$suggestions,
       cannot_answer = checked$cannot_answer, definitions = checked$definitions,
       tab = unname(QUESTION_INTENT_TABS[checked$intent]))
}

question_failure_text <- function(error_class) {
  switch(error_class,
         no_credit = "the language-model account has no credit",
         rate_limit = "the language model is rate limited",
         timeout = "the language model timed out",
         parse_error = "the model's reply could not be read",
         "the language model is unavailable")
}
