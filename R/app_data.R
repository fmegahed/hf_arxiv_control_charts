# Loading the app's data: one row per paper, all tracks in one data frame.
#
# The factsheet is the primary table (one row per paper, keyed by the base
# arXiv id). Metadata ids carry a version, so the join is on arxiv_base_id(id).
# Everything is read once at start-up and kept in memory.

app_data_dir <- function() Sys.getenv("QEW_DATA_DIR", "data")

app_settings_path <- function(root = ".") file.path(root, "config", "app_settings.json")

app_settings_load <- function(path = app_settings_path()) {
  settings <- jsonlite::read_json(path, simplifyVector = FALSE)
  for (key in c("real_data_values", "review_paper_types")) {
    settings[[key]] <- as.character(unlist(settings[[key]]))
  }
  settings
}

reliability_load <- function(path = file.path("config", "field_reliability_v1.json")) {
  if (!file.exists(path)) return(NULL)
  jsonlite::read_json(path, simplifyVector = TRUE)
}

# Names of the fields every track has with the same definition.
spec_shared_fields <- function(spec) {
  vapply(c(spec$fields$common_head, spec$fields$common_tail), function(f) f$name, character(1))
}

# Tracks in which a field exists.
spec_field_tracks <- function(spec, name) {
  has <- vapply(names(spec$tracks), function(track) {
    name %in% vapply(spec_fields(spec, track), function(f) f$name, character(1))
  }, logical(1))
  names(spec$tracks)[has]
}

# Definition of a field. Shared fields are the same in every track, so any
# track will do when none is given.
spec_field_any <- function(spec, name, track = NULL) {
  if (is.null(track) || !nzchar(track)) {
    tracks <- spec_field_tracks(spec, name)
    if (length(tracks) == 0L) stop("Unknown field: ", name)
    track <- tracks[[1]]
  }
  spec_field(spec, track, name)
}

parse_utc <- function(x) {
  as.POSIXct(sub("Z$", "", gsub("T", " ", x, fixed = TRUE)), tz = "UTC",
             tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d"), optional = TRUE)
}

as_flag <- function(x) {
  if (is.logical(x)) return(x)
  out <- rep(NA, length(x))
  out[x %in% c("TRUE", "true", "T", "1")] <- TRUE
  out[x %in% c("FALSE", "false", "F", "0")] <- FALSE
  out
}

# "A|B|C|D" -> "A, B, C and 1 more".
shorten_authors <- function(authors, keep = 3L) {
  vapply(authors, function(cell) {
    names <- split_values(cell)
    if (length(names) == 0L) return(NA_character_)
    if (length(names) <= keep) return(paste(names, collapse = ", "))
    paste0(paste(names[seq_len(keep)], collapse = ", "), " and ", length(names) - keep, " more")
  }, character(1), USE.NAMES = FALSE)
}

count_authors <- function(authors) {
  vapply(authors, function(cell) length(split_values(cell)), integer(1), USE.NAMES = FALSE)
}

# arXiv titles and abstracts use dollar signs for math. The app typesets only
# \( \) and \[ \], so they are converted once with the same routine that
# cleans factsheet text.
clean_metadata_text <- function(x) {
  needs <- !is.na(x) & grepl("[$<\r\t]", x)
  x[needs] <- vapply(x[needs], function(value) sanitize_narrative(value)$text, character(1),
                     USE.NAMES = FALSE)
  x <- gsub("\\s*\n\\s*", " ", x)
  trimws(x)
}

# The label shown for a stored value: the catch-all becomes "Other: <term>".
display_value <- function(value, other_term = NA_character_) {
  other <- !is.na(value) & value == NONE_OF_LISTED
  term <- rep_len(other_term, length(value))
  value[other] <- ifelse(is.na(term[other]) | !nzchar(term[other]), "Other",
                         paste0("Other: ", term[other]))
  value
}

# Column of a field that holds its primary (or only) label.
primary_column <- function(field) {
  if (identical(field$kind, "primary_additional")) paste0(field$name, "_primary") else field$name
}

# Primary label of a field for every row, ready for display.
primary_display <- function(df, field) {
  column <- primary_column(field)
  if (!column %in% names(df)) return(rep(NA_character_, nrow(df)))
  term_column <- paste0(field$name, "_other_term")
  term <- if (term_column %in% names(df)) df[[term_column]] else NA_character_
  display_value(df[[column]], term)
}

# One track: factsheet joined to the latest metadata version of each paper.
load_track_papers <- function(spec, track, data_dir = app_data_dir()) {
  info <- spec_track(spec, track)
  factsheet <- read_factsheet(file.path(data_dir, info$factsheet_csv))
  metadata <- read_factsheet(file.path(data_dir, info$metadata_csv))
  if (is.null(factsheet) || is.null(metadata)) {
    return(list(papers = NULL, problem = paste0("Data files for ", info$label, " were not found in '", data_dir, "'.")))
  }
  if (!all(c("paper_id", "status") %in% names(factsheet))) {
    return(list(papers = NULL, problem = paste0(
      "The factsheet for ", info$label, " in '", data_dir, "' is not in the version 2 layout (no paper_id column).")))
  }
  factsheet <- factsheet[!duplicated(factsheet$paper_id), , drop = FALSE]
  factsheet$track <- track

  metadata <- keep_latest_version(metadata)
  metadata$paper_id <- arxiv_base_id(metadata$id)
  metadata <- metadata[!duplicated(metadata$paper_id), , drop = FALSE]
  meta_columns <- intersect(c("paper_id", "submitted", "updated", "title", "abstract", "authors",
                              "link_abstract", "link_pdf", "comment", "journal_ref", "doi",
                              "primary_category", "categories"), names(metadata))
  names(metadata)[names(metadata) == "id"] <- "metadata_id"

  has_metadata <- factsheet$paper_id %in% metadata$paper_id
  papers <- dplyr::inner_join(factsheet, metadata[, c(meta_columns, "metadata_id"), drop = FALSE],
                              by = "paper_id")

  papers$submitted_date <- as.Date(parse_utc(papers$submitted))
  papers$year <- as.integer(format(papers$submitted_date, "%Y"))
  papers$title <- clean_metadata_text(papers$title)
  papers$abstract <- clean_metadata_text(papers$abstract)
  papers$authors_short <- shorten_authors(papers$authors)
  papers$n_authors <- count_authors(papers$authors)
  papers$code_public <- as_flag(papers$code_public)
  for (field in spec_fields(spec, track)) {
    if (identical(field$kind, "tristate") && field$name %in% names(papers)) {
      papers[[field$name]] <- as_flag(papers[[field$name]])
    }
  }
  papers$topic <- primary_display(papers, spec_field(spec, track, info$core_filters$topic))
  papers$method <- primary_display(papers, spec_field(spec, track, info$core_filters$method))
  papers$link_abstract <- paste0("https://arxiv.org/abs/", papers$paper_id)
  papers$link_pdf <- paste0("https://arxiv.org/pdf/", papers$metadata_id)
  search_columns <- intersect(c("summary", "key_results", "limitations_stated"), names(papers))
  search_text <- paste(papers$title, papers$abstract)
  for (column in search_columns) search_text <- paste(search_text, ifelse(is.na(papers[[column]]), "", papers[[column]]))
  papers$search_text <- tolower(search_text)

  list(papers = papers, problem = NULL,
       n_without_metadata = sum(!has_metadata),
       n_awaiting_factsheet = sum(!metadata$paper_id %in% factsheet$paper_id))
}

# Which model and schema produced the factsheets that are actually loaded.
provenance_table <- function(papers) {
  ok <- papers[papers$status == "ok", , drop = FALSE]
  if (nrow(ok) == 0L) {
    return(data.frame(track = character(0), llm_model = character(0), schema_version = character(0),
                      n = integer(0), first = character(0), last = character(0), stringsAsFactors = FALSE))
  }
  ok$llm_model[is.na(ok$llm_model)] <- "not recorded"
  ok$schema_version[is.na(ok$schema_version)] <- "not recorded"
  ok$day <- substr(ok$extracted_at, 1L, 10L)
  out <- dplyr::summarise(dplyr::group_by(ok, track, llm_model, schema_version),
                          n = dplyr::n(),
                          first = suppressWarnings(min(day, na.rm = TRUE)),
                          last = suppressWarnings(max(day, na.rm = TRUE)), .groups = "drop")
  as.data.frame(out, stringsAsFactors = FALSE)
}

# Date the data were last updated: the latest extraction time, or the latest
# submission date when no extraction time is recorded. Never the wall clock.
data_current_date <- function(papers) {
  stamps <- as.Date(parse_utc(papers$extracted_at))
  if (any(!is.na(stamps))) return(max(stamps, na.rm = TRUE))
  if (any(!is.na(papers$submitted_date))) return(max(papers$submitted_date, na.rm = TRUE))
  as.Date(NA)
}

load_app_data <- function(spec, data_dir = app_data_dir()) {
  loaded <- lapply(stats::setNames(names(spec$tracks), names(spec$tracks)),
                   function(track) load_track_papers(spec, track, data_dir))
  problems <- unlist(lapply(loaded, function(x) x$problem), use.names = FALSE)
  tables <- Filter(Negate(is.null), lapply(loaded, function(x) x$papers))
  papers <- if (length(tables) > 0L) as.data.frame(dplyr::bind_rows(tables), stringsAsFactors = FALSE) else NULL
  if (is.null(papers)) {
    return(list(papers = NULL, problems = problems, data_dir = data_dir))
  }
  papers <- papers[order(papers$submitted_date, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  rownames(papers) <- NULL
  # One name per person, for the author counts (R/authors.R).
  papers$authors_merged <- merge_author_names(papers$authors)
  years <- papers$year[!is.na(papers$year)]
  list(
    papers = papers,
    problems = problems,
    data_dir = data_dir,
    year_range = if (length(years) > 0L) range(years) else c(NA_integer_, NA_integer_),
    data_date = data_current_date(papers),
    provenance = provenance_table(papers),
    n_without_metadata = vapply(loaded, function(x) as.integer(x$n_without_metadata %||% 0L), integer(1)),
    n_awaiting_factsheet = vapply(loaded, function(x) as.integer(x$n_awaiting_factsheet %||% 0L), integer(1))
  )
}

# A paper found by two tracks' searches has a factsheet in each, so a row is
# identified by track and paper id together.
paper_key <- function(papers) {
  if (is.null(papers$track)) papers$paper_id else paste(papers$track, papers$paper_id)
}

# Scores (relevance) of the rows of `papers`; NA where a row has none.
score_for <- function(papers, scores) {
  if (is.null(scores) || nrow(scores) == 0L) return(rep(NA_real_, nrow(papers)))
  if (is.null(scores$key)) return(scores$score[match(papers$paper_id, scores$paper_id)])
  scores$score[match(paper_key(papers), scores$key)]
}

# The factsheet row of one paper. When the paper is in several tracks, the
# row of `track` is returned if there is one, otherwise the first.
find_paper <- function(papers, paper_id, track = NULL) {
  if (is.null(paper_id) || length(paper_id) == 0L || is.na(paper_id)) return(NULL)
  rows <- papers[papers$paper_id == arxiv_base_id(paper_id), , drop = FALSE]
  if (nrow(rows) == 0L) return(NULL)
  if (!is.null(track) && any(rows$track == track)) rows <- rows[rows$track == track, , drop = FALSE]
  rows[1, , drop = FALSE]
}

# Other tracks in which the same paper has a factsheet.
paper_other_tracks <- function(papers, paper) {
  rows <- papers[papers$paper_id == paper$paper_id & papers$track != paper$track &
                   papers$status != "failed", , drop = FALSE]
  unique(rows$track)
}
