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
    conditions = do.call(ellmer::type_object, c(
      list(.description = "Fill a field only when the question clearly asks for one of its listed options. Leave every other field out."),
      conditions)),
    residual = ellmer::type_string(paste(
      "The part of the question that none of the fields above can express, as a short noun phrase",
      "(for example 'wind turbines'). Use an empty string when the fields cover the whole question."))
  )
}

question_system_prompt <- function(spec, context_track = NULL, year_range = NULL) {
  tracks <- vapply(names(spec$tracks), function(id) {
    paste0("- ", id, ": ", spec$tracks[[id]]$label, ". ", spec$tracks[[id]]$description)
  }, character(1))
  paste0(
    "You translate a question about a database of arXiv papers in quality engineering into search filters. ",
    "You do not answer the question. Use only the options listed in the form. ",
    "Never guess a filter that the question does not ask for; put anything the form cannot express into 'residual'.\n\n",
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
filter_spec_to_state <- function(raw, spec) {
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
  residual <- raw$residual
  state$residual <- if (is.character(residual) && length(residual) == 1L && !is.na(residual)) residual else ""
  state
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
validate_filter_spec <- function(raw, spec, year_range, context_track = NULL) {
  state <- filter_spec_to_state(raw, spec)
  no_track_named <- is.null(raw$track) || identical(raw$track, ALL_TRACKS_KEY)
  checked <- sanitize_state(state, spec, year_range, context_track = context_track)
  clean <- checked$state
  # Inside a track, a question that names no track stays in that track.
  if (is.null(clean$track) && no_track_named && !is.null(context_track)) clean$track <- context_track
  if (nzchar(clean$residual)) clean$sort <- "relevance"
  clean$conditions <- drop_redundant_code_condition(clean)
  clean <- normalize_state(clean)
  interpretation <- raw$interpretation
  list(state = clean, dropped = checked$dropped,
       interpretation = if (is.character(interpretation) && length(interpretation) == 1L) interpretation else "")
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
  state$keywords <- keyword_terms(question)
  if (length(state$keywords) > 0L) state$sort <- "relevance"
  normalize_state(state)
}

# Question -> list(ok, mode, state, message, dropped).
#   mode "model"    the model's form, validated
#   mode "keyword"  the model could not be reached; keyword search instead
interpret_question <- function(question, spec, year_range, context_track = NULL,
                               model_fn = question_model_fn(spec)) {
  problem <- check_question(question)
  if (!is.null(problem)) return(list(ok = FALSE, mode = "none", message = problem))
  question <- trimws(question)
  raw <- tryCatch(model_fn(question_system_prompt(spec, context_track, year_range), question),
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
  checked <- validate_filter_spec(raw, spec, year_range, context_track)
  list(ok = TRUE, mode = "model", state = checked$state, dropped = checked$dropped,
       message = checked$interpretation)
}

question_failure_text <- function(error_class) {
  switch(error_class,
         no_credit = "the language-model account has no credit",
         rate_limit = "the language model is rate limited",
         timeout = "the language model timed out",
         parse_error = "the model's reply could not be read",
         "the language model is unavailable")
}
