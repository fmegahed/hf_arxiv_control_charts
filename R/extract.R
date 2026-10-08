# Turning model output into factsheet records.
#
# A paper goes through up to three model calls: screen (title and abstract),
# classify (PDF) and narrate (PDF). The calls themselves are injected as `llm`,
# a list of functions, so this file has no network code and is fully testable.
#
# Each llm function returns list(ok, data, usage, error_class, error_message).

EVIDENCE_MAX_CHARS <- 300L
OTHER_TERM_MAX_CHARS <- 60L
PUBLIC_CODE_SOURCES <- c("Public repository", "Package registry", "Supplementary material",
                         "In the paper or appendix", "Personal or lab website")

clean_short_text <- function(x, max_chars) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return(NA_character_)
  text <- sanitize_narrative(as.character(x))$text
  text <- gsub("\n", " ", text, fixed = TRUE)
  if (!nzchar(text)) return(NA_character_)
  if (nchar(text) > max_chars) text <- paste0(substr(text, 1L, max_chars - 3L), "...")
  text
}

as_values <- function(x) {
  x <- as.character(unlist(x, use.names = FALSE))
  unique(trimws(x[!is.na(x) & nzchar(trimws(x))]))
}

# Apply the labelling rules to one field of raw model output. Returns
# list(columns = named list, flags = character).
postprocess_field <- function(field, raw, max_additional = 2L) {
  name <- field$name
  columns <- list()
  flags <- character()

  if (field$kind == "single") {
    value <- as_values(raw[[name]])
    value <- value[value %in% field_choices(field)]
    columns[[name]] <- if (length(value) == 0L) NA_character_ else value[1]

  } else if (field$kind == "primary_additional") {
    primary <- as_values(raw[[paste0(name, "_primary")]])
    primary <- primary[primary %in% field_choices(field)]
    primary <- if (length(primary) == 0L) NA_character_ else primary[1]

    additional <- as_values(raw[[paste0(name, "_additional")]])
    additional <- additional[additional %in% field_values(field)]
    additional <- setdiff(additional, c(primary, ADDITIONAL_NONE, "Not applicable"))
    if (identical(primary, "Not applicable")) additional <- character(0)
    if (length(additional) > max_additional) {
      additional <- additional[seq_len(max_additional)]
      flags <- c(flags, "truncated_additional")
    }
    columns[[paste0(name, "_primary")]] <- primary
    columns[[paste0(name, "_additional")]] <- collapse_values(additional)
    columns[[name]] <- if (is.na(primary)) NA_character_ else collapse_values(c(primary, additional))

  } else if (field$kind == "multi") {
    values <- as_values(raw[[name]])
    values <- values[values %in% field_values(field)]
    real <- setdiff(values, field$sentinel)
    columns[[name]] <- collapse_values(if (length(real) > 0L) real else field$sentinel)

  } else if (field$kind == "tristate") {
    value <- as_values(raw[[name]])
    columns[[name]] <- if (identical(value, "Yes")) TRUE else if (identical(value, "No")) FALSE else NA

  } else if (field$kind == "text") {
    columns[[name]] <- clean_short_text(raw[[name]], 600L)

  } else if (field$kind == "urls") {
    urls <- as_values(raw[[name]])
    urls <- urls[grepl("^https?://[^[:space:]|]+$", urls)]
    columns[[name]] <- collapse_values(urls)
  }

  if (field_flag(field, "other")) {
    label_column <- if (field$kind == "primary_additional") paste0(name, "_primary") else name
    term <- clean_short_text(raw[[paste0(name, "_other_term")]], OTHER_TERM_MAX_CHARS)
    is_other <- identical(columns[[label_column]], NONE_OF_LISTED)
    if (!is_other) {
      term <- NA_character_
    } else if (is.na(term) || tolower(term) %in% c(NOT_APPLICABLE_TERM, "na", "none")) {
      term <- "unspecified"
      flags <- c(flags, "other_term_missing")
    }
    columns[[paste0(name, "_other_term")]] <- term
  }
  if (field_flag(field, "evidence")) {
    columns[[paste0(name, "_evidence")]] <- clean_short_text(raw[[paste0(name, "_evidence")]],
                                                             EVIDENCE_MAX_CHARS)
  }
  list(columns = columns, flags = flags)
}

postprocess_classification <- function(raw, spec, track) {
  columns <- list()
  flags <- character()
  for (field in spec_fields(spec, track)) {
    out <- postprocess_field(field, raw, spec$limits$max_additional_labels)
    columns <- c(columns, out$columns)
    flags <- c(flags, out$flags)
  }
  columns$code_public <- if (is.na(columns$code_availability)) NA else
    columns$code_availability %in% PUBLIC_CODE_SOURCES
  list(columns = columns, flags = unique(flags))
}

postprocess_narrative <- function(raw) {
  flags <- character()
  clean <- function(x) {
    out <- sanitize_narrative(if (is.null(x)) NA_character_ else as.character(x))
    if (out$repaired) flags <<- c(flags, "math_repaired")
    if (is.na(out$text) || !nzchar(out$text)) NA_character_ else out$text
  }

  glossary <- Filter(function(g) !is.null(g$term) && nzchar(trimws(g$term)), raw$glossary)
  glossary_terms <- vapply(glossary, function(g) trimws(g$term), character(1))
  glossary_text <- vapply(glossary, function(g) {
    paste0(trimws(g$term), " = ", gsub(LIST_SEP, "/", clean(g$meaning), fixed = TRUE))
  }, character(1))

  equations <- Filter(function(e) !is.null(e$latex) && nzchar(trimws(e$latex)), raw$key_equations)
  if (any(!vapply(equations, function(e) sanitize_latex(e$latex)$ok, logical(1)))) {
    flags <- c(flags, "latex_invalid")
  }

  columns <- list(
    glossary = collapse_values(glossary_text),
    summary = clean(raw$summary),
    key_results = clean(raw$key_results),
    key_equations = if (length(equations) == 0L) "Not applicable" else render_equations(equations),
    limitations_stated = clean(raw$limitations_stated),
    limitations_unstated = clean(raw$limitations_unstated),
    future_work_stated = clean(raw$future_work_stated),
    future_work_unstated = clean(raw$future_work_unstated)
  )
  shown <- paste(stats::na.omit(c(columns$summary, columns$key_results)), collapse = " ")
  if (length(find_undefined_acronyms(shown, glossary_terms)) > 0L) {
    flags <- c(flags, "undefined_acronym")
  }
  list(columns = columns, flags = unique(flags))
}

# Columns that must be filled in a record with status "ok".
required_columns <- function(spec, track) {
  required <- character()
  for (field in spec_fields(spec, track)) {
    required <- c(required, switch(
      field$kind,
      single = field$name,
      primary_additional = c(paste0(field$name, "_primary"), field$name),
      multi = field$name,
      text = field$name,
      character(0)
    ))
  }
  c(required, "summary", "key_results", "key_equations", "limitations_stated",
    "limitations_unstated", "future_work_stated", "future_work_unstated")
}

# An "ok" record has every required column filled.
validate_record <- function(record, spec, track) {
  if (!identical(record$status, "ok")) return(character(0))
  required <- required_columns(spec, track)
  missing <- required[vapply(required, function(col) {
    value <- record[[col]]
    is.null(value) || length(value) == 0L || is.na(value)
  }, logical(1))]
  if (length(missing) == 0L) character(0) else paste0("missing: ", paste(missing, collapse = ", "))
}

base_record <- function(paper, track, spec, model, now = Sys.time()) {
  list(
    paper_id = arxiv_base_id(paper$id),
    arxiv_version = arxiv_version(paper$id),
    id = paper$id,
    track = track,
    status = NA_character_,
    error_class = NA_character_,
    error_message = NA_character_,
    scope_decision = NA_character_,
    scope_category = NA_character_,
    scope_reason = NA_character_,
    schema_version = spec$schema_version,
    prompt_version = spec$prompt_version,
    llm_model = model,
    extracted_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    qa_flags = NA_character_
  )
}

fail_record <- function(record, usage, error_class, error_message) {
  record$status <- "failed"
  record$error_class <- error_class
  record$error_message <- substr(gsub("[\r\n]+", " ", error_message), 1L, 300L)
  c(record, usage)
}

# Run the stages for one paper. `fetch_pdf(paper)` returns
# list(ok, path, truncated, error_class, error_message).
extract_paper <- function(paper, track, spec, llm, fetch_pdf, model, now = Sys.time()) {
  record <- base_record(paper, track, spec, model, now)
  usage <- empty_usage()
  flags <- character()

  screen <- llm$screen(track, paper)
  usage <- add_usage(usage, screen$usage)
  if (!screen$ok) return(fail_record(record, usage, screen$error_class, screen$error_message))
  record$scope_decision <- screen$data$scope_decision
  record$scope_category <- screen$data$scope_category
  record$scope_reason <- clean_short_text(screen$data$scope_reason, 300L)
  if (identical(record$scope_decision, "out_of_scope")) {
    record$status <- "out_of_scope"
    return(c(record, usage))
  }

  pdf <- fetch_pdf(paper)
  if (!pdf$ok) return(fail_record(record, usage, pdf$error_class, pdf$error_message))
  if (isTRUE(pdf$truncated)) flags <- c(flags, "pdf_truncated")

  classify <- llm$classify(track, pdf$path)
  usage <- add_usage(usage, classify$usage)
  if (!classify$ok) return(fail_record(record, usage, classify$error_class, classify$error_message))
  labels <- postprocess_classification(classify$data, spec, track)

  narrate <- llm$narrate(track, pdf$path, labels_as_text(labels$columns, spec, track))
  usage <- add_usage(usage, narrate$usage)
  if (!narrate$ok) return(fail_record(record, usage, narrate$error_class, narrate$error_message))
  narrative <- postprocess_narrative(narrate$data)

  record <- c(record, labels$columns, narrative$columns)
  record$status <- "ok"
  flags <- unique(c(flags, labels$flags, narrative$flags))
  record$qa_flags <- collapse_values(flags)

  problems <- validate_record(record, spec, track)
  if (length(problems) > 0L) {
    return(fail_record(record[names(base_record(paper, track, spec, model, now))], usage,
                       "validation_error", problems))
  }
  c(record, usage)
}

# Every column of a v2 factsheet, in order.
factsheet_columns <- function(spec, track) {
  c("paper_id", "arxiv_version", "id", "track", "status", "error_class", "error_message", "attempts",
    "scope_decision", "scope_category", "scope_reason",
    spec_label_columns(spec, track), "code_public", spec_narrative_columns(spec),
    "schema_version", "prompt_version", "llm_model", "extracted_at", "qa_flags",
    "input_tokens", "cached_input_tokens", "output_tokens", "cost_usd", "n_calls")
}

# Storage type of every factsheet column: "logical", "integer", "double" or
# "character". Fixed types keep rows combinable whatever values they hold.
factsheet_column_types <- function(spec, track) {
  columns <- factsheet_columns(spec, track)
  types <- stats::setNames(rep("character", length(columns)), columns)
  tristate <- vapply(Filter(function(f) f$kind == "tristate", spec_fields(spec, track)),
                     function(f) f$name, character(1))
  types[c(tristate, "code_public")] <- "logical"
  types[c("arxiv_version", "attempts", "n_calls")] <- "integer"
  types[c("input_tokens", "cached_input_tokens", "output_tokens", "cost_usd")] <- "double"
  types
}

# One-row tibble with the full column set and fixed types; absent columns are NA.
record_to_row <- function(record, spec, track) {
  types <- factsheet_column_types(spec, track)
  row <- lapply(stats::setNames(names(types), names(types)), function(col) {
    value <- record[[col]]
    if (is.null(value) || length(value) == 0L) value <- NA
    methods::as(value, types[[col]])
  })
  tibble::as_tibble(row)
}

# Re-apply the fixed column types after reading a factsheet from CSV.
coerce_factsheet <- function(df, spec, track) {
  types <- factsheet_column_types(spec, track)
  for (col in names(types)) {
    df[[col]] <- if (col %in% names(df)) methods::as(df[[col]], types[[col]]) else
      methods::as(rep(NA, nrow(df)), types[[col]])
  }
  df[names(types)]
}

# Merge new records into a factsheet: one row per paper, newest record wins,
# except that a failed attempt never replaces an earlier usable record. The
# attempts counter accumulates across failures and resets on success.
merge_records <- function(factsheet, new_rows) {
  if (is.null(factsheet) || nrow(factsheet) == 0L) {
    new_rows$attempts <- ifelse(new_rows$status == "failed", 1L, 0L)
    return(new_rows)
  }
  for (i in seq_len(nrow(new_rows))) {
    row <- new_rows[i, , drop = FALSE]
    at <- match(row$paper_id, factsheet$paper_id)
    if (is.na(at)) {
      row$attempts <- if (row$status == "failed") 1L else 0L
      factsheet <- dplyr::bind_rows(factsheet, row)
    } else if (row$status == "failed") {
      previous <- factsheet$attempts[at]
      if (factsheet$status[at] == "failed") {
        row$attempts <- (if (is.na(previous)) 0L else previous) + 1L
        factsheet[at, ] <- row
      }
    } else {
      row$attempts <- 0L
      factsheet[at, ] <- row
    }
  }
  factsheet
}
