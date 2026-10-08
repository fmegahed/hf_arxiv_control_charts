# Expressing v1 factsheets in the v2 layout.
#
# The mapping (config/bridge_v1_v2.csv) translates old column values to new
# ones. It is used to compare versions and to give the app something to show
# for a paper that has not been re-extracted yet. A bridged row is an
# approximation: v1 had no primary label, no evidence and different rules.

BRIDGED_SCHEMA_VERSION <- "1.0.0-bridged"

# v1 values deliberately left without a v2 counterpart.
BRIDGE_DROPPED <- list(
  spc = list(performance_metrics = "Other", evaluation_type = "Other"),
  exp_design = list(evaluation_type = c("Other", "Markov chain", "Integral equation", "Economic design")),
  reliability = list(
    modeling_approach = "Hybrid/Ensemble", data_type = "Mixture of types",
    maintenance_policy = "Other",
    evaluation_type = c("Other", "Markov chain", "Integral equation", "Economic design"))
)

read_bridge <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
}

V1_RELEVANCE_FIELD <- c(spc = "is_spc_paper", exp_design = "is_exp_design_paper",
                        reliability = "is_reliability_paper")

# New values for one v1 row and one v2 field, in the order they were found.
bridged_values <- function(row, mapping, new_column) {
  rules <- mapping[mapping$new_column == new_column, , drop = FALSE]
  values <- character()
  for (old_column in unique(rules$old_column)) {
    present <- split_values(row[[old_column]])
    hit <- rules[rules$old_column == old_column & rules$old_value %in% present, , drop = FALSE]
    values <- c(values, hit$new_value[order(match(hit$old_value, present))])
  }
  unique(values)
}

bridge_row <- function(row, track, spec, mapping) {
  relevant <- as.logical(row[[V1_RELEVANCE_FIELD[[track]]]])
  record <- list(
    paper_id = arxiv_base_id(row$id), arxiv_version = arxiv_version(row$id), id = row$id,
    track = track, schema_version = BRIDGED_SCHEMA_VERSION, prompt_version = NA_character_,
    llm_model = row$llm_model, extracted_at = row$extracted_at, attempts = 0L
  )
  if (is.na(row$summary) || is.na(relevant)) {
    record$status <- "failed"
    record$error_class <- "legacy_failure"
    return(record)
  }
  if (!relevant) {
    record$status <- "out_of_scope"
    record$scope_decision <- "out_of_scope"
    return(record)
  }
  record$status <- "ok"
  record$scope_decision <- "in_scope"
  record$scope_category <- "in_scope"

  max_additional <- spec$limits$max_additional_labels
  for (field in spec_fields(spec, track)) {
    name <- field$name
    values <- bridged_values(row, mapping, name)
    listed <- setdiff(values, NONE_OF_LISTED)
    if (field$kind == "single") {
      choice <- if (length(listed) > 0L) listed[1] else if (length(values) > 0L) values[1] else NA_character_
      record[[name]] <- choice
    } else if (field$kind == "primary_additional") {
      ordered <- c(listed, setdiff(values, listed))
      primary <- if (length(ordered) > 0L) ordered[1] else NA_character_
      additional <- utils::head(setdiff(listed, primary), max_additional)
      record[[paste0(name, "_primary")]] <- primary
      record[[paste0(name, "_additional")]] <- collapse_values(additional)
      record[[name]] <- if (is.na(primary)) NA_character_ else collapse_values(c(primary, additional))
    } else if (field$kind == "multi") {
      real <- setdiff(listed, field$sentinel)
      record[[name]] <- collapse_values(if (length(real) > 0L) real else
        if (field$sentinel %in% values) field$sentinel else character(0))
    } else if (field$kind == "tristate" && name %in% names(row)) {
      record[[name]] <- as.logical(row[[name]])
    } else if (field$kind == "text" && name %in% names(row)) {
      record[[name]] <- sanitize_narrative(row[[name]])$text
    } else if (field$kind == "urls" && name %in% names(row)) {
      record[[name]] <- row[[name]]
    }
    if (field_flag(field, "other")) {
      label <- record[[if (field$kind == "primary_additional") paste0(name, "_primary") else name]]
      record[[paste0(name, "_other_term")]] <- if (identical(label, NONE_OF_LISTED)) "unspecified" else NA_character_
    }
  }
  # "Phase II|Both" and similar contradictions resolve to the broader label.
  if (track == "spc") {
    phases <- bridged_values(row, mapping, "phase")
    record$phase <- if ("Self-starting" %in% phases) "Self-starting" else
      if ("Phase I and Phase II" %in% phases || all(c("Phase I", "Phase II") %in% phases)) "Phase I and Phase II" else
        if (length(phases) > 0L) phases[1] else NA_character_
  }
  record$code_public <- if (is.null(record$code_availability) || is.na(record$code_availability)) NA else
    record$code_availability %in% PUBLIC_CODE_SOURCES
  for (column in c("summary", "key_results", "key_equations", "limitations_stated",
                   "limitations_unstated", "future_work_stated", "future_work_unstated")) {
    record[[column]] <- sanitize_narrative(row[[column]])$text
  }
  record
}

# v1 factsheet (read as text) -> v2-layout factsheet, one row per paper.
bridge_v1_to_v2 <- function(v1, track, spec, mapping) {
  mapping <- mapping[mapping$track %in% c("all", track), , drop = FALSE]
  v1 <- keep_latest_version(v1)
  rows <- lapply(seq_len(nrow(v1)), function(i) {
    record_to_row(bridge_row(as.list(v1[i, ]), track, spec, mapping), spec, track)
  })
  dplyr::bind_rows(rows)
}

# v1 values that the mapping neither translates nor lists as dropped.
unmapped_v1_values <- function(v1, track, mapping) {
  mapping <- mapping[mapping$track %in% c("all", track), , drop = FALSE]
  dropped <- BRIDGE_DROPPED[[track]]
  problems <- character()
  for (old_column in unique(mapping$old_column)) {
    if (!old_column %in% names(v1)) next
    seen <- count_values(v1[[old_column]])$value
    known <- c(mapping$old_value[mapping$old_column == old_column], dropped[[old_column]])
    extra <- setdiff(seen, known)
    if (length(extra) > 0L) problems <- c(problems, paste0(old_column, ": ", extra))
  }
  problems
}
