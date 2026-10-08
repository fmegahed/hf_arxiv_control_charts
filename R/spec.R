# The factsheet specification (config/factsheet_spec.json) is the single
# source of truth for tracks, fields, allowed values and their definitions.
# The extraction schema, the prompts, the CSV layout, the app's filters and its
# help text are all derived from it.
#
# Field kinds and the columns each one produces in a factsheet:
#   single              X                (+ X_evidence, X_other_term)
#   primary_additional  X_primary, X_additional, X (combined; primary first)
#                                        (+ X_evidence, X_other_term)
#   multi               X                (+ X_evidence)
#   tristate            X (logical)
#   text                X
#   urls                X

NONE_OF_LISTED <- "None of the listed"
ADDITIONAL_NONE <- "None"
NOT_APPLICABLE_TERM <- "n/a"
TRISTATE_VALUES <- c("Yes", "No", "Unclear or not applicable")
TRACK_IDS <- c("spc", "exp_design", "reliability")

spec_path <- function(root = ".") file.path(root, "config", "factsheet_spec.json")

spec_load <- function(path = spec_path()) {
  jsonlite::read_json(path, simplifyVector = FALSE)
}

spec_track <- function(spec, track) {
  if (!track %in% names(spec$tracks)) stop("Unknown track: ", track)
  spec$tracks[[track]]
}

# Classification fields of a track, in the order the model fills them.
spec_fields <- function(spec, track) {
  spec_track(spec, track)
  c(spec$fields$common_head, spec$fields[[track]], spec$fields$common_tail)
}

spec_field <- function(spec, track, name) {
  for (field in spec_fields(spec, track)) if (identical(field$name, name)) return(field)
  stop("Unknown field '", name, "' for track ", track)
}

field_flag <- function(field, flag) isTRUE(field[[flag]])

# Values listed in the spec, without the catch-all.
field_values <- function(field) {
  if (identical(field$kind, "tristate")) return(TRISTATE_VALUES)
  vapply(field$values, function(v) v[[1]], character(1))
}

field_definitions <- function(field) {
  defs <- lapply(field$values, function(v) if (is.null(v[[2]])) NA_character_ else v[[2]])
  stats::setNames(unlist(defs), field_values(field))
}

# Values the model may return for the main label, catch-all included.
field_choices <- function(field) {
  values <- field_values(field)
  if (field_flag(field, "other")) c(values, NONE_OF_LISTED) else values
}

# Values allowed in the "additional" list of a primary_additional field.
field_additional_choices <- function(field) {
  c(setdiff(field_values(field), "Not applicable"), ADDITIONAL_NONE)
}

field_columns <- function(field) {
  name <- field$name
  columns <- switch(
    field$kind,
    primary_additional = c(paste0(name, "_primary"), paste0(name, "_additional"), name),
    name
  )
  if (field_flag(field, "other")) columns <- c(columns, paste0(name, "_other_term"))
  if (field_flag(field, "evidence")) columns <- c(columns, paste0(name, "_evidence"))
  columns
}

# Columns holding several values joined by LIST_SEP.
field_list_columns <- function(field) {
  switch(
    field$kind,
    primary_additional = c(paste0(field$name, "_additional"), field$name),
    multi = field$name,
    urls = field$name,
    character(0)
  )
}

spec_label_columns <- function(spec, track) {
  unlist(lapply(spec_fields(spec, track), field_columns), use.names = FALSE)
}

spec_list_cols <- function(spec, track) {
  unlist(lapply(spec_fields(spec, track), field_list_columns), use.names = FALSE)
}

spec_narrative_columns <- function(spec) {
  vapply(spec$narrative_fields, function(f) f$name, character(1))
}

# Fields a user can filter on. tier is "core" (always visible) or "more".
spec_filters <- function(spec, track) {
  fields <- Filter(function(f) !is.null(f$filter) && f$filter %in% c("core", "more"),
                   spec_fields(spec, track))
  lapply(fields, function(f) {
    list(column = f$name, label = f$label, group = f$group, tier = f$filter,
         kind = f$kind, values = field_choices(f))
  })
}

# ---- Text shown to the model -------------------------------------------------

options_block <- function(values, definitions) {
  lines <- ifelse(is.na(definitions), paste0("- ", values),
                  paste0("- ", values, ": ", definitions))
  paste(lines, collapse = "\n")
}

field_description <- function(field) {
  if (!field$kind %in% c("single", "primary_additional", "multi")) return(field$question)
  values <- field_values(field)
  definitions <- field_definitions(field)
  if (field_flag(field, "other")) {
    values <- c(values, NONE_OF_LISTED)
    definitions <- c(definitions, "Use only when no option above fits. Never combine it with a listed option.")
  }
  paste0(field$question, "\nOptions:\n", options_block(values, definitions))
}

evidence_description <- function(field) {
  paste0("Evidence for '", field$label, "'. Copy at most 25 words from the paper that show which ",
         "option applies, or give a pointer such as 'Section 3.2'. Fill this before the label.")
}

other_term_description <- function(field) {
  paste0("If you chose '", NONE_OF_LISTED, "' for '", field$label, "', name what the paper uses in ",
         "at most 6 words. Otherwise write '", NOT_APPLICABLE_TERM, "'.")
}

additional_description <- function(field, max_additional) {
  paste0("Other options for '", field$label, "' that this paper also proposes or evaluates with its ",
         "own results. At most ", max_additional, ". Do not repeat the primary label. ",
         "Use ['", ADDITIONAL_NONE, "'] if there are no others. When in doubt, leave it out.")
}

# ---- ellmer types ------------------------------------------------------------

# Named list of ellmer types for one field, evidence first so the model writes
# its evidence before it commits to a label.
field_properties <- function(field, max_additional = 2L) {
  name <- field$name
  props <- list()
  if (field_flag(field, "evidence")) {
    props[[paste0(name, "_evidence")]] <- ellmer::type_string(evidence_description(field))
  }
  if (field$kind == "single") {
    props[[name]] <- ellmer::type_enum(field_choices(field), field_description(field))
  } else if (field$kind == "primary_additional") {
    props[[paste0(name, "_primary")]] <- ellmer::type_enum(field_choices(field), field_description(field))
    props[[paste0(name, "_additional")]] <- ellmer::type_array(
      ellmer::type_enum(field_additional_choices(field)),
      additional_description(field, max_additional)
    )
  } else if (field$kind == "multi") {
    props[[name]] <- ellmer::type_array(ellmer::type_enum(field_values(field)),
                                        paste0(field_description(field),
                                               "\nUse ['", field$sentinel, "'] alone when nothing else applies."))
  } else if (field$kind == "tristate") {
    props[[name]] <- ellmer::type_enum(TRISTATE_VALUES, field$question)
  } else if (field$kind == "text") {
    props[[name]] <- ellmer::type_string(field$question)
  } else if (field$kind == "urls") {
    props[[name]] <- ellmer::type_array(ellmer::type_string(), field$question)
  } else {
    stop("Unknown field kind: ", field$kind)
  }
  if (field_flag(field, "other")) {
    props[[paste0(name, "_other_term")]] <- ellmer::type_string(other_term_description(field))
  }
  props
}

build_classify_type <- function(spec, track) {
  max_additional <- spec$limits$max_additional_labels
  props <- unlist(lapply(spec_fields(spec, track), field_properties,
                         max_additional = max_additional), recursive = FALSE)
  do.call(ellmer::type_object, props)
}

scope_category_values <- function(spec) {
  vapply(spec$scope_categories, function(v) v[[1]], character(1))
}

build_screen_type <- function(spec) {
  categories <- scope_category_values(spec)
  definitions <- vapply(spec$scope_categories, function(v) v[[2]], character(1))
  ellmer::type_object(
    scope_reason = ellmer::type_string(
      "One sentence of at most 30 words saying what the paper is about and why it does or does not belong. Fill this first."),
    scope_decision = ellmer::type_enum(
      c("in_scope", "borderline", "out_of_scope"),
      "in_scope: the inclusion rule clearly holds. out_of_scope: an exclusion clearly applies. borderline: you are unsure."),
    scope_category = ellmer::type_enum(
      categories,
      paste0("Why. Use 'in_scope' when the decision is in_scope or borderline.\nOptions:\n",
             options_block(categories, definitions)))
  )
}

build_narrative_type <- function(spec, track) {
  guide <- spec_track(spec, track)$narrative
  ellmer::type_object(
    glossary = ellmer::type_array(
      ellmer::type_object(
        term = ellmer::type_string("An acronym or symbol used in the fields below."),
        meaning = ellmer::type_string("What it stands for, in plain words.")
      ),
      "Every acronym and symbol you use in the fields below, at most 10. Fill this first."
    ),
    summary = ellmer::type_string(paste(
      "A summary of 4 to 6 plain sentences for a reader who has not seen the paper.", guide$summary)),
    key_results = ellmer::type_string(paste(
      "The main results in 3 to 5 plain sentences, with the numbers the paper reports.",
      guide$key_results,
      "For a review or a software paper, describe what it covers or what the software does instead of ranking methods.")),
    key_equations = ellmer::type_array(
      ellmer::type_object(
        name = ellmer::type_string("A short name, for example 'Charting statistic'."),
        latex = ellmer::type_string("The equation in LaTeX with no dollar signs or other delimiters, at most 300 characters."),
        explanation = ellmer::type_string("One sentence that names every symbol in the equation in words.")
      ),
      paste("The 1 to 4 equations that define the paper's method:", guide$key_equations,
            "Use an empty list for papers with no central equations.")
    ),
    limitations_stated = ellmer::type_string(
      "Limitations the authors themselves state, in 1 to 3 sentences. Write 'None stated' if they state none."),
    limitations_unstated = ellmer::type_string(paste(
      "Limitations you notice that the authors do not state, in 2 to 3 sentences. Be specific to this paper.",
      guide$limitations_unstated)),
    future_work_stated = ellmer::type_string(
      "Future work the authors themselves propose, in 1 to 3 sentences. Write 'None stated' if they propose none."),
    future_work_unstated = ellmer::type_string(paste(
      "Research directions you would suggest that the authors do not mention, in 2 to 3 sentences. Be specific to this paper.",
      guide$future_work_unstated))
  )
}

# ---- Generated files ---------------------------------------------------------

# data/tracks.json is generated for external readers; nothing reads it back.
tracks_json_content <- function(spec) {
  lapply(stats::setNames(names(spec$tracks), names(spec$tracks)), function(id) {
    track <- spec$tracks[[id]]
    list(id = id, label = track$label, short_label = track$short_label, query = track$query,
         icon = track$icon, color = track$color, description = track$description,
         metadata_csv = track$metadata_csv, factsheet_csv = track$factsheet_csv,
         schema_version = spec$schema_version)
  })
}

write_tracks_json <- function(spec, path) {
  jsonlite::write_json(tracks_json_content(spec), path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}
