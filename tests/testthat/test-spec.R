spec <- spec_load(app_path("config", "factsheet_spec.json"))

all_fields <- function() {
  unlist(lapply(TRACK_IDS, function(track) spec_fields(spec, track)), recursive = FALSE)
}

test_that("the spec defines the three tracks with everything the pipeline reads", {
  expect_setequal(names(spec$tracks), TRACK_IDS)
  for (track in TRACK_IDS) {
    info <- spec_track(spec, track)
    for (key in c("label", "short_label", "topic_name", "query", "color", "metadata_csv",
                  "factsheet_csv", "scope", "narrative", "core_filters")) {
      expect_false(is.null(info[[key]]), info = paste(track, key))
    }
    expect_setequal(names(info$scope), c("include", "exclude", "example_in", "example_out"))
    expect_setequal(names(info$narrative),
                    c("summary", "key_results", "key_equations", "limitations_unstated",
                      "future_work_unstated"))
  }
  expect_error(spec_track(spec, "nope"), "Unknown track")
})

test_that("users see SPM while internal ids and file names stay spc", {
  expect_match(spec$tracks$spc$label, "SPM")
  expect_identical(spec$tracks$spc$short_label, "SPM")
  expect_identical(spec$tracks$spc$factsheet_csv, "spc_factsheet.csv")
})

test_that("field names are unique within a track and kinds are known", {
  for (track in TRACK_IDS) {
    fields <- spec_fields(spec, track)
    names_ <- vapply(fields, function(f) f$name, character(1))
    expect_identical(anyDuplicated(names_), 0L, info = track)
    expect_true(all(vapply(fields, function(f) f$kind, character(1)) %in%
                      c("single", "primary_additional", "multi", "tristate", "text", "urls")))
    expect_identical(anyDuplicated(spec_label_columns(spec, track)), 0L, info = track)
  }
})

test_that("allowed values are safe to store and to show", {
  for (field in all_fields()) {
    if (!field$kind %in% c("single", "primary_additional", "multi")) next
    values <- field_values(field)
    expect_identical(anyDuplicated(values), 0L, info = field$name)
    expect_false(any(grepl(LIST_SEP, values, fixed = TRUE)), info = field$name)
    expect_false(any(grepl("only", values, ignore.case = TRUE) & !grepl("theory only", values)),
                 info = paste(field$name, "avoid 'only' in labels"))
    expect_false(any(values %in% c("Other", NONE_OF_LISTED)), info = field$name)
    expect_lte(length(values), 18L)
  }
})

test_that("every multi field has a sentinel that is one of its values", {
  for (field in all_fields()) {
    if (field$kind != "multi") next
    expect_true(field$sentinel %in% field_values(field), info = field$name)
  }
})

test_that("the catch-all is offered exactly where the spec says so", {
  for (field in all_fields()) {
    expect_identical(NONE_OF_LISTED %in% field_choices(field), field_flag(field, "other"),
                     info = field$name)
  }
})

test_that("phase is single-valued and separates estimation from Phase I", {
  phase <- spec_field(spec, "spc", "phase")
  expect_identical(phase$kind, "single")
  expect_setequal(field_values(phase),
                  c("Phase I", "Phase II", "Phase I and Phase II", "Self-starting", "Not applicable"))
  expect_match(phase$question, "NOT Phase I")
  expect_true("Estimated from a reference sample" %in%
                field_values(spec_field(spec, "spc", "phase_parameters")))
})

test_that("Bayesian and nonparametric are approaches, not data structures", {
  expect_false(any(c("Bayesian", "Nonparametric") %in% field_values(spec_field(spec, "spc", "chart_family"))))
  expect_true("Bayesian" %in% field_values(spec_field(spec, "spc", "chart_approach")))
})

test_that("reliability separates censoring, measurement and data source", {
  expect_true(all(c("Right-censored", "No censoring") %in%
                    field_values(spec_field(spec, "reliability", "censoring"))))
  expect_true("Degradation measurements" %in%
                field_values(spec_field(spec, "reliability", "measurement_kind")))
  expect_true("Simulated data" %in% field_values(spec_field(spec, "reliability", "data_source")))
  expect_true("Bayesian" %in% field_values(spec_field(spec, "reliability", "inference_paradigm")))
  expect_false("Bayesian" %in% field_values(spec_field(spec, "reliability", "model_family")))
})

test_that("the classify schema has the properties the spec implies, evidence first", {
  for (track in TRACK_IDS) {
    type <- build_classify_type(spec, track)
    props <- names(type@properties)
    expected <- character()
    for (field in spec_fields(spec, track)) {
      n <- field$name
      if (field_flag(field, "evidence")) expected <- c(expected, paste0(n, "_evidence"))
      expected <- c(expected, if (field$kind == "primary_additional")
        c(paste0(n, "_primary"), paste0(n, "_additional")) else n)
      if (field_flag(field, "other")) expected <- c(expected, paste0(n, "_other_term"))
    }
    expect_identical(props, expected, info = track)
  }
})

test_that("enum values sent to the model are exactly the spec's choices", {
  type <- build_classify_type(spec, "spc")
  phase <- type@properties$phase
  expect_identical(phase@values, field_choices(spec_field(spec, "spc", "phase")))
  primary <- type@properties$chart_statistic_primary
  expect_true(NONE_OF_LISTED %in% primary@values)
  additional <- type@properties$chart_statistic_additional
  expect_true(ADDITIONAL_NONE %in% additional@items@values)
  expect_false(NONE_OF_LISTED %in% additional@items@values)
})

test_that("field descriptions carry the question, every option and its definition", {
  field <- spec_field(spec, "reliability", "reliability_topic")
  text <- field_description(field)
  expect_match(text, field$question, fixed = TRUE)
  for (value in field_values(field)) expect_match(text, paste0("- ", value), fixed = TRUE)
  expect_match(text, "Never combine it with a listed option", fixed = TRUE)
  expect_identical(field_description(spec_field(spec, "spc", "sample_size_requirements")),
                   spec_field(spec, "spc", "sample_size_requirements")$question)
})

test_that("list columns are exactly the multi-valued columns", {
  expect_setequal(
    spec_list_cols(spec, "spc"),
    c("chart_approach", "chart_statistic_additional", "chart_statistic", "evaluation_type",
      "performance_metrics", "application_domain_additional", "application_domain",
      "data_source", "software_platform", "software_urls"))
  for (track in TRACK_IDS) {
    expect_true(all(spec_list_cols(spec, track) %in% spec_label_columns(spec, track)))
  }
})

test_that("filters come from the spec and every track has its two core filters", {
  for (track in TRACK_IDS) {
    filters <- spec_filters(spec, track)
    columns <- vapply(filters, function(f) f$column, character(1))
    core <- columns[vapply(filters, function(f) f$tier, character(1)) == "core"]
    expect_true(all(unlist(spec_track(spec, track)$core_filters) %in% core), info = track)
    expect_true(all(c("application_domain", "code_availability") %in% core), info = track)
    expect_true(all(columns %in% factsheet_columns(spec, track)), info = track)
    expect_true(all(vapply(filters, function(f) f$group, character(1)) %in%
                      c("Method", "Data", "Evaluation", "Software")))
  }
})

test_that("the screen and narrative schemas have the expected fields in order", {
  screen <- build_screen_type(spec)
  expect_identical(names(screen@properties), c("scope_reason", "scope_decision", "scope_category"))
  expect_identical(screen@properties$scope_decision@values, c("in_scope", "borderline", "out_of_scope"))
  for (track in TRACK_IDS) {
    narrative <- build_narrative_type(spec, track)
    expect_identical(names(narrative@properties), spec_narrative_columns(spec))
    expect_match(narrative@properties$summary@description, spec_track(spec, track)$narrative$summary,
                 fixed = TRUE)
  }
})

test_that("DOE and reliability narrative guidance does not inherit SPM wording", {
  for (track in c("exp_design", "reliability")) {
    text <- paste(unlist(spec_track(spec, track)$narrative), collapse = " ")
    expect_false(grepl("ARL|run length|control chart|Phase I|SPC", text), info = track)
  }
})

test_that("tracks.json content is generated from the spec", {
  content <- tracks_json_content(spec)
  expect_setequal(names(content), TRACK_IDS)
  expect_identical(content$reliability$query, spec$tracks$reliability$query)
  expect_identical(content$spc$schema_version, spec$schema_version)
  path <- file.path(withr::local_tempdir(), "tracks.json")
  write_tracks_json(spec, path)
  expect_identical(jsonlite::read_json(path)$exp_design$label, spec$tracks$exp_design$label)
})

test_that("every model named in the spec has a price", {
  expect_true(all(unlist(spec$models) %in% names(PRICES)))
})

test_that("every arXiv query fits in one request line", {
  # The arXiv server rejects a request line over 4,094 bytes. 250 bytes are
  # left for the host path and the paging and sorting parameters.
  for (track in TRACK_IDS) {
    encoded <- utils::URLencode(spec$tracks[[track]]$query, reserved = TRUE)
    expect_lt(nchar(encoded, type = "bytes") + 250L, 4094L, label = track)
  }
})
