spec <- spec_load(app_path("config", "factsheet_spec.json"))
mapping <- read_bridge(app_path("config", "bridge_v1_v2.csv"))

test_that("every mapping row targets a real v2 field and an allowed value", {
  for (i in seq_len(nrow(mapping))) {
    tracks <- if (mapping$track[i] == "all") TRACK_IDS else mapping$track[i]
    for (track in tracks) {
      field <- spec_field(spec, track, mapping$new_column[i])
      expect_true(mapping$new_value[i] %in% field_choices(field),
                  info = paste(track, mapping$new_column[i], mapping$new_value[i]))
    }
  }
})

test_that("every value in the frozen v1 factsheets is mapped or explicitly dropped", {
  frozen <- app_path("data", "frozen", "v1")
  skip_if_not(dir.exists(frozen), "v1 snapshot not present")
  for (track in TRACK_IDS) {
    v1 <- read_factsheet(file.path(frozen, paste0(track, "_factsheet.csv")))
    expect_identical(unmapped_v1_values(v1, track, mapping), character(0), info = track)
  }
})

v1_row <- function(...) {
  base <- list(
    id = "2403.01234v2", llm_model = "gpt-5.2-2025-12-11", extracted_at = "2026-02-01",
    is_spc_paper = "TRUE", chart_family = "Univariate|Bayesian|Other", chart_statistic = "Shewhart|Other",
    phase = "Phase II|Both", application_domain = "Theoretical/simulation only",
    assumes_normality = "FALSE", handles_autocorrelation = NA, handles_missing_data = NA,
    evaluation_type = "Simulation study|Other", performance_metrics = "ARL (Average Run Length)",
    sample_size_requirements = "Not discussed", code_used = "TRUE", software_platform = "R",
    code_availability_source = "Public repository (GitHub/GitLab)", software_urls = "https://github.com/x/y",
    summary = "Costs $5 with $\\delta = 1$.", key_equations = "$Z_t$", key_results = "r",
    limitations_stated = "None stated", limitations_unstated = "u",
    future_work_stated = "None stated", future_work_unstated = "f"
  )
  tibble::as_tibble(utils::modifyList(base, list(...)))
}

test_that("an in-scope SPC row is translated field by field", {
  out <- bridge_v1_to_v2(v1_row(), "spc", spec, mapping)
  expect_identical(out$status, "ok")
  expect_identical(out$paper_id, "2403.01234")
  expect_identical(out$schema_version, BRIDGED_SCHEMA_VERSION)
  expect_identical(out$chart_family, "Univariate")
  expect_identical(out$chart_approach, "Bayesian")
  expect_identical(out$chart_statistic_primary, "Shewhart-type")
  expect_identical(out$chart_statistic_other_term, NA_character_)
  expect_identical(out$phase, "Phase I and Phase II")
  expect_identical(out$application_domain_primary, "No specific application (general methodology)")
  expect_identical(out$data_source, "Simulated data")
  expect_identical(out$evaluation_type, "Simulation study")
  expect_identical(out$code_availability, "Public repository")
  expect_true(out$code_public)
  expect_false(out$assumes_normality)
  expect_identical(out$summary, "Costs USD 5 with \\(\\delta = 1\\).")
})

test_that("a value that was only 'Other' becomes the catch-all with an unspecified term", {
  out <- bridge_v1_to_v2(v1_row(chart_statistic = "Other"), "spc", spec, mapping)
  expect_identical(out$chart_statistic_primary, NONE_OF_LISTED)
  expect_identical(out$chart_statistic_other_term, "unspecified")
})

test_that("self-starting moves from chart family to phase", {
  out <- bridge_v1_to_v2(v1_row(chart_family = "Univariate|Self-starting", phase = "Phase II"),
                         "spc", spec, mapping)
  expect_identical(out$phase, "Self-starting")
  expect_identical(out$chart_family, "Univariate")
})

test_that("out-of-scope and failed v1 rows carry no labels", {
  out <- bridge_v1_to_v2(v1_row(is_spc_paper = "FALSE"), "spc", spec, mapping)
  expect_identical(out$status, "out_of_scope")
  expect_true(is.na(out$chart_family))
  failed <- bridge_v1_to_v2(v1_row(summary = NA, is_spc_paper = NA), "spc", spec, mapping)
  expect_identical(c(failed$status, failed$error_class), c("failed", "legacy_failure"))
})

test_that("bridging keeps one row per paper, the latest version", {
  both <- dplyr::bind_rows(v1_row(id = "2403.01234v1", key_results = "old"), v1_row())
  out <- bridge_v1_to_v2(both, "spc", spec, mapping)
  expect_identical(nrow(out), 1L)
  expect_identical(out$id, "2403.01234v2")
})

test_that("the frozen v1 factsheets bridge into valid v2 rows", {
  frozen <- app_path("data", "frozen", "v1")
  skip_if_not(dir.exists(frozen), "v1 snapshot not present")
  for (track in TRACK_IDS) {
    v1 <- read_factsheet(file.path(frozen, paste0(track, "_factsheet.csv")))
    out <- bridge_v1_to_v2(v1, track, spec, mapping)
    expect_identical(names(out), factsheet_columns(spec, track))
    expect_identical(anyDuplicated(out$paper_id), 0L)
    expect_true(all(out$status %in% c("ok", "out_of_scope", "failed")))
    for (field in spec_fields(spec, track)) {
      if (!field$kind %in% c("single", "multi")) next
      seen <- count_values(out[[field$name]])$value
      expect_true(all(seen %in% field_choices(field)), info = paste(track, field$name))
    }
  }
})
