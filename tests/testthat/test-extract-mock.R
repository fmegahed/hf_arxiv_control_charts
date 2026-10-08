spec <- spec_load(app_path("config", "factsheet_spec.json"))

usage <- function(input = 1000, cached = 0, output = 100) {
  list(input_tokens = input, cached_input_tokens = cached, output_tokens = output,
       cost_usd = compute_cost("gpt-6-luna", input, cached, output))
}
ok <- function(data, ...) list(ok = TRUE, data = data, usage = usage(...))
fail <- function(class, message) list(ok = FALSE, data = NULL, usage = NULL,
                                      error_class = class, error_message = message)

good_classification <- list(
  paper_type = "New method",
  chart_family_evidence = "a single quality characteristic", chart_family = "Univariate",
  chart_family_other_term = "n/a",
  chart_approach_evidence = "distribution-free", chart_approach = list("Nonparametric (distribution-free)"),
  chart_statistic_evidence = "an EWMA of ranks", chart_statistic_primary = "EWMA",
  chart_statistic_additional = list("None"), chart_statistic_other_term = "n/a",
  phase_evidence = "online monitoring", phase = "Phase II",
  phase_parameters = "Estimated from a reference sample",
  assumes_normality = "No", handles_autocorrelation = "No", handles_missing_data = "Unclear or not applicable",
  evaluation_type = list("Simulation study", "Real-data example"),
  performance_metrics = list("Average run length (ARL)"),
  sample_size_requirements = "Not discussed",
  application_domain_primary = "Manufacturing", application_domain_additional = list("None"),
  application_domain_other_term = "n/a",
  data_source = list("Simulated data", "Real data: field or operational"),
  code_used = "Yes", software_platform = list("R"), code_availability = "Public repository",
  software_urls = list("https://github.com/x/y")
)
good_narrative <- list(
  glossary = list(list(term = "EWMA", meaning = "exponentially weighted moving average")),
  summary = "An exponentially weighted moving average (EWMA) chart of ranks is proposed.",
  key_results = "It detects small shifts faster than competitors.",
  key_equations = list(list(name = "Statistic", latex = "Z_t = \\lambda R_t + (1-\\lambda) Z_{t-1}",
                            explanation = "Z_t is the statistic, R_t the rank and lambda the weight.")),
  limitations_stated = "None stated", limitations_unstated = "Independence is assumed.",
  future_work_stated = "None stated", future_work_unstated = "Extend to autocorrelated data."
)

# A fake llm that records which stages were called.
fake_llm <- function(screen, classify = ok(good_classification, 50000),
                     narrate = ok(good_narrative, 50000, 49000)) {
  calls <- character()
  list(
    screen = function(track, paper) { calls <<- c(calls, "screen"); screen },
    classify = function(track, pdf_path) { calls <<- c(calls, "classify"); classify },
    narrate = function(track, pdf_path, labels_text) {
      calls <<- c(calls, "narrate")
      attr(narrate, "labels_text") <- labels_text
      narrate
    },
    calls = function() calls
  )
}
in_scope <- ok(list(scope_reason = "Proposes a control chart.", scope_decision = "in_scope",
                    scope_category = "in_scope"), 600, 0, 40)
fetch_ok <- function(paper) list(ok = TRUE, path = "x.pdf", truncated = FALSE)
paper <- list(id = "2403.01234v2", title = "A chart", abstract = "We propose a chart.", categories = "stat.ME")
now <- as.POSIXct("2026-10-08 12:00:00", tz = "UTC")

test_that("an in-scope paper runs all three stages and yields a complete ok record", {
  llm <- fake_llm(in_scope)
  record <- extract_paper(paper, "spc", spec, llm, fetch_ok, "gpt-6-luna", now)

  expect_identical(llm$calls(), c("screen", "classify", "narrate"))
  expect_identical(record$status, "ok")
  expect_identical(record$paper_id, "2403.01234")
  expect_identical(record$arxiv_version, 2L)
  expect_identical(record$schema_version, spec$schema_version)
  expect_identical(record$extracted_at, "2026-10-08T12:00:00Z")
  expect_identical(record$chart_statistic, "EWMA")
  expect_identical(record$phase, "Phase II")
  expect_false(record$assumes_normality)
  expect_true(record$code_public)
  expect_identical(record$scope_decision, "in_scope")
  expect_identical(validate_record(record, spec, "spc"), character(0))
})

test_that("usage is summed over the three calls, with cached tokens priced lower", {
  record <- extract_paper(paper, "spc", spec, fake_llm(in_scope), fetch_ok, "gpt-6-luna", now)
  expect_identical(record$n_calls, 3L)
  expect_equal(record$input_tokens, 600 + 50000 + 50000)
  expect_equal(record$cached_input_tokens, 49000)
  expect_equal(record$output_tokens, 40 + 100 + 100)
  expected <- (600 * 0.10 + 40 * 0.50 + 50000 * 0.10 + 100 * 0.50 +
                 1000 * 0.10 + 49000 * 0.01 + 100 * 0.50) / 1e6
  expect_equal(record$cost_usd, expected)
})

test_that("an out-of-scope paper stops after the screen and gets no labels", {
  out <- ok(list(scope_reason = "About machine-learning predictions.", scope_decision = "out_of_scope",
                 scope_category = "different_meaning_of_keyword"), 600, 0, 40)
  llm <- fake_llm(out)
  record <- extract_paper(paper, "reliability", spec, llm, fetch_ok, "gpt-6-luna", now)
  expect_identical(llm$calls(), "screen")
  expect_identical(record$status, "out_of_scope")
  expect_identical(record$scope_category, "different_meaning_of_keyword")
  expect_null(record$reliability_topic)
  expect_identical(record$n_calls, 1L)
})

test_that("a borderline paper is extracted", {
  borderline <- ok(list(scope_reason = "Unclear.", scope_decision = "borderline",
                        scope_category = "in_scope"))
  llm <- fake_llm(borderline)
  record <- extract_paper(paper, "spc", spec, llm, fetch_ok, "gpt-6-luna", now)
  expect_identical(llm$calls(), c("screen", "classify", "narrate"))
  expect_identical(record$status, "ok")
  expect_identical(record$scope_decision, "borderline")
})

test_that("a failure at any stage gives a failed record with the error and the usage so far", {
  screen_fail <- extract_paper(paper, "spc", spec, fake_llm(fail("rate_limit", "429")), fetch_ok, "gpt-6-luna", now)
  expect_identical(c(screen_fail$status, screen_fail$error_class), c("failed", "rate_limit"))
  expect_identical(screen_fail$n_calls, 0L)

  llm <- fake_llm(in_scope, classify = fail("api_error", "boom\nline two"))
  classify_fail <- extract_paper(paper, "spc", spec, llm, fetch_ok, "gpt-6-luna", now)
  expect_identical(llm$calls(), c("screen", "classify"))
  expect_identical(classify_fail$status, "failed")
  expect_identical(classify_fail$error_message, "boom line two")
  expect_identical(classify_fail$n_calls, 1L)
  expect_null(classify_fail$summary)

  narrate_fail <- extract_paper(paper, "spc", spec, fake_llm(in_scope, narrate = fail("parse_error", "bad json")),
                                fetch_ok, "gpt-6-luna", now)
  expect_identical(c(narrate_fail$status, narrate_fail$error_class), c("failed", "parse_error"))
  expect_identical(narrate_fail$n_calls, 2L)
  expect_null(narrate_fail$chart_statistic)
})

test_that("a PDF that cannot be fetched fails before any PDF call; a cut PDF is flagged", {
  llm <- fake_llm(in_scope)
  no_pdf <- function(paper) list(ok = FALSE, error_class = "pdf_download", error_message = "HTTP 404")
  record <- extract_paper(paper, "spc", spec, llm, no_pdf, "gpt-6-luna", now)
  expect_identical(llm$calls(), "screen")
  expect_identical(c(record$status, record$error_class, record$error_message),
                   c("failed", "pdf_download", "HTTP 404"))

  cut <- function(paper) list(ok = TRUE, path = "x.pdf", truncated = TRUE)
  record <- extract_paper(paper, "spc", spec, fake_llm(in_scope), cut, "gpt-6-luna", now)
  expect_true("pdf_truncated" %in% split_values(record$qa_flags))
})

test_that("model output missing a required label is a validation failure, not an ok record", {
  incomplete <- good_classification
  incomplete$phase <- "Both"
  record <- extract_paper(paper, "spc", spec, fake_llm(in_scope, classify = ok(incomplete)),
                          fetch_ok, "gpt-6-luna", now)
  expect_identical(c(record$status, record$error_class), c("failed", "validation_error"))
  expect_match(record$error_message, "phase")
  expect_null(record$chart_family)
})

test_that("the narrate stage is told the labels in plain words", {
  seen <- NULL
  llm <- fake_llm(in_scope)
  llm$narrate <- function(track, pdf_path, labels_text) { seen <<- labels_text; ok(good_narrative) }
  extract_paper(paper, "spc", spec, llm, fetch_ok, "gpt-6-luna", now)
  expect_match(seen, "Charting statistic: EWMA", fixed = TRUE)
  expect_match(seen, "Phase: Phase II", fixed = TRUE)
  expect_match(seen, "How it is evaluated: Simulation study; Real-data example", fixed = TRUE)
})

test_that("records become typed rows with the full column set", {
  ok_record <- extract_paper(paper, "spc", spec, fake_llm(in_scope), fetch_ok, "gpt-6-luna", now)
  failed <- extract_paper(paper, "spc", spec, fake_llm(fail("timeout", "t")), fetch_ok, "gpt-6-luna", now)
  rows <- dplyr::bind_rows(record_to_row(ok_record, spec, "spc"), record_to_row(failed, spec, "spc"))
  expect_identical(names(rows), factsheet_columns(spec, "spc"))
  expect_type(rows$assumes_normality, "logical")
  expect_type(rows$arxiv_version, "integer")
  expect_type(rows$cost_usd, "double")
  expect_type(rows$chart_family, "character")
  expect_true(is.na(rows$chart_family[2]))
})

test_that("a factsheet survives a write, read and type coercion unchanged", {
  record <- extract_paper(paper, "spc", spec, fake_llm(in_scope), fetch_ok, "gpt-6-luna", now)
  row <- record_to_row(record, spec, "spc")
  row$attempts <- 0L
  path <- file.path(withr::local_tempdir(), "f.csv")
  write_factsheet_atomic(row, path)
  back <- coerce_factsheet(read_factsheet(path), spec, "spc")
  expect_equal(as.list(back), as.list(row))
})

test_that("the status invariant holds: ok rows are complete, other rows carry no labels", {
  ok_row <- record_to_row(extract_paper(paper, "spc", spec, fake_llm(in_scope), fetch_ok, "gpt-6-luna", now), spec, "spc")
  bad_row <- record_to_row(extract_paper(paper, "spc", spec, fake_llm(fail("timeout", "t")), fetch_ok, "gpt-6-luna", now), spec, "spc")
  required <- required_columns(spec, "spc")
  expect_false(any(is.na(unlist(ok_row[required]))))
  expect_true(all(is.na(unlist(bad_row[c(spec_label_columns(spec, "spc"), spec_narrative_columns(spec))]))))
})

test_that("merging keeps one row per paper and never lets a failure overwrite a good record", {
  row <- function(id, status) {
    r <- record_to_row(list(paper_id = id, id = paste0(id, "v1"), status = status), spec, "spc")
    r
  }
  sheet <- merge_records(NULL, dplyr::bind_rows(row("1", "ok"), row("2", "failed")))
  expect_identical(sheet$attempts, c(0L, 1L))

  sheet <- merge_records(sheet, row("2", "failed"))
  expect_identical(sheet$attempts[sheet$paper_id == "2"], 2L)

  sheet <- merge_records(sheet, row("1", "failed"))
  expect_identical(sheet$status[sheet$paper_id == "1"], "ok")

  sheet <- merge_records(sheet, row("2", "ok"))
  expect_identical(sheet$status[sheet$paper_id == "2"], "ok")
  expect_identical(sheet$attempts[sheet$paper_id == "2"], 0L)

  sheet <- merge_records(sheet, row("3", "out_of_scope"))
  expect_identical(sheet$paper_id, c("1", "2", "3"))
})

test_that("classify_error sorts provider messages into retryable classes", {
  expect_identical(classify_error("HTTP 429 Too Many Requests. You have no credits remaining"), "no_credit")
  expect_identical(classify_error("HTTP 429 Too Many Requests. Rate limit reached"), "rate_limit")
  expect_identical(classify_error("Request timed out"), "timeout")
  expect_identical(classify_error("This model's maximum context length is 1050000 tokens"), "pdf_too_large")
  expect_identical(classify_error("Failed to parse JSON"), "parse_error")
  expect_identical(classify_error("HTTP 500 Internal Server Error"), "api_error")
})

test_that("an unsupported provider is an error, not a silent default", {
  expect_error(create_chat("ollama", "x"), "Unsupported provider")
})
