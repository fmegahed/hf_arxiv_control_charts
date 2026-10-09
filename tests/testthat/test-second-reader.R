spec <- spec_load(app_path("config", "factsheet_spec.json"))

usage <- function(cost = 0.01) list(input_tokens = 1000, cached_input_tokens = 0, output_tokens = 100, cost_usd = cost)
ok <- function(data, cost = 0.01) list(ok = TRUE, data = data, usage = usage(cost))
fail <- function(class) list(ok = FALSE, data = NULL, usage = NULL, error_class = class, error_message = class)

classification <- list(
  paper_type = "New method",
  chart_family_evidence = "a single quality characteristic", chart_family = "Univariate", chart_family_other_term = "n/a",
  chart_approach_evidence = "none", chart_approach = list("None"),
  chart_statistic_evidence = "an EWMA of ranks", chart_statistic_primary = "EWMA",
  chart_statistic_additional = list("CUSUM", "Shewhart-type"), chart_statistic_other_term = "n/a",
  phase_evidence = "the first 150 observations are Phase I data", phase = "Phase I and Phase II",
  phase_parameters = "Estimated from a reference sample",
  assumes_normality = "No", handles_autocorrelation = "No", handles_missing_data = "No",
  evaluation_type = list("Simulation study"), performance_metrics = list("Average run length (ARL)"),
  sample_size_requirements = "Not discussed",
  application_domain_primary = "Manufacturing", application_domain_additional = list("None"),
  application_domain_other_term = "n/a", data_source = list("Simulated data"),
  code_used = "Yes", software_platform = list("R"), code_availability = "Not shared", software_urls = list()
)
narrative <- list(glossary = list(), summary = "A summary.", key_results = "Results.", key_equations = list(),
                  limitations_stated = "None stated", limitations_unstated = "u",
                  future_work_stated = "None stated", future_work_unstated = "f")
screen_in <- ok(list(scope_reason = "A chart.", scope_decision = "in_scope", scope_category = "in_scope"))

# Second reader agrees with the first on every single-answer label unless told otherwise.
agreeing <- list(paper_type = "New method", chart_family = "Univariate", chart_statistic = "EWMA",
                 phase = "Phase I and Phase II", phase_parameters = "Estimated from a reference sample",
                 application_domain = "Manufacturing", code_availability = "Not shared")
second_with <- function(...) {
  labels <- utils::modifyList(agreeing, list(...))
  ok(list(labels = labels, confidence = lapply(labels, function(x) 0.9), text_cut = FALSE, model = "jev-test"), 0.001)
}

make_llm_fake <- function(second = second_with(), tiebreak = NULL) {
  calls <- character(); asked <- NULL; told <- NULL
  list(
    screen = function(track, paper) screen_in,
    classify = function(track, pdf_path) ok(classification),
    narrate = function(track, pdf_path, labels_text) { told <<- labels_text; ok(narrative) },
    second = function(track, paper, pdf_path) { calls <<- c(calls, "second"); second },
    tiebreak = function(track, pdf_path, fields) { calls <<- c(calls, "tiebreak"); asked <<- fields; tiebreak },
    calls = function() calls, asked = function() asked, told = function() told
  )
}
paper <- list(id = "2403.01234v2", title = "A chart", abstract = "We propose a chart.", categories = "stat.ME")
fetch <- function(paper) list(ok = TRUE, path = "x.pdf", truncated = FALSE)
run <- function(llm) extract_paper(paper, "spc", spec, llm, fetch, "gpt-6-luna",
                                   as.POSIXct("2026-10-08 12:00:00", tz = "UTC"))

test_that("the spec marks the main labels of every track for a tie-break", {
  expect_setequal(arbitrated_fields(spec, "spc"), c("chart_family", "chart_statistic", "phase"))
  expect_setequal(arbitrated_fields(spec, "exp_design"), c("design_type", "design_objective"))
  expect_setequal(arbitrated_fields(spec, "reliability"), c("reliability_topic", "model_family"))
  for (track in TRACK_IDS) {
    expect_true(all(unlist(spec_track(spec, track)$core_filters) %in% arbitrated_fields(spec, track)))
  }
  expect_true(all(c(spec$models$second_reader, spec$models$tie_break) %in% names(PRICES)))
})

test_that("the second reader gets one choice question per single-answer label with the spec's options", {
  questions <- second_reader_questions(spec, "spc")
  expect_setequal(names(questions), c("paper_type", "chart_family", "chart_statistic", "phase",
                                      "phase_parameters", "application_domain", "code_availability"))
  phase <- questions$phase
  expect_identical(phase$type, "choice")
  expect_identical(names(phase$criteria), field_choices(spec_field(spec, "spc", "phase")))
  expect_match(phase$instructions, "NOT Phase I", fixed = TRUE)
  expect_match(phase$instructions, "proposes or evaluates with its own results", fixed = TRUE)
  expect_true(NONE_OF_LISTED %in% names(questions$chart_statistic$criteria))
})

test_that("paper text drops the reference list and keeps the start and end of long papers", {
  pages <- c("Title\n\nIntroduction text.", "Method text.\n\nConclusion text.\n\nReferences\n[1] A paper.\n[2] Another.")
  short <- prepare_paper_text(pages, 10000)
  expect_false(short$cut)
  expect_match(short$text, "Conclusion text.", fixed = TRUE)
  expect_false(grepl("Another", short$text, fixed = TRUE))

  early <- prepare_paper_text(c("See the References\nsection below.", paste(rep("body", 50), collapse = " ")), 10000)
  expect_match(early$text, "body body", fixed = TRUE)

  long <- prepare_paper_text(paste0("START ", paste(rep("middle", 5000), collapse = " "), " END"), 1000)
  expect_true(long$cut)
  expect_match(long$text, "^START")
  expect_match(long$text, "END$")
  expect_match(long$text, "middle of the paper omitted", fixed = TRUE)
  expect_lt(nchar(long$text), 1100)
})

test_that("answers outside the allowed options are ignored", {
  parsed <- parse_second_reader(list(phase = list(choice = "Phase II", confidence = 0.8),
                                     chart_family = list(choice = "Made up", confidence = 1),
                                     paper_type = list(choice = "Theory")), spec, "spc")
  expect_identical(parsed$labels, c(paper_type = "Theory", phase = "Phase II"))
  expect_equal(parsed$confidence[["phase"]], 0.8)
  expect_true(is.na(parsed$confidence[["paper_type"]]))
})

test_that("when both readers agree every label is confirmed and no tie-break is requested", {
  llm <- make_llm_fake()
  record <- run(llm)
  expect_identical(llm$calls(), "second")
  expect_identical(record$status, "ok")
  expect_setequal(split_values(record$labels_confirmed), names(agreeing))
  expect_identical(record$labels_disputed, NA_character_)
  expect_identical(record$labels_resolved, NA_character_)
  expect_identical(record$labels_changed, NA_character_)
  expect_identical(record$phase, "Phase I and Phase II")
  expect_identical(record$n_calls, 4L)
})

test_that("a dispute on a main label goes to the tie-break, whose answer and evidence are stored", {
  llm <- make_llm_fake(
    second = second_with(phase = "Phase II"),
    tiebreak = ok(list(phase_evidence = "we propose Phase II charts", phase = "Phase II"), 0.09))
  record <- run(llm)
  expect_identical(llm$calls(), c("second", "tiebreak"))
  expect_identical(llm$asked(), "phase")
  expect_identical(record$phase, "Phase II")
  expect_identical(record$phase_evidence, "we propose Phase II charts")
  expect_identical(record$labels_resolved, "phase")
  expect_identical(record$labels_changed, "phase")
  expect_false("phase" %in% split_values(record$labels_confirmed))
  expect_match(llm$told(), "Phase: Phase II", fixed = TRUE)
  expect_equal(record$cost_usd, 0.01 * 3 + 0.001 + 0.09)
  expect_identical(record$n_calls, 5L)
})

test_that("a tie-break that sides with the first reader resolves the label without changing it", {
  llm <- make_llm_fake(
    second = second_with(chart_family = "Multivariate"),
    tiebreak = ok(list(chart_family_evidence = "one characteristic", chart_family = "Univariate",
                       chart_family_other_term = "n/a")))
  record <- run(llm)
  expect_identical(record$chart_family, "Univariate")
  expect_identical(record$labels_resolved, "chart_family")
  expect_identical(record$labels_changed, NA_character_)
})

test_that("a changed primary label is removed from the additional labels and the combined column rebuilt", {
  llm <- make_llm_fake(
    second = second_with(chart_statistic = "CUSUM"),
    tiebreak = ok(list(chart_statistic_evidence = "a CUSUM is proposed", chart_statistic_primary = "CUSUM",
                       chart_statistic_other_term = "n/a")))
  record <- run(llm)
  expect_identical(record$chart_statistic_primary, "CUSUM")
  expect_identical(record$chart_statistic_additional, "Shewhart-type")
  expect_identical(record$chart_statistic, "CUSUM|Shewhart-type")
  expect_identical(record$chart_statistic_evidence, "a CUSUM is proposed")
})

test_that("a tie-break choosing the catch-all keeps its term", {
  llm <- make_llm_fake(
    second = second_with(chart_statistic = NONE_OF_LISTED),
    tiebreak = ok(list(chart_statistic_evidence = "runs rules", chart_statistic_primary = NONE_OF_LISTED,
                       chart_statistic_other_term = "run rules chart")))
  record <- run(llm)
  expect_identical(record$chart_statistic_primary, NONE_OF_LISTED)
  expect_identical(record$chart_statistic_other_term, "run rules chart")
})

test_that("several disputed main labels go to one tie-break call", {
  llm <- make_llm_fake(
    second = second_with(phase = "Phase II", chart_family = "Multivariate"),
    tiebreak = ok(list(phase = "Phase II", chart_family = "Univariate")))
  record <- run(llm)
  expect_setequal(llm$asked(), c("chart_family", "phase"))
  expect_identical(sum(llm$calls() == "tiebreak"), 1L)
  expect_setequal(split_values(record$labels_resolved), c("chart_family", "phase"))
  expect_identical(record$labels_changed, "phase")
})

test_that("a dispute on a label that is not a main label is recorded but not sent to the tie-break", {
  llm <- make_llm_fake(second = second_with(paper_type = "Theory", application_domain = "Pharmaceutical"))
  record <- run(llm)
  expect_identical(llm$calls(), "second")
  expect_setequal(split_values(record$labels_disputed), c("paper_type", "application_domain"))
  expect_identical(record$paper_type, "New method")
  expect_identical(record$labels_resolved, NA_character_)
})

test_that("a failing second reader or tie-break never fails the paper", {
  no_second <- run(make_llm_fake(second = fail("second_reader_error")))
  expect_identical(no_second$status, "ok")
  expect_true("second_reader_failed" %in% split_values(no_second$qa_flags))
  expect_identical(no_second$labels_confirmed, NA_character_)
  expect_identical(no_second$n_calls, 3L)

  no_tiebreak <- run(make_llm_fake(second = second_with(phase = "Phase II"), tiebreak = fail("rate_limit")))
  expect_identical(no_tiebreak$status, "ok")
  expect_identical(no_tiebreak$phase, "Phase I and Phase II")
  expect_true("tie_break_failed" %in% split_values(no_tiebreak$qa_flags))
  expect_identical(no_tiebreak$labels_disputed, "phase")
  expect_identical(no_tiebreak$labels_resolved, NA_character_)
})

test_that("a tie-break answer outside the options leaves the first reader's label in place", {
  llm <- make_llm_fake(second = second_with(phase = "Phase II"), tiebreak = ok(list(phase = "Both")))
  record <- run(llm)
  expect_identical(record$phase, "Phase I and Phase II")
  expect_identical(record$labels_changed, NA_character_)
})

test_that("without a second reader the pipeline runs as before", {
  llm <- make_llm_fake()
  llm$second <- NULL
  record <- run(llm)
  expect_identical(record$status, "ok")
  expect_identical(record$n_calls, 3L)
  expect_identical(record$labels_confirmed, NA_character_)
})

test_that("a shortened paper text is flagged", {
  cut <- second_with()
  cut$data$text_cut <- TRUE
  expect_true("second_reader_text_cut" %in% split_values(run(make_llm_fake(second = cut))$qa_flags))
})

test_that("a record with a tie-break rebuilds identically from its raw outputs", {
  llm <- make_llm_fake(
    second = second_with(phase = "Phase II", chart_statistic = "CUSUM"),
    tiebreak = ok(list(phase_evidence = "Phase II charts", phase = "Phase II",
                       chart_statistic_evidence = "a CUSUM", chart_statistic_primary = "CUSUM",
                       chart_statistic_other_term = "n/a")))
  record <- run(llm)
  path <- file.path(withr::local_tempdir(), "raw.jsonl")
  append_raw(path, record)
  rebuilt <- record_from_raw(read_raw(path)[[1]], spec, "spc")
  original <- record_to_row(record, spec, "spc")
  expect_equal(as.list(record_to_row(rebuilt, spec, "spc")), as.list(original))
  expect_identical(original$labels_changed, "chart_statistic|phase")
})

test_that("the tie-break schema and prompt cover exactly the disputed fields", {
  type <- build_tiebreak_type(spec, "spc", c("phase", "chart_statistic"))
  expect_identical(names(type@properties),
                   c("phase_evidence", "phase", "chart_statistic_evidence", "chart_statistic_primary",
                     "chart_statistic_other_term"))
  prompt <- tiebreak_user_prompt(spec, "spc", c("phase", "chart_statistic"))
  expect_match(prompt, "Decide only these fields: Phase; Charting statistic.", fixed = TRUE)
})

test_that("the review columns are part of the factsheet layout", {
  for (track in TRACK_IDS) {
    expect_true(all(c("labels_confirmed", "labels_disputed", "labels_resolved", "labels_changed") %in%
                      factsheet_columns(spec, track)))
  }
})
