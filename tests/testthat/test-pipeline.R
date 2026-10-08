spec <- spec_load(app_path("config", "factsheet_spec.json"))

metadata <- tibble::tibble(
  id = c("1.1v1", "2.2v1", "3.3v1", "4.4v1"),
  title = paste("Paper", 1:4), abstract = "An abstract.", categories = "stat.ME"
)
usage <- function() list(input_tokens = 1000, cached_input_tokens = 0, output_tokens = 100, cost_usd = 0.01)
ok <- function(data) list(ok = TRUE, data = data, usage = usage())
fail <- function(class) list(ok = FALSE, data = NULL, usage = NULL, error_class = class, error_message = class)
screen_out <- list(scope_reason = "Another field.", scope_decision = "out_of_scope",
                   scope_category = "other_field_entirely")

# Every paper is screened out unless listed in `failing`.
llm_screening <- function(failing = list()) {
  seen <- character()
  list(
    screen = function(track, paper) {
      seen <<- c(seen, paper$id)
      if (!is.null(failing[[paper$id]])) fail(failing[[paper$id]]) else ok(screen_out)
    },
    classify = function(...) stop("not expected"),
    narrate = function(...) stop("not expected"),
    seen = function() seen
  )
}
no_pdf <- function(paper) stop("not expected")
quiet <- function(...) invisible(NULL)
run <- function(llm, factsheet = NULL, dir = withr::local_tempdir(.local_envir = parent.frame()), ...) {
  run_track("spc", spec, metadata, factsheet, llm, no_pdf, "gpt-6-luna",
            factsheet_path = file.path(dir, "spc_factsheet.csv"),
            raw_path = file.path(dir, "raw", "spc.jsonl"), log = quiet, ...)
}

test_that("a run processes every pending paper, writes the factsheet and the raw log", {
  dir <- withr::local_tempdir()
  out <- run(llm_screening(), dir = dir)
  expect_identical(out$processed, 4L)
  expect_identical(out$remaining, 0L)
  expect_true(is.na(out$stopped))
  expect_equal(out$spent_usd, 0.04)
  saved <- coerce_factsheet(read_factsheet(file.path(dir, "spc_factsheet.csv")), spec, "spc")
  expect_identical(saved$paper_id, c("1.1", "2.2", "3.3", "4.4"))
  expect_true(all(saved$status == "out_of_scope"))
  expect_identical(length(read_raw(file.path(dir, "raw", "spc.jsonl"))), 4L)
})

test_that("a second run skips finished papers and retries failed ones", {
  dir <- withr::local_tempdir()
  first <- run(llm_screening(failing = list("2.2v1" = "timeout")), dir = dir)
  expect_identical(first$factsheet$status[first$factsheet$paper_id == "2.2"], "failed")
  expect_identical(first$factsheet$attempts[first$factsheet$paper_id == "2.2"], 1L)

  llm <- llm_screening()
  second <- run(llm, factsheet = first$factsheet, dir = dir)
  expect_identical(llm$seen(), "2.2v1")
  expect_identical(second$processed, 1L)
  expect_true(all(second$factsheet$status == "out_of_scope"))
  expect_identical(nrow(second$factsheet), 4L)
})

test_that("running out of credit stops the run without marking the paper as failed", {
  out <- run(llm_screening(failing = list("3.3v1" = "no_credit")))
  expect_identical(out$stopped, "no_credit")
  expect_identical(out$processed, 2L)
  expect_identical(out$factsheet$paper_id, c("1.1", "2.2"))
  expect_identical(out$remaining, 2L)
})

test_that("paper, spend and time limits each stop the run and report what is left", {
  by_count <- run(llm_screening(), max_papers = 3)
  expect_identical(c(by_count$stopped, by_count$processed, by_count$remaining), c("max_papers", "3", "1"))

  by_spend <- run(llm_screening(), max_spend_usd = 0.015)
  expect_identical(by_spend$stopped, "max_spend")
  expect_identical(by_spend$processed, 2L)

  ticks <- 0
  clock <- function() { ticks <<- ticks + 1; as.POSIXct("2026-10-08", tz = "UTC") + ticks * 60 }
  by_time <- run(llm_screening(), time_budget_min = 5, now = clock)
  expect_identical(by_time$stopped, "time_budget")
  expect_lt(by_time$processed, 4L)
})

test_that("shards partition the papers with no overlap and nothing left out", {
  ids <- sprintf("24%02d.%05d", 1:40, 1:40)
  membership <- vapply(1:3, function(s) in_shard(ids, s, 3L), logical(length(ids)))
  expect_true(all(rowSums(membership) == 1))
  expect_true(all(colSums(membership) > 0))
  expect_true(all(in_shard(ids)))

  a <- llm_screening(); b <- llm_screening()
  run(a, shard = 1L, n_shards = 2L); run(b, shard = 2L, n_shards = 2L)
  expect_setequal(c(a$seen(), b$seen()), metadata$id)
  expect_identical(length(intersect(a$seen(), b$seen())), 0L)
})

test_that("a factsheet rebuilt from raw outputs equals the one written during the run", {
  full_classification <- list(
    paper_type = "New method", chart_family = "Univariate", chart_family_evidence = "one variable",
    chart_family_other_term = "n/a", chart_approach = list("None"), chart_approach_evidence = "none",
    chart_statistic_primary = "CUSUM", chart_statistic_additional = list("None"),
    chart_statistic_other_term = "n/a", chart_statistic_evidence = "a CUSUM",
    phase = "Phase II", phase_evidence = "online", phase_parameters = "Known parameters assumed",
    assumes_normality = "Yes", handles_autocorrelation = "No", handles_missing_data = "No",
    evaluation_type = list("Simulation study"), performance_metrics = list("Average run length (ARL)"),
    sample_size_requirements = "Not discussed", application_domain_primary = "Manufacturing",
    application_domain_additional = list("None"), application_domain_other_term = "n/a",
    data_source = list("Simulated data"), code_used = "Yes", software_platform = list("R"),
    code_availability = "Not shared", software_urls = list())
  narrative <- list(glossary = list(), summary = "Costs $5.", key_results = "r", key_equations = list(),
                    limitations_stated = "None stated", limitations_unstated = "u",
                    future_work_stated = "None stated", future_work_unstated = "f")
  llm <- list(
    screen = function(track, paper) if (paper$id == "1.1v1")
      ok(list(scope_reason = "A chart.", scope_decision = "in_scope", scope_category = "in_scope")) else ok(screen_out),
    classify = function(track, pdf_path) ok(full_classification),
    narrate = function(track, pdf_path, labels_text) ok(narrative)
  )
  dir <- withr::local_tempdir()
  out <- run_track("spc", spec, metadata, NULL, llm, function(p) list(ok = TRUE, path = "x", truncated = TRUE),
                   "gpt-6-luna", factsheet_path = file.path(dir, "f.csv"),
                   raw_path = file.path(dir, "raw.jsonl"), log = quiet)
  expect_identical(out$factsheet$status[1], "ok")
  expect_identical(out$factsheet$summary[1], "Costs USD 5.")

  rebuilt <- rebuild_factsheet(read_raw(file.path(dir, "raw.jsonl")), spec, "spc")
  expect_equal(as.list(rebuilt), as.list(out$factsheet))
})

test_that("command-line arguments parse into named values and flags", {
  args <- parse_cli_args(c("--track", "spc", "--max-papers", "50", "--retry-failed", "--model", "gpt-6-luna"),
                         defaults = list(track = "all", shard = "1"))
  expect_identical(args$track, "spc")
  expect_identical(args$max_papers, "50")
  expect_true(args$retry_failed)
  expect_identical(args$shard, "1")
  expect_identical(parse_cli_args(character(0), list(a = 1))$a, 1)
})
