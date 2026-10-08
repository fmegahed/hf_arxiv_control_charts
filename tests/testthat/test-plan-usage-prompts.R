spec <- spec_load(app_path("config", "factsheet_spec.json"))

metadata <- data.frame(id = c("1.1v1", "2.2v3", "3.3v1", "4.4v2", "5.5v1", "6.6v1", "2.2v2"),
                       stringsAsFactors = FALSE)
factsheet <- data.frame(
  paper_id = c("2.2", "3.3", "4.4", "5.5", "6.6"),
  arxiv_version = c(2L, 1L, 2L, 1L, 1L),
  status = c("ok", "failed", "out_of_scope", "failed", "ok"),
  attempts = c(0L, 2L, 0L, 5L, 0L),
  schema_version = c("2.0.0", "2.0.0", "2.0.0", "2.0.0", "1.0.0"),
  stringsAsFactors = FALSE
)
action <- function(plan, id) plan$action[plan$paper_id == id]

test_that("with no factsheet every paper is extracted once, at its latest version", {
  plan <- plan_extraction(metadata, NULL, "2.0.0")
  expect_identical(sort(plan$paper_id), c("1.1", "2.2", "3.3", "4.4", "5.5", "6.6"))
  expect_true(all(plan$action == "extract"))
  expect_identical(plan$id[plan$paper_id == "2.2"], "2.2v3")
})

test_that("each cache and retry rule gives the expected action", {
  plan <- plan_extraction(metadata, factsheet, "2.0.0", max_attempts = 5L)
  expect_identical(action(plan, "1.1"), "extract")       # no record
  expect_identical(action(plan, "2.2"), "new_version")   # ok, but arXiv has v3
  expect_identical(action(plan, "3.3"), "retry")         # failed, attempts remain
  expect_identical(action(plan, "4.4"), "skip")          # out of scope, same version
  expect_identical(action(plan, "5.5"), "skip")          # failed, attempts exhausted
  expect_identical(action(plan, "6.6"), "skip")          # older schema, daily run
})

test_that("a failed row is never treated as done, and retry_failed overrides the attempt cap", {
  plan <- plan_extraction(metadata, factsheet, "2.0.0", max_attempts = 5L, retry_failed = TRUE)
  expect_identical(action(plan, "5.5"), "retry")
  expect_identical(action(plan, "3.3"), "retry")
})

test_that("a backfill re-extracts records made with an older schema", {
  plan <- plan_extraction(metadata, factsheet, "2.0.0", backfill = TRUE)
  expect_identical(action(plan, "6.6"), "reextract")
  expect_identical(action(plan, "4.4"), "skip")
  expect_setequal(pending_papers(plan)$paper_id, c("1.1", "2.2", "3.3", "6.6"))
})

test_that("cost uses fresh, cached and output prices", {
  expect_equal(compute_cost("gpt-6-luna", 1e5, 0, 0), 0.010)
  expect_equal(compute_cost("gpt-6-luna", 1e5, 1e5, 0), 0.001)
  expect_equal(compute_cost("gpt-6-luna", 0, 0, 1e6), 0.50)
  expect_equal(compute_cost("gpt-6.1-sol", 55000, 0, 2000), (55000 * 2 + 2000 * 10) / 1e6)
  expect_equal(compute_cost("gpt-6.1-sol", 55000, 50000, 2000), (5000 * 2 + 50000 * 0.10 + 2000 * 10) / 1e6)
})

test_that("requests above the long-context threshold use the higher rates", {
  expect_equal(compute_cost("gpt-6.1-sol", 481841, 0, 154), (481841 * 4 + 154 * 15) / 1e6)
  expect_equal(compute_cost("gpt-6-luna", 272000, 0, 0), 272000 * 0.10 / 1e6)
  expect_equal(compute_cost("gpt-6-luna", 272001, 0, 0), 272001 * 0.20 / 1e6)
})

test_that("an unknown model gives NA with a warning instead of a wrong number", {
  expect_warning(cost <- compute_cost("mystery-model", 10, 0, 10), "No price")
  expect_true(is.na(cost))
})

test_that("usage accumulates and ignores calls that returned no usage", {
  total <- add_usage(empty_usage(), list(input_tokens = 10, cached_input_tokens = 4,
                                         output_tokens = 2, cost_usd = 0.5))
  total <- add_usage(total, NULL)
  total <- add_usage(total, list(input_tokens = 1, cached_input_tokens = 0, output_tokens = 1, cost_usd = 0.25))
  expect_identical(total$n_calls, 2L)
  expect_equal(c(total$input_tokens, total$cached_input_tokens, total$output_tokens, total$cost_usd),
               c(11, 4, 3, 0.75))
})

test_that("the usage log appends rows under one header", {
  path <- file.path(withr::local_tempdir(), "logs", "usage.csv")
  u <- list(input_tokens = 10, cached_input_tokens = 0, output_tokens = 2, cost_usd = 0.001)
  append_usage_log(path, "1.1", "spc", "classify", "gpt-6-luna", u, "ok")
  append_usage_log(path, "1.1", "spc", "narrate", "gpt-6-luna", u, "ok")
  log <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_identical(nrow(log), 2L)
  expect_identical(log$stage, c("classify", "narrate"))
})

test_that("the screen prompt states the track's inclusion and exclusion rules and the paper", {
  prompt <- screen_user_prompt(spec, "reliability", "Reliable ensembles", "We study calibration.",
                               "stat.ML|cs.LG")
  scope <- spec$tracks$reliability$scope
  for (part in c(scope$include, scope$exclude, scope$example_in, scope$example_out,
                 "Title: Reliable ensembles", "stat.ML, cs.LG", "We study calibration.")) {
    expect_match(prompt, part, fixed = TRUE)
  }
  expect_match(screen_user_prompt(spec, "spc", "T", NA, NA), "Abstract: not available", fixed = TRUE)
})

test_that("the classify prompt carries the gate, the label cap and the catch-all rule", {
  prompt <- classify_user_prompt(spec, "exp_design")
  expect_match(prompt, "design of experiments", fixed = TRUE)
  expect_match(prompt, "proposes or evaluates with its own results", fixed = TRUE)
  expect_match(prompt, paste0("at most ", spec$limits$max_additional_labels), fixed = TRUE)
  expect_match(prompt, NONE_OF_LISTED, fixed = TRUE)
})

test_that("the narrate prompt forbids LaTeX outside equations and includes the labels when given", {
  prompt <- narrate_user_prompt(spec, "spc", "Phase: Phase II")
  expect_match(prompt, "Do not use LaTeX, backslashes or dollar signs", fixed = TRUE)
  expect_match(prompt, "The paper was labelled as follows:\nPhase: Phase II", fixed = TRUE)
  expect_false(grepl("labelled as follows", narrate_user_prompt(spec, "spc", "")))
})

test_that("classify and narrate share one system prompt so the paper is a common prefix", {
  expect_false(grepl("label|describe", PAPER_SYSTEM_PROMPT))
  expect_match(PAPER_SYSTEM_PROMPT, "The paper comes first", fixed = TRUE)
})

test_that("dotenv loading sets missing variables and leaves existing ones alone", {
  path <- file.path(withr::local_tempdir(), ".env")
  writeLines(c("# comment", "QEW_TEST_A=from_file", "QEW_TEST_B = \"quoted value\"", "", "not a pair"), path)
  withr::local_envvar(QEW_TEST_A = "already_set", QEW_TEST_B = NA)
  load_dotenv(path)
  expect_identical(Sys.getenv("QEW_TEST_A"), "already_set")
  expect_identical(Sys.getenv("QEW_TEST_B"), "quoted value")
  expect_silent(load_dotenv(file.path(dirname(path), "absent.env")))
})
