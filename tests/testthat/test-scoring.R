sheet <- tibble::tibble(
  paper_id = c("1", "2", "3", "4"),
  status = c("ok", "ok", "out_of_scope", "failed"),
  phase = c("Phase II", "Phase I and Phase II", NA, NA),
  chart_approach = c("None", "Bayesian|Robust", NA, NA),
  chart_statistic = c("EWMA|MEWMA", "None of the listed", NA, NA),
  chart_statistic_primary = c("EWMA", "None of the listed", NA, NA),
  qa_flags = c(NA, "latex_invalid|math_repaired", NA, NA)
)
row <- function(id) sheet[sheet$paper_id == id, ]

test_that("equals, includes and excludes are checked on whole values", {
  expect_true(check_assertion(row("1"), "phase", "equals", "Phase II"))
  expect_false(check_assertion(row("2"), "phase", "equals", "Phase II"))
  expect_true(check_assertion(row("1"), "chart_statistic", "includes", "MEWMA"))
  expect_false(check_assertion(row("1"), "chart_statistic", "includes", "EWM"))
  expect_true(check_assertion(row("1"), "chart_approach", "excludes", "Bayesian"))
  expect_false(check_assertion(row("2"), "chart_approach", "excludes", "Bayesian"))
  expect_error(check_assertion(row("1"), "phase", "matches", "x"), "Unknown assertion")
})

test_that("scope assertions read the record status", {
  expect_true(check_assertion(row("3"), "scope", "equals", "out_of_scope"))
  expect_false(check_assertion(row("1"), "scope", "equals", "out_of_scope"))
  expect_true(check_assertion(row("1"), "scope", "equals", "in_scope"))
})

test_that("a label assertion fails on a screened-out paper and is NA when the paper has no usable record", {
  expect_false(check_assertion(row("3"), "phase", "equals", "Phase II"))
  expect_true(is.na(check_assertion(row("4"), "phase", "equals", "Phase II")))
  expect_true(is.na(check_assertion(row("absent"), "phase", "equals", "Phase II")))
  expect_true(is.na(check_assertion(NULL, "scope", "equals", "in_scope")))
})

test_that("score_assertions evaluates each row against its track's factsheet", {
  assertions <- data.frame(
    paper_id = c("1", "2", "3", "9"), track = c("spc", "spc", "spc", "reliability"),
    field = c("phase", "chart_approach", "scope", "scope"),
    assertion = c("equals", "excludes", "equals", "equals"),
    value = c("Phase II", "Bayesian", "out_of_scope", "out_of_scope"), stringsAsFactors = FALSE)
  scored <- score_assertions(assertions, list(spc = sheet))
  expect_identical(scored$passed, c(TRUE, FALSE, TRUE, NA))
})

test_that("agreement compares papers that are ok in both runs", {
  other <- sheet
  other$phase[1] <- "Phase I"
  other$status[2] <- "failed"
  result <- agreement(sheet, other, "phase")
  expect_identical(result$n, 1L)
  expect_identical(result$agree, 0)
  expect_identical(result$differing, "1")
  expect_identical(agreement(sheet, sheet, "phase")$agree, 1)
  expect_identical(agreement(sheet, sheet, "qa_flags")$agree, 1)
  expect_true(is.na(agreement(sheet[0, ], sheet, "phase")$agree))
})

test_that("catch-all and flag rates are shares of ok records", {
  expect_identical(catch_all_rate(sheet, "chart_statistic_primary"), 0.5)
  expect_identical(flag_rate(sheet, "latex_invalid"), 0.5)
  expect_identical(flag_rate(sheet, "pdf_truncated"), 0)
  expect_true(is.na(catch_all_rate(sheet[0, ], "chart_statistic_primary")))
})

test_that("evidence quotes are matched ignoring case, punctuation and line breaks", {
  text <- "We propose a\nnon-parametric EWMA control chart; it monitors location."
  expect_true(evidence_found("a non-parametric EWMA control chart", text))
  expect_true(evidence_found("Non parametric ewma control chart, it monitors...", text))
  expect_false(evidence_found("a Bayesian CUSUM control chart is proposed", text))
  expect_true(is.na(evidence_found("Section 3.2", text)))
  expect_true(is.na(evidence_found("EWMA chart", text)))
  expect_true(is.na(evidence_found(NA_character_, text)))
})
