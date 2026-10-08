test_that("collapse and split round-trip single, multiple and empty values", {
  for (x in list("CUSUM", c("CUSUM", "EWMA"), c("Phase I and Phase II", "Self-starting"))) {
    expect_identical(split_values(collapse_values(x)), x)
  }
  expect_identical(collapse_values(character(0)), NA_character_)
  expect_identical(collapse_values(NULL), NA_character_)
  expect_identical(collapse_values(c(NA, "")), NA_character_)
  expect_identical(split_values(NA_character_), character(0))
  expect_identical(split_values(""), character(0))
})

test_that("collapse trims whitespace, drops blanks and accepts lists", {
  expect_identical(collapse_values(list(" a ", "", NA, "b")), "a|b")
})

test_that("has_value matches whole elements, not substrings", {
  x <- c("Bayesian design|Optimal design", "Bayesian", NA, "")
  expect_identical(has_value(x, "Bayesian"), c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(has_value(x, "Optimal design"), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("count_values tabulates elements and honours exclusions", {
  x <- c("A|B", "A", NA, "B|C|A")
  counts <- count_values(x)
  expect_identical(counts$value[1], "A")
  expect_identical(counts$n[counts$value == "A"], 3L)
  expect_identical(counts$n[counts$value == "C"], 1L)
  expect_false("B" %in% count_values(x, exclude = "B")$value)
  expect_identical(nrow(count_values(c(NA, ""))), 0L)
})

test_that("factsheets keep their values and missing cells through a write and read", {
  path <- file.path(withr::local_tempdir(), "sub", "f.csv")
  df <- tibble::tibble(
    paper_id = c("2403.01230", "math/0406049"),
    arxiv_version = c(2L, NA),
    code_public = c(TRUE, NA),
    tags = c("A|B", NA),
    cost_usd = c(0.0123, NA),
    extracted_at = c("2026-10-08T12:00:00Z", NA),
    summary = c("Costs USD 5, with \\(x < 1\\) and a \"quote\",\nnew line", NA)
  )
  write_factsheet_atomic(df, path)
  back <- read_factsheet(path)
  expect_false(file.exists(paste0(path, ".tmp")))
  expect_true(all(vapply(back, is.character, logical(1))))
  expect_identical(back$paper_id, df$paper_id)
  expect_identical(as.integer(back$arxiv_version), df$arxiv_version)
  expect_identical(as.logical(back$code_public), df$code_public)
  expect_identical(back$tags, df$tags)
  expect_equal(as.numeric(back$cost_usd), df$cost_usd)
  expect_identical(back$extracted_at, df$extracted_at)
  expect_identical(back$summary, df$summary)
  expect_null(read_factsheet(file.path(dirname(path), "absent.csv")))
})
