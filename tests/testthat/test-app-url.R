YEARS <- c(2002L, 2026L)

full_state <- function() {
  state <- new_filter_state("spc")
  state$year_from <- 2020L
  state$year_to <- 2025L
  state <- set_condition(state, "chart_approach", c("Nonparametric (distribution-free)", "Bayesian"), "spc")
  state <- set_condition(state, "chart_statistic", "EWMA", "spc", role = "primary")
  state <- set_condition(state, "data_source", "Real data: field or operational")
  state$public_code <- TRUE
  state$real_data <- TRUE
  state$include_screened <- TRUE
  state$residual <- "wind turbines & gearboxes"
  state$sort <- "relevance"
  state
}

test_that("the landing page has no query string and decodes back to itself", {
  expect_equal(encode_view(new_view()), "")
  expect_equal(decode_view("", TEST_SPEC, YEARS), new_view())
  expect_equal(decode_view(NULL, TEST_SPEC, YEARS), new_view())
})

test_that("a view survives the round trip through the URL", {
  views <- list(
    new_view("browse", filters = new_filter_state("spc")),
    new_view("browse", filters = new_filter_state()),
    new_view("browse", tab = "landscape", paper = "2501.00001", filters = full_state()),
    new_view("browse", tab = "library", filters = utils::modifyList(
      new_filter_state("reliability"), list(keywords = c("wind", "turbines"), reviews_only = TRUE,
                                            year_from = 2024L, sort = "relevance")))
  )
  for (view in views) {
    query <- encode_view(view)
    expect_match(query, "^\\?track=")
    expect_false(grepl("[ |()]", query))
    expect_equal(decode_view(query, TEST_SPEC, YEARS), view)
  }
})

test_that("the query string is readable", {
  expect_equal(encode_view(new_view("browse", filters = new_filter_state("spc"))), "?track=spc")
  expect_equal(encode_view(new_view("browse", filters = new_filter_state())), "?track=all")
  query <- encode_view(new_view("browse", tab = "landscape", paper = "2501.00001", filters = full_state()))
  expect_match(query, "tab=landscape", fixed = TRUE)
  expect_match(query, "paper=2501.00001", fixed = TRUE)
  expect_match(query, "years=2020-2025", fixed = TRUE)
  expect_match(query, "f=chart_statistic.primary.spc%3AEWMA", fixed = TRUE)
  expect_match(query, "code=1", fixed = TRUE)
})

test_that("a hand-edited or stale URL is cleaned, not trusted", {
  view <- decode_view("?track=nonsense&tab=secrets&years=1500-2025&f=chart_approach.any.spc:Bayesian|Quantum&f=bogus..:x&sort=sideways&paper=<script>",
                      TEST_SPEC, YEARS)
  expect_equal(view$view, "browse")
  expect_null(view$filters$track)
  expect_equal(view$tab, "explore")
  expect_null(view$paper)
  expect_equal(view$filters$year_from, 2002L)
  expect_length(view$filters$conditions, 1L)
  expect_equal(view$filters$conditions[[1]]$values, "Bayesian")
  expect_equal(view$filters$sort, "newest")
})

test_that("a link to a paper alone opens it across all tracks, by base id", {
  view <- decode_view("?paper=2501.00001v3", TEST_SPEC, YEARS)
  expect_equal(view$view, "browse")
  expect_equal(view$paper, "2501.00001")
  expect_null(view$filters$track)
  expect_equal(paper_href("2501.00001", "spc"), "?track=spc&paper=2501.00001")
  expect_equal(paper_href("math/0406049"), "?track=all&paper=math%2F0406049")
  expect_equal(decode_view(paper_href("math/0406049"), TEST_SPEC, YEARS)$paper, "math/0406049")
})

test_that("open-ended year ranges round trip", {
  state <- new_filter_state("spc")
  state$year_from <- 2024L
  query <- encode_view(new_view("browse", filters = state))
  expect_match(query, "years=2024-", fixed = TRUE)
  expect_equal(decode_view(query, TEST_SPEC, YEARS)$filters, state)
})
