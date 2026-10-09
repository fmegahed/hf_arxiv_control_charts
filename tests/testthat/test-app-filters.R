state_with <- function(track = NULL, ...) utils::modifyList(new_filter_state(track), list(...))

test_that("by default only in-scope papers are selected; failed rows never are", {
  papers <- fixture_papers()
  expect_equal(ids_of(papers, new_filter_state("spc")), c("2001.00003", "2401.00002", "2501.00001"))
  with_screened <- ids_of(papers, state_with("spc", include_screened = TRUE))
  expect_true("2501.00004" %in% with_screened)
  expect_false("2501.00005" %in% with_screened)
  expect_length(ids_of(papers, new_filter_state()), 7L)
})

test_that("year filters are inclusive and may be open ended", {
  papers <- fixture_papers()
  expect_equal(ids_of(papers, state_with("spc", year_from = 2024L)), c("2401.00002", "2501.00001"))
  expect_equal(ids_of(papers, state_with("spc", year_to = 2020L)), "2001.00003")
  expect_equal(ids_of(papers, state_with("spc", year_from = 2025L, year_to = 2025L)), "2501.00001")
})

test_that("conditions match whole elements, never substrings", {
  papers <- fixture_papers()
  ewma <- set_condition(new_filter_state("spc"), "chart_statistic", "EWMA", "spc")
  expect_equal(ids_of(papers, ewma), "2501.00001")       # "MEWMA" contains "EWMA" but is another value
  bayes <- set_condition(new_filter_state("spc"), "chart_approach", "Bayesian", "spc")
  expect_equal(ids_of(papers, bayes), c("2401.00002", "2501.00001"))
  expect_equal(has_any_value(c("MEWMA", "EWMA|CUSUM", NA, ""), "EWMA"), c(FALSE, TRUE, FALSE, FALSE))
})

test_that("has_any_value agrees with has_value on real cells", {
  cells <- c(fixture_papers()$data_source, " Simulated data | Public benchmark dataset ", NA)
  for (value in c("Simulated data", "Public benchmark dataset", "Real data")) {
    expect_identical(has_any_value(cells, value), has_value(cells, value), info = value)
  }
})

test_that("each kind of field filters correctly", {
  papers <- fixture_papers()
  spc <- new_filter_state("spc")
  # single
  expect_equal(ids_of(papers, set_condition(spc, "phase", "Phase I", "spc")), "2401.00002")
  # multi, several values are alternatives
  expect_equal(ids_of(papers, set_condition(spc, "software_platform", c("Python", "MATLAB"))),
               c("2401.00002", "2501.00001"))
  # primary_additional: any label, or the primary one only
  expect_equal(ids_of(papers, set_condition(spc, "chart_statistic", "CUSUM", "spc")), "2501.00001")
  expect_length(ids_of(papers, set_condition(spc, "chart_statistic", "CUSUM", "spc", role = "primary")), 0L)
  # tristate
  expect_equal(ids_of(papers, set_condition(spc, "assumes_normality", TRISTATE_VALUES[1], "spc")), "2401.00002")
  expect_equal(ids_of(papers, set_condition(spc, "assumes_normality", TRISTATE_VALUES[2], "spc")), "2501.00001")
  expect_equal(ids_of(papers, set_condition(spc, "assumes_normality", TRISTATE_VALUES[3], "spc")), "2001.00003")
  # catch-all
  expect_equal(ids_of(papers, set_condition(spc, "chart_family", NONE_OF_LISTED, "spc")), "2001.00003")
  # two conditions must both hold
  both <- set_condition(set_condition(spc, "chart_approach", "Bayesian", "spc"), "phase", "Phase II", "spc")
  expect_equal(ids_of(papers, both), "2501.00001")
})

test_that("switches use code_public, the real-data values and the review types", {
  papers <- fixture_papers()
  expect_equal(ids_of(papers, state_with(public_code = TRUE)), c("2203.00009", "2501.00001", "2502.00006"))
  expect_equal(ids_of(papers, state_with(real_data = TRUE)), c("2501.00001", "2502.00006", "2503.00008"))
  expect_equal(ids_of(papers, state_with(reviews_only = TRUE)), "2001.00003")
  expect_equal(ids_of(papers, state_with("spc", public_code = TRUE, real_data = TRUE)), "2501.00001")
})

test_that("across tracks a shared field filters everything and a track field only its track", {
  papers <- fixture_papers()
  manufacturing <- set_condition(new_filter_state(), "application_domain", "Manufacturing")
  expect_equal(ids_of(papers, manufacturing), c("2203.00009", "2501.00001", "2502.00006"))
  expect_null(manufacturing$conditions[[1]]$track)

  spc_only <- set_condition(new_filter_state(), "chart_approach", "Bayesian", "spc")
  selected <- ids_of(papers, spc_only)
  expect_true(all(c("2401.00002", "2501.00001") %in% selected))
  expect_false("2001.00003" %in% selected)                       # an SPM paper that does not match
  expect_true(all(c("2502.00006", "1902.00007", "2503.00008") %in% selected))   # other tracks untouched
  chips <- vapply(state_chips(spc_only, TEST_SPEC), function(chip) chip$label, character(1))
  expect_match(chips, "SPM papers only", all = FALSE)

  expect_null(condition_track(TEST_SPEC, "data_source"))
  expect_equal(condition_track(TEST_SPEC, "chart_family"), "spc")
  expect_true(is.na(condition_track(TEST_SPEC, "evaluation_type")))     # same name, three definitions
  expect_equal(condition_track(TEST_SPEC, "evaluation_type", context_track = "reliability"), "reliability")
})

test_that("keyword search needs at least one term and scores by share of terms", {
  papers <- fixture_papers()
  state <- state_with(keywords = c("wind", "turbines"))
  expect_equal(ids_of(papers, state), "2503.00008")
  expect_equal(keyword_terms("Which SPC papers use the wind turbines in 2025?"), c("spc", "wind", "turbines"))
  scores <- keyword_scores(papers, c("wind", "zzzz"))
  expect_equal(scores[papers$paper_id == "2503.00008"], 0.5)
  expect_equal(keyword_scores(papers, character(0)), rep(0, nrow(papers)))
})

test_that("scope counts state what is shown, in scope, screened out and failed", {
  papers <- fixture_papers()
  counts <- scope_counts(papers, state_with("spc", year_from = 2024L), TEST_SPEC, TEST_SETTINGS)
  expect_equal(counts[c("shown", "in_scope", "screened_out", "failed", "total")],
               list(shown = 2L, in_scope = 3L, screened_out = 1L, failed = 1L, total = 3L))
  expect_equal(counts$reasons$category, "keyword_in_passing")
  all_tracks <- scope_counts(papers, state_with(include_screened = TRUE), TEST_SPEC, TEST_SETTINGS)
  expect_equal(all_tracks$total, 9L)
  expect_equal(all_tracks$screened_out, 2L)
  expect_equal(sum(all_tracks$reasons$n), 2L)
})

test_that("chips mirror the state and removing one changes only that part", {
  state <- state_with("spc", year_from = 2025L, year_to = 2025L, public_code = TRUE, residual = "wind turbines",
                      sort = "relevance")
  state <- set_condition(state, "chart_approach", "Nonparametric (distribution-free)", "spc")
  chips <- state_chips(state, TEST_SPEC)
  expect_equal(vapply(chips, function(chip) chip$id, character(1)),
               c("track", "year", "cond:1", "code", "crit:1"))
  expect_equal(vapply(chips, function(chip) chip$label, character(1)),
               c("Track: SPM", "Year: 2025", "Approach: Nonparametric (distribution-free)", "Code: public",
                 "Ranked by relevance to: wind turbines"))

  expect_null(remove_chip(state, "track")$track)
  expect_null(remove_chip(state, "year")$year_from)
  expect_length(remove_chip(state, "cond:1")$conditions, 0L)
  expect_false(remove_chip(state, "code")$public_code)
  no_residual <- remove_chip(state, "crit:1")
  expect_equal(no_residual$residual, "")
  expect_equal(no_residual$sort, "newest")
  expect_equal(remove_chip(state, "cond:9"), state)
  expect_true(has_active_filters(state))
  cleared <- clear_filters(state)
  expect_equal(cleared, new_filter_state("spc"))
  expect_false(has_active_filters(cleared))
})

test_that("set_condition replaces a slot and an empty selection removes it", {
  state <- set_condition(new_filter_state("spc"), "chart_approach", "Bayesian", "spc")
  state <- set_condition(state, "chart_approach", c("Robust", "Adaptive"), "spc")
  expect_length(state$conditions, 1L)
  expect_equal(get_condition_values(state, "chart_approach", "spc"), c("Robust", "Adaptive"))
  expect_length(set_condition(state, "chart_approach", NULL, "spc")$conditions, 0L)
})

test_that("sanitize_state drops what the spec does not know and clamps years", {
  raw <- list(track = "astrology", year_from = 1800, year_to = 2025,
              conditions = list(new_condition("chart_approach", c("Bayesian", "Quantum"), "spc"),
                                new_condition("no_such_field", "x"),
                                new_condition("sample_size_requirements", "x", "spc"),
                                new_condition("evaluation_type", "Simulation study")),
              public_code = TRUE, residual = "  wind  ", sort = "relevance")
  out <- sanitize_state(raw, TEST_SPEC, c(2002L, 2026L))
  expect_null(out$state$track)
  expect_equal(out$state$year_from, 2002L)
  expect_equal(out$state$year_to, 2025L)
  expect_length(out$state$conditions, 1L)
  expect_equal(out$state$conditions[[1]]$values, "Bayesian")
  expect_true(out$state$public_code)
  expect_equal(out$state$residual, "wind")
  expect_equal(out$state$sort, "relevance")
  expect_match(out$dropped, "unknown track", all = FALSE)
  expect_match(out$dropped, "'Quantum' is not a value of Approach", all = FALSE)
  expect_match(out$dropped, "unknown field 'no_such_field'", all = FALSE)
  expect_match(out$dropped, "cannot be filtered", all = FALSE)
  expect_match(out$dropped, "needs a track", all = FALSE)
  expect_match(out$dropped, "1800 is outside the data", all = FALSE)
  # a full year range is no filter; relevance sort needs something to rank by
  plain <- sanitize_state(list(year_from = 2002, year_to = 2026, sort = "relevance"), TEST_SPEC, c(2002L, 2026L))
  expect_null(plain$state$year_from)
  expect_equal(plain$state$sort, "newest")
})

test_that("sorting is by date or by relevance", {
  papers <- apply_filters(fixture_papers(), new_filter_state("spc"), TEST_SPEC, TEST_SETTINGS)
  expect_equal(sort_papers(papers, "newest")$paper_id[1], "2501.00001")
  expect_equal(sort_papers(papers, "oldest")$paper_id[1], "2001.00003")
  scores <- data.frame(paper_id = c("2401.00002", "2501.00001"), score = c(0.9, 0.2))
  expect_equal(sort_papers(papers, "relevance", scores)$paper_id, c("2401.00002", "2501.00001", "2001.00003"))
})
