YEARS <- c(2002L, 2026L)
OWNER_QUESTION <- "Which SPC papers submitted on arXiv in 2025 use nonparametric methods and provide public code?"

fake_form <- function(...) {
  utils::modifyList(list(interpretation = "A reading.", track = "all", public_code = FALSE, real_data = FALSE,
                         reviews_only = FALSE, conditions = list(), residual = ""), list(...))
}

owner_form <- function() {
  fake_form(interpretation = "SPM papers from 2025 with nonparametric charts and public code.",
            track = "spc", year_from = 2025L, year_to = 2025L, public_code = TRUE,
            conditions = list(spc__chart_approach = list("Nonparametric (distribution-free)")))
}

test_that("the form offers every filterable field once, with track fields prefixed", {
  entries <- question_fields(TEST_SPEC)
  keys <- names(entries)
  expect_false(any(duplicated(keys)))
  expect_true(all(c("paper_type", "application_domain", "data_source", "software_platform",
                    "code_availability") %in% keys))
  expect_true(all(c("spc__chart_family", "spc__evaluation_type", "exp_design__evaluation_type",
                    "reliability__model_family") %in% keys))
  expect_false(any(grepl("sample_size_requirements|software_urls|code_used", keys)))
  expect_null(entries$data_source$track)
  expect_equal(entries$spc__chart_family$track, "spc")
  n_filters <- length(unique(unlist(lapply(TRACK_IDS, function(track) {
    vapply(spec_filters(TEST_SPEC, track), function(f) {
      if (f$column %in% spec_shared_fields(TEST_SPEC)) f$column else paste(track, f$column)
    }, character(1))
  }))))
  expect_length(entries, n_filters)
})

test_that("the ellmer type and the system prompt are built from the spec", {
  type <- build_question_type(TEST_SPEC)
  expect_s3_class(type, "ellmer::TypeObject")
  expect_setequal(names(type@properties),
                  c("interpretation", "track", "year_from", "year_to", "public_code", "real_data",
                    "reviews_only", "conditions", "residual"))
  conditions <- type@properties$conditions@properties
  expect_setequal(names(conditions), names(question_fields(TEST_SPEC)))
  approach <- conditions$spc__chart_approach
  expect_false(approach@required)
  expect_equal(approach@items@values, field_choices(spec_field(TEST_SPEC, "spc", "chart_approach")))
  expect_match(approach@description, "distribution-free", fixed = TRUE)
  expect_equal(conditions$spc__assumes_normality@items@values, TRISTATE_VALUES[1:2])
  expect_equal(type@properties$track@values, c(TRACK_IDS, ALL_TRACKS_KEY))

  prompt <- question_system_prompt(TEST_SPEC, "spc", YEARS)
  expect_match(prompt, TEST_SPEC$tracks$reliability$label, fixed = TRUE)
  expect_match(prompt, "currently looking at the track 'spc'", fixed = TRUE)
  expect_match(prompt, "2002 to 2026", fixed = TRUE)
  expect_match(question_system_prompt(TEST_SPEC), "all tracks", fixed = TRUE)
})

test_that("the owner's example question becomes four chips and selects the right papers", {
  result <- interpret_question(OWNER_QUESTION, TEST_SPEC, YEARS, model_fn = function(system_prompt, question) {
    expect_equal(question, OWNER_QUESTION)
    expect_match(system_prompt, "Tracks:")
    owner_form()
  })
  expect_true(result$ok)
  expect_equal(result$mode, "model")
  expect_length(result$dropped, 0L)
  expect_equal(vapply(state_chips(result$state, TEST_SPEC), function(chip) chip$label, character(1)),
               c("Track: SPM", "Year: 2025", "Approach: Nonparametric (distribution-free)", "Code: public"))
  expect_equal(result$state$sort, "newest")
  expect_equal(ids_of(fixture_papers(), result$state), "2501.00001")
})

test_that("the example question matches four papers in the development data", {
  skip_without_dev_data()
  data <- load_app_data(TEST_SPEC, dev_data_dir())
  result <- interpret_question(OWNER_QUESTION, TEST_SPEC, data$year_range, model_fn = function(...) owner_form())
  hits <- apply_filters(data$papers, result$state, TEST_SPEC, TEST_SETTINGS)
  expect_true(all(hits$track == "spc" & hits$year == 2025L & hits$code_public))
  expect_true(all(has_value(hits$chart_approach, "Nonparametric (distribution-free)")))
  expect_gt(nrow(hits), 0L)
})

test_that("validation drops unknown fields and values and reports them", {
  raw <- fake_form(track = "spc", conditions = list(
    spc__chart_approach = list("Bayesian", "Made up"),
    spc__no_such_field = list("x"),
    reliability__model_family = list("Surrogate model"),
    application_domain = list("Manufacturing", "Moon mining")))
  out <- validate_filter_spec(raw, TEST_SPEC, YEARS)
  fields <- vapply(out$state$conditions, function(cond) cond$field, character(1))
  expect_setequal(fields, c("chart_approach", "model_family", "application_domain"))
  expect_equal(get_condition_values(out$state, "chart_approach", "spc"), "Bayesian")
  expect_equal(get_condition_values(out$state, "application_domain"), "Manufacturing")
  expect_equal(get_condition_values(out$state, "model_family", "reliability"), "Surrogate model")
  expect_match(out$dropped, "'Made up' is not a value of Approach", all = FALSE)
  expect_match(out$dropped, "'Moon mining'", all = FALSE)
  expect_match(out$dropped, "unknown field 'no_such_field'", all = FALSE)
  expect_equal(out$interpretation, "A reading.")
})

test_that("years are clamped to the data and an unknown track is dropped", {
  out <- validate_filter_spec(fake_form(track = "chemistry", year_from = 1990L, year_to = 2030L), TEST_SPEC, YEARS)
  expect_null(out$state$track)
  expect_null(out$state$year_from)            # clamped to the full range, which is no filter
  expect_length(grep("outside the data", out$dropped), 2L)
  expect_match(out$dropped, "unknown track 'chemistry'", all = FALSE)
  swapped <- validate_filter_spec(fake_form(year_from = 2025L, year_to = 2020L), TEST_SPEC, YEARS)$state
  expect_equal(c(swapped$year_from, swapped$year_to), c(2020L, 2025L))
})

test_that("an empty residual ranks nothing and a residual switches to relevance order", {
  empty <- validate_filter_spec(fake_form(residual = ""), TEST_SPEC, YEARS)$state
  expect_equal(empty$residual, "")
  expect_equal(empty$sort, "newest")
  missing <- validate_filter_spec(fake_form(residual = NULL), TEST_SPEC, YEARS)$state
  expect_equal(missing$residual, "")
  with_residual <- validate_filter_spec(fake_form(residual = " wind turbines "), TEST_SPEC, YEARS)$state
  expect_equal(with_residual$residual, "wind turbines")
  expect_equal(with_residual$sort, "relevance")
})

test_that("the track in view is kept unless the question names another", {
  inside <- validate_filter_spec(fake_form(track = "all"), TEST_SPEC, YEARS, context_track = "spc")$state
  expect_equal(inside$track, "spc")
  other <- validate_filter_spec(fake_form(track = "reliability"), TEST_SPEC, YEARS, context_track = "spc")$state
  expect_equal(other$track, "reliability")
  landing <- validate_filter_spec(fake_form(track = "all"), TEST_SPEC, YEARS)$state
  expect_null(landing$track)
})

test_that("chips and the model's form translate both ways through the filter state", {
  state <- validate_filter_spec(owner_form(), TEST_SPEC, YEARS)$state
  chips <- state_chips(state, TEST_SPEC)
  # removing a chip is a pure state change: no model is involved
  without_year <- remove_chip(state, "year")
  expect_equal(length(state_chips(without_year, TEST_SPEC)), length(chips) - 1L)
  expect_length(ids_of(fixture_papers(), without_year), 1L)
  # adding one by hand gives the same state as the model would have
  by_hand <- new_filter_state("spc")
  by_hand$year_from <- by_hand$year_to <- 2025L
  by_hand$public_code <- TRUE
  by_hand <- set_condition(by_hand, "chart_approach", "Nonparametric (distribution-free)", "spc")
  expect_equal(by_hand, state)
})

test_that("a failing model falls back to keyword search and says so", {
  failing <- function(system_prompt, question) stop("HTTP 429 Too Many Requests: you exceeded your current quota")
  result <- interpret_question(OWNER_QUESTION, TEST_SPEC, YEARS, context_track = "spc", model_fn = failing)
  expect_true(result$ok)
  expect_equal(result$mode, "keyword")
  expect_equal(result$error_class, "no_credit")
  expect_match(result$message, "could not be translated")
  expect_match(result$message, "no credit")
  expect_match(result$message, "keyword search of titles, abstracts and summaries")
  expect_equal(result$state$track, "spc")
  expect_equal(c(result$state$year_from, result$state$year_to), c(2025L, 2025L))
  expect_true(all(c("nonparametric", "public", "code") %in% result$state$keywords))
  expect_equal(result$state$sort, "relevance")
  garbage <- interpret_question("wind", TEST_SPEC, YEARS, model_fn = function(...) "not a list")
  expect_equal(garbage$mode, "keyword")
})

test_that("questions are length-capped and rate-limited", {
  called <- FALSE
  model <- function(...) { called <<- TRUE; fake_form() }
  too_long <- interpret_question(strrep("a", QUESTION_MAX_CHARS + 1L), TEST_SPEC, YEARS, model_fn = model)
  expect_false(too_long$ok)
  expect_match(too_long$message, as.character(QUESTION_MAX_CHARS))
  expect_false(interpret_question("   ", TEST_SPEC, YEARS, model_fn = model)$ok)
  expect_false(called)
  expect_null(check_question(strrep("a", QUESTION_MAX_CHARS)))

  now <- as.numeric(as.POSIXct("2026-01-01 12:00:00", tz = "UTC"))
  expect_true(rate_limit_ok(numeric(0), now))
  expect_true(rate_limit_ok(rep(now - 1, QUESTION_RATE_LIMIT - 1L), now))
  expect_false(rate_limit_ok(rep(now - 1, QUESTION_RATE_LIMIT), now))
  expect_true(rate_limit_ok(rep(now - QUESTION_RATE_WINDOW_SEC - 1, QUESTION_RATE_LIMIT), now))
})

test_that("a list of every public code source is dropped when the public-code switch is on", {
  fields <- function(form) vapply(validate_filter_spec(form, TEST_SPEC, YEARS)$state$conditions,
                                  function(cond) cond$field, character(1))
  all_public <- list(code_availability = as.list(PUBLIC_CODE_SOURCES))
  expect_false("code_availability" %in% fields(fake_form(public_code = TRUE, conditions = all_public)))
  # without the switch the list is the only filter, so it stays
  expect_true("code_availability" %in% fields(fake_form(conditions = all_public)))
  # a narrower list says more than the switch, so it stays
  one <- list(code_availability = as.list(PUBLIC_CODE_SOURCES[1]))
  expect_true("code_availability" %in% fields(fake_form(public_code = TRUE, conditions = one)))
})
