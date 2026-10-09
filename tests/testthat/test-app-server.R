# Interactions of the server and its modules, with the model services faked.

approach_form <- function(residual = "") {
  list(interpretation = "Nonparametric SPM charts.", track = "all", public_code = FALSE, real_data = FALSE,
       reviews_only = FALSE, residual = residual,
       conditions = list(spc__chart_approach = list("Nonparametric (distribution-free)", "Imaginary")))
}

test_deps <- function(question_fn = function(system_prompt, question) approach_form(),
                      rank_fn = function(papers, residual, settings, ...) {
                        list(scores = data.frame(paper_id = papers$paper_id,
                                                 score = ifelse(grepl("wind", papers$search_text), 0.9, 0.1),
                                                 stringsAsFactors = FALSE),
                             source = "jev", threshold = JEV_THRESHOLD, message = "", capped = FALSE,
                             total = nrow(papers), considered = nrow(papers), residual = residual)
                      }) {
  app_deps(TEST_SPEC, TEST_SETTINGS, fixture_data(), reliability = NULL, question_fn = question_fn,
           check_fn = function(question, conditions) list(keep = conditions, demote = list(), checked = FALSE),
           rank_fn = rank_fn, chat_factory = function(system_prompt) stop("HTTP 429 quota exceeded"))
}

module_app <- function(state = new_filter_state("spc"), bookmarks = character(0)) {
  filters <- shiny::reactiveVal(state)
  list(filters = shiny::reactive(filters()), set_filters = function(new) filters(new),
       papers = shiny::reactive(apply_filters(fixture_papers(), filters(), TEST_SPEC, TEST_SETTINGS)),
       ranking = shiny::reactive(NULL), bookmarks = shiny::reactive(bookmarks),
       paper = shiny::reactive(NULL), close_paper = function() NULL, current = filters)
}

test_that("the page starts on the landing view and a track opens the tabs", {
  shiny::testServer(app_server(test_deps()), {
    expect_equal(output$mode, "landing")
    session$setInputs(qew_select_track = "spc")
    expect_equal(output$mode, "tabs")
    expect_equal(filters()$track, "spc")
    expect_equal(nrow(filtered()), 3L)
    expect_equal(counts()$screened_out, 1L)
    expect_match(flat(output$scope_line$html), "Showing 3 of 3 in-scope papers")
    expect_match(flat(output$scope_line$html), "1 screened out, 1 not processed")
    expect_match(output$chips$html, "Track: SPM")
    expect_match(output$header$html, "Statistical Process Monitoring \\(SPM\\)")
    expect_match(output$header$html, "Data current as of 01 March 2026")
  })
})

test_that("removing the track chip searches all tracks; clear all keeps the track", {
  shiny::testServer(app_server(test_deps()), {
    session$setInputs(qew_select_track = "spc")
    session$setInputs(qew_chip_remove = "track")
    expect_null(filters()$track)
    expect_equal(nrow(filtered()), 7L)
    expect_match(output$header$html, "All tracks")
    session$setInputs(qew_select_track = "reliability")
    filters(utils::modifyList(filters(), list(public_code = TRUE)))
    expect_equal(nrow(filtered()), 1L)
    session$setInputs(clear_all = 1)
    expect_equal(filters(), new_filter_state("reliability"))
  })
})

test_that("a question becomes chips, invalid values are reported, and no track chip is added on the landing page", {
  asked_with <- NULL
  deps <- test_deps(question_fn = function(system_prompt, question) {
    asked_with <<- question
    approach_form()
  })
  shiny::testServer(app_server(deps), {
    session$setInputs(question_landing = "nonparametric charts", ask_landing = 1)
    expect_equal(asked_with, "nonparametric charts")
    expect_equal(output$mode, "tabs")
    expect_null(filters()$track)
    expect_equal(filters()$conditions[[1]]$values, "Nonparametric (distribution-free)")
    expect_equal(filters()$conditions[[1]]$track, "spc")
    expect_match(output$chips$html, "SPM papers only")
    expect_match(output$note$html, "Nonparametric SPM charts.")
    expect_match(output$note$html, "Imaginary")
    # SPM papers are narrowed to the one match; other tracks are untouched
    expect_setequal(filtered()$paper_id, c("2501.00001", "2502.00006", "1902.00007", "2503.00008", "2203.00009"))
    # removing the chip needs no model call
    asked_with <<- NULL
    session$setInputs(qew_chip_remove = "cond:1")
    expect_null(asked_with)
    expect_equal(nrow(filtered()), 7L)
  })
})

test_that("inside a track the question keeps the track", {
  shiny::testServer(app_server(test_deps()), {
    session$setInputs(qew_select_track = "spc")
    session$setInputs(question = "nonparametric", ask_go = 1)
    expect_equal(filters()$track, "spc")
    expect_equal(filtered()$paper_id, "2501.00001")
  })
})

test_that("a residual is ranked after filtering and the ranking follows the chips", {
  ranked <- list()
  deps <- test_deps(
    question_fn = function(system_prompt, question) {
      list(interpretation = "Papers about wind turbines.", track = "all", public_code = FALSE, real_data = FALSE,
           reviews_only = FALSE, conditions = list(), residual = "wind turbines")
    },
    rank_fn = function(papers, residual, settings, ...) {
      ranked[[length(ranked) + 1L]] <<- papers$paper_id
      list(scores = data.frame(paper_id = papers$paper_id,
                               score = ifelse(papers$paper_id == "2503.00008", 0.95, 0.05), stringsAsFactors = FALSE),
           source = "jev", threshold = JEV_THRESHOLD, message = "", capped = FALSE, total = nrow(papers),
           considered = nrow(papers), residual = residual)
    })
  shiny::testServer(app_server(deps), {
    session$setInputs(qew_select_track = "reliability")
    session$setInputs(question = "wind turbines", ask_go = 1)
    expect_equal(filters()$residual, "wind turbines")
    expect_equal(filters()$sort, "relevance")
    expect_null(current_ranking())                    # nothing shown until the scores arrive
    session$elapse(400)
    expect_length(ranked, 1L)
    expect_setequal(ranked[[1]], c("2503.00008", "2203.00009"))    # only papers that passed the filters
    expect_equal(current_ranking()$label, "wind turbines")
    expect_match(output$chips$html, "Ranked by relevance to: wind turbines")
    groups <- group_by_relevance(filtered(), current_ranking()$scores, current_ranking()$threshold)
    expect_equal(groups$likely$paper_id, "2503.00008")
    expect_equal(groups$less_likely$paper_id, "2203.00009")
    # narrowing the filters does not score again
    filters(utils::modifyList(filters(), list(year_from = 2025L)))
    session$elapse(400)
    expect_length(ranked, 1L)
    # removing the residual chip removes the ranking
    session$setInputs(qew_chip_remove = "residual")
    session$elapse(400)
    expect_null(current_ranking())
    expect_equal(filters()$sort, "newest")
  })
})

test_that("when the model fails the app falls back to keyword search and says so", {
  deps <- test_deps(question_fn = function(...) stop("HTTP 429: insufficient_quota, check your billing"))
  shiny::testServer(app_server(deps), {
    session$setInputs(qew_select_track = "reliability")
    session$setInputs(question = "wind turbines in 2025", ask_go = 1)
    expect_equal(filters()$keywords, c("wind", "turbines"))
    expect_equal(filters()$year_from, 2025L)
    expect_match(output$note$html, "could not be translated")
    expect_match(output$note$html, "no credit")
    expect_equal(filtered()$paper_id, "2503.00008")
    session$elapse(400)
    expect_equal(current_ranking()$source, "keyword")
  })
})

test_that("questions are rate-limited per session and length-capped without calling the model", {
  calls <- 0L
  deps <- test_deps(question_fn = function(...) { calls <<- calls + 1L; approach_form() })
  shiny::testServer(app_server(deps), {
    session$setInputs(qew_select_track = "spc")
    for (i in seq_len(QUESTION_RATE_LIMIT + 2L)) session$setInputs(question = paste("question", i), ask_go = i)
    expect_equal(calls, QUESTION_RATE_LIMIT)
    expect_match(output$note$html, "reached the limit")
  })
  calls <- 0L
  shiny::testServer(app_server(deps), {
    session$setInputs(qew_select_track = "spc")
    session$setInputs(question = strrep("x", QUESTION_MAX_CHARS + 1L), ask_go = 1)
    expect_equal(calls, 0L)
    expect_match(output$note$html, "limited to")
  })
})

test_that("a paper opens from anywhere and a tag click filters and returns to the list", {
  shiny::testServer(app_server(test_deps()), {
    session$setInputs(qew_open_paper = "2502.00006v3")           # from the landing page, versioned id
    expect_equal(output$mode, "paper")
    expect_equal(nav$paper, "2502.00006")
    expect_equal(app$paper()$track, "exp_design")
    session$setInputs(qew_tag_click = list(field = "design_type", value = "Optimal design", track = "exp_design",
                                           role = "any"))
    expect_equal(output$mode, "tabs")
    expect_null(nav$paper)
    expect_setequal(filtered()$paper_id[filtered()$track == "exp_design"], c("2502.00006", "1902.00007"))
    # a tag that is not in the spec is ignored
    before <- filters()
    session$setInputs(qew_tag_click = list(field = "design_type", value = "Invented", track = "exp_design", role = "any"))
    expect_equal(filters(), before)
    # a failed row is never opened as a paper
    session$setInputs(qew_open_paper = "2501.00005")
    expect_null(app$paper())
  })
})

test_that("a tag of another track switches to that track", {
  shiny::testServer(app_server(test_deps()), {
    session$setInputs(qew_select_track = "spc")
    session$setInputs(qew_tag_click = list(field = "design_type", value = "Optimal design", track = "exp_design",
                                           role = "primary"))
    expect_equal(filters()$track, "exp_design")
    expect_equal(filtered()$paper_id, "1902.00007")
  })
})

test_that("the add-filter list is generated from the spec for the scope", {
  spc <- add_filter_choices(TEST_SPEC, "spc")
  expect_setequal(names(spc), c("Method", "Data", "Evaluation", "Software"))
  expect_equal(spc$Method[["Approach"]], "spc:chart_approach")
  expect_equal(spc$Data[["Application domain"]], ":application_domain")
  expect_false("Reference sample guidance" %in% names(spc$Method))        # filter tier "none"
  all_tracks <- add_filter_choices(TEST_SPEC)
  expect_true("SPM papers only: Method" %in% names(all_tracks))
  expect_equal(all_tracks[["Reliability papers only: Evaluation"]][["How it is evaluated"]],
               "reliability:evaluation_type")
  expect_equal(parse_field_key("spc:chart_approach"), list(track = "spc", field = "chart_approach"))
  expect_null(parse_field_key(""))
  shiny::testServer(app_server(test_deps()), {
    session$setInputs(qew_select_track = "spc")
    session$setInputs(add_field = "spc:chart_approach")
    session$setInputs(add_values = c("Bayesian", "Robust"), add_apply = 1)
    expect_equal(get_condition_values(filters(), "chart_approach", "spc"), c("Bayesian", "Robust"))
    expect_setequal(filtered()$paper_id, c("2501.00001", "2401.00002"))
  })
})

test_that("Explore inputs and the shared state stay in step", {
  app <- module_app()
  deps <- test_deps()
  shiny::testServer(explore_server, args = list(id = "explore", app = app, deps = deps), {
    session$setInputs(years = c(2019L, 2025L), topic = NULL, method = NULL, domain = NULL, public_code = FALSE,
                      real_data = FALSE, reviews_only = FALSE, include_screened = FALSE, sort = "newest",
                      columns = NULL)
    expect_equal(app$current(), new_filter_state("spc"))
    session$setInputs(public_code = TRUE)
    expect_true(app$current()$public_code)
    expect_match(output$heading, "Papers \\(1\\)")
    session$setInputs(public_code = FALSE, years = c(2024L, 2025L))
    expect_equal(app$current()$year_from, 2024L)
    expect_null(app$current()$year_to)                               # the upper end is the data maximum
    session$setInputs(years = c(2019L, 2025L), method = "CUSUM")
    expect_equal(get_condition_values(app$current(), "chart_statistic", "spc"), "CUSUM")
    expect_equal(nrow(grouped()$likely), 1L)
    session$setInputs(method = NULL, domain = "Healthcare and medical")
    expect_length(get_condition_values(app$current(), "chart_statistic", "spc"), 0L)
    expect_equal(grouped()$likely$paper_id, "2401.00002")
    session$setInputs(domain = NULL, include_screened = TRUE)
    expect_equal(nrow(grouped()$likely), 4L)
    session$setInputs(include_screened = FALSE, reviews_only = TRUE)
    expect_equal(grouped()$likely$paper_id, "2001.00003")
    session$setInputs(reviews_only = FALSE, real_data = TRUE, sort = "oldest")
    expect_equal(app$current()$sort, "oldest")
    expect_equal(grouped()$likely$paper_id, "2501.00001")
    expect_equal(output$has_track, "1")
  })
})

test_that("Explore shows an empty state instead of an empty table", {
  state <- utils::modifyList(new_filter_state("spc"), list(year_to = 2019L))
  shiny::testServer(explore_server, args = list(id = "explore", app = module_app(state), deps = test_deps()), {
    expect_match(output$main$html, "No papers match these filters")
    expect_null(output$less_likely)
  })
})

test_that("the Library lists bookmarks from every track whatever the filters are", {
  state <- utils::modifyList(new_filter_state("spc"), list(year_to = 2019L))      # filters select nothing
  app <- module_app(state, bookmarks = c("2503.00008v1", "2501.00001", "2501.00005", "9999.99999"))
  shiny::testServer(library_server, args = list(id = "library", app = app, deps = test_deps()), {
    expect_setequal(saved()$paper_id, c("2503.00008", "2501.00001"))
    expect_match(output$heading, "Bookmarked papers \\(2\\)")
    expect_match(flat(output$notice$html), "2 bookmarked papers are not in the current database")
  })
  shiny::testServer(library_server, args = list(id = "library", app = module_app(), deps = test_deps()), {
    expect_match(output$body$html, "No bookmarks yet")
  })
})

test_that("Authors follows the shared selection", {
  shiny::testServer(authors_server, args = list(id = "authors", app = module_app(), deps = test_deps()), {
    expect_equal(counts()$n[counts()$author == "Ann Author"], 3L)
    expect_match(output$top_title, "of 3 papers in the current selection")
    session$setInputs(author = "Di Fourth")
    expect_equal(author_papers()$paper_id, "2501.00001")
    expect_match(flat(output$author_info$html), "1 paper in the current selection")
  })
})

test_that("the paper page reports a missing paper and renders an existing one", {
  app <- module_app()
  app$paper <- shiny::reactive(find_paper(fixture_papers(), "2501.00001"))
  shiny::testServer(paper_server, args = list(id = "paper", app = app, deps = test_deps()), {
    expect_match(output$view$html, "paper-view")
    expect_match(output$view$html, "Chat with this paper")
  })
  shiny::testServer(paper_server, args = list(id = "paper", app = module_app(), deps = test_deps()), {
    expect_match(output$view$html, "not in the database")
  })
})

test_that("the page layout has the question box, four tabs and no stat tiles", {
  html <- as.character(app_ui(test_deps()))
  for (text in c("Ask all tracks", "Explore", "Landscape", "Authors", "Library", "+ add filter",
                 TEST_SETTINGS$authors)) {
    expect_match(html, text, fixed = TRUE)
  }
  expect_false(grepl("stat-card|landing-stat|feature-pill|Deep Dive", html))
  expect_match(html, paste0("maxlength=\"", QUESTION_MAX_CHARS, "\""), fixed = TRUE)
  broken <- test_deps()
  broken$data <- list(papers = NULL, problems = "Data files were not found.", data_dir = "nowhere")
  expect_match(as.character(app_ui(broken)), "cannot start", fixed = TRUE)
})

test_that("a landing card shows the in-scope count and where it comes from", {
  papers <- fixture_papers()
  rows <- papers[papers$track == "spc", , drop = FALSE]
  html <- as.character(landing_track_card("spc", TEST_SPEC$tracks$spc, rows))
  expect_match(html, paste0("track-card-number[^>]*>[[:space:]]*", sum(rows$status == "ok"), "[[:space:]]*<"))
  expect_match(html, paste0(nrow(rows), " found by the arXiv search, ", sum(rows$status == "out_of_scope"), " screened out"),
               fixed = TRUE)
  expect_match(html, "?track=spc", fixed = TRUE)
  with_failed <- rows; with_failed$status[1] <- "failed"
  expect_match(as.character(landing_track_card("spc", TEST_SPEC$tracks$spc, with_failed)),
               paste0(", ", sum(with_failed$status == "failed"), " not processed"), fixed = TRUE)
  none_failed <- rows[rows$status != "failed", , drop = FALSE]
  expect_false(grepl("not processed", as.character(landing_track_card("spc", TEST_SPEC$tracks$spc, none_failed))))
})

test_that("the landing page explains the screen with the spec's examples and links to the help", {
  papers <- fixture_papers()
  html <- as.character(landing_scope_note(TEST_SPEC, papers))
  expect_match(html, paste0(sum(papers$status == "out_of_scope"), " of ", nrow(papers), " papers"), fixed = TRUE)
  for (id in names(TEST_SPEC$tracks)) {
    expect_match(html, htmltools::htmlEscape(TEST_SPEC$tracks[[id]]$scope$example_out), fixed = TRUE, info = id)
  }
  expect_match(html, "data-help=\"scope\"", fixed = TRUE)
  expect_match(html, "data-help=\"changes\"", fixed = TRUE)
  expect_match(html, "Include screened-out papers", fixed = TRUE)
  expect_match(as.character(landing_scope_note(TEST_SPEC, papers[0, ])), "0 of 0 papers", fixed = TRUE)
})

test_that("a question about counts opens Landscape, and the note reports suggestions and limits", {
  form <- list(interpretation = "How many SPM papers use nonparametric charts.", intent = "count_or_trend",
               track = "all", public_code = FALSE, real_data = FALSE, reviews_only = FALSE,
               conditions = list(spc__chart_approach = list("Nonparametric (distribution-free)"),
                                 application_domain = list("Healthcare and medical")),
               guessed = list("application_domain"),
               cannot_answer = "The data hold no citation counts.")
  shiny::testServer(app_server(test_deps(question_fn = function(system_prompt, question) form)), {
    session$setInputs(qew_select_track = "spc")
    session$setInputs(question = "how many nonparametric charts, most cited first", ask_go = 1)
    expect_equal(nav$tab, "landscape")
    expect_length(filters()$conditions, 1L)
    html <- output$note$html
    expect_match(html, "Landscape tab is open", fixed = TRUE)
    expect_match(html, "Not answered: ", fixed = TRUE)
    expect_match(html, "no citation counts", fixed = TRUE)
    expect_match(html, "data-field=\"application_domain\"", fixed = TRUE)
    expect_match(html, "Application domain: Healthcare and medical", fixed = TRUE)
    # clicking the suggestion adds it as a filter
    session$setInputs(qew_tag_click = list(field = "application_domain", value = "Healthcare and medical",
                                           track = "", role = "any"))
    expect_length(filters()$conditions, 2L)
  })
})

test_that("an author question filters by author, ranks nothing, and points to screened-out matches", {
  papers <- fixture_papers()
  hidden <- papers[papers$status == "out_of_scope" & papers$track == "spc", ][1, ]
  name <- split_values(hidden$authors)[1]
  ranked <- 0L
  deps <- test_deps(
    question_fn = function(system_prompt, question) {
      list(interpretation = "Papers by one author.", intent = "find_papers", track = "all", public_code = FALSE,
           real_data = FALSE, reviews_only = FALSE, conditions = list(), authors = list(name))
    },
    rank_fn = function(papers, residual, settings, ...) { ranked <<- ranked + 1L; stop("must not rank") })
  shiny::testServer(app_server(deps), {
    session$setInputs(qew_select_track = "spc")
    session$setInputs(question = paste("papers by", name), ask_go = 1)
    session$elapse(400)
    expect_equal(filters()$authors, name)
    expect_equal(ranked, 0L)
    expect_match(output$chips$html, paste0("Author: ", name), fixed = TRUE)
    in_scope <- sum(author_mask(papers$authors, name) & papers$status == "ok" & papers$track == "spc")
    expect_equal(nrow(filtered()), in_scope)
    expect_gte(counts()$hidden_matches, 1L)
    expect_match(output$scope_line$html, "also match", fixed = TRUE)
    session$setInputs(show_screened = 1)
    expect_true(filters()$include_screened)
    expect_equal(counts()$hidden_matches, 0L)
    expect_true(hidden$paper_id %in% filtered()$paper_id)
  })
})

test_that("a criterion the model cannot judge leaves the papers unranked and says why", {
  deps <- test_deps(
    question_fn = function(system_prompt, question) {
      list(interpretation = "x", intent = "find_papers", track = "all", public_code = FALSE, real_data = FALSE,
           reviews_only = FALSE, conditions = list(), criteria = list(list(text = "funded by industry", exclude = FALSE)))
    },
    rank_fn = function(papers, residual, settings, ...) {
      list(scores = empty_scores(), source = "undecided", threshold = 0,
           message = "The factsheets do not say enough to judge \"funded by industry\".", capped = FALSE,
           total = nrow(papers), considered = nrow(papers), residual = residual)
    })
  shiny::testServer(app_server(deps), {
    session$setInputs(qew_select_track = "spc")
    session$setInputs(question = "funded by industry", ask_go = 1)
    session$elapse(400)
    expect_equal(current_ranking()$source, "undecided")
    expect_equal(nrow(current_ranking()$scores), 0L)
  })
})
