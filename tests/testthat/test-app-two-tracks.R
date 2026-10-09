# A paper found by the searches of two tracks has a factsheet in each. Rows
# are then identified by track and paper id together.

two_track_papers <- function() {
  papers <- fixture_papers()
  twin <- papers[papers$paper_id == "2203.00009", ]          # a reliability paper ...
  twin$track <- "exp_design"                                  # ... also found by the DOE search
  twin$summary <- "The same paper, read for its experimental design."
  rbind(papers, twin)
}

test_that("row keys are unique even when a paper is in two tracks", {
  papers <- two_track_papers()
  expect_true(any(duplicated(papers$paper_id)))
  expect_false(any(duplicated(paper_key(papers))))
})

test_that("the factsheet of the requested track is opened, else the first", {
  papers <- two_track_papers()
  expect_equal(find_paper(papers, "2203.00009", "exp_design")$track, "exp_design")
  expect_equal(find_paper(papers, "2203.00009", "reliability")$track, "reliability")
  expect_equal(find_paper(papers, "2203.00009")$track, "reliability")
  expect_equal(find_paper(papers, "2203.00009v4", "spc")$track, "reliability")   # not in that track: first
  expect_null(find_paper(papers, "0000.00000"))
  expect_null(find_paper(papers, NULL))
  paper <- find_paper(papers, "2203.00009", "reliability")
  expect_equal(paper_other_tracks(papers, paper), "exp_design")
  expect_length(paper_other_tracks(papers, find_paper(papers, "2501.00001")), 0L)
  html <- as.character(paper_view(paper, TEST_SPEC, TEST_SETTINGS, also_in = "exp_design"))
  expect_match(html, "data-paper-track=\"exp_design\"", fixed = TRUE)
  expect_match(html, "separate factsheet", fixed = TRUE)
})

test_that("links and the URL carry the track of the factsheet when it is not the track in view", {
  expect_equal(paper_href(c("2203.00009", "2203.00009"), NULL, c("reliability", "exp_design")),
               c("?track=all&paper=2203.00009&ptrack=reliability", "?track=all&paper=2203.00009&ptrack=exp_design"))
  expect_equal(paper_href("2203.00009", "reliability", "reliability"), "?track=reliability&paper=2203.00009")
  view <- new_view("browse", paper = "2203.00009", paper_track = "exp_design", filters = new_filter_state())
  query <- encode_view(view)
  expect_match(query, "ptrack=exp_design", fixed = TRUE)
  expect_equal(decode_view(query, TEST_SPEC, c(2002L, 2026L)), view)
  same <- new_view("browse", paper = "2203.00009", paper_track = "reliability",
                   filters = new_filter_state("reliability"))
  expect_false(grepl("ptrack", encode_view(same), fixed = TRUE))
  expect_null(decode_view("?track=all&paper=2203.00009&ptrack=nonsense", TEST_SPEC, c(2002L, 2026L))$paper_track)
  expect_null(new_view(paper_track = "spc")$paper_track)                  # no paper, no paper track
})

test_that("each factsheet of a two-track paper keeps its own relevance score", {
  papers <- two_track_papers()
  papers <- papers[papers$paper_id == "2203.00009", ]
  scores <- data.frame(paper_id = papers$paper_id, key = paper_key(papers), score = c(0.2, 0.8))
  expect_equal(score_for(papers, scores), c(0.2, 0.8))
  expect_equal(score_for(papers[2:1, ], scores), c(0.8, 0.2))
  groups <- group_by_relevance(papers, scores, 0.5)
  expect_equal(groups$likely$track, "exp_design")
  expect_equal(groups$less_likely$track, "reliability")
  expect_equal(score_for(papers, NULL), c(NA_real_, NA_real_))
  # scores without keys (one row per paper) still match by paper id
  expect_equal(score_for(papers, data.frame(paper_id = "2203.00009", score = 0.6)), c(0.6, 0.6))
  service <- function(bodies, api_key, url, ...) lapply(bodies, function(body) {
    keys <- names(body$questions)
    answers <- lapply(keys, function(key) list(type = "noul", noul = if (grepl("experimental design", body$state$papers[[sub("_c[0-9]+$", "", key)]]$text)) 0.9 else 0.1))
    list(status = 200L, body = list(answers = stats::setNames(answers, keys), usage = list(input_tokens = 1, output_tokens = 1)))
  })
  ranking <- rank_papers(papers, "design", TEST_SETTINGS, api_key = "k", perform = service, sleep = function(s) NULL)
  expect_equal(score_for(papers, ranking$scores), c(0.1, 0.9))
  table <- results_table(papers, TEST_SPEC, NULL, scores = ranking$scores)
  expect_equal(table$Relevance, c(0.1, 0.9))
  expect_equal(table$row_track, c("reliability", "exp_design"))
})

test_that("the server opens the factsheet of the row that was clicked", {
  data <- fixture_data()
  data$papers <- two_track_papers()
  deps <- app_deps(TEST_SPEC, TEST_SETTINGS, data, reliability = NULL, question_fn = function(...) stop("unused"),
                   rank_fn = function(...) stop("unused"), chat_factory = function(...) stop("unused"))
  shiny::testServer(app_server(deps), {
    session$setInputs(qew_select_track = "reliability")
    session$setInputs(qew_open_paper = list(id = "2203.00009", track = "reliability"))
    expect_equal(app$paper()$track, "reliability")
    expect_null(nav$paper_track)                                 # same as the track in view
    session$setInputs(qew_chip_remove = "track")
    session$setInputs(qew_open_paper = list(id = "2203.00009", track = "exp_design"))
    expect_equal(app$paper()$track, "exp_design")
    expect_match(encode_view(current_view()), "ptrack=exp_design", fixed = TRUE)
    session$setInputs(qew_open_paper = "2501.00001")             # a plain id still works
    expect_equal(app$paper()$paper_id, "2501.00001")
  })
})

test_that("reasons for screening are tallied whatever their number", {
  papers <- fixture_papers()
  one <- scope_counts(papers, new_filter_state("spc"), TEST_SPEC, TEST_SETTINGS)$reasons
  expect_equal(one, data.frame(category = "keyword_in_passing", n = 1L, stringsAsFactors = FALSE))
  none <- scope_counts(papers, new_filter_state("exp_design"), TEST_SPEC, TEST_SETTINGS)$reasons
  expect_equal(nrow(none), 0L)
  unrecorded <- papers
  unrecorded$scope_category[unrecorded$status == "out_of_scope"] <- NA
  expect_equal(scope_counts(unrecorded, new_filter_state(), TEST_SPEC, TEST_SETTINGS)$reasons$category,
               "reason_not_recorded")
  expect_equal(scope_category_text(TEST_SPEC, "reason_not_recorded"), "Reason not recorded.")
  expect_equal(scope_category_text(TEST_SPEC, "keyword_in_passing"), TEST_SPEC$scope_categories[[3]][[2]])
})
