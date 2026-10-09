jev_papers <- function(n = 25L) {
  data.frame(paper_id = sprintf("25%02d.%05d", 1L, seq_len(n)),
             title = paste("Title", seq_len(n)),
             summary = ifelse(seq_len(n) %% 5L == 0L, NA, paste("Summary", seq_len(n))),
             abstract = paste("Abstract", seq_len(n)),
             submitted_date = as.Date("2025-01-01") + seq_len(n),
             authors = paste0("Author ", seq_len(n), "|Second Author"),
             search_text = tolower(paste("title", seq_len(n), ifelse(seq_len(n) <= 3L, "wind turbines", "bearings"))),
             stringsAsFactors = FALSE)
}

# A fake service: answers every question with a probability derived from the
# paper's number, and records what it was sent.
fake_service <- function(statuses = NULL, answer = function(number, criterion, detail) number / 100) {
  log <- new.env()
  log$calls <- list()
  perform <- function(bodies, api_key, url, ...) {
    round <- length(log$calls) + 1L
    log$calls[[round]] <- list(bodies = bodies, api_key = api_key, url = url)
    lapply(seq_along(bodies), function(i) {
      status <- if (is.null(statuses)) 200L else statuses(round, i)
      if (!identical(status, 200L)) return(list(status = status, body = NULL))
      keys <- names(bodies[[i]]$questions)
      answers <- lapply(keys, function(key) {
        paper <- bodies[[i]]$state$papers[[sub("_c[0-9]+$", "", key)]]
        number <- as.integer(sub("Title ", "", paper$title))
        criterion <- as.integer(sub("^.*_c", "", key))
        list(type = "noul", noul = answer(number, criterion, grepl("^Summary: |^Abstract: ", paper$text)))
      })
      list(status = 200L, body = list(model = bodies[[i]]$model, answers = stats::setNames(answers, keys),
                                      usage = list(input_tokens = 100, output_tokens = 10)))
    })
  }
  list(perform = perform, log = log)
}

no_sleep <- function(seconds) invisible(NULL)

test_that("a request carries one yes/no question per paper with backticked state paths", {
  papers <- jev_papers(3L)
  body <- jev_request_body(papers, "wind turbines", TEST_SETTINGS$jev$model)
  expect_equal(body$model, TEST_SETTINGS$jev$model)
  expect_equal(body$state$topics, list(c1 = "wind turbines"))
  expect_named(body$state$papers, c("p1", "p2", "p3"))
  expect_named(body$questions, c("p1_c1", "p2_c1", "p3_c1"))
  expect_equal(body$questions$p2_c1$type, "noul")
  expect_match(body$questions$p2_c1$instructions, "`papers.p2`", fixed = TRUE)
  expect_match(body$questions$p2_c1$instructions, "`topics.c1`", fixed = TRUE)
  expect_named(body$questions$p1_c1$criteria, c("true", "false"))
  expect_equal(body$state$papers$p1$text, "Summary 1")
  # the authors are shown, so that a name is judged against them
  expect_equal(body$state$papers$p1$authors, "Author 1; Second Author")
  expect_match(body$questions$p1_c1$criteria$true, "one of the paper's authors", fixed = TRUE)
  json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  expect_true(jsonlite::validate(json))
  expect_equal(jsonlite::fromJSON(json)$questions$p1_c1$type, "noul")
})

test_that("the abstract is used when a paper has no summary, and text is capped", {
  papers <- jev_papers(5L)
  expect_equal(jev_paper_text(papers)[5], "Abstract 5")
  papers$summary[1] <- strrep("x", JEV_TEXT_MAX_CHARS + 500L)
  expect_equal(nchar(jev_paper_text(papers)[1]), JEV_TEXT_MAX_CHARS)
})

test_that("papers are split into batches of the configured size", {
  batches <- jev_batches(25L, 10L)
  expect_equal(lengths(batches), c(10L, 10L, 5L))
  expect_equal(unlist(batches), 1:25)
  expect_length(jev_batches(0L), 0L)
  expect_true(JEV_BATCH_SIZE >= 10L && JEV_BATCH_SIZE <= 12L)
})

test_that("only the most recent papers up to the cap are candidates", {
  papers <- jev_papers(25L)
  capped <- jev_candidates(papers, cap = 10L)
  expect_equal(nrow(capped), 10L)
  expect_true(attr(capped, "capped"))
  expect_equal(min(capped$submitted_date), sort(papers$submitted_date, decreasing = TRUE)[10])
  expect_false(attr(jev_candidates(papers, cap = 300L), "capped"))
})

test_that("a response is parsed into probabilities in key order", {
  body <- list(answers = list(p2_c1 = list(type = "noul", noul = 0.2), p1_c1 = list(type = "noul", noul = 0.9),
                              p3_c1 = list(type = "noul", noul = 7), p4_c1 = list(type = "noul"),
                              p1_c2 = list(type = "noul", noul = 0.4)))
  parsed <- jev_parse_response(body, 5L, 2L)
  expect_equal(parsed[, 1], c(0.9, 0.2, 1, NA, NA))
  expect_equal(parsed[, 2], c(0.4, NA, NA, NA, NA))
  expect_true(all(is.na(jev_parse_response(NULL, 1L, 1L))))
})

test_that("scoring sends every paper once and maps answers back to papers", {
  papers <- jev_papers(25L)
  service <- fake_service()
  result <- jev_score(papers, "wind", "secret-key", TEST_SETTINGS, perform = service$perform, sleep = no_sleep)
  expect_length(service$log$calls, 1L)
  expect_length(service$log$calls[[1]]$bodies, 3L)
  expect_equal(service$log$calls[[1]]$api_key, "secret-key")
  expect_equal(service$log$calls[[1]]$url, TEST_SETTINGS$jev$url)
  expect_equal(dim(result$by_criterion), c(25L, 1L))
  expect_equal(result$by_criterion[, 1], seq_len(25L) / 100)
  expect_equal(result$n_scored, 25L)
  expect_equal(result$n_requests, 3L)
  expect_equal(result$usage[["input_tokens"]], 300)
})

test_that("requests answered with 429 or 529 are retried after a pause", {
  papers <- jev_papers(25L)
  slept <- numeric(0)
  service <- fake_service(function(round, i) if (round == 1L && i == 2L) 429L else if (round == 2L) 529L else 200L)
  result <- jev_score(papers, "wind", "k", TEST_SETTINGS, perform = service$perform,
                      sleep = function(seconds) slept <<- c(slept, seconds))
  expect_length(service$log$calls, 3L)
  expect_length(service$log$calls[[2]]$bodies, 1L)                # only the failed batch is sent again
  expect_equal(names(service$log$calls[[2]]$bodies[[1]]$state$papers)[1], "p1")
  expect_equal(slept, c(jev_backoff(1L), jev_backoff(2L)))
  expect_gt(slept[2], slept[1])
  expect_equal(result$n_scored, 25L)
  expect_equal(result$n_requests, 5L)
})

test_that("retries stop after the limit and other errors are not retried", {
  papers <- jev_papers(12L)
  always_busy <- fake_service(function(round, i) 429L)
  result <- jev_score(papers, "wind", "k", TEST_SETTINGS, perform = always_busy$perform, sleep = no_sleep)
  expect_length(always_busy$log$calls, JEV_MAX_TRIES)
  expect_equal(result$n_scored, 0L)
  rejected <- fake_service(function(round, i) 401L)
  result <- jev_score(papers, "wind", "k", TEST_SETTINGS, perform = rejected$perform, sleep = no_sleep)
  expect_length(rejected$log$calls, 1L)
  expect_match(jev_failure_text(result$statuses), "rejected the access key")
  expect_match(jev_failure_text(c(200L, NA)), "time limit")
  expect_match(jev_failure_text(c(529L, 200L)), "busy")
  expect_match(jev_failure_text(422L), "422")
  broken <- function(...) stop("connection refused")
  expect_equal(jev_score(papers, "wind", "k", TEST_SETTINGS, perform = broken, sleep = no_sleep)$n_scored, 0L)
})

test_that("ranking uses the service when a key is set", {
  papers <- jev_papers(25L)
  service <- fake_service()
  ranking <- rank_papers(papers, "wind turbines", TEST_SETTINGS, api_key = "k", perform = service$perform,
                         sleep = no_sleep)
  expect_equal(ranking$source, "jev")
  expect_equal(ranking$threshold, JEV_THRESHOLD)
  expect_false(ranking$capped)
  expect_equal(ranking$considered, 25L)
  expect_equal(ranking$message, "")
  capped <- rank_papers(papers, "wind", TEST_SETTINGS, api_key = "k", perform = fake_service()$perform,
                        sleep = no_sleep, cap = 10L)
  expect_true(capped$capped)
  expect_equal(c(capped$considered, capped$total), c(10L, 25L))
  expect_equal(nrow(capped$scores), 10L)
})

test_that("without a key, or when the service fails, ranking falls back to keywords and says why", {
  papers <- jev_papers(25L)
  never <- function(...) stop("the service must not be called without a key")
  no_key <- rank_papers(papers, "wind turbines", TEST_SETTINGS, api_key = "", perform = never)
  expect_equal(no_key$source, "keyword")
  expect_match(no_key$message, "no access key is configured")
  expect_equal(no_key$scores$score[1:4], c(1, 1, 1, 0))
  expect_equal(no_key$threshold, KEYWORD_THRESHOLD)

  down <- fake_service(function(round, i) 529L)
  failed <- rank_papers(papers, "wind turbines", TEST_SETTINGS, api_key = "k", perform = down$perform,
                        sleep = no_sleep)
  expect_equal(failed$source, "keyword")
  expect_match(failed$message, "the service is busy")
  expect_equal(nrow(failed$scores), 25L)

  partial <- fake_service(function(round, i) if (i == 3L) 422L else 200L)
  some <- rank_papers(papers, "wind", TEST_SETTINGS, api_key = "k", perform = partial$perform, sleep = no_sleep)
  expect_equal(some$source, "jev")
  expect_match(some$message, "5 papers could not be scored")
  expect_equal(rank_papers(papers[0, ], "wind", TEST_SETTINGS, api_key = "k", perform = never)$source, "none")
})

test_that("papers below the threshold are set aside, not dropped", {
  papers <- jev_papers(6L)
  scores <- data.frame(paper_id = papers$paper_id[1:5], score = c(0.9, 0.5, 0.49, NA, 0.7))
  groups <- group_by_relevance(papers, scores, threshold = 0.5)
  expect_equal(groups$likely$paper_id, papers$paper_id[c(1, 5, 2)])          # ordered by probability
  expect_setequal(groups$less_likely$paper_id, papers$paper_id[c(3, 4, 6)])  # low, unscored, never sent
  expect_equal(groups$less_likely$paper_id[1], papers$paper_id[3])
  expect_equal(nrow(groups$likely) + nrow(groups$less_likely), nrow(papers))
  expect_equal(JEV_THRESHOLD, 0.5)
})

test_that("scores are remembered per residual so narrowing costs no new requests", {
  papers <- jev_papers(25L)
  calls <- 0L
  rank_fn <- function(papers, residual, settings, ...) {
    calls <<- calls + 1L
    rank_papers(papers, residual, settings, api_key = "k", perform = fake_service()$perform, sleep = no_sleep)
  }
  first <- ranking_update(NULL, papers[1:10, ], "wind", TEST_SETTINGS, rank_fn)
  expect_equal(calls, 1L)
  expect_equal(nrow(first$ranking$scores), 10L)
  expect_equal(first$ranking$label, "wind")
  narrower <- ranking_update(first$cache, papers[1:4, ], "wind", TEST_SETTINGS, rank_fn)
  expect_equal(calls, 1L)                                           # nothing new to score
  wider <- ranking_update(narrower$cache, papers[1:15, ], "wind", TEST_SETTINGS, rank_fn)
  expect_equal(calls, 2L)
  expect_equal(nrow(wider$ranking$scores), 15L)
  other <- ranking_update(wider$cache, papers[1:4, ], "bearings", TEST_SETTINGS, rank_fn)
  expect_equal(calls, 3L)
  expect_equal(nrow(other$ranking$scores), 4L)

  failing <- function(papers, residual, settings, ...) rank_papers(papers, residual, settings, api_key = "")
  fallback <- ranking_update(NULL, papers, "wind turbines", TEST_SETTINGS, failing)
  expect_equal(fallback$ranking$source, "keyword")
  expect_equal(fallback$ranking$label, "wind turbines")
  expect_match(fallback$ranking$message, "no access key")
})

test_that("the key is read from JEV_API_KEY and never appears in a result", {
  papers <- jev_papers(3L)
  withr::with_envvar(c(JEV_API_KEY = ""), {
    ranking <- rank_papers(papers, "wind", TEST_SETTINGS)
    expect_equal(ranking$source, "keyword")
  })
  service <- fake_service()
  withr::with_envvar(c(JEV_API_KEY = "key-from-environment"), {
    ranking <- rank_papers(papers, "wind", TEST_SETTINGS, perform = service$perform, sleep = no_sleep)
  })
  expect_equal(service$log$calls[[1]]$api_key, "key-from-environment")
  expect_false(grepl("key-from-environment", paste(utils::capture.output(str(ranking)), collapse = " ")))
})

test_that("papers sharing words with the criteria are read before more recent ones", {
  papers <- jev_papers(25L)                       # papers 1 to 3 mention wind turbines and are the oldest
  by_words <- jev_candidates(papers, cap = 5L, terms = c("wind", "turbines"))
  expect_true(all(papers$paper_id[1:3] %in% by_words$paper_id))
  expect_false(any(papers$paper_id[1:3] %in% jev_candidates(papers, cap = 5L)$paper_id))
})

test_that("several criteria give one question each, and the weakest decides", {
  papers <- jev_papers(3L)
  body <- jev_request_body(papers, "wind turbines; -machine learning", TEST_SETTINGS$jev$model)
  expect_equal(body$state$topics, list(c1 = "wind turbines", c2 = "machine learning"))
  expect_length(body$questions, 6L)
  expect_match(body$questions$p3_c2$instructions, "`topics.c2`", fixed = TRUE)

  criteria <- parse_criteria("wind turbines; -machine learning")
  scores <- matrix(c(0.9, 0.9, 0.2,   0.1, 0.8, 0.1), ncol = 2L)
  expect_equal(combine_criteria_scores(scores, criteria), c(0.9, 0.2, 0.2))   # min(p1, 1 - p2)
  expect_equal(combine_criteria_scores(matrix(c(0.9, NA), ncol = 2L), criteria), NA_real_)

  service <- fake_service(answer = function(number, criterion, detail) if (criterion == 1L) 0.9 else 0.8)
  ranking <- rank_papers(jev_papers(12L), "wind turbines; -machine learning", TEST_SETTINGS, api_key = "k",
                         perform = service$perform, sleep = no_sleep)
  expect_equal(ranking$source, "jev")
  expect_equal(ranking$scores$score, rep(0.2, 12L))
})

test_that("papers the first reading leaves undecided are read again on the fuller factsheet", {
  papers <- jev_papers(12L)
  papers$key_results <- paste("Result", seq_len(12L))
  expect_match(jev_paper_detail(papers)[1], "^Summary: Summary 1\nKey results: Result 1\nAbstract: Abstract 1$")
  expect_equal(jev_borderline_rows(matrix(c(0.1, 0.5, 0.65, 0.9, NA), ncol = 1L)), c(2L, 3L))
  expect_length(jev_borderline_rows(matrix(seq(0.31, 0.69, length.out = 100L), ncol = 1L)), JEV_DETAIL_MAX_PAPERS)

  # first reading: papers 1 to 3 undecided; second reading settles them
  service <- fake_service(answer = function(number, criterion, detail) {
    if (detail) 0.95 else if (number <= 3L) 0.5 else 0.05
  })
  ranking <- rank_papers(papers, "wind", TEST_SETTINGS, api_key = "k", perform = service$perform, sleep = no_sleep)
  expect_equal(ranking$source, "jev")
  expect_equal(ranking$n_second_reading, 3L)
  expect_equal(sort(ranking$scores$score, decreasing = TRUE)[1:4], c(0.95, 0.95, 0.95, 0.05))
  detail_call <- service$log$calls[[2]]$bodies
  expect_length(detail_call, 1L)                                  # three papers fit one detail batch
  expect_match(detail_call[[1]]$state$papers$p1$text, "Key results: ", fixed = TRUE)
})

test_that("answers near one half mean the criterion cannot be judged, and nothing is ranked", {
  expect_equal(jev_undecided_criteria(matrix(c(rep(0.6, 8), 0.05, 0.95, rep(0.05, 10)), ncol = 2L)), 1L)
  expect_length(jev_undecided_criteria(matrix(c(rep(0.95, 8), 0.05, 0.6), ncol = 1L)), 0L)   # most match: decided
  expect_length(jev_undecided_criteria(matrix(rep(0.6, JEV_GUARD_MIN_PAPERS - 1L), ncol = 1L)), 0L)

  guessing <- fake_service(answer = function(number, criterion, detail) 0.6)
  ranking <- rank_papers(jev_papers(25L), "most cited", TEST_SETTINGS, api_key = "k", perform = guessing$perform,
                         sleep = no_sleep)
  expect_equal(ranking$source, "undecided")
  expect_equal(nrow(ranking$scores), 0L)
  expect_match(ranking$message, "do not say enough to judge \"most cited\"", fixed = TRUE)
  shown <- ranking_update(NULL, jev_papers(25L), "most cited", TEST_SETTINGS, function(papers, residual, settings) {
    rank_papers(papers, residual, settings, api_key = "k", perform = guessing$perform, sleep = no_sleep)
  })
  expect_equal(shown$ranking$source, "undecided")
  expect_equal(shown$ranking$label, "most cited")
  expect_equal(nrow(shown$cache$scores), 0L)                      # nothing is remembered as a score
})

test_that("a 'similar to' criterion sends the reference paper, and says so when it is unknown", {
  papers <- jev_papers(12L)
  references <- criteria_references("like:2501.00002", papers)
  expect_equal(references[["2501.00002"]]$title, "Title 2")
  expect_length(criteria_references("like:9999.99999; wind", papers), 0L)
  with_reference <- c(TEST_SETTINGS, list(references = references))
  body <- jev_request_body(papers[1:2, ], "like:2501.00002", TEST_SETTINGS$jev$model, references = references)
  expect_equal(body$state$references$c1$title, "Title 2")
  expect_null(body$state$topics)
  expect_match(body$questions$p1_c1$instructions, "`references.c1`", fixed = TRUE)
  service <- fake_service()
  expect_equal(rank_papers(papers, "like:2501.00002", with_reference, api_key = "k", perform = service$perform,
                           sleep = no_sleep)$source, "jev")
  unknown <- rank_papers(papers, "like:9999.99999", TEST_SETTINGS, api_key = "k",
                         perform = function(...) stop("must not be called"))
  expect_equal(unknown$source, "undecided")
  expect_match(unknown$message, "9999.99999 is not in this database", fixed = TRUE)
})

test_that("an undecided criterion is left out and the papers are ranked by the others", {
  service <- fake_service(answer = function(number, criterion, detail) if (criterion == 2L) 0.6 else number / 100)
  ranking <- rank_papers(jev_papers(25L), "wind; small budgets", TEST_SETTINGS, api_key = "k",
                         perform = service$perform, sleep = no_sleep)
  expect_equal(ranking$source, "jev")
  expect_match(ranking$message, "do not say enough to judge \"small budgets\"", fixed = TRUE)
  expect_equal(sort(ranking$scores$score), seq_len(25L) / 100)      # ranked by the first criterion alone
})

test_that("a second model decides which of the filters the question asks for", {
  conditions <- list(new_condition("application_domain", c("Healthcare and medical", "Manufacturing")),
                     new_condition("chart_approach", "Nonparametric (distribution-free)", "spc"))
  request <- jev_filter_check_body("nonparametric charts in hospitals", conditions, TEST_SPEC, TEST_SETTINGS$jev$model)
  expect_equal(request$body$state$question, "nonparametric charts in hospitals")
  expect_named(request$body$questions, c("f1", "f2", "f3"))
  expect_equal(request$body$state$options$f2$option, "Manufacturing")
  expect_true(nzchar(request$body$state$options$f3$meaning))
  expect_match(request$body$questions$f2$instructions, "`options.f2.option`", fixed = TRUE)

  answers <- function(values) function(bodies, api_key, url, ...) {
    keys <- names(bodies[[1]]$questions)
    list(list(status = 200L, body = list(answers = stats::setNames(
      lapply(values, function(v) list(type = "noul", noul = v)), keys))))
  }
  check <- function(perform, key = "k") {
    check_question_filters("nonparametric charts in hospitals", conditions, TEST_SPEC, TEST_SETTINGS,
                           api_key = key, perform = perform)
  }
  verdict <- check(answers(c(0.9, 0.1, 0.8)))
  expect_true(verdict$checked)
  expect_equal(lapply(verdict$keep, function(cond) cond$values),
               list("Healthcare and medical", "Nonparametric (distribution-free)"))
  expect_equal(verdict$demote[[1]]$field, "application_domain")
  expect_equal(verdict$demote[[1]]$values, "Manufacturing")
  expect_length(check(answers(c(0.1, 0.1, 0.9)))$keep, 1L)           # a field with no value left is dropped
  # a missing answer keeps the filter; an unreachable service or no key keeps them all
  expect_length(check(answers(c(0.9, NA, 0.9)))$demote, 0L)
  expect_false(check(function(...) stop("down"))$checked)
  expect_equal(check(function(...) stop("down"))$keep, conditions)
  expect_false(check(function(...) stop("must not be called"), key = "")$checked)
  expect_false(check_question_filters("q", list(), TEST_SPEC, TEST_SETTINGS, api_key = "k")$checked)
})
