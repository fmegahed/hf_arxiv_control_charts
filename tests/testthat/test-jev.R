jev_papers <- function(n = 25L) {
  data.frame(paper_id = sprintf("25%02d.%05d", 1L, seq_len(n)),
             title = paste("Title", seq_len(n)),
             summary = ifelse(seq_len(n) %% 5L == 0L, NA, paste("Summary", seq_len(n))),
             abstract = paste("Abstract", seq_len(n)),
             submitted_date = as.Date("2025-01-01") + seq_len(n),
             search_text = tolower(paste("title", seq_len(n), ifelse(seq_len(n) <= 3L, "wind turbines", "bearings"))),
             stringsAsFactors = FALSE)
}

# A fake service: answers every question with a probability derived from the
# paper's number, and records what it was sent.
fake_service <- function(statuses = NULL) {
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
        number <- as.integer(sub("Title ", "", bodies[[i]]$state$papers[[key]]$title))
        list(type = "noul", noul = number / 100)
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
  expect_equal(body$state$topic, "wind turbines")
  expect_named(body$state$papers, c("p1", "p2", "p3"))
  expect_named(body$questions, c("p1", "p2", "p3"))
  expect_equal(body$questions$p2$type, "noul")
  expect_match(body$questions$p2$instructions, "`papers.p2`", fixed = TRUE)
  expect_match(body$questions$p2$instructions, "`topic`", fixed = TRUE)
  expect_named(body$questions$p1$criteria, c("true", "false"))
  expect_equal(body$state$papers$p1$text, "Summary 1")
  json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  expect_true(jsonlite::validate(json))
  expect_equal(jsonlite::fromJSON(json)$questions$p1$type, "noul")
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
  body <- list(answers = list(p2 = list(type = "noul", noul = 0.2), p1 = list(type = "noul", noul = 0.9),
                              p3 = list(type = "noul", noul = 7), p4 = list(type = "noul")))
  expect_equal(jev_parse_response(body, c("p1", "p2", "p3", "p4", "p5")), c(0.9, 0.2, 1, NA, NA))
  expect_equal(jev_parse_response(NULL, "p1"), NA_real_)
})

test_that("scoring sends every paper once and maps answers back to papers", {
  papers <- jev_papers(25L)
  service <- fake_service()
  result <- jev_score(papers, "wind", "secret-key", TEST_SETTINGS, perform = service$perform, sleep = no_sleep)
  expect_length(service$log$calls, 1L)
  expect_length(service$log$calls[[1]]$bodies, 3L)
  expect_equal(service$log$calls[[1]]$api_key, "secret-key")
  expect_equal(service$log$calls[[1]]$url, TEST_SETTINGS$jev$url)
  expect_equal(result$scores$paper_id, papers$paper_id)
  expect_equal(result$scores$score, seq_len(25L) / 100)
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
