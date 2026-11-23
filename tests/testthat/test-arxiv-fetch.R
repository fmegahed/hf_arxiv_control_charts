# fetch_arxiv_metadata() against a fake arXiv. The fake answers like
# aRxiv 0.18: a data frame with aRxiv's columns and a "total_results"
# attribute, a "Rate exceeded." warning with an empty page, or an error.

arxiv_page <- function(ids, total) {
  each <- function(x) rep(x, length.out = length(ids))  # an empty page has zero rows
  page <- data.frame(
    id = ids, submitted = each("2026-11-18 04:22:50"), updated = each("2026-11-18 04:22:50"),
    title = paste0("Paper ", ids)[seq_along(ids)], abstract = each("An abstract."),
    authors = each("A. Author|B. Author"), affiliations = each(""),
    link_abstract = paste0("https://arxiv.org/abs/", ids)[seq_along(ids)],
    link_pdf = paste0("https://arxiv.org/pdf/", ids)[seq_along(ids)], link_doi = each(""),
    comment = each(""), journal_ref = each(""), doi = each(""),
    primary_category = each("stat.AP"), categories = each("stat.AP|stat.OT"),
    stringsAsFactors = FALSE
  )
  attr(page, "total_results") <- as.integer(total)
  page
}

# What aRxiv 0.18 returns when arXiv answers 200 with "Rate exceeded.".
throttled_page <- function() {
  warning('arXiv error: "Rate exceeded."')
  page <- arxiv_page(character(), 0L)
  attr(page, "total_results") <- integer(0)
  page
}

# A fake arXiv holding `ids`. `answers` scripts the replies by call number:
# "ok", "error" (HTTP 429), "throttled" (empty 200) or "short" (a page
# missing its last paper). Calls after the script answer "ok".
fake_arxiv <- function(ids, answers = character()) {
  calls <- list()
  search <- function(query, start, limit, batchsize, sort_by, ascending, ...) {
    calls[[length(calls) + 1L]] <<- list(query = query, start = start, limit = limit,
                                          batchsize = batchsize, sort_by = sort_by,
                                          ascending = ascending,
                                          timeout = as.numeric(Sys.getenv("aRxiv_timeout")))
    answer <- if (length(calls) <= length(answers)) answers[[length(calls)]] else "ok"
    rows <- ids[seq_along(ids) > start & seq_along(ids) <= start + limit]
    switch(answer,
           ok = arxiv_page(rows, length(ids)),
           short = arxiv_page(utils::head(rows, -1L), length(ids)),
           throttled = throttled_page(),
           error = stop("Too Many Requests (RFC 6585) (HTTP 429)."))
  }
  list(search = search, calls = function() calls,
       starts = function() vapply(calls, function(x) x$start, numeric(1)))
}

waits_recorder <- function() {
  waits <- numeric()
  list(wait = function(s) waits <<- c(waits, s), waits = function() waits)
}

quiet <- function(...) invisible(NULL)
ids5 <- c("2601.00001v1", "2601.00002v1", "2601.00003v2", "2601.00004v1", "2601.00005v1")

test_that("pages are fetched up to arXiv's reported total, with no extra request", {
  arxiv <- fake_arxiv(ids5)
  out <- fetch_arxiv_metadata("ti:x", page_size = 2L, search = arxiv$search,
                              wait = waits_recorder()$wait, log = quiet)
  expect_identical(out$id, ids5)
  expect_identical(attr(out, "total_results"), 5L)
  expect_identical(arxiv$starts(), c(0, 2, 4))
})

test_that("each request is one page, oldest submissions first, with a long timeout", {
  arxiv <- fake_arxiv(ids5)
  fetch_arxiv_metadata("ti:x", page_size = 2L, search = arxiv$search,
                       wait = waits_recorder()$wait, log = quiet)
  first <- arxiv$calls()[[1]]
  expect_identical(first$query, "ti:x")
  # limit == batchsize keeps aRxiv from splitting a page into 100-paper requests.
  expect_equal(c(first$limit, first$batchsize), c(2, 2))
  # A stable order means a paper submitted mid-fetch cannot shift earlier pages.
  expect_identical(first$sort_by, "submitted")
  expect_true(first$ascending)
  # aRxiv's own 30 s timeout is what failed on 2026-11-23.
  expect_gt(first$timeout, 30)
})

test_that("a throttled page is retried without fetching earlier pages again", {
  arxiv <- fake_arxiv(ids5, answers = c("ok", "error", "ok", "ok"))
  rec <- waits_recorder()
  out <- fetch_arxiv_metadata("ti:x", page_size = 2L, wait_s = 10, search = arxiv$search,
                              wait = rec$wait, log = quiet)
  expect_identical(out$id, ids5)
  expect_identical(arxiv$starts(), c(0, 2, 2, 4))
  expect_length(rec$waits(), 1L)
})

test_that("an empty 'Rate exceeded.' page counts as throttled, and its warning is absorbed", {
  arxiv <- fake_arxiv(ids5, answers = c("throttled", "ok", "ok", "ok"))
  expect_no_warning(
    out <- fetch_arxiv_metadata("ti:x", page_size = 2L, wait_s = 10, search = arxiv$search,
                                wait = waits_recorder()$wait, log = quiet)
  )
  expect_identical(out$id, ids5)
  expect_identical(arxiv$starts(), c(0, 0, 2, 4))
})

test_that("a page reporting zero results counts as throttled, not as an empty track", {
  # aRxiv 0.20 NEWS: arXiv "sometimes giving empty results rather than an error".
  arxiv <- fake_arxiv(ids5, answers = "ok")
  empty_then_real <- function(...) {
    if (length(arxiv$calls()) == 0L) {
      arxiv$search(...)  # record the call
      return(arxiv_page(character(), 0L))
    }
    arxiv$search(...)
  }
  out <- fetch_arxiv_metadata("ti:x", page_size = 2L, wait_s = 10, search = empty_then_real,
                              wait = waits_recorder()$wait, log = quiet)
  expect_identical(out$id, ids5)
  expect_identical(arxiv$starts(), c(0, 0, 2, 4))
})

test_that("a page missing papers is retried rather than kept", {
  arxiv <- fake_arxiv(ids5, answers = c("ok", "short", "ok", "ok"))
  out <- fetch_arxiv_metadata("ti:x", page_size = 2L, wait_s = 10, search = arxiv$search,
                              wait = waits_recorder()$wait, log = quiet)
  expect_identical(out$id, ids5)
  expect_identical(arxiv$starts(), c(0, 2, 2, 4))
})

test_that("a page that never succeeds returns NULL after the allowed attempts, waiting longer each time", {
  arxiv <- fake_arxiv(ids5, answers = c("ok", rep("error", 10)))
  rec <- waits_recorder()
  out <- fetch_arxiv_metadata("ti:x", page_size = 2L, attempts = 4L, wait_s = 10,
                              search = arxiv$search, wait = rec$wait, log = quiet)
  expect_null(out)
  # One good first page, then four attempts at the second page.
  expect_identical(arxiv$starts(), c(0, 2, 2, 2, 2))
  # Three waits between four attempts: 10, 20, 40 s, each within +/- 25% jitter.
  waits <- rec$waits()
  expect_length(waits, 3L)
  expect_true(all(waits >= c(7.5, 15, 30) & waits <= c(12.5, 25, 50)))
})
