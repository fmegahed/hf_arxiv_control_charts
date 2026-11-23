# Searching arXiv for a track's query, one page at a time.
#
# Since early 2026 the arXiv API throttles by server load: it answers
# 429 "Rate exceeded." to clients well under its documented limit, and can
# hold a request for 30 s first. aRxiv 0.18 turns a throttled page into an
# error or an empty page, and its own paging stops at the first empty page,
# so a search can come back short without failing. This code therefore:
#   - asks for large pages, so a track takes a few requests instead of dozens;
#   - keeps a page only if it holds every paper arXiv's own total says it
#     should, and retries a bad page after growing, jittered waits;
#   - returns NULL rather than a partial result.
# The search, the clock and the log are passed in, so this is testable with fakes.

fetch_arxiv_metadata <- function(query, page_size = 1000L, attempts = 5L, wait_s = 120,
                                 timeout_s = 180, search = aRxiv::arxiv_search,
                                 wait = Sys.sleep, log = message) {
  withr::local_envvar(aRxiv_timeout = timeout_s)
  pages <- list()
  start <- 0L
  repeat {
    page <- fetch_arxiv_page(query, start, page_size, attempts, wait_s, search, wait, log)
    if (is.null(page)) return(NULL)
    pages[[length(pages) + 1L]] <- page
    total <- attr(page, "total_results")
    start <- start + page_size
    if (start >= total) break
  }
  out <- do.call(rbind, pages)
  attr(out, "total_results") <- total
  out
}

# One page, retried until it holds every paper arXiv says it should.
fetch_arxiv_page <- function(query, start, page_size, attempts, wait_s, search, wait, log) {
  where <- sprintf("arXiv page from %d", start)
  for (attempt in seq_len(attempts)) {
    if (attempt > 1L) {
      pause <- wait_s * 2^(attempt - 2L) * stats::runif(1, 0.75, 1.25)
      log(sprintf("%s: waiting %.0f s before attempt %d of %d", where, pause, attempt, attempts))
      wait(pause)
    }
    page <- tryCatch(
      withCallingHandlers(
        search(query = query, start = start, limit = page_size, batchsize = page_size,
               sort_by = "submitted", ascending = TRUE),
        warning = function(w) {
          log(sprintf("%s: %s", where, conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        log(sprintf("%s: %s", where, conditionMessage(e)))
        NULL
      }
    )
    if (arxiv_page_complete(page, start, page_size)) return(page)
    if (!is.null(page)) {
      log(sprintf("%s: incomplete page (%d papers, arXiv total %s)", where, nrow(page),
                  paste(attr(page, "total_results"), collapse = "")))
    }
  }
  log(sprintf("%s: no complete page after %d attempts", where, attempts))
  NULL
}

# A page is complete when arXiv reported a total beyond `start` and the page
# holds every paper between `start` and the end of the page or of the results.
arxiv_page_complete <- function(page, start, page_size) {
  if (is.null(page)) return(FALSE)
  total <- attr(page, "total_results")
  if (length(total) != 1L || is.na(total) || total <= start) return(FALSE)
  nrow(page) == min(page_size, total - start)
}
