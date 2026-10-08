# Ranking papers by the part of a question that no filter covers.
#
# The ranking uses a decision model that returns, for each paper, the
# probability that it is about the topic. It writes no text. Papers are sent
# in small batches, one yes/no question per paper, with the batches requested
# in parallel. The function that performs the HTTP requests is passed in, so
# the logic here is tested without a network.
#
# The service address and model name live in config/app_settings.json.

JEV_BATCH_SIZE <- 10L
JEV_MAX_PAPERS <- 300L
JEV_THRESHOLD <- 0.5
JEV_TIMEOUT_SEC <- 45
JEV_MAX_TRIES <- 3L
JEV_MAX_ACTIVE <- 8L
JEV_TEXT_MAX_CHARS <- 1500L
JEV_RETRY_STATUS <- c(429L, 529L)
KEYWORD_THRESHOLD <- 0.5

# The most recent `cap` papers; attribute "capped" says whether any were left out.
jev_candidates <- function(papers, cap = JEV_MAX_PAPERS) {
  ordered <- papers[order(papers$submitted_date, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  out <- utils::head(ordered, cap)
  attr(out, "capped") <- nrow(papers) > cap
  attr(out, "total") <- nrow(papers)
  out
}

# Title plus summary, or title plus abstract when there is no summary.
jev_paper_text <- function(papers) {
  body <- ifelse(is.na(papers$summary) | !nzchar(papers$summary), papers$abstract, papers$summary)
  body[is.na(body)] <- ""
  substr(body, 1L, JEV_TEXT_MAX_CHARS)
}

jev_batches <- function(n, size = JEV_BATCH_SIZE) {
  if (n == 0L) return(list())
  unname(split(seq_len(n), ceiling(seq_len(n) / size)))
}

# Request body for one batch. Keys p1, p2, ... tie answers back to papers.
jev_request_body <- function(papers, residual, model) {
  keys <- paste0("p", seq_len(nrow(papers)))
  text <- jev_paper_text(papers)
  state_papers <- lapply(seq_len(nrow(papers)), function(i) list(title = papers$title[i], text = text[i]))
  questions <- lapply(keys, function(key) {
    list(type = "noul",
         instructions = paste0("Is the paper in `papers.", key, "` about `topic`?"),
         criteria = list(
           true = "The paper's subject, method, data or application clearly involves the topic.",
           false = "The topic is absent from the paper or only mentioned in passing."))
  })
  list(model = model,
       state = list(topic = residual, papers = stats::setNames(state_papers, keys)),
       questions = stats::setNames(questions, keys))
}

# Perform the requests in parallel. Returns one list(status, body) per body;
# status is NA when no HTTP response arrived (timeout, no connection).
jev_perform <- function(bodies, api_key, url, timeout = JEV_TIMEOUT_SEC, max_active = JEV_MAX_ACTIVE) {
  requests <- lapply(bodies, function(body) {
    httr2::request(url) |>
      httr2::req_headers(Authorization = paste("Bearer", api_key), .redact = "Authorization") |>
      httr2::req_body_json(body, auto_unbox = TRUE) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(resp) FALSE)
  })
  responses <- httr2::req_perform_parallel(requests, on_error = "continue", max_active = max_active,
                                           progress = FALSE)
  lapply(responses, function(resp) {
    if (!inherits(resp, "httr2_response")) return(list(status = NA_integer_, body = NULL))
    body <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
    list(status = httr2::resp_status(resp), body = body)
  })
}

# Probabilities from one response body, in the order of `keys`; NA where missing.
jev_parse_response <- function(body, keys) {
  vapply(keys, function(key) {
    value <- body$answers[[key]]$noul
    if (is.numeric(value) && length(value) == 1L && !is.na(value)) min(max(as.numeric(value), 0), 1) else NA_real_
  }, numeric(1), USE.NAMES = FALSE)
}

jev_backoff <- function(attempt) 2^attempt

# Score every paper against the residual. Requests answered with 429 or 529
# are retried after a pause, up to max_tries rounds in total.
jev_score <- function(papers, residual, api_key, settings, perform = jev_perform,
                      sleep = Sys.sleep, max_tries = JEV_MAX_TRIES, batch_size = JEV_BATCH_SIZE) {
  batches <- jev_batches(nrow(papers), batch_size)
  bodies <- lapply(batches, function(rows) jev_request_body(papers[rows, , drop = FALSE], residual,
                                                            settings$jev$model))
  score <- rep(NA_real_, nrow(papers))
  status <- rep(NA_integer_, length(batches))
  usage <- c(input_tokens = 0, output_tokens = 0)
  pending <- seq_along(batches)
  n_requests <- 0L
  for (attempt in seq_len(max_tries)) {
    if (length(pending) == 0L) break
    if (attempt > 1L) sleep(jev_backoff(attempt - 1L))
    results <- tryCatch(perform(bodies[pending], api_key, settings$jev$url),
                        error = function(e) lapply(pending, function(i) list(status = NA_integer_, body = NULL)))
    n_requests <- n_requests + length(pending)
    retry <- integer(0)
    for (k in seq_along(pending)) {
      index <- pending[k]
      result <- results[[k]]
      status[index] <- result$status %||% NA_integer_
      if (isTRUE(result$status == 200L)) {
        rows <- batches[[index]]
        score[rows] <- jev_parse_response(result$body, paste0("p", seq_along(rows)))
        for (key in names(usage)) usage[[key]] <- usage[[key]] + as.numeric(result$body$usage[[key]] %||% 0)
      } else if (isTRUE(result$status %in% JEV_RETRY_STATUS)) {
        retry <- c(retry, index)
      }
    }
    pending <- retry
  }
  list(scores = data.frame(paper_id = papers$paper_id, key = paper_key(papers), score = score,
                           stringsAsFactors = FALSE),
       n_scored = sum(!is.na(score)), n_unscored = sum(is.na(score)),
       statuses = status, n_requests = n_requests, usage = usage)
}

jev_failure_text <- function(statuses) {
  bad <- unique(statuses[is.na(statuses) | statuses != 200L])
  if (length(bad) == 0L) return("")
  if (any(is.na(bad))) return("no response before the time limit")
  if (401L %in% bad) return("the service rejected the access key")
  if (any(bad %in% JEV_RETRY_STATUS)) return("the service is busy")
  paste0("the service answered with status ", paste(bad, collapse = ", "))
}

# Rank papers by relevance to `residual`.
# Returns list(scores, source, capped, total, considered, message):
#   source "jev"      probabilities from the decision model
#   source "keyword"  share of the residual's words found in the paper's text,
#                     used when no key is set or the service fails
rank_papers <- function(papers, residual, settings, api_key = Sys.getenv("JEV_API_KEY"),
                        perform = jev_perform, sleep = Sys.sleep, cap = JEV_MAX_PAPERS) {
  candidates <- jev_candidates(papers, cap)
  base <- list(capped = isTRUE(attr(candidates, "capped")), total = nrow(papers),
               considered = nrow(candidates), residual = residual)
  keyword <- function(reason) {
    terms <- keyword_terms(residual)
    scores <- data.frame(paper_id = papers$paper_id, key = paper_key(papers),
                         score = keyword_scores(papers, terms), stringsAsFactors = FALSE)
    c(list(scores = scores, source = "keyword", threshold = KEYWORD_THRESHOLD,
           message = paste0("Relevance ranking is unavailable (", reason,
                            "), so papers are ordered by how many words of the phrase they contain.")),
      utils::modifyList(base, list(capped = FALSE, considered = nrow(papers))))
  }
  if (nrow(papers) == 0L) {
    return(c(list(scores = data.frame(paper_id = character(0), key = character(0), score = numeric(0)),
                  source = "none",
                  threshold = JEV_THRESHOLD, message = ""), base))
  }
  if (!nzchar(api_key %||% "")) return(keyword("no access key is configured"))
  result <- jev_score(candidates, residual, api_key, settings, perform = perform, sleep = sleep)
  if (result$n_scored == 0L) return(keyword(jev_failure_text(result$statuses)))
  message <- ""
  if (result$n_unscored > 0L) {
    message <- paste0(result$n_unscored, " papers could not be scored (", jev_failure_text(result$statuses),
                      ") and are listed with the less likely matches.")
  }
  c(list(scores = result$scores, source = "jev", threshold = JEV_THRESHOLD, message = message,
         usage = result$usage, n_requests = result$n_requests), base)
}

# Split ranked papers at the threshold. Papers below it, unscored or outside
# the scored set are "less likely matches": listed separately, never dropped.
group_by_relevance <- function(papers, scores, threshold = JEV_THRESHOLD) {
  score <- score_for(papers, scores)
  papers$relevance <- score
  likely <- !is.na(score) & score >= threshold
  order_by <- function(df) df[order(-ifelse(is.na(df$relevance), -1, df$relevance),
                                    -as.numeric(df$submitted_date)), , drop = FALSE]
  list(likely = order_by(papers[likely, , drop = FALSE]),
       less_likely = order_by(papers[!likely, , drop = FALSE]))
}

# Scores are remembered per residual, so narrowing the filters costs no new
# requests and widening them scores only the papers not seen before.
# Returns list(cache, ranking); `ranking` is what the results table shows.
ranking_update <- function(cache, papers, residual, settings, rank_fn = rank_papers) {
  if (is.null(cache) || !identical(cache$residual, residual)) {
    cache <- list(residual = residual,
                  scores = data.frame(paper_id = character(0), key = character(0), score = numeric(0),
                                      stringsAsFactors = FALSE))
  }
  candidates <- jev_candidates(papers)
  todo <- candidates[!paper_key(candidates) %in% cache$scores$key, , drop = FALSE]
  message <- ""
  if (nrow(todo) > 0L) {
    result <- rank_fn(todo, residual, settings)
    if (!identical(result$source, "jev")) {
      fallback <- rank_papers(papers, residual, settings, api_key = "")
      fallback$message <- result$message
      fallback$label <- residual
      return(list(cache = cache, ranking = fallback))
    }
    new_scores <- result$scores[!is.na(result$scores$score), , drop = FALSE]
    if (is.null(new_scores$key)) new_scores$key <- paper_key(todo)[match(new_scores$paper_id, todo$paper_id)]
    cache$scores <- rbind(cache$scores, new_scores[, c("paper_id", "key", "score"), drop = FALSE])
    message <- result$message
  }
  list(cache = cache,
       ranking = list(scores = cache$scores, source = "jev", threshold = JEV_THRESHOLD, message = message,
                      capped = isTRUE(attr(candidates, "capped")), total = nrow(papers),
                      considered = nrow(candidates), label = residual))
}

# Ranking shown for a keyword search (no residual): every listed paper
# contains at least one of the words, so nothing is set aside.
keyword_ranking <- function(papers, keywords) {
  list(scores = data.frame(paper_id = papers$paper_id, key = paper_key(papers),
                           score = keyword_scores(papers, keywords), stringsAsFactors = FALSE),
       source = "keyword", threshold = 0, message = "", capped = FALSE, total = nrow(papers),
       considered = nrow(papers), label = paste(keywords, collapse = ", "))
}
