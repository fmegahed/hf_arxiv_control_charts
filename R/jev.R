# Judging papers against the parts of a question that no filter covers.
#
# Those parts are relevance criteria (R/criteria.R). A decision model returns,
# for each paper and each criterion, the probability that the paper meets it.
# It writes no text, and it can only judge what it is shown, so the work is
# done in passes:
#   1. choose the papers to read: those whose text shares most words with the
#      criteria, then the most recent;
#   2. first reading, on title, authors and summary;
#   3. second reading of the papers the first one left undecided, on the
#      fuller factsheet;
#   4. a check that the answers are decisions at all. When most answers sit
#      near one half, the factsheets do not hold what the criterion asks for,
#      and the app says so instead of listing matches.
# Papers are sent in small batches requested in parallel. The function that
# performs the HTTP requests is passed in, so the logic is tested without a
# network. The service address and model name live in config/app_settings.json.

JEV_BATCH_SIZE <- 10L
JEV_MAX_PAPERS <- 300L
JEV_THRESHOLD <- 0.5
JEV_TIMEOUT_SEC <- 45
JEV_MAX_TRIES <- 3L
JEV_MAX_ACTIVE <- 8L
JEV_TEXT_MAX_CHARS <- 1500L
JEV_RETRY_STATUS <- c(429L, 529L)
KEYWORD_THRESHOLD <- 0.5
# Second reading: which first answers count as undecided, and how much is read.
JEV_BORDERLINE <- c(0.3, 0.7)
JEV_DETAIL_BATCH_SIZE <- 4L
JEV_DETAIL_MAX_PAPERS <- 60L
JEV_DETAIL_MAX_CHARS <- 5000L
JEV_DETAIL_SECTIONS <- c(summary = "Summary", key_results = "Key results",
                         limitations_stated = "Limitations stated by the authors",
                         future_work_stated = "Future work stated by the authors", abstract = "Abstract")
# The check of step 4: a criterion cannot be judged when at least this share
# of the answers lies in the band around one half.
JEV_UNCERTAIN_BAND <- c(0.35, 0.75)
JEV_UNCERTAIN_SHARE <- 0.5
JEV_GUARD_MIN_PAPERS <- 10L

# Up to `cap` papers for the model to read: most shared words first, then
# most recent. Attribute "capped" says whether any were left out.
jev_candidates <- function(papers, cap = JEV_MAX_PAPERS, terms = character(0)) {
  overlap <- if (length(terms) > 0L && !is.null(papers$search_text)) keyword_scores(papers, terms) else rep(0, nrow(papers))
  ordered <- papers[order(-overlap, -as.numeric(papers$submitted_date), na.last = TRUE), , drop = FALSE]
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

# The fuller factsheet for the second reading, section by section.
jev_paper_detail <- function(papers) {
  sections <- JEV_DETAIL_SECTIONS[names(JEV_DETAIL_SECTIONS) %in% names(papers)]
  pieces <- lapply(names(sections), function(column) {
    cell <- as.character(papers[[column]])
    ifelse(is.na(cell) | !nzchar(cell), "", paste0(sections[[column]], ": ", cell))
  })
  text <- do.call(paste, c(pieces, sep = "\n"))
  substr(trimws(gsub("\n{2,}", "\n", text)), 1L, JEV_DETAIL_MAX_CHARS)
}

jev_batches <- function(n, size = JEV_BATCH_SIZE) {
  if (n == 0L) return(list())
  unname(split(seq_len(n), ceiling(seq_len(n) / size)))
}

jev_question_key <- function(paper, criterion) paste0("p", paper, "_c", criterion)

# Request body for one batch: one yes/no question per paper and criterion.
# `references` holds the papers named by "similar to" criteria, by arXiv id.
jev_request_body <- function(papers, residual, model, detail = FALSE, references = list()) {
  criteria <- parse_criteria(residual)
  paper_keys <- paste0("p", seq_len(nrow(papers)))
  text <- if (detail) jev_paper_detail(papers) else jev_paper_text(papers)
  # The authors are part of the state so that a person's name which reaches
  # this step is judged against them. Without them the model cannot tell and
  # answers near one half for every paper.
  authors <- gsub(LIST_SEP, "; ", ifelse(is.na(papers$authors %||% NA), "", papers$authors %||% ""), fixed = TRUE)
  state_papers <- lapply(seq_len(nrow(papers)), function(i) {
    list(title = papers$title[i], authors = authors[i], text = text[i])
  })
  topics <- list()
  state_references <- list()
  questions <- list()
  for (j in seq_along(criteria)) {
    criterion <- criteria[[j]]
    name <- paste0("c", j)
    if (is.null(criterion$similar_to)) topics[[name]] <- criterion$text
    else state_references[[name]] <- references[[criterion$similar_to]]
    for (i in seq_along(paper_keys)) {
      paper_path <- paste0("`papers.", paper_keys[i], "`")
      questions[[jev_question_key(i, j)]] <- if (is.null(criterion$similar_to)) {
        list(type = "noul",
             instructions = paste0("Does the paper in ", paper_path, " match what the reader is looking for in `topics.", name, "`?"),
             criteria = list(
               true = paste("The paper's subject, method, data, application or reported results clearly involve the topic.",
                            "If the topic is a person's name, that person is one of the paper's authors."),
               false = paste("The topic is absent from the paper or only mentioned in passing.",
                             "If the topic is a person's name, that person is not among the paper's authors.",
                             "If the topic is not something a paper could be about, answer false.")))
      } else {
        list(type = "noul",
             instructions = paste0("Is the paper in ", paper_path, " about the same problem, solved with a related method, as the paper in `references.", name, "`?"),
             criteria = list(
               true = "Both papers address the same kind of problem and their methods are closely related.",
               false = "The papers share only a broad field, or differ in the problem they address."))
      }
    }
  }
  state <- list(papers = stats::setNames(state_papers, paper_keys))
  if (length(topics) > 0L) state$topics <- topics
  if (length(state_references) > 0L) state$references <- state_references
  list(model = model, state = state, questions = questions)
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


# Probabilities from one response body: a matrix with one row per paper of
# the batch and one column per criterion; NA where an answer is missing.
jev_parse_response <- function(body, n_papers, n_criteria) {
  out <- matrix(NA_real_, nrow = n_papers, ncol = n_criteria)
  for (i in seq_len(n_papers)) {
    for (j in seq_len(n_criteria)) {
      value <- body$answers[[jev_question_key(i, j)]]$noul
      if (is.numeric(value) && length(value) == 1L && !is.na(value)) out[i, j] <- min(max(as.numeric(value), 0), 1)
    }
  }
  out
}

jev_backoff <- function(attempt) 2^attempt

# One reading: every paper against every criterion. Requests answered with
# 429 or 529 are retried after a pause, up to max_tries rounds in total.
# Returns the matrix of probabilities (papers x criteria) and what it cost.
jev_score <- function(papers, residual, api_key, settings, perform = jev_perform,
                      sleep = Sys.sleep, max_tries = JEV_MAX_TRIES, batch_size = JEV_BATCH_SIZE,
                      detail = FALSE) {
  n_criteria <- length(parse_criteria(residual))
  batches <- jev_batches(nrow(papers), batch_size)
  bodies <- lapply(batches, function(rows) {
    jev_request_body(papers[rows, , drop = FALSE], residual, settings$jev$model, detail = detail,
                     references = settings$references %||% list())
  })
  by_criterion <- matrix(NA_real_, nrow = nrow(papers), ncol = n_criteria)
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
        by_criterion[rows, ] <- jev_parse_response(result$body, length(rows), n_criteria)
        for (key in names(usage)) usage[[key]] <- usage[[key]] + as.numeric(result$body$usage[[key]] %||% 0)
      } else if (isTRUE(result$status %in% JEV_RETRY_STATUS)) {
        retry <- c(retry, index)
      }
    }
    pending <- retry
  }
  scored <- rowSums(!is.na(by_criterion)) == n_criteria & n_criteria > 0L
  list(by_criterion = by_criterion, n_scored = sum(scored), n_unscored = sum(!scored),
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

# Rows the first reading left undecided on some criterion, the least decided
# first, at most `max_papers`.
jev_borderline_rows <- function(by_criterion, band = JEV_BORDERLINE, max_papers = JEV_DETAIL_MAX_PAPERS) {
  if (nrow(by_criterion) == 0L || ncol(by_criterion) == 0L) return(integer(0))
  distance <- apply(abs(by_criterion - 0.5), 1L, function(row) if (all(is.na(row))) NA_real_ else min(row, na.rm = TRUE))
  inside <- apply(by_criterion, 1L, function(row) any(!is.na(row) & row >= band[1] & row <= band[2]))
  rows <- which(inside)
  utils::head(rows[order(distance[rows])], max_papers)
}

# Criteria the model could not decide: most of its answers lie near one half.
jev_undecided_criteria <- function(by_criterion, band = JEV_UNCERTAIN_BAND, share = JEV_UNCERTAIN_SHARE,
                                   min_papers = JEV_GUARD_MIN_PAPERS) {
  which(vapply(seq_len(ncol(by_criterion)), function(j) {
    answers <- by_criterion[!is.na(by_criterion[, j]), j]
    length(answers) >= min_papers && mean(answers >= band[1] & answers <= band[2]) >= share
  }, logical(1)))
}

empty_scores <- function() {
  data.frame(paper_id = character(0), key = character(0), score = numeric(0), stringsAsFactors = FALSE)
}

# Rank papers by the criteria in `residual`.
# Returns list(scores, source, threshold, message, capped, total, considered):
#   source "jev"        probabilities from the decision model
#   source "keyword"    share of the criteria's words found in the paper's
#                       text, used when no key is set or the service fails
#   source "undecided"  the model could not judge a criterion from the
#                       factsheets; nothing is ranked and `message` says why
rank_papers <- function(papers, residual, settings, api_key = Sys.getenv("JEV_API_KEY"),
                        perform = jev_perform, sleep = Sys.sleep, cap = JEV_MAX_PAPERS) {
  criteria <- parse_criteria(residual)
  references <- settings$references %||% list()
  candidates <- jev_candidates(papers, cap, criteria_terms(criteria, references))
  base <- list(capped = isTRUE(attr(candidates, "capped")), total = nrow(papers),
               considered = nrow(candidates), residual = residual)
  keyword <- function(reason) {
    terms <- criteria_terms(criteria, references)
    scores <- data.frame(paper_id = papers$paper_id, key = paper_key(papers),
                         score = keyword_scores(papers, terms), stringsAsFactors = FALSE)
    c(list(scores = scores, source = "keyword", threshold = KEYWORD_THRESHOLD,
           message = paste0("Relevance ranking is unavailable (", reason,
                            "), so papers are ordered by how many words of the phrase they contain.")),
      utils::modifyList(base, list(capped = FALSE, considered = nrow(papers))))
  }
  undecided <- function(message) {
    c(list(scores = empty_scores(), source = "undecided", threshold = 0, message = message), base)
  }
  if (nrow(papers) == 0L || length(criteria) == 0L) {
    return(c(list(scores = empty_scores(), source = "none", threshold = JEV_THRESHOLD, message = ""), base))
  }
  missing_reference <- Filter(function(criterion) {
    !is.null(criterion$similar_to) && is.null(references[[criterion$similar_to]])
  }, criteria)
  if (length(missing_reference) > 0L) {
    return(undecided(paste0("Paper ", missing_reference[[1]]$similar_to,
                            " is not in this database, so papers cannot be compared with it.")))
  }
  if (!nzchar(api_key %||% "")) return(keyword("no access key is configured"))

  first <- jev_score(candidates, residual, api_key, settings, perform = perform, sleep = sleep)
  if (first$n_scored == 0L) return(keyword(jev_failure_text(first$statuses)))
  by_criterion <- first$by_criterion
  usage <- first$usage
  n_requests <- first$n_requests

  again <- jev_borderline_rows(by_criterion)
  if (length(again) > 0L) {
    second <- jev_score(candidates[again, , drop = FALSE], residual, api_key, settings, perform = perform,
                        sleep = sleep, batch_size = JEV_DETAIL_BATCH_SIZE, detail = TRUE)
    better <- !is.na(second$by_criterion)
    by_criterion[again, ][better] <- second$by_criterion[better]
    usage <- usage + second$usage
    n_requests <- n_requests + second$n_requests
  }

  # A criterion the model cannot decide is left out of the ranking. When no
  # criterion is left, nothing is ranked.
  stuck <- jev_undecided_criteria(by_criterion)
  message <- ""
  if (length(stuck) > 0L) {
    labels <- vapply(criteria[stuck], function(criterion) paste0("\"", criterion$text, "\""), character(1))
    message <- paste0(
      "The factsheets do not say enough to judge ", paste(labels, collapse = " and "),
      ", so the papers are not ranked by it. Remove it, or ask for a subject, method, application or result.")
    if (length(stuck) == length(criteria)) return(undecided(message))
    by_criterion <- by_criterion[, -stuck, drop = FALSE]
    criteria <- criteria[-stuck]
  }

  score <- combine_criteria_scores(by_criterion, criteria)
  if (first$n_unscored > 0L) {
    message <- trimws(paste(message, paste0(
      first$n_unscored, " papers could not be scored (", jev_failure_text(first$statuses),
      ") and are listed with the less likely matches.")))
  }
  c(list(scores = data.frame(paper_id = candidates$paper_id, key = paper_key(candidates), score = score,
                             stringsAsFactors = FALSE),
         source = "jev", threshold = JEV_THRESHOLD, message = message,
         usage = usage, n_requests = n_requests, n_second_reading = length(again)), base)
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

# The papers named by "similar to" criteria, as the model needs them.
criteria_references <- function(residual, all_papers) {
  ids <- unlist(lapply(parse_criteria(residual), function(criterion) criterion$similar_to))
  out <- list()
  for (id in unique(ids)) {
    row <- all_papers[all_papers$paper_id == id, , drop = FALSE]
    if (nrow(row) > 0L) out[[id]] <- list(title = row$title[1], text = jev_paper_text(row[1, , drop = FALSE]))
  }
  out
}

# Scores are remembered per residual, so narrowing the filters costs no new
# requests and widening them scores only the papers not seen before.
# Returns list(cache, ranking); `ranking` is what the results table shows.
ranking_update <- function(cache, papers, residual, settings, rank_fn = rank_papers) {
  if (is.null(cache) || !identical(cache$residual, residual)) {
    cache <- list(residual = residual, scores = empty_scores())
  }
  criteria <- parse_criteria(residual)
  candidates <- jev_candidates(papers, terms = criteria_terms(criteria, settings$references %||% list()))
  todo <- candidates[!paper_key(candidates) %in% cache$scores$key, , drop = FALSE]
  message <- ""
  if (nrow(todo) > 0L) {
    result <- rank_fn(todo, residual, settings)
    if (identical(result$source, "undecided")) {
      result$label <- residual
      return(list(cache = cache, ranking = result))
    }
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

# ---- A second opinion on the filters read from a question --------------------

# The language model sometimes adds a filter the question does not ask for
# ("lithium-ion batteries" becomes the application domain "Semiconductor and
# electronics"), which silently hides papers. The decision model is asked, for
# every filter value, whether the question asks for it.
jev_filter_check_body <- function(question, conditions, spec, model) {
  options <- list()
  questions <- list()
  index <- list()
  for (i in seq_along(conditions)) {
    cond <- conditions[[i]]
    field <- spec_field_any(spec, cond$field, cond$track)
    definitions <- tryCatch(field_definitions(field), error = function(e) NULL)
    for (value in cond$values) {
      key <- paste0("f", length(options) + 1L)
      definition <- if (!is.null(definitions) && value %in% names(definitions)) definitions[[value]] else NA
      options[[key]] <- list(field = field$label, option = value,
                             meaning = if (is.na(definition)) "" else unname(definition))
      questions[[key]] <- list(
        type = "noul",
        instructions = paste0("A reader searches a database of research papers with the request in `question`. ",
                              "Does the request ask for papers whose `options.", key, ".field` is `options.", key, ".option`?"),
        criteria = list(
          true = "The request names this option, a plain synonym of it, or something that is an instance of it by definition.",
          false = "The request does not mention it. It is only a guess about what the wanted papers might also be."))
      index[[key]] <- list(condition = i, value = value)
    }
  }
  list(body = list(model = model, state = list(question = question, options = options), questions = questions),
       index = index)
}

# Split conditions into those the question asks for and those it does not.
# When the service cannot be reached, every condition is kept.
check_question_filters <- function(question, conditions, spec, settings, api_key = Sys.getenv("JEV_API_KEY"),
                                   perform = jev_perform, threshold = JEV_THRESHOLD) {
  unchecked <- list(keep = conditions, demote = list(), checked = FALSE)
  if (length(conditions) == 0L || !nzchar(api_key %||% "")) return(unchecked)
  request <- jev_filter_check_body(question, conditions, spec, settings$jev$model)
  result <- tryCatch(perform(list(request$body), api_key, settings$jev$url)[[1]], error = function(e) NULL)
  if (is.null(result) || !isTRUE(result$status == 200L)) return(unchecked)
  keep <- list()
  demote <- list()
  for (i in seq_along(conditions)) {
    cond <- conditions[[i]]
    keys <- names(request$index)[vapply(request$index, function(entry) entry$condition == i, logical(1))]
    asked <- vapply(keys, function(key) {
      value <- result$body$answers[[key]]$noul
      !(is.numeric(value) && length(value) == 1L && !is.na(value) && value < threshold)
    }, logical(1))
    values <- vapply(request$index[keys], function(entry) entry$value, character(1))
    if (any(asked)) keep <- c(keep, list(utils::modifyList(cond, list(values = unname(values[asked])))))
    if (any(!asked)) demote <- c(demote, list(utils::modifyList(cond, list(values = unname(values[!asked])))))
  }
  list(keep = keep, demote = demote, checked = TRUE)
}
