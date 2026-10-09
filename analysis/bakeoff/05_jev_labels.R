# Bake-off extra: label the sample with JEV (TypeSafe's decision model) instead
# of a generative model, so the label stage can be compared three ways.
#
# JEV reads text, not PDFs, and has a budget of 32k tokens for the state plus
# the longest question, so each paper is sent as extracted text: the start of
# the paper and its closing section, with the reference list removed. Questions
# are generated from config/factsheet_spec.json: one choice question per
# single-valued or primary label, one yes/no question per value of every
# multi-valued label.
#
# Usage (from the app directory):
#   Rscript analysis/bakeoff/05_jev_labels.R [--label jev] [--track spc] [--max-chars 85000]
# Output: analysis/bakeoff/local/runs/<label>/<track>_factsheet.csv in the v2
# layout (labels only), plus <track>_jev.jsonl with every probability.

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
load_dotenv()

JEV_URL <- "https://api.typesafe.ai/v1/systemone"
JEV_MODEL <- "jev-latest"
JEV_USD_PER_MTOK <- 0.042
YES <- 0.5

spec <- spec_load()
args <- parse_cli_args(commandArgs(trailingOnly = TRUE),
                       defaults = list(label = "jev", max_chars = "85000", pdf_cache = "pdf_cache"))
local_dir <- file.path("analysis", "bakeoff", "local")
run_dir <- file.path(local_dir, "runs", args$label)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
sample <- read_factsheet(file.path(local_dir, "sample.csv"))
tracks <- if (is.null(args$track)) TRACK_IDS else args$track

# ---- paper text ---------------------------------------------------------------

paper_text <- function(id, max_chars) {
  path <- file.path(args$pdf_cache, paste0(gsub("/", "_", id, fixed = TRUE), ".pdf"))
  pages <- suppressMessages(tryCatch(pdftools::pdf_text(path), error = function(e) character(0)))
  text <- paste(pages, collapse = "\n")
  text <- gsub("[ \t]+", " ", text)
  text <- gsub("\n{3,}", "\n\n", text)
  # Drop the reference list: the last heading-like "References" or "Bibliography".
  hits <- gregexpr("\n\\s*(References|REFERENCES|Bibliography|BIBLIOGRAPHY)\\s*\n", text)[[1]]
  if (hits[1] != -1L && utils::tail(hits, 1) > 0.5 * nchar(text)) text <- substr(text, 1L, utils::tail(hits, 1))
  if (nchar(text) <= max_chars) return(list(text = text, cut = FALSE))
  head_chars <- floor(0.72 * max_chars)
  tail_chars <- max_chars - head_chars
  list(text = paste0(substr(text, 1L, head_chars), "\n[...middle of the paper omitted...]\n",
                     substr(text, nchar(text) - tail_chars + 1L, nchar(text))), cut = TRUE)
}

# ---- questions from the spec ----------------------------------------------------

GATE <- paste("Judge only what THIS paper proposes or evaluates with its own results.",
              "Methods that appear only as background, in the literature review, or as a competitor do not count.")

option_map <- function(field) {
  values <- field_choices(field)
  defs <- field_definitions(field)[values]
  defs[values == NONE_OF_LISTED] <- "No listed option fits this paper."
  stats::setNames(lapply(defs, function(d) if (is.na(d)) NULL else d), values)
}

questions_for <- function(spec, track) {
  questions <- list()
  for (field in spec_fields(spec, track)) {
    name <- field$name
    if (field$kind %in% c("single", "primary_additional")) {
      questions[[paste0("choice__", name)]] <- list(
        type = "choice", instructions = paste(field$question, GATE), criteria = option_map(field))
    }
    if (field$kind %in% c("multi", "primary_additional")) {
      values <- setdiff(field_values(field), c(field$sentinel, "Not applicable"))
      defs <- field_definitions(field)
      for (i in seq_along(values)) {
        definition <- defs[[values[i]]]
        questions[[paste0("noul__", name, "__", i)]] <- list(
          type = "noul",
          instructions = paste0("Field: ", field$label, ". Does this option apply to the paper in `paper_text`? Option: ",
                                values[i], if (is.na(definition)) "" else paste0(" (", definition, ")"), ". ", GATE),
          criteria = list(true = "The paper itself proposes, uses or reports this.",
                          false = "The paper does not, or only mentions it in passing."))
      }
    }
    if (field$kind == "tristate") {
      questions[[paste0("noul__", name, "__1")]] <- list(
        type = "noul", instructions = paste(field$question, "Answer for the method proposed in `paper_text`."),
        criteria = list(true = "Yes, the paper states or clearly implies this.", false = "No."))
    }
  }
  questions
}

# ---- one request per paper --------------------------------------------------------

jev_request <- function(paper, track, questions, max_chars) {
  body <- paper_text(paper$id, max_chars)
  state <- list(title = paper$title, abstract = paper$abstract, paper_text = body$text)
  req <- httr2::request(JEV_URL) |>
    httr2::req_auth_bearer_token(Sys.getenv("JEV_API_KEY")) |>
    httr2::req_body_json(list(model = JEV_MODEL, state = state, questions = questions), auto_unbox = TRUE) |>
    httr2::req_retry(max_tries = 4, is_transient = function(r) httr2::resp_status(r) %in% c(429L, 529L),
                     backoff = function(i) 2 * i) |>
    httr2::req_timeout(120) |>
    httr2::req_error(is_error = function(r) FALSE)
  list(req = req, cut = body$cut, chars = nchar(body$text))
}

record_from_answers <- function(paper, track, answers, usage, cut) {
  record <- list(paper_id = paper$paper_id, arxiv_version = arxiv_version(paper$id), id = paper$id, track = track,
                 status = "ok", scope_decision = "in_scope", scope_category = "in_scope",
                 schema_version = spec$schema_version, prompt_version = spec$prompt_version, llm_model = JEV_MODEL,
                 extracted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 qa_flags = if (cut) "text_truncated" else NA_character_,
                 input_tokens = usage$input_tokens, cached_input_tokens = 0, output_tokens = usage$output_tokens,
                 cost_usd = usage$input_tokens * JEV_USD_PER_MTOK / 1e6, n_calls = 1L)
  probs <- list()
  max_additional <- spec$limits$max_additional_labels
  for (field in spec_fields(spec, track)) {
    name <- field$name
    choice <- answers[[paste0("choice__", name)]]
    yes <- NULL
    if (field$kind %in% c("multi", "primary_additional")) {
      values <- setdiff(field_values(field), c(field$sentinel, "Not applicable"))
      yes <- stats::setNames(vapply(seq_along(values), function(i) {
        answers[[paste0("noul__", name, "__", i)]]$noul
      }, numeric(1)), values)
      probs[[name]] <- as.list(round(yes, 3))
    }
    if (field$kind == "single") {
      record[[name]] <- choice$choice
      probs[[paste0(name, "__confidence")]] <- choice$confidence
    } else if (field$kind == "primary_additional") {
      primary <- choice$choice
      also <- names(sort(yes[yes >= YES & names(yes) != primary], decreasing = TRUE))
      also <- utils::head(also, max_additional)
      if (identical(primary, "Not applicable")) also <- character(0)
      record[[paste0(name, "_primary")]] <- primary
      record[[paste0(name, "_additional")]] <- collapse_values(also)
      record[[name]] <- collapse_values(c(primary, also))
      probs[[paste0(name, "__confidence")]] <- choice$confidence
    } else if (field$kind == "multi") {
      chosen <- names(sort(yes[yes >= YES], decreasing = TRUE))
      record[[name]] <- collapse_values(if (length(chosen) > 0L) chosen else field$sentinel)
    } else if (field$kind == "tristate") {
      p <- answers[[paste0("noul__", name, "__1")]]$noul
      record[[name]] <- if (p >= 0.7) TRUE else if (p <= 0.3) FALSE else NA
      probs[[name]] <- round(p, 3)
    }
    if (field_flag(field, "other")) {
      label <- record[[if (field$kind == "primary_additional") paste0(name, "_primary") else name]]
      record[[paste0(name, "_other_term")]] <- if (identical(label, NONE_OF_LISTED)) "unspecified" else NA_character_
    }
  }
  record$code_public <- record$code_availability %in% PUBLIC_CODE_SOURCES
  list(record = record, probs = probs)
}

for (track in tracks) {
  papers <- sample[sample$track == track, , drop = FALSE]
  questions <- questions_for(spec, track)
  out_path <- file.path(run_dir, paste0(track, "_factsheet.csv"))
  log_path <- file.path(run_dir, paste0(track, "_jev.jsonl"))
  unlink(log_path)
  rows <- list()
  started <- Sys.time()
  tokens <- 0
  for (chunk in split(seq_len(nrow(papers)), ceiling(seq_len(nrow(papers)) / 8))) {
    max_chars <- as.numeric(args$max_chars)
    pending <- chunk
    # A paper whose text is over the token budget is retried with less text.
    for (attempt in 1:4) {
      if (length(pending) == 0L) break
      built <- lapply(pending, function(i) jev_request(as.list(papers[i, ]), track, questions, max_chars))
      resps <- httr2::req_perform_parallel(lapply(built, function(b) b$req), on_error = "continue", progress = FALSE)
      retry <- integer()
      for (k in seq_along(pending)) {
        i <- pending[k]
        resp <- resps[[k]]
        status <- if (inherits(resp, "httr2_response")) httr2::resp_status(resp) else NA_integer_
        if (identical(status, 200L)) {
          body <- httr2::resp_body_json(resp)
          out <- record_from_answers(as.list(papers[i, ]), track, body$answers, body$usage, built[[k]]$cut)
          rows[[length(rows) + 1L]] <- record_to_row(out$record, spec, track)
          tokens <- tokens + body$usage$input_tokens
          cat(jsonlite::toJSON(list(paper_id = papers$paper_id[i], chars = built[[k]]$chars, cut = built[[k]]$cut,
                                    usage = body$usage, model = body$model, probs = out$probs),
                               auto_unbox = TRUE, null = "null"), "\n", file = log_path, append = TRUE, sep = "")
        } else {
          retry <- c(retry, i)
          if (attempt == 4L) {
            detail <- if (inherits(resp, "httr2_response")) substr(httr2::resp_body_string(resp), 1L, 200L) else conditionMessage(resp)
            message(sprintf("[%s] %s failed: %s %s", track, papers$id[i], status, detail))
          }
        }
      }
      pending <- retry
      max_chars <- floor(max_chars * 0.7)
    }
  }
  sheet <- dplyr::bind_rows(rows)
  sheet$attempts <- 0L
  write_factsheet_atomic(sheet, out_path)
  cat(sprintf("%-12s %d of %d papers, %.0f s, %.0f input tokens per paper, USD %.4f in total\n", track, nrow(sheet),
              nrow(papers), as.numeric(difftime(Sys.time(), started, units = "secs")), tokens / max(nrow(sheet), 1),
              tokens * JEV_USD_PER_MTOK / 1e6))
}
