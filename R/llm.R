# Model calls and PDF retrieval. This is the only file that talks to the
# network for extraction; everything else receives these functions as
# arguments.

get_openai_api_key <- function() Sys.getenv("OPENAI_API_KEY")

# Load KEY=VALUE lines from a .env file into the process environment.
# Existing variables are not overwritten. Values are never printed.
load_dotenv <- function(paths = c(".env", "../../.env")) {
  for (path in paths[file.exists(paths)]) {
    for (line in readLines(path, warn = FALSE)) {
      line <- trimws(line)
      if (!nzchar(line) || startsWith(line, "#") || !grepl("=", line, fixed = TRUE)) next
      key <- trimws(sub("=.*$", "", line))
      value <- gsub("^['\"]|['\"]$", "", trimws(sub("^[^=]*=", "", line)))
      if (!nzchar(Sys.getenv(key))) do.call(Sys.setenv, stats::setNames(list(value), key))
    }
  }
  invisible(NULL)
}

create_chat <- function(provider, model, system_prompt = NULL) {
  switch(
    provider,
    openai = ellmer::chat_openai(model = model, system_prompt = system_prompt,
                                 credentials = get_openai_api_key, echo = "none"),
    stop("Unsupported provider: ", provider)
  )
}

classify_error <- function(message) {
  if (grepl("429|rate limit|Too Many Requests", message, ignore.case = TRUE)) {
    if (grepl("credits|quota|billing", message, ignore.case = TRUE)) "no_credit" else "rate_limit"
  } else if (grepl("timeout|timed out", message, ignore.case = TRUE)) {
    "timeout"
  } else if (grepl("context|too large|maximum.*tokens|413", message, ignore.case = TRUE)) {
    "pdf_too_large"
  } else if (grepl("parse|JSON|schema", message, ignore.case = TRUE)) {
    "parse_error"
  } else {
    "api_error"
  }
}

# One structured call on a fresh chat. `contents` is a list of prompt parts in
# the order they are sent.
structured_call <- function(provider, model, system_prompt, contents, type) {
  tryCatch({
    chat <- create_chat(provider, model, system_prompt)
    data <- do.call(chat$chat_structured, c(contents, list(type = type)))
    tokens <- chat$get_tokens()
    input <- sum(tokens$input, tokens$cached_input, na.rm = TRUE)
    cached <- sum(tokens$cached_input, na.rm = TRUE)
    output <- sum(tokens$output, na.rm = TRUE)
    list(ok = TRUE, data = data,
         usage = list(input_tokens = input, cached_input_tokens = cached, output_tokens = output,
                      cost_usd = compute_cost(model, input, cached, output)))
  }, error = function(e) {
    message <- conditionMessage(e)
    list(ok = FALSE, data = NULL, usage = NULL,
         error_class = classify_error(message), error_message = message)
  })
}

SECOND_READER_URL <- "https://api.typesafe.ai/v1/systemone"

# One request to the decision model: the paper's text and one choice question
# per single-answer label. A request over the size limit is retried with less
# text. Returns the same shape as structured_call().
second_reader_call <- function(spec, track, paper, pdf_path, model = spec$models$second_reader,
                               api_key = Sys.getenv("JEV_API_KEY"), url = SECOND_READER_URL) {
  tryCatch({
    if (!nzchar(api_key)) stop("JEV_API_KEY is not set")
    pages <- suppressMessages(pdftools::pdf_text(pdf_path))
    questions <- second_reader_questions(spec, track)
    max_chars <- spec$limits$second_reader_max_chars
    for (attempt in 1:3) {
      text <- prepare_paper_text(pages, max_chars)
      request <- httr2::request(url)
      request <- httr2::req_auth_bearer_token(request, api_key)
      request <- httr2::req_body_json(
        request, auto_unbox = TRUE,
        data = list(model = model, questions = questions,
                    state = list(title = paper$title, abstract = paper$abstract, paper_text = text$text)))
      request <- httr2::req_retry(request, max_tries = 4, backoff = function(i) 2 * i,
                                  is_transient = function(r) httr2::resp_status(r) %in% c(429L, 529L))
      request <- httr2::req_timeout(request, 120)
      request <- httr2::req_error(request, is_error = function(r) FALSE)
      response <- httr2::req_perform(request)
      if (httr2::resp_status(response) == 200L) break
      if (attempt == 3L || httr2::resp_status(response) %in% c(401L, 403L)) {
        stop("HTTP ", httr2::resp_status(response), ": ", substr(httr2::resp_body_string(response), 1L, 200L))
      }
      max_chars <- floor(0.7 * max_chars)
    }
    body <- httr2::resp_body_json(response)
    parsed <- parse_second_reader(body$answers, spec, track)
    input <- body$usage$input_tokens
    list(ok = TRUE,
         data = list(labels = as.list(parsed$labels), confidence = as.list(parsed$confidence),
                     text_cut = text$cut, model = body$model),
         usage = list(input_tokens = input, cached_input_tokens = 0, output_tokens = 0,
                      cost_usd = compute_cost(model, input, 0, 0)))
  }, error = function(e) {
    list(ok = FALSE, data = NULL, usage = NULL, error_class = "second_reader_error",
         error_message = conditionMessage(e))
  })
}

# The stage functions used by extract_paper(). `second` and `tiebreak` are
# included when the spec names models for them; pass NULL to run without one.
make_llm <- function(spec, model = spec$models$extraction, provider = "openai",
                     screen_model = model, second_reader = spec$models$second_reader,
                     tie_break_model = spec$models$tie_break) {
  types <- list()
  type_for <- function(key, build) {
    if (is.null(types[[key]])) types[[key]] <<- build()
    types[[key]]
  }
  list(
    screen = function(track, paper) {
      structured_call(
        provider, screen_model, SCREEN_SYSTEM_PROMPT,
        list(screen_user_prompt(spec, track, paper$title, paper$abstract, paper$categories)),
        type_for("screen", function() build_screen_type(spec)))
    },
    classify = function(track, pdf_path) {
      structured_call(
        provider, model, PAPER_SYSTEM_PROMPT,
        list(ellmer::content_pdf_file(pdf_path), classify_user_prompt(spec, track)),
        type_for(paste0("classify_", track), function() build_classify_type(spec, track)))
    },
    narrate = function(track, pdf_path, labels_text) {
      structured_call(
        provider, model, PAPER_SYSTEM_PROMPT,
        list(ellmer::content_pdf_file(pdf_path), narrate_user_prompt(spec, track, labels_text)),
        type_for(paste0("narrate_", track), function() build_narrative_type(spec, track)))
    },
    second = if (is.null(second_reader)) NULL else function(track, paper, pdf_path) {
      second_reader_call(spec, track, paper, pdf_path, model = second_reader)
    },
    tiebreak = if (is.null(tie_break_model)) NULL else function(track, pdf_path, fields) {
      structured_call(
        provider, tie_break_model, PAPER_SYSTEM_PROMPT,
        list(ellmer::content_pdf_file(pdf_path), tiebreak_user_prompt(spec, track, fields)),
        build_tiebreak_type(spec, track, fields))
    }
  )
}

# Download a paper's PDF once into cache_dir, politely. PDFs longer than
# max_pages are cut to their first max_pages pages.
make_fetch_pdf <- function(cache_dir, max_pages = 80L, delay_sec = 3,
                           user_agent = "QE-ArXiv-Watch/2.0 (mailto:fmegahed@miamioh.edu)") {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  function(paper) {
    file_id <- gsub("/", "_", paper$id, fixed = TRUE)
    path <- file.path(cache_dir, paste0(file_id, ".pdf"))
    if (!file.exists(path)) {
      url <- paste0("https://arxiv.org/pdf/", paper$id)
      result <- tryCatch({
        httr2::request(url) |>
          httr2::req_user_agent(user_agent) |>
          httr2::req_retry(max_tries = 3, backoff = function(i) 5 * i) |>
          httr2::req_timeout(120) |>
          httr2::req_perform(path = path)
        NULL
      }, error = function(e) conditionMessage(e))
      Sys.sleep(delay_sec)
      if (!is.null(result)) {
        unlink(path)
        return(list(ok = FALSE, error_class = "pdf_download", error_message = result))
      }
    }
    pages <- tryCatch(qpdf::pdf_length(path), error = function(e) NA_integer_)
    if (is.na(pages)) {
      unlink(path)
      return(list(ok = FALSE, error_class = "pdf_download", error_message = "File is not a readable PDF"))
    }
    if (pages <= max_pages) return(list(ok = TRUE, path = path, truncated = FALSE, pages = pages))
    short <- file.path(cache_dir, paste0(file_id, "_first", max_pages, ".pdf"))
    if (!file.exists(short)) qpdf::pdf_subset(path, pages = seq_len(max_pages), output = short)
    list(ok = TRUE, path = short, truncated = TRUE, pages = pages)
  }
}
