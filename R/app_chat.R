# Chat with one paper and chat with the bookmarked collection.
# Both use the chat model named in the specification.

COLLECTION_CHAT_MAX_PDFS <- 10L

MATH_INSTRUCTION <- "Write mathematics in LaTeX between \\( and \\) for inline formulas and between \\[ and \\] for displayed ones. Do not use dollar signs as math delimiters."

paper_chat_system_prompt <- function(paper) {
  paste0(
    "You help a reader understand one academic paper. Answer from the attached PDF. ",
    "When you refer to a finding, name the section it comes from. ",
    "If the paper does not contain the answer, say so. Be concise. ", MATH_INSTRUCTION, "\n\n",
    "Paper metadata:\n",
    "Title: ", paper$title, "\n",
    "Authors: ", gsub(LIST_SEP, ", ", paper$authors, fixed = TRUE), "\n",
    "Submitted: ", as.character(paper$submitted_date)
  )
}

collection_chat_system_prompt <- function(spec) {
  topics <- vapply(spec$tracks, function(track) track$topic_name, character(1))
  paste0(
    "You help a reader compare a collection of academic papers in quality engineering (",
    paste(topics, collapse = ", "), "). Answer from the attached PDFs. ",
    "Identify shared themes and methods, compare approaches, point out complementary or conflicting findings, ",
    "and name gaps. Refer to papers by first author or title. ",
    "If the papers do not contain the answer, say so. ", MATH_INSTRUCTION
  )
}

# The bookmarked papers whose PDFs are attached: the most recent ones up to the cap.
collection_chat_papers <- function(papers, max_pdfs = COLLECTION_CHAT_MAX_PDFS) {
  ordered <- papers[order(papers$submitted_date, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  utils::head(ordered, max_pdfs)
}

PAPER_CHAT_SUGGESTIONS <- c(
  "What is the main contribution?",
  "Explain the method step by step",
  "What assumptions does the method make?",
  "What data are used to evaluate it?",
  "What are the limitations?"
)

chat_error_text <- function(message) {
  reason <- switch(classify_error(message),
                   no_credit = "the language-model account has no credit",
                   rate_limit = "the language model is rate limited",
                   timeout = "the language model timed out",
                   pdf_too_large = "the PDFs are too large for the model",
                   "the language model returned an error")
  paste0("The chat is unavailable: ", reason, ".")
}

# ---- Keeping math intact through the chat's markdown renderer ----------------
#
# The chat window renders answers as markdown, which would turn "\(" into "("
# and read "_" and "*" inside formulas as emphasis. math_guard() returns a
# function that is fed the answer piece by piece and gives back text in which
# every punctuation character between math delimiters is backslash-escaped, so
# the markdown renderer reproduces the formula exactly and the browser can
# typeset it. Call it with NULL at the end to flush what is held back.

MATH_TOKEN <- "\\\\\\\\|\\\\[()\\[\\]]"

math_guard <- function() {
  in_math <- FALSE
  held <- ""
  escape <- function(text) gsub("([[:punct:]])", "\\\\\\1", gsub("\\s*\n\\s*", " ", text))
  function(chunk) {
    final <- is.null(chunk)
    text <- paste0(held, if (final) "" else chunk)
    held <<- ""
    if (!final && grepl("\\\\$", text)) {
      # A trailing backslash may be the first half of a delimiter.
      held <<- "\\"
      text <- substr(text, 1L, nchar(text) - 1L)
    }
    if (!nzchar(text)) return("")
    hits <- gregexpr(MATH_TOKEN, text, perl = TRUE)[[1]]
    if (hits[1] == -1L) return(if (in_math) escape(text) else text)
    tokens <- regmatches(text, list(hits))[[1]]
    between <- regmatches(text, list(hits), invert = TRUE)[[1]]
    out <- character(0)
    for (i in seq_along(between)) {
      out <- c(out, if (in_math) escape(between[i]) else between[i])
      if (i > length(tokens)) next
      token <- tokens[i]
      opens <- token %in% c("\\(", "\\[")
      closes <- token %in% c("\\)", "\\]")
      if (opens && !in_math) in_math <<- TRUE
      out <- c(out, if (in_math || closes) escape(token) else token)
      if (closes && in_math) in_math <<- FALSE
    }
    paste(out, collapse = "")
  }
}

# The same for a whole answer at once.
guard_math_text <- function(text) {
  guard <- math_guard()
  paste0(guard(text), guard(NULL))
}

# Wrap a model's streamed answer (an async generator of text pieces).
guard_math_stream <- coro::async_generator(function(stream) {
  guard <- math_guard()
  for (chunk in coro::await_each(stream)) {
    piece <- guard(as.character(chunk))
    if (nzchar(piece)) coro::yield(piece)
  }
  rest <- guard(NULL)
  if (nzchar(rest)) coro::yield(rest)
})
