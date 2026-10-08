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
