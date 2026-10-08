# Cleaning model-written text and equations before storage.
#
# Stored text follows one contract that the app relies on:
#   * math is delimited only by \( \) (inline) and \[ \] (display);
#   * there is no bare dollar sign;
#   * there are no control characters other than newline.
#
# JSON string escapes can silently eat the start of a LaTeX command: "\beta"
# arrives as <backspace>"eta", "\rho" as <carriage return>"ho". The repairs
# below restore those when the surrounding letters make the intent clear.

# Letters that complete a TeX command after each JSON escape character.
.TEX_AFTER_ESCAPE <- list(
  "\b" = c("ar", "eta", "ig", "igg", "Big", "Bigg", "inom", "oldsymbol", "m", "f", "egin",
           "ullet", "ot", "igcup", "igcap", "igl", "igr", "oxed", "eth", "ackslash"),
  "\f" = c("rac", "orall", "lat", "rown", "box"),
  "\r" = c("ho", "ight", "angle", "ceil", "floor", "m", "ightarrow", "Rightarrow", "aise"),
  "\t" = c("heta", "au", "imes", "ext", "ilde", "extbf", "extit", "extrm", "frac",
           "riangle", "op", "an", "anh"),
  # Newline is common in ordinary text, so only unambiguous completions.
  "\n" = c("u", "abla", "eq", "otin", "eg", "ewline", "leq", "geq", "mid")
)
.ESCAPE_LETTER <- c("\b" = "b", "\f" = "f", "\r" = "r", "\t" = "t", "\n" = "n")

# Restore TeX commands damaged by JSON escapes and drop other control
# characters. With latex = FALSE, tab and newline are ordinary whitespace.
repair_control_chars <- function(x, latex = FALSE) {
  if (is.na(x)) return(list(text = x, repaired = FALSE))
  original <- x
  escapes <- if (latex) names(.TEX_AFTER_ESCAPE) else c("\b", "\f", "\r")
  for (esc in escapes) {
    # Longest completions first so "\beta" is not read as "\b" + "eta" after
    # a shorter match such as "\be".
    tails <- .TEX_AFTER_ESCAPE[[esc]]
    tails <- tails[order(-nchar(tails))]
    pattern <- paste0(esc, "(", paste(tails, collapse = "|"), ")(?![a-zA-Z])")
    x <- gsub(pattern, paste0("\\\\", .ESCAPE_LETTER[[esc]], "\\1"), x, perl = TRUE)
  }
  x <- gsub("\r\n?", "\n", x)
  x <- gsub("\t", " ", x, fixed = TRUE)
  x <- gsub("[\\x01-\\x09\\x0B-\\x1F\\x7F]", "", x, perl = TRUE)
  list(text = x, repaired = !identical(x, original))
}

.count <- function(pattern, x, fixed = FALSE) {
  m <- gregexpr(pattern, x, fixed = fixed, perl = !fixed)[[1]]
  if (m[1] == -1L) 0L else length(m)
}

# Problems that would make an equation fail to typeset or be unsafe to show.
latex_problems <- function(x) {
  problems <- character()
  unescaped <- gsub("\\\\[{}]", "", x)
  if (.count("{", unescaped, fixed = TRUE) != .count("}", unescaped, fixed = TRUE)) {
    problems <- c(problems, "unbalanced braces")
  }
  if (.count("\\\\left(?![a-zA-Z])", x) != .count("\\\\right(?![a-zA-Z])", x)) {
    problems <- c(problems, "unbalanced \\left/\\right")
  }
  if (.count("\\\\begin\\{", x) != .count("\\\\end\\{", x)) {
    problems <- c(problems, "unbalanced \\begin/\\end")
  }
  if (grepl("\\\\(def|newcommand|renewcommand|href|url|includegraphics|input|write)(?![a-zA-Z])",
            x, perl = TRUE)) {
    problems <- c(problems, "disallowed command")
  }
  if (grepl("$", x, fixed = TRUE)) problems <- c(problems, "dollar sign inside equation")
  if (!nzchar(trimws(x))) problems <- c(problems, "empty")
  problems
}

# Clean one equation written without delimiters. Returns list(latex, ok,
# problems, repaired). Outer delimiters the model added anyway are removed.
sanitize_latex <- function(x) {
  if (is.null(x) || is.na(x)) {
    return(list(latex = NA_character_, ok = FALSE, problems = "empty", repaired = FALSE))
  }
  fixed <- repair_control_chars(x, latex = TRUE)
  latex <- gsub("\n", " ", fixed$text, fixed = TRUE)
  latex <- trimws(latex)
  for (pair in list(c("^\\$\\$", "\\$\\$$"), c("^\\$", "\\$$"),
                    c("^\\\\\\[", "\\\\\\]$"), c("^\\\\\\(", "\\\\\\)$"))) {
    if (grepl(pair[1], latex) && grepl(pair[2], latex)) {
      latex <- trimws(sub(pair[2], "", sub(pair[1], "", latex)))
    }
  }
  # "<" starts an HTML tag when followed by a letter; a space keeps it text.
  latex <- gsub("<(?=[A-Za-z/!])", "< ", latex, perl = TRUE)
  problems <- latex_problems(latex)
  list(latex = latex, ok = length(problems) == 0L, problems = problems,
       repaired = fixed$repaired || !identical(latex, trimws(x)))
}

# Remove math delimiters that have no partner (an opener never closed, or a
# closer never opened), leaving their content as plain text. MathJax would
# otherwise swallow the rest of the paragraph.
drop_unmatched_delims <- function(text, open, close) {
  hits <- gregexpr(paste0("\\Q", open, "\\E|\\Q", close, "\\E"), text, perl = TRUE)[[1]]
  if (hits[1] == -1L) return(text)
  tokens <- substring(text, hits, hits + 1L)
  drop <- logical(length(tokens))
  pending <- 0L
  for (i in seq_along(tokens)) {
    if (tokens[i] == open) {
      if (pending != 0L) drop[pending] <- TRUE
      pending <- i
    } else if (pending == 0L) {
      drop[i] <- TRUE
    } else {
      pending <- 0L
    }
  }
  if (pending != 0L) drop[pending] <- TRUE
  for (pos in rev(hits[drop])) {
    text <- paste0(substring(text, 1L, pos - 1L), substring(text, pos + 2L))
  }
  text
}

.MATH_HINT <- "[\\\\^_=]"

# Clean a narrative field (summary, results, limitations ...).
# Dollar signs are resolved in this order: "$$..$$" display math, currency
# ("$" directly before a digit, with no math syntax before the next "$"),
# "$..$" inline math, and any leftover "$" is dropped.
sanitize_narrative <- function(x) {
  if (is.null(x) || is.na(x)) return(list(text = NA_character_, repaired = FALSE))
  fixed <- repair_control_chars(x, latex = FALSE)
  text <- fixed$text

  text <- gsub("\\$\\$(.+?)\\$\\$", "\\\\[\\1\\\\]", text, perl = TRUE)

  # Walk the remaining single dollars left to right.
  pieces <- strsplit(text, "$", fixed = TRUE)[[1]]
  if (endsWith(text, "$")) pieces <- c(pieces, "")
  if (length(pieces) > 1L) {
    out <- pieces[1]
    i <- 2L
    while (i <= length(pieces)) {
      segment <- pieces[i]
      is_last <- i == length(pieces)
      looks_like_money <- grepl("^[0-9]", segment) && (is_last || !grepl(.MATH_HINT, segment))
      if (looks_like_money) {
        out <- paste0(out, "USD ", segment)
        i <- i + 1L
      } else if (!is_last && nzchar(trimws(segment))) {
        out <- paste0(out, "\\(", trimws(segment), "\\)", pieces[i + 1L])
        i <- i + 2L
      } else {
        out <- paste0(out, segment)
        i <- i + 1L
      }
    }
    text <- out
  }

  text <- drop_unmatched_delims(text, "\\(", "\\)")
  text <- drop_unmatched_delims(text, "\\[", "\\]")
  text <- gsub("<(?=[A-Za-z/!])", "< ", text, perl = TRUE)
  text <- gsub("[ ]{2,}", " ", text)
  text <- trimws(text)
  list(text = text, repaired = !identical(text, trimws(x)))
}

# Uppercase tokens of 2 to 6 letters that are neither in the glossary nor
# expanded in the text as "Full Name (ACR)".
find_undefined_acronyms <- function(text, glossary_terms = character(0),
                                    allow = c("USD", "II", "III", "IV", "VI", "US", "UK", "EU")) {
  if (is.na(text)) return(character(0))
  plain <- gsub("\\\\\\(.*?\\\\\\)|\\\\\\[.*?\\\\\\]", " ", text, perl = TRUE)
  tokens <- unique(regmatches(plain, gregexpr("\\b[A-Z][A-Z0-9]{1,5}\\b", plain, perl = TRUE))[[1]])
  tokens <- tokens[grepl("[A-Z]{2}", tokens)]
  expanded <- vapply(tokens, function(tok) grepl(paste0("(", tok, ")"), plain, fixed = TRUE),
                     logical(1))
  setdiff(tokens[!expanded], c(glossary_terms, allow))
}

# Display string for the equations of one paper. Each item is a list with
# name, latex and explanation. Invalid LaTeX is shown as code, never typeset.
render_equations <- function(equations) {
  if (length(equations) == 0L) return(NA_character_)
  parts <- vapply(equations, function(eq) {
    clean <- sanitize_latex(eq$latex)
    body <- if (clean$ok) paste0("\\[", clean$latex, "\\]") else paste0("`", clean$latex, "`")
    explanation <- sanitize_narrative(eq$explanation)$text
    paste0(trimws(eq$name), ": ", body, " ", explanation)
  }, character(1))
  paste(parts, collapse = "\n\n")
}

# Checks the storage contract on any text bound for a factsheet.
validate_stored_text <- function(x) {
  problems <- character()
  if (is.na(x)) return(list(ok = TRUE, problems = problems))
  if (grepl("$", x, fixed = TRUE)) problems <- c(problems, "bare dollar sign")
  if (grepl("[\\x01-\\x09\\x0B-\\x1F\\x7F]", x, perl = TRUE)) problems <- c(problems, "control character")
  if (.count("\\(", x, fixed = TRUE) != .count("\\)", x, fixed = TRUE)) {
    problems <- c(problems, "unbalanced inline math")
  }
  if (.count("\\[", x, fixed = TRUE) != .count("\\]", x, fixed = TRUE)) {
    problems <- c(problems, "unbalanced display math")
  }
  list(ok = length(problems) == 0L, problems = problems)
}
