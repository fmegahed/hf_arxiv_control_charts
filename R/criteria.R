# Relevance criteria: the parts of a question that no filter can express.
#
# They live in the filter state as one string (`residual`), so a view can be
# shared in a link and typed by hand:
#   "wind turbines"                          one criterion
#   "wind turbines; small samples"           two; a paper has to meet both
#   "wind turbines; -machine learning"       the second must NOT hold
#   "like:2609.31338"                        similar to that paper
# A decision model judges each criterion for each paper (R/jev.R).

CRITERIA_SEP <- ";"
CRITERION_EXCLUDE_PREFIX <- "-"
CRITERION_SIMILAR_PREFIX <- "like:"
CRITERIA_MAX <- 4L
CRITERION_MAX_CHARS <- 120L

new_criterion <- function(text, exclude = FALSE, similar_to = NULL) {
  list(text = trimws(text), exclude = isTRUE(exclude), similar_to = similar_to)
}

# "a; -b; like:id" -> list of criteria. Empty pieces are dropped.
parse_criteria <- function(residual) {
  if (is.null(residual) || length(residual) != 1L || is.na(residual) || !nzchar(trimws(residual))) return(list())
  pieces <- trimws(strsplit(residual, CRITERIA_SEP, fixed = TRUE)[[1]])
  pieces <- pieces[nzchar(pieces)]
  out <- lapply(pieces, function(piece) {
    exclude <- startsWith(piece, CRITERION_EXCLUDE_PREFIX)
    if (exclude) piece <- trimws(substring(piece, nchar(CRITERION_EXCLUDE_PREFIX) + 1L))
    if (startsWith(tolower(piece), CRITERION_SIMILAR_PREFIX)) {
      id <- arxiv_base_id(trimws(substring(piece, nchar(CRITERION_SIMILAR_PREFIX) + 1L)))
      return(new_criterion(id, exclude, similar_to = id))
    }
    new_criterion(substr(piece, 1L, CRITERION_MAX_CHARS), exclude)
  })
  out <- Filter(function(criterion) nzchar(criterion$text), out)
  utils::head(out[!duplicated(vapply(out, criterion_code, character(1)))], CRITERIA_MAX)
}

criterion_code <- function(criterion) {
  paste0(if (criterion$exclude) CRITERION_EXCLUDE_PREFIX else "",
         if (!is.null(criterion$similar_to)) CRITERION_SIMILAR_PREFIX else "",
         criterion$text)
}

format_criteria <- function(criteria) {
  paste(vapply(criteria, criterion_code, character(1)), collapse = paste0(CRITERIA_SEP, " "))
}

# The text a criterion may not contain, because it is the separator.
clean_criterion_text <- function(text) trimws(gsub(CRITERIA_SEP, ",", text, fixed = TRUE))

criterion_label <- function(criterion) {
  if (!is.null(criterion$similar_to)) {
    return(paste0(if (criterion$exclude) "Not similar to: " else "Similar to: ", criterion$similar_to))
  }
  paste0(if (criterion$exclude) "Not about: " else "Ranked by relevance to: ", criterion$text)
}

# Words used to choose which papers the decision model reads first.
criteria_terms <- function(criteria, references = list()) {
  wanted <- Filter(function(criterion) !criterion$exclude, criteria)
  text <- vapply(wanted, function(criterion) {
    if (is.null(criterion$similar_to)) return(criterion$text)
    references[[criterion$similar_to]]$title %||% ""
  }, character(1))
  # Plural endings are cut off, so that "microarrays" also finds "microarray".
  # The words are matched as parts of words, so a shortened word still matches.
  terms <- keyword_terms(paste(text, collapse = " "))
  long <- nchar(terms) > 4L
  terms[long] <- sub("(ies|es|s)$", "", terms[long])
  unique(terms)
}

# One score per paper from a matrix with one column per criterion: a paper
# has to meet every criterion, so the weakest one decides. An excluded
# criterion counts as 1 - p.
combine_criteria_scores <- function(scores, criteria) {
  if (length(criteria) == 0L || nrow(scores) == 0L) return(rep(NA_real_, nrow(scores)))
  for (j in seq_along(criteria)) if (criteria[[j]]$exclude) scores[, j] <- 1 - scores[, j]
  apply(scores, 1L, function(row) if (anyNA(row)) NA_real_ else min(row))
}
