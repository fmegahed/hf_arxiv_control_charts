# Reading and writing factsheets.
#
# Multi-valued fields are stored in CSV as one string with values joined by
# LIST_SEP. No allowed value may contain the separator (enforced by the spec
# tests), so the round trip is lossless.

LIST_SEP <- "|"

# Character vector -> single string. NULL, empty and all-NA collapse to NA.
collapse_values <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- as.character(unlist(x, use.names = FALSE))
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0L) return(NA_character_)
  paste(trimws(x), collapse = LIST_SEP)
}

# Single string -> character vector. NA and "" give character(0).
split_values <- function(x) {
  if (length(x) == 0L || is.na(x) || !nzchar(x)) return(character(0))
  trimws(strsplit(x, LIST_SEP, fixed = TRUE)[[1]])
}

# TRUE where the delimited cell contains `value` as a whole element.
# (A substring test would match "Bayesian" inside "Bayesian design".)
has_value <- function(x, value) {
  vapply(x, function(cell) value %in% split_values(cell), logical(1), USE.NAMES = FALSE)
}

# Tabulate the elements of a delimited column, most frequent first.
count_values <- function(x, exclude = character(0)) {
  all_values <- unlist(lapply(x, split_values), use.names = FALSE)
  all_values <- all_values[!all_values %in% exclude]
  if (length(all_values) == 0L) {
    return(data.frame(value = character(0), n = integer(0), stringsAsFactors = FALSE))
  }
  tab <- sort(table(all_values), decreasing = TRUE)
  data.frame(value = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)
}

# Every column is read as text. Guessing types would turn ids such as
# "2403.01234" into numbers and reformat timestamps; callers that need typed
# columns apply coerce_factsheet().
read_factsheet <- function(path) {
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                  na = c("", "NA"), progress = FALSE)
}

# Write to a temporary file in the same directory and rename, so a crash never
# leaves a half-written factsheet behind.
write_factsheet_atomic <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  readr::write_csv(df, tmp, na = "NA")
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}
