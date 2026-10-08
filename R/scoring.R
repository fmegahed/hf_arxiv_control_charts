# Scoring extraction runs: regression assertions drawn from author feedback,
# agreement between runs, and summary rates. Pure functions; used by the
# bake-off and by the live regression test.

# Does one factsheet row satisfy one assertion? Returns TRUE, FALSE, or NA when
# the assertion cannot be evaluated (the paper has no usable record).
check_assertion <- function(row, field, assertion, value) {
  if (is.null(row) || nrow(row) == 0L || identical(row$status, "failed")) return(NA)
  if (identical(field, "scope")) {
    decision <- if (identical(row$status, "out_of_scope")) "out_of_scope" else "in_scope"
    return(identical(decision, value))
  }
  if (!identical(row$status, "ok")) return(FALSE)
  present <- split_values(row[[field]])
  switch(
    assertion,
    equals = identical(present, value),
    includes = value %in% present,
    excludes = !(value %in% present),
    stop("Unknown assertion: ", assertion)
  )
}

# assertions: data frame with paper_id, track, field, assertion, value.
# factsheets: named list of v2 factsheets by track.
score_assertions <- function(assertions, factsheets) {
  assertions$passed <- vapply(seq_len(nrow(assertions)), function(i) {
    sheet <- factsheets[[assertions$track[i]]]
    row <- if (is.null(sheet)) NULL else sheet[sheet$paper_id %in% assertions$paper_id[i], , drop = FALSE]
    check_assertion(row, assertions$field[i], assertions$assertion[i], assertions$value[i])
  }, logical(1))
  assertions
}

# Share of papers on which two runs give the same value in `column`,
# among papers with an ok record in both.
agreement <- function(a, b, column) {
  both <- intersect(a$paper_id[a$status %in% "ok"], b$paper_id[b$status %in% "ok"])
  if (length(both) == 0L) return(list(n = 0L, agree = NA_real_, differing = character(0)))
  va <- a[[column]][match(both, a$paper_id)]
  vb <- b[[column]][match(both, b$paper_id)]
  same <- (is.na(va) & is.na(vb)) | (!is.na(va) & !is.na(vb) & va == vb)
  list(n = length(both), agree = mean(same), differing = both[!same])
}

# Share of ok records where the field's main label is the catch-all.
catch_all_rate <- function(sheet, column) {
  values <- sheet[[column]][sheet$status %in% "ok"]
  if (length(values) == 0L) return(NA_real_)
  mean(values %in% NONE_OF_LISTED)
}

flag_rate <- function(sheet, flag) {
  ok <- sheet$status %in% "ok"
  if (!any(ok)) return(NA_real_)
  mean(has_value(sheet$qa_flags[ok], flag))
}

normalize_for_match <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub(" +", " ", x))
}

# Is an evidence quote found in the paper text? Section pointers ("Section
# 3.2") and very short quotes cannot be checked and return NA.
evidence_found <- function(evidence, paper_text) {
  if (is.na(evidence) || !nzchar(evidence)) return(NA)
  quote <- normalize_for_match(sub("\\.\\.\\.$", "", evidence))
  if (grepl("^(section|sec|table|figure|fig|abstract|appendix|eq|equation) ?[0-9a-z. ]{0,12}$", quote)) return(NA)
  words <- strsplit(quote, " ", fixed = TRUE)[[1]]
  if (length(words) < 4L) return(NA)
  grepl(quote, normalize_for_match(paper_text), fixed = TRUE)
}
