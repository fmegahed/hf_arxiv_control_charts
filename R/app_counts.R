# Counting for the charts. Pure functions from a data frame of papers to small
# tables; the plotting code in app_charts.R only draws what these return.

RISING_RECENT_YEARS <- 3L
RISING_MIN_PAPERS <- 5L
TOP_AUTHORS_SHOWN <- 15L
COMPOSITION_BARS_SHOWN <- 12L

OTHER_LABEL <- "Other (not in the list)"

tidy_value <- function(value) ifelse(value == NONE_OF_LISTED, OTHER_LABEL, value)

# Papers per submission year and track, with empty years filled in.
papers_per_year <- function(papers) {
  papers <- papers[!is.na(papers$year), , drop = FALSE]
  if (nrow(papers) == 0L) {
    return(data.frame(year = integer(0), track = character(0), n = integer(0), stringsAsFactors = FALSE))
  }
  grid <- expand.grid(year = seq(min(papers$year), max(papers$year)), track = unique(papers$track),
                      stringsAsFactors = FALSE)
  counts <- as.data.frame(table(year = papers$year, track = papers$track), stringsAsFactors = FALSE,
                          responseName = "n")
  counts$year <- as.integer(counts$year)
  out <- merge(grid, counts, by = c("year", "track"), all.x = TRUE)
  out$n[is.na(out$n)] <- 0L
  out[order(out$year, out$track), , drop = FALSE]
}

# How many papers carry each value of one column. `total` is the number of
# papers with a recorded label, the denominator of `share`.
composition <- function(papers, column, exclude = character(0)) {
  empty <- data.frame(value = character(0), label = character(0), n = integer(0), share = numeric(0),
                      total = integer(0), stringsAsFactors = FALSE)
  if (!column %in% names(papers)) return(empty)
  cells <- papers[[column]]
  if (is.logical(cells)) cells <- ifelse(is.na(cells), NA, ifelse(cells, TRISTATE_VALUES[1], TRISTATE_VALUES[2]))
  labelled <- !is.na(cells) & nzchar(cells)
  counts <- count_values(cells[labelled], exclude = exclude)
  if (nrow(counts) == 0L) return(empty)
  data.frame(value = counts$value, label = tidy_value(counts$value), n = counts$n,
             share = counts$n / sum(labelled), total = sum(labelled), stringsAsFactors = FALSE)
}

# Yearly counts of chosen values of a column, with the year's paper count.
tag_trend <- function(papers, column, values) {
  papers <- papers[!is.na(papers$year), , drop = FALSE]
  if (nrow(papers) == 0L || length(values) == 0L || !column %in% names(papers)) {
    return(data.frame(year = integer(0), value = character(0), n = integer(0), papers = integer(0),
                      share = numeric(0), stringsAsFactors = FALSE))
  }
  years <- seq(min(papers$year), max(papers$year))
  long <- long_values(papers[[column]])
  total <- as.integer(table(factor(papers$year, levels = years)))
  rows <- lapply(values, function(value) {
    n <- as.integer(table(factor(papers$year[unique(long$row[long$value == value])], levels = years)))
    data.frame(year = years, value = value, n = n, papers = total,
               share = ifelse(total > 0L, n / total, NA_real_), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Cross-tabulation of two columns. A paper with several labels is counted in
# every cell it belongs to. Returns the long table and the two axis orders
# (most frequent first).
gap_map <- function(papers, column_a, column_b, exclude = character(0)) {
  empty <- list(cells = data.frame(a = character(0), b = character(0), n = integer(0),
                                   stringsAsFactors = FALSE),
                a_values = character(0), b_values = character(0), papers = 0L)
  if (!all(c(column_a, column_b) %in% names(papers)) || nrow(papers) == 0L) return(empty)
  as_text <- function(x) if (is.logical(x)) ifelse(is.na(x), NA, ifelse(x, TRISTATE_VALUES[1], TRISTATE_VALUES[2])) else x
  long_a <- unique(long_values(as_text(papers[[column_a]])))
  long_b <- unique(long_values(as_text(papers[[column_b]])))
  long_a <- long_a[!long_a$value %in% exclude, , drop = FALSE]
  long_b <- long_b[!long_b$value %in% exclude, , drop = FALSE]
  pairs <- merge(long_a, long_b, by = "row", suffixes = c("_a", "_b"))
  if (nrow(pairs) == 0L) return(empty)
  first_seen <- function(long) {
    kept <- long[long$row %in% pairs$row, , drop = FALSE]
    names(sort(table(kept$value), decreasing = TRUE))
  }
  a_values <- first_seen(long_a)
  b_values <- first_seen(long_b)
  grid <- expand.grid(a = a_values, b = b_values, stringsAsFactors = FALSE)
  counts <- as.data.frame(table(a = pairs$value_a, b = pairs$value_b), stringsAsFactors = FALSE,
                          responseName = "n")
  cells <- merge(grid, counts, by = c("a", "b"), all.x = TRUE)
  cells$n[is.na(cells$n)] <- 0L
  list(cells = cells, a_values = a_values, b_values = b_values, papers = length(unique(pairs$row)))
}

# Tags ranked by the change in their share of papers between the most recent
# `recent_years` years and all earlier years. `columns` is a named character
# vector: names are field labels, values are column names. Tags carried by
# fewer than `min_papers` papers in total are left out: their shares are noise.
rising_tags <- function(papers, columns, latest_year, recent_years = RISING_RECENT_YEARS,
                        min_papers = RISING_MIN_PAPERS, exclude = character(0)) {
  empty <- data.frame(field = character(0), column = character(0), value = character(0),
                      n_recent = integer(0), n_earlier = integer(0), papers_recent = integer(0),
                      papers_earlier = integer(0), share_recent = numeric(0),
                      share_earlier = numeric(0), change = numeric(0), stringsAsFactors = FALSE)
  papers <- papers[!is.na(papers$year), , drop = FALSE]
  cutoff <- latest_year - recent_years + 1L
  recent <- papers$year >= cutoff
  if (sum(recent) == 0L || sum(!recent) == 0L) return(empty)
  rows <- lapply(seq_along(columns), function(i) {
    column <- columns[[i]]
    if (!column %in% names(papers) || is.logical(papers[[column]])) return(NULL)
    long <- unique(long_values(papers[[column]]))
    long <- long[!long$value %in% exclude, , drop = FALSE]
    if (nrow(long) == 0L) return(NULL)
    values <- unique(long$value)
    count <- function(rows) as.integer(table(factor(long$value[rows], levels = values)))
    data.frame(field = names(columns)[i], column = column, value = values,
               n_recent = count(recent[long$row]), n_earlier = count(!recent[long$row]),
               papers_recent = sum(recent), papers_earlier = sum(!recent), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) return(empty)
  out <- out[out$n_recent + out$n_earlier >= min_papers, , drop = FALSE]
  out$share_recent <- out$n_recent / out$papers_recent
  out$share_earlier <- out$n_earlier / out$papers_earlier
  out$change <- out$share_recent - out$share_earlier
  out <- out[order(-out$change), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Share of papers with public code per year. Papers with no recorded code
# status are not in the denominator.
code_share_by_year <- function(papers) {
  papers <- papers[!is.na(papers$year) & !is.na(papers$code_public), , drop = FALSE]
  if (nrow(papers) == 0L) {
    return(data.frame(year = integer(0), papers = integer(0), public = integer(0), share = numeric(0)))
  }
  years <- seq(min(papers$year), max(papers$year))
  total <- vapply(years, function(y) sum(papers$year == y), integer(1))
  public <- vapply(years, function(y) sum(papers$year == y & papers$code_public), integer(1))
  data.frame(year = years, papers = total, public = public,
             share = ifelse(total > 0L, public / total, NA_real_))
}

# Each paper in exactly one group by the data it analyses.
data_use_split <- function(papers, settings, spec) {
  field <- spec_field_any(spec, settings$data_field)
  cells <- papers[[settings$data_field]]
  real <- has_any_value(cells, settings$real_data_values)
  simulated <- has_any_value(cells, settings$simulated_data_value)
  none <- has_any_value(cells, field$sentinel)
  group <- ifelse(is.na(cells), "Not recorded",
           ifelse(real & simulated, "Real and simulated data",
           ifelse(real, "Real data only",
           ifelse(simulated, "Simulated data only",
           ifelse(none, field$sentinel, "Not recorded")))))
  levels <- c("Real and simulated data", "Real data only", "Simulated data only", field$sentinel, "Not recorded")
  counts <- table(factor(group, levels = levels))
  out <- data.frame(group = levels, n = as.integer(counts), stringsAsFactors = FALSE)
  out$share <- if (nrow(papers) > 0L) out$n / nrow(papers) else NA_real_
  out$uses_real <- out$group %in% levels[1:2]
  out[out$n > 0L | out$group != "Not recorded", , drop = FALSE]
}

# One row per (author, paper).
author_table <- function(papers) {
  if (nrow(papers) == 0L) {
    return(data.frame(author = character(0), paper_id = character(0), year = integer(0),
                      stringsAsFactors = FALSE))
  }
  names <- lapply(papers$authors_merged %||% papers$authors, split_values)
  data.frame(author = unlist(names, use.names = FALSE),
             paper_id = rep(papers$paper_id, lengths(names)),
             year = rep(papers$year, lengths(names)), stringsAsFactors = FALSE)
}

author_counts <- function(papers) {
  authors <- author_table(papers)
  if (nrow(authors) == 0L) return(data.frame(author = character(0), n = integer(0), stringsAsFactors = FALSE))
  counts <- sort(table(authors$author), decreasing = TRUE)
  data.frame(author = names(counts), n = as.integer(counts), stringsAsFactors = FALSE)
}

team_sizes <- function(papers) {
  sizes <- papers$n_authors[!is.na(papers$n_authors) & papers$n_authors > 0L]
  if (length(sizes) == 0L) return(data.frame(size = integer(0), n = integer(0)))
  counts <- table(sizes)
  data.frame(size = as.integer(names(counts)), n = as.integer(counts))
}

# Values of a field that say "nothing applies"; left out of charts.
field_null_values <- function(field) {
  c(field$sentinel %||% character(0))
}
