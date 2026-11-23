# The weekly digest's week, and the archive its RSS feed is built from.
#
# Each digest is kept as data/weekly_digests/<week end>.json: the digest as
# written to data/weekly_digest.json plus its rendered RSS <item>. The feed
# lists the newest weeks from that archive, so a week written late (a missed
# Monday) is added to the feed without displacing the current week.

# The days a digest covers: the seven days before `week_end` and `week_end`
# itself, as the weekly job has always counted them.
digest_window <- function(week_end, days = 7L) {
  week_end <- as.Date(week_end)
  list(start = week_end - days, end = week_end)
}

papers_in_window <- function(papers, window) {
  submitted <- papers$submitted_date
  papers[!is.na(submitted) & submitted >= window$start & submitted <= window$end, , drop = FALSE]
}

# The --week-end argument: today when absent, otherwise a past or current date.
parse_week_end <- function(value, today = Sys.Date()) {
  if (is.null(value) || identical(value, "")) return(today)
  well_formed <- is.character(value) && length(value) == 1L && grepl("^\\d{4}-\\d{2}-\\d{2}$", value)
  week_end <- if (well_formed) as.Date(value, format = "%Y-%m-%d") else as.Date(NA)
  if (is.na(week_end)) stop("--week-end must be a date written YYYY-MM-DD, got: ", paste(value))
  if (week_end > today) stop("--week-end ", value, " is in the future")
  week_end
}

save_digest_archive <- function(digest, rss_item, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(digest$metadata$week_end, ".json"))
  jsonlite::write_json(c(digest, list(rss_item = rss_item)), path, pretty = TRUE, auto_unbox = TRUE)
  invisible(path)
}

# data/weekly_digest.json: the newest week of `digests` (from latest_digests()),
# in the form the weekly job has always written. A missing value was read
# back as NULL, so null = "null" writes it as null again rather than as {}.
write_latest_digest <- function(digests, path) {
  newest <- digests[[1]]
  newest$rss_item <- NULL
  jsonlite::write_json(newest, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(path)
}

# Up to `n` archived digests, newest week first.
latest_digests <- function(dir, n = 10L) {
  files <- list.files(dir, pattern = "^\\d{4}-\\d{2}-\\d{2}\\.json$", full.names = TRUE)
  files <- utils::head(sort(files, decreasing = TRUE), n)
  lapply(files, jsonlite::read_json, simplifyVector = FALSE)
}
