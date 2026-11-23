# The weekly digest's week, and the archive its feed is built from.

papers_on <- function(dates) {
  data.frame(id = paste0("p", seq_along(dates)), submitted_date = as.Date(dates),
             stringsAsFactors = FALSE)
}

digest_for <- function(week_end, n_papers = 1L) {
  list(
    metadata = list(generated_at = "2026-11-23T12:00:00Z",
                    week_start = format(as.Date(week_end) - 7, "%Y-%m-%d"),
                    week_end = week_end, version = "3.1.0"),
    summary = list(total_papers = n_papers, papers_by_track = list(spc = n_papers)),
    synthesis = paste("Digest for", week_end),
    papers = lapply(seq_len(n_papers), function(i) list(id = paste0("p", i), track = "spc"))
  )
}

item_for <- function(week_end) {
  sprintf("    <item>\n      <guid isPermaLink=\"false\">qe-weekly-%s</guid>\n    </item>", week_end)
}

test_that("a week covers the seven days before its end and the end day itself", {
  window <- digest_window(as.Date("2026-11-16"))
  expect_identical(window$start, as.Date("2026-11-09"))
  expect_identical(window$end, as.Date("2026-11-16"))
})

test_that("only papers submitted inside the week are kept, both end days included", {
  papers <- papers_on(c("2026-11-08", "2026-11-09", "2026-11-12", "2026-11-16", "2026-11-17", NA))
  kept <- papers_in_window(papers, digest_window(as.Date("2026-11-16")))
  # A backfilled week must not take papers submitted after it.
  expect_identical(kept$id, c("p2", "p3", "p4"))
})

test_that("the week ends today unless a past date is given", {
  today <- as.Date("2026-11-23")
  expect_identical(parse_week_end(NULL, today = today), today)
  expect_identical(parse_week_end("", today = today), today)
  expect_identical(parse_week_end("2026-11-16", today = today), as.Date("2026-11-16"))
  expect_error(parse_week_end("2026-11-30", today = today), "future")
  expect_error(parse_week_end("16/11/2026", today = today), "YYYY-MM-DD")
  expect_error(parse_week_end("2026-02-30", today = today), "YYYY-MM-DD")
  # parse_cli_args() gives TRUE for "--week-end" with no value.
  expect_error(parse_week_end(TRUE, today = today), "YYYY-MM-DD")
})

test_that("the archive returns the newest weeks first, whatever order they were written in", {
  dir <- withr::local_tempdir()
  for (week_end in c("2026-11-16", "2026-11-23", "2026-11-09")) {
    save_digest_archive(digest_for(week_end), item_for(week_end), dir)
  }
  latest <- latest_digests(dir, n = 2L)
  expect_identical(vapply(latest, function(d) d$metadata$week_end, ""), c("2026-11-23", "2026-11-16"))
  expect_identical(latest[[1]]$rss_item, item_for("2026-11-23"))
})

test_that("writing a week again replaces it instead of adding a second entry", {
  dir <- withr::local_tempdir()
  save_digest_archive(digest_for("2026-11-23", n_papers = 1L), item_for("2026-11-23"), dir)
  save_digest_archive(digest_for("2026-11-23", n_papers = 3L), item_for("2026-11-23"), dir)
  latest <- latest_digests(dir)
  expect_length(latest, 1L)
  expect_identical(latest[[1]]$summary$total_papers, 3L)
})

test_that("an archived digest reads back as it was written", {
  dir <- withr::local_tempdir()
  digest <- digest_for("2026-11-16", n_papers = 2L)
  item <- paste0("    <item>\n      <description><![CDATA[\n<p>α & ≈ \"x\"</p>\n]]></description>\n",
                 "    </item>")
  save_digest_archive(digest, item, dir)
  back <- latest_digests(dir)[[1]]
  expect_identical(back$rss_item, item)
  back$rss_item <- NULL
  expect_identical(back, digest)
})

test_that("the current digest file is the newest week, written as the weekly job always wrote it", {
  dir <- withr::local_tempdir()
  newest <- digest_for("2026-11-23")
  newest$papers[[1]]$link <- NA_character_  # read back as NULL, which jsonlite writes as {} by default
  save_digest_archive(newest, item_for("2026-11-23"), dir)
  save_digest_archive(digest_for("2026-11-16"), item_for("2026-11-16"), dir)
  out <- withr::local_tempfile(fileext = ".json")
  write_latest_digest(latest_digests(dir), out)
  expected <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(newest, expected, pretty = TRUE, auto_unbox = TRUE)
  expect_identical(readLines(out), readLines(expected))
})

test_that("an empty or missing archive has no digests", {
  expect_length(latest_digests(withr::local_tempdir()), 0L)
  expect_length(latest_digests(file.path(withr::local_tempdir(), "none")), 0L)
})
