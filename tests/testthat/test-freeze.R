write_fixture_data <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  factsheet <- data.frame(
    is_spc_paper = c(TRUE, FALSE, NA),
    summary = c("a", "b", NA),
    id = c("1v1", "2v1", "3v2"),
    llm_model = "m1",
    stringsAsFactors = FALSE
  )
  utils::write.csv(factsheet, file.path(dir, "spc_factsheet.csv"), row.names = FALSE)
  utils::write.csv(data.frame(id = c("1v1", "2v1")), file.path(dir, "spc_arxiv_metadata.csv"),
                   row.names = FALSE)
  writeLines('{"spc": {}}', file.path(dir, "tracks.json"))
  c("spc_factsheet.csv", "spc_arxiv_metadata.csv", "tracks.json")
}

read_bytes <- function(path) readBin(path, "raw", file.size(path))

test_that("freeze copies files byte for byte and records per-file facts", {
  src <- withr::local_tempdir()
  dest <- file.path(withr::local_tempdir(), "v1")
  files <- write_fixture_data(src)

  manifest <- freeze_snapshot(src, dest, source_commit = "abc123", label = "v1", files = files)

  for (f in files) {
    expect_identical(read_bytes(file.path(src, f)), read_bytes(file.path(dest, f)))
  }
  expect_identical(manifest$source_commit, "abc123")
  fs <- manifest$files[[1]]
  expect_identical(fs$rows, 3L)
  expect_identical(fs$distinct_ids, 3L)
  expect_identical(fs$relevance_field, "is_spc_paper")
  expect_identical(c(fs$relevance_true, fs$relevance_false, fs$relevance_na), c(1L, 1L, 1L))
  expect_identical(fs$failed_rows, 1L)
  expect_null(manifest$files[[2]]$relevance_field)
  expect_null(manifest$files[[3]]$rows)
})

test_that("a fresh snapshot verifies and any later edit is detected", {
  src <- withr::local_tempdir()
  dest <- file.path(withr::local_tempdir(), "v1")
  files <- write_fixture_data(src)
  freeze_snapshot(src, dest, source_commit = "abc123", label = "v1", files = files)

  expect_true(verify_manifest(dest)$ok)

  cat("extra\n", file = file.path(dest, "tracks.json"), append = TRUE)
  check <- verify_manifest(dest)
  expect_false(check$ok)
  expect_match(check$problems, "tracks.json: checksum differs")

  unlink(file.path(dest, "spc_arxiv_metadata.csv"))
  expect_true(any(grepl("spc_arxiv_metadata.csv: missing", verify_manifest(dest)$problems)))
})

test_that("freeze refuses to overwrite a snapshot or to run with missing files", {
  src <- withr::local_tempdir()
  dest <- file.path(withr::local_tempdir(), "v1")
  files <- write_fixture_data(src)
  freeze_snapshot(src, dest, source_commit = "abc123", label = "v1", files = files)

  expect_error(freeze_snapshot(src, dest, "abc123", "v1", files = files), "already exists")
  expect_error(freeze_snapshot(src, file.path(dest, "x"), "abc123", "v1",
                               files = c(files, "nope.csv")), "missing files: nope.csv")
  expect_identical(verify_manifest(withr::local_tempdir())$problems, "MANIFEST.json not found")
})

test_that("the committed v1 snapshot is intact", {
  frozen <- app_path("data", "frozen", "v1")
  skip_if_not(dir.exists(frozen), "v1 snapshot not present")
  check <- verify_manifest(frozen)
  expect_true(check$ok, info = paste(check$problems, collapse = "; "))
})
