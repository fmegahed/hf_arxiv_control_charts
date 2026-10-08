test_that("settings refer only to fields and values that exist in the spec", {
  s <- TEST_SETTINGS
  shared <- spec_shared_fields(TEST_SPEC)
  for (key in c("data_field", "paper_type_field", "domain_field", "platform_field", "code_field")) {
    expect_true(s[[key]] %in% shared, info = key)
  }
  for (track in TRACK_IDS) expect_silent(spec_field(TEST_SPEC, track, s$evaluation_field))
  data_values <- field_values(spec_field_any(TEST_SPEC, s$data_field))
  expect_true(all(s$real_data_values %in% data_values))
  expect_true(s$simulated_data_value %in% data_values)
  expect_false(s$simulated_data_value %in% s$real_data_values)
  expect_true(all(s$review_paper_types %in% field_values(spec_field_any(TEST_SPEC, s$paper_type_field))))
  expect_match(s$issue_url, "^https://github.com/fmegahed/hf_arxiv_control_charts/issues/new$")
  expect_true(nzchar(s$jev$model) && grepl("^https://", s$jev$url))
})

test_that("no value or field name can break the URL and list encodings", {
  for (track in TRACK_IDS) {
    for (field in spec_fields(TEST_SPEC, track)) {
      expect_false(grepl("[.:;]", field$name), info = field$name)
      if (!is.null(field$values)) expect_false(any(grepl("|", field_values(field), fixed = TRUE)))
    }
  }
})

test_that("the loader joins on the base id and keeps one row per paper", {
  data <- fixture_data()
  papers <- data$papers
  expect_null(data$problems)
  expect_equal(nrow(papers), 10L)                       # 11 factsheet rows, one without metadata
  expect_false(any(duplicated(papers$paper_id)))
  expect_false("2501.00099" %in% papers$paper_id)
  expect_equal(unname(data$n_without_metadata[["spc"]]), 1L)
  expect_equal(unname(data$n_awaiting_factsheet[["spc"]]), 1L)

  first <- papers[papers$paper_id == "2501.00001", ]
  expect_equal(first$metadata_id, "2501.00001v2")       # latest metadata version, not v1
  expect_equal(first$year, 2025L)
  expect_equal(first$link_pdf, "https://arxiv.org/pdf/2501.00001v2")
  expect_equal(first$link_abstract, "https://arxiv.org/abs/2501.00001")
  expect_equal(first$authors_short, "Ann Author, Bo Writer, Cy Third and 1 more")
  expect_equal(first$n_authors, 4L)
  expect_true(first$code_public)
  expect_identical(first$assumes_normality, FALSE)
  expect_equal(first$topic, "Univariate")
  expect_equal(first$method, "EWMA")
  expect_equal(first$primary_category, "stat.AP")
  expect_type(papers$code_public, "logical")
  expect_equal(data$year_range, c(2019L, 2025L))
})

test_that("metadata math is converted to the storage contract and tags cannot open", {
  title <- fixture_papers()$title[fixture_papers()$paper_id == "2501.00001"]
  expect_false(grepl("$", title, fixed = TRUE))
  expect_match(title, "\\(\\bar{X}\\)", fixed = TRUE)
  expect_false(grepl("<[A-Za-z/]", title))
})

test_that("the catch-all label is shown as Other with the paper's own term", {
  papers <- fixture_papers()
  expect_equal(papers$topic[papers$paper_id == "2001.00003"], "Other: compositional data")
  expect_equal(display_value(c("EWMA", NONE_OF_LISTED), c(NA, NA)), c("EWMA", "Other"))
})

test_that("provenance and the data date come from the data, not the clock", {
  data <- fixture_data()
  expect_equal(data$data_date, as.Date("2026-03-01"))
  expect_setequal(data$provenance$track, TRACK_IDS)
  expect_true(all(data$provenance$llm_model == "test-extractor"))
  expect_equal(sum(data$provenance$n), sum(data$papers$status == "ok"))
  no_stamp <- data$papers
  no_stamp$extracted_at <- NA_character_
  expect_equal(data_current_date(no_stamp), max(no_stamp$submitted_date))
})

test_that("a directory in the old layout is reported, not half-loaded", {
  dir <- tempfile("old_layout_")
  dir.create(dir)
  for (track in TRACK_IDS) {
    info <- TEST_SPEC$tracks[[track]]
    readr::write_csv(data.frame(id = "2501.00001v1", summary = "x"), file.path(dir, info$factsheet_csv))
    readr::write_csv(fixture_metadata_row("2501.00001", 2025), file.path(dir, info$metadata_csv))
  }
  data <- load_app_data(TEST_SPEC, dir)
  expect_null(data$papers)
  expect_length(data$problems, 3L)
  expect_match(data$problems[1], "version 2 layout")
  expect_null(app_deps(TEST_SPEC, TEST_SETTINGS, data, reliability = NULL, question_fn = identity)$ctx)
})

test_that("the data directory comes from QEW_DATA_DIR", {
  withr::with_envvar(c(QEW_DATA_DIR = "somewhere/else"), expect_equal(app_data_dir(), "somewhere/else"))
  withr::with_envvar(c(QEW_DATA_DIR = NA), expect_equal(app_data_dir(), "data"))
})

test_that("the development dataset loads with consistent counts", {
  skip_without_dev_data()
  data <- load_app_data(TEST_SPEC, dev_data_dir())
  papers <- data$papers
  expect_null(data$problems)
  expect_false(any(duplicated(paper_key(papers))))           # one row per paper within a track
  for (track in TRACK_IDS) {
    info <- TEST_SPEC$tracks[[track]]
    factsheet <- read_factsheet(file.path(dev_data_dir(), info$factsheet_csv))
    metadata <- read_factsheet(file.path(dev_data_dir(), info$metadata_csv))
    joinable <- factsheet$paper_id %in% arxiv_base_id(metadata$id)
    expect_equal(sum(papers$track == track), sum(joinable), info = track)
    expect_equal(unname(data$n_without_metadata[[track]]), sum(!joinable), info = track)
    expect_equal(sum(papers$track == track & papers$status == "ok"),
                 sum(factsheet$status[joinable] == "ok"), info = track)
  }
  expect_true(all(papers$paper_id == arxiv_base_id(papers$metadata_id)))
  expect_true(all(!is.na(papers$year)))
  expect_true(all(is.na(papers$topic[papers$status != "ok"])))
  expect_false(any(grepl("$", papers$title, fixed = TRUE)))
})
