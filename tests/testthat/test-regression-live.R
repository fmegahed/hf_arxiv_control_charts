# Opt-in live check of the extraction against assertions drawn from author
# feedback. It calls the model (about USD 1 with the default model) and needs
# the private sample file, so it is skipped unless both are available:
#   RUN_LIVE_LLM=1 Rscript tests/testthat.R
# Pass thresholds: every hard scope assertion and at least 90% of the other
# hard assertions.

test_that("the current model, schema and prompts satisfy the author-feedback assertions", {
  skip_if_not(identical(Sys.getenv("RUN_LIVE_LLM"), "1"), "live model test not requested")
  sample_path <- app_path("analysis", "bakeoff", "local", "sample.csv")
  skip_if_not(file.exists(sample_path), "private sample file not present")
  load_dotenv(c(app_path(".env"), app_path("..", "..", ".env")))
  skip_if_not(nzchar(Sys.getenv("OPENAI_API_KEY")), "OPENAI_API_KEY not set")

  spec <- spec_load(app_path("config", "factsheet_spec.json"))
  sample <- read_factsheet(sample_path)
  assertions <- utils::read.csv(app_path("analysis", "bakeoff", "regression_assertions.csv"),
                                stringsAsFactors = FALSE)
  assertions$paper_id <- sample$paper_id[match(assertions$review_row, as.integer(sample$review_row))]
  expect_false(anyNA(assertions$paper_id))

  model <- spec$models$extraction
  llm <- make_llm(spec, model = model)
  fetch_pdf <- make_fetch_pdf(app_path("pdf_cache"), max_pages = spec$limits$max_pdf_pages)
  out_dir <- withr::local_tempdir()
  sheets <- list()
  for (track in unique(assertions$track)) {
    wanted <- unique(assertions$paper_id[assertions$track == track])
    metadata <- sample[sample$track == track & sample$paper_id %in% wanted, , drop = FALSE]
    result <- run_track(track, spec, metadata, NULL, llm, fetch_pdf, model,
                        factsheet_path = file.path(out_dir, paste0(track, ".csv")), log = function(...) NULL)
    skip_if(identical(result$stopped, "no_credit"), "the API account has no credit")
    sheets[[track]] <- result$factsheet
  }

  scored <- score_assertions(assertions, sheets)
  hard <- scored[scored$strength == "hard", ]
  report <- paste(sprintf("row %s %s %s %s: %s", hard$review_row, hard$field, hard$assertion,
                          hard$value, hard$passed), collapse = "\n")
  scope <- hard$passed[hard$field == "scope"]
  labels <- hard$passed[hard$field != "scope"]
  expect_true(all(scope %in% TRUE), info = report)
  expect_gte(mean(labels %in% TRUE), 0.9, label = paste0("share of label assertions passed\n", report))
})
