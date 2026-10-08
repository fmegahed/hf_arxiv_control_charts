# Daily update: refresh arXiv metadata for every track, then extract
# factsheets for new papers, new versions and earlier failures.
#
# Tracks, queries, the schema and the model all come from
# config/factsheet_spec.json. Usage (from the app directory):
#   Rscript 01_daily_update.r
# Environment:
#   OPENAI_API_KEY        required for extraction
#   EXTRACTION_ENABLED    "false" refreshes metadata only (default "true")
#   DAILY_MAX_SPEND_USD   stop extracting after this much spend (default 5)
#   DAILY_TIME_BUDGET_MIN stop extracting after this many minutes (default 240)

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
load_dotenv()

spec <- spec_load()
data_dir <- "data"
model <- spec$models$extraction
extraction_enabled <- !identical(tolower(Sys.getenv("EXTRACTION_ENABLED", "true")), "false")
budget <- as.numeric(Sys.getenv("DAILY_MAX_SPEND_USD", "5"))
time_budget <- as.numeric(Sys.getenv("DAILY_TIME_BUDGET_MIN", "240"))
started <- Sys.time()

search_arxiv <- function(query, limit = 10000L) {
  aRxiv::arxiv_search(query = query, limit = limit) |>
    tibble::as_tibble() |>
    dplyr::mutate(pdf_url = paste0("https://arxiv.org/pdf/", id)) |>
    dplyr::arrange(dplyr::desc(submitted))
}

llm <- make_llm(spec, model = model)
fetch_pdf <- make_fetch_pdf(file.path(tempdir(), "pdf_cache"), max_pages = spec$limits$max_pdf_pages)

for (track in TRACK_IDS) {
  info <- spec_track(spec, track)
  metadata_path <- file.path(data_dir, info$metadata_csv)
  factsheet_path <- file.path(data_dir, info$factsheet_csv)

  metadata <- tryCatch(search_arxiv(info$query), error = function(e) {
    warning(sprintf("[%s] arXiv search failed: %s", track, conditionMessage(e)))
    NULL
  })
  # An empty or failed search must never wipe the stored metadata.
  if (is.null(metadata) || nrow(metadata) == 0L) {
    message(sprintf("[%s] No search results; keeping existing metadata.", track))
    next
  }
  readr::write_csv(metadata, metadata_path)
  message(sprintf("[%s] %d papers in metadata.", track, nrow(metadata)))
  if (!extraction_enabled) next

  existing <- read_factsheet(factsheet_path)
  if (!is.null(existing) && !"paper_id" %in% names(existing)) {
    stop(factsheet_path, " is not in the v2 layout. Run the re-extraction and cutover first.")
  }
  factsheet <- if (is.null(existing)) NULL else coerce_factsheet(existing, spec, track)
  metadata <- read_factsheet(metadata_path)

  elapsed <- as.numeric(difftime(Sys.time(), started, units = "mins"))
  result <- run_track(
    track, spec, metadata, factsheet, llm, fetch_pdf, model,
    factsheet_path = factsheet_path,
    raw_path = file.path(data_dir, "raw_v2", paste0(track, ".jsonl")),
    time_budget_min = time_budget - elapsed, max_spend_usd = budget
  )
  budget <- budget - result$spent_usd
  message(sprintf("[%s] processed %d, remaining %d, spent USD %.3f%s", track, result$processed,
                  result$remaining, result$spent_usd,
                  if (is.na(result$stopped)) "" else paste0(", stopped: ", result$stopped)))
}

write_tracks_json(spec, file.path(data_dir, "tracks.json"))
