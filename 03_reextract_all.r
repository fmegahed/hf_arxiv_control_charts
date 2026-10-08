# Re-extract every paper with the current schema (the v2 backfill).
#
# Resumable: run it again and it continues where it stopped. Writes to
# data/staging_v2/ and never touches the published factsheets in data/.
#
# Usage (from the app directory):
#   Rscript 03_reextract_all.r [--track spc|exp_design|reliability|all]
#       [--model gpt-6-luna] [--max-papers N] [--time-budget-min 300]
#       [--max-spend-usd 60] [--shard 1 --n-shards 4] [--retry-failed] [--dry-run]
#       [--metadata-dir data] [--out-dir data/staging_v2] [--pdf-cache pdf_cache]
#
# Several shards can run at the same time in separate processes; each writes
# its own part file. Run with --merge to combine the parts into one factsheet
# per track once all shards are done.

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
load_dotenv()

spec <- spec_load()
args <- parse_cli_args(commandArgs(trailingOnly = TRUE), defaults = list(
  track = "all", model = spec$models$extraction, max_papers = "Inf", time_budget_min = "300",
  max_spend_usd = "60", shard = "1", n_shards = "1", metadata_dir = "data",
  out_dir = file.path("data", "staging_v2"), pdf_cache = "pdf_cache"
))
tracks <- if (identical(args$track, "all")) TRACK_IDS else args$track
shard <- as.integer(args$shard)
n_shards <- as.integer(args$n_shards)
parts_dir <- file.path(args$out_dir, "parts")
part_name <- function(track, s = shard) sprintf("%s_%dof%d", track, s, n_shards)

if (isTRUE(args$merge)) {
  for (track in tracks) {
    parts <- list.files(parts_dir, pattern = paste0("^", track, "_[0-9]+of[0-9]+\\.csv$"), full.names = TRUE)
    sheets <- lapply(parts, function(p) coerce_factsheet(read_factsheet(p), spec, track))
    merged <- dplyr::bind_rows(sheets)
    merged <- merged[!duplicated(merged$paper_id), , drop = FALSE]
    out <- file.path(args$out_dir, spec_track(spec, track)$factsheet_csv)
    write_factsheet_atomic(merged, out)
    cat(sprintf("%-12s %d parts, %d papers: %s -> %s\n", track, length(parts), nrow(merged),
                paste(names(table(merged$status)), table(merged$status), collapse = ", "), out))
  }
  quit(save = "no")
}

llm <- make_llm(spec, model = args$model)
fetch_pdf <- make_fetch_pdf(args$pdf_cache, max_pages = spec$limits$max_pdf_pages)
remaining_spend <- as.numeric(args$max_spend_usd)
started <- Sys.time()

for (track in tracks) {
  info <- spec_track(spec, track)
  metadata <- read_factsheet(file.path(args$metadata_dir, info$metadata_csv))
  part_path <- file.path(parts_dir, paste0(part_name(track), ".csv"))
  existing <- read_factsheet(part_path)
  factsheet <- if (is.null(existing)) NULL else coerce_factsheet(existing, spec, track)

  if (isTRUE(args$dry_run)) {
    plan <- plan_extraction(metadata, factsheet, spec$schema_version, spec$limits$max_attempts,
                            backfill = TRUE, retry_failed = isTRUE(args$retry_failed))
    plan <- plan[in_shard(plan$paper_id, shard, n_shards), , drop = FALSE]
    cat(sprintf("%-12s shard %d/%d: %s\n", track, shard, n_shards,
                paste(names(table(plan$action)), table(plan$action), collapse = ", ")))
    next
  }

  elapsed <- as.numeric(difftime(Sys.time(), started, units = "mins"))
  result <- run_track(
    track, spec, metadata, factsheet, llm, fetch_pdf, args$model,
    factsheet_path = part_path,
    raw_path = file.path(args$out_dir, "raw", paste0(part_name(track), ".jsonl")),
    backfill = TRUE, retry_failed = isTRUE(args$retry_failed),
    max_papers = as.numeric(args$max_papers),
    time_budget_min = as.numeric(args$time_budget_min) - elapsed,
    max_spend_usd = remaining_spend, shard = shard, n_shards = n_shards
  )
  remaining_spend <- remaining_spend - result$spent_usd
  cat(sprintf("%-12s processed %d, remaining %d, spent USD %.3f%s\n", track, result$processed,
              result$remaining, result$spent_usd,
              if (is.na(result$stopped)) "" else paste0(", stopped: ", result$stopped)))
  if (!is.na(result$stopped) && result$stopped %in% c("no_credit", "max_spend", "time_budget")) break
}
