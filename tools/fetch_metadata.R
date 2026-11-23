# Search arXiv with each track's query from the spec and write the metadata
# to a directory, in the same form as the daily job writes it. Used before a
# re-extraction so the published files in data/ are not touched.
#
# Usage (from the app directory):
#   Rscript tools/fetch_metadata.R [--out-dir data/staging_v2/metadata] [--track all]
# A track whose search fails or returns nothing is skipped and reported.

for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f)

spec <- spec_load()
args <- parse_cli_args(commandArgs(trailingOnly = TRUE),
                       defaults = list(out_dir = file.path("data", "staging_v2", "metadata"), track = "all"))
tracks <- if (identical(args$track, "all")) TRACK_IDS else args$track
dir.create(args$out_dir, recursive = TRUE, showWarnings = FALSE)

for (track in tracks) {
  info <- spec_track(spec, track)
  # Pages are retried and checked against arXiv's total (see R/arxiv_fetch.R).
  metadata <- fetch_arxiv_metadata(info$query, log = function(m) message(sprintf("[%s] %s", track, m)))
  if (is.null(metadata) || nrow(metadata) == 0L) {
    cat(sprintf("%-12s FAILED, nothing written\n", track))
    next
  }
  expected <- attr(metadata, "total_results")
  metadata <- tibble::as_tibble(metadata) |>
    dplyr::mutate(pdf_url = paste0("https://arxiv.org/pdf/", id)) |>
    dplyr::arrange(dplyr::desc(submitted))
  readr::write_csv(metadata, file.path(args$out_dir, info$metadata_csv))
  cat(sprintf("%-12s expected %s, wrote %d rows, %d distinct papers\n", track, expected, nrow(metadata),
              length(unique(arxiv_base_id(metadata$id)))))
}
