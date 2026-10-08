# Build a development dataset in the v2 layout by bridging the frozen v1
# factsheets. Lets the app be run and tested before the re-extraction exists.
#
# Usage (from the app directory): Rscript tools/build_dev_data.R [out_dir]
# Then run the app with QEW_DATA_DIR pointing at out_dir.

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1L) args[[1]] else file.path("data", "dev_v2")
frozen <- file.path("data", "frozen", "v1")

spec <- spec_load()
mapping <- read_bridge(file.path("config", "bridge_v1_v2.csv"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (track in TRACK_IDS) {
  info <- spec_track(spec, track)
  v1 <- read_factsheet(file.path(frozen, info$factsheet_csv))
  v2 <- bridge_v1_to_v2(v1, track, spec, mapping)
  write_factsheet_atomic(v2, file.path(out_dir, info$factsheet_csv))
  file.copy(file.path(frozen, info$metadata_csv), file.path(out_dir, info$metadata_csv), overwrite = TRUE)
  cat(sprintf("%-12s %4d papers: %s\n", track, nrow(v2),
              paste(names(table(v2$status)), table(v2$status), collapse = ", ")))
}
write_tracks_json(spec, file.path(out_dir, "tracks.json"))
