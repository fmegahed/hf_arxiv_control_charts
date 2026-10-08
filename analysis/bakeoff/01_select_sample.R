# Bake-off step 1: choose the papers both models will be run on.
#
# The sample is every author-reviewed paper plus a stratified random draw per
# track. The list of reviewed papers is sensitive (it shows which authors took
# part), so everything this script writes goes to analysis/bakeoff/local/,
# which is git-ignored.
#
# Usage (from the app directory):
#   AUTHOR_REVIEWS_CSV=../../author_reviews.csv Rscript analysis/bakeoff/01_select_sample.R

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

reviews_path <- Sys.getenv("AUTHOR_REVIEWS_CSV", file.path("..", "..", "author_reviews.csv"))
out_dir <- file.path("analysis", "bakeoff", "local")
frozen <- file.path("data", "frozen", "v1")
extra <- c(spc = 20L, exp_design = 30L, reliability = 30L)
out_of_scope_share <- c(spc = 0.2, exp_design = 0.2, reliability = 0.4)
set.seed(20261008)

reviews <- utils::read.csv(reviews_path, stringsAsFactors = FALSE)[, c("arxiv_id", "track")]
reviews$row <- seq_len(nrow(reviews))
spec <- spec_load()
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sample <- list()
for (track in TRACK_IDS) {
  info <- spec_track(spec, track)
  factsheet <- read_factsheet(file.path(frozen, info$factsheet_csv))
  metadata <- keep_latest_version(read_factsheet(file.path(frozen, info$metadata_csv)))
  flag <- as.logical(factsheet[[V1_RELEVANCE_FIELD[[track]]]])
  v1_scope <- stats::setNames(ifelse(is.na(flag), "failed", ifelse(flag, "in", "out")),
                              arxiv_base_id(factsheet$id))

  reviewed <- arxiv_base_id(reviews$arxiv_id[reviews$track == track])
  pool <- setdiff(arxiv_base_id(metadata$id), reviewed)
  pool_out <- pool[v1_scope[pool] %in% "out"]
  pool_in <- pool[v1_scope[pool] %in% "in"]
  n_out <- min(length(pool_out), round(extra[[track]] * out_of_scope_share[[track]]))
  drawn <- c(sample(pool_out, n_out), sample(pool_in, min(length(pool_in), extra[[track]] - n_out)))

  chosen <- c(reviewed, drawn)
  rows <- metadata[match(chosen, arxiv_base_id(metadata$id)), , drop = FALSE]
  rows$paper_id <- chosen
  rows$track <- track
  rows$reviewed <- chosen %in% reviewed
  rows$review_row <- reviews$row[match(chosen, arxiv_base_id(reviews$arxiv_id))]
  rows$v1_scope <- unname(v1_scope[chosen])
  rows <- rows[!is.na(rows$id), , drop = FALSE]
  sample[[track]] <- rows
  cat(sprintf("%-12s %3d papers (%d reviewed, %d v1 out of scope)\n", track, nrow(rows),
              sum(rows$reviewed), sum(rows$v1_scope %in% "out")))
}
sample <- dplyr::bind_rows(sample)
write_factsheet_atomic(sample, file.path(out_dir, "sample.csv"))
cat("Wrote", nrow(sample), "papers to", file.path(out_dir, "sample.csv"), "\n")
