# Bake-off step 2: run one model over the sample.
#
# Usage (from the app directory):
#   Rscript analysis/bakeoff/02_run_model.R --model gpt-6-luna --label luna
#   Rscript analysis/bakeoff/02_run_model.R --model gpt-6-luna --label luna_b
#   Rscript analysis/bakeoff/02_run_model.R --model gpt-6.1-sol --label sol
#
# The screen always uses --screen-model (default gpt-6-luna) unless the label
# run sets it, so the PDF stages of both models see the same set of papers
# only when you pass --force-in-scope. With --force-in-scope every sampled
# paper goes through classify and narrate regardless of the screen decision,
# which is what a like-for-like comparison of the PDF stages needs.
# Resumable; outputs go to analysis/bakeoff/local/runs/<label>/.
# Add --track spc|exp_design|reliability to run one track, so the three tracks
# of a label can run as three processes at once.

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
load_dotenv()

spec <- spec_load()
args <- parse_cli_args(commandArgs(trailingOnly = TRUE),
                       defaults = list(model = "gpt-6-luna", label = "luna", max_spend_usd = "25",
                                       pdf_cache = "pdf_cache"))
local_dir <- file.path("analysis", "bakeoff", "local")
run_dir <- file.path(local_dir, "runs", args$label)
sample <- read_factsheet(file.path(local_dir, "sample.csv"))

llm <- make_llm(spec, model = args$model,
                screen_model = if (is.null(args$screen_model)) args$model else args$screen_model)
if (isTRUE(args$force_in_scope)) {
  screen <- llm$screen
  llm$screen <- function(track, paper) {
    result <- screen(track, paper)
    if (result$ok && identical(result$data$scope_decision, "out_of_scope")) {
      result$data$screen_said <- "out_of_scope"
      result$data$scope_decision <- "borderline"
    }
    result
  }
}
fetch_pdf <- make_fetch_pdf(args$pdf_cache, max_pages = spec$limits$max_pdf_pages)
budget <- as.numeric(args$max_spend_usd)

tracks <- if (is.null(args$track)) TRACK_IDS else args$track
for (track in tracks) {
  metadata <- sample[sample$track == track, , drop = FALSE]
  path <- file.path(run_dir, paste0(track, "_factsheet.csv"))
  existing <- read_factsheet(path)
  factsheet <- if (is.null(existing)) NULL else coerce_factsheet(existing, spec, track)
  result <- run_track(track, spec, metadata, factsheet, llm, fetch_pdf, args$model,
                      factsheet_path = path, raw_path = file.path(run_dir, paste0(track, ".jsonl")),
                      max_spend_usd = budget, checkpoint_every = 1L)
  budget <- budget - result$spent_usd
  cat(sprintf("%-12s processed %d, remaining %d, spent USD %.3f%s\n", track, result$processed,
              result$remaining, result$spent_usd,
              if (is.na(result$stopped)) "" else paste0(", stopped: ", result$stopped)))
  if (!is.na(result$stopped)) break
}
