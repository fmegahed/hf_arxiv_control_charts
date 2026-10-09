# Bake-off step 6: summarise the three-model pipeline run (first reader,
# second reader, tie-break) against the single-model runs.
#
# Usage (from the app directory):
#   Rscript analysis/bakeoff/06_pipeline_summary.R [--run pipeline] [--first luna] [--reference sol]
# Prints to the console. Nothing here calls a model.

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

spec <- spec_load()
args <- parse_cli_args(commandArgs(trailingOnly = TRUE),
                       defaults = list(run = "pipeline", first = "luna", reference = "sol"))
local_dir <- file.path("analysis", "bakeoff", "local")
sample <- read_factsheet(file.path(local_dir, "sample.csv"))

load_sheets <- function(label) {
  lapply(stats::setNames(TRACK_IDS, TRACK_IDS), function(track) {
    sheet <- read_factsheet(file.path(local_dir, "runs", label, paste0(track, "_factsheet.csv")))
    if (is.null(sheet)) NULL else coerce_factsheet(sheet, spec, track)
  })
}
filled <- function(x) lengths(lapply(x, split_values)) > 0L
run <- load_sheets(args$run)
first <- load_sheets(args$first)
reference <- load_sheets(args$reference)

cat("== Status, calls and cost\n")
for (track in TRACK_IDS) {
  sheet <- run[[track]]
  ok <- sheet[sheet$status == "ok", ]
  resolved <- filled(ok$labels_resolved)
  changed <- filled(ok$labels_changed)
  disputed <- filled(ok$labels_disputed)
  cat(sprintf("%-12s n=%d ok=%d out=%d failed=%d | tie-break %d (%.0f%%), label changed %d, other disputes left %d\n",
              track, nrow(sheet), nrow(ok), sum(sheet$status == "out_of_scope"), sum(sheet$status == "failed"),
              sum(resolved), 100 * mean(resolved), sum(changed), sum(disputed)))
  cat(sprintf("             cost: total %.3f, per paper %.4f, ok without tie-break %.4f, ok with tie-break %.4f, out of scope %.5f\n",
              sum(sheet$cost_usd), mean(sheet$cost_usd), mean(ok$cost_usd[!resolved]), mean(ok$cost_usd[resolved]),
              mean(sheet$cost_usd[sheet$status == "out_of_scope"])))
  flags <- table(unlist(lapply(sheet$qa_flags, split_values)))
  if (length(flags) > 0L) cat("             flags:", paste(names(flags), flags, sep = "=", collapse = ", "), "\n")
}
all_rows <- do.call(rbind, lapply(run, function(s) s[c("status", "cost_usd", "labels_resolved")]))
cat(sprintf("all          n=%d, total USD %.3f, per paper %.4f, per in-scope paper %.4f\n",
            nrow(all_rows), sum(all_rows$cost_usd), mean(all_rows$cost_usd),
            mean(all_rows$cost_usd[all_rows$status == "ok"])))

cat("\n== Arbitrated fields: agreement with the reference run on papers in scope in both\n")
for (track in TRACK_IDS) {
  for (name in arbitrated_fields(spec, track)) {
    column <- answer_column(spec_field(spec, track, name))
    ids <- intersect(run[[track]]$paper_id[run[[track]]$status == "ok"],
                     reference[[track]]$paper_id[reference[[track]]$status == "ok"])
    ids <- intersect(ids, first[[track]]$paper_id[first[[track]]$status == "ok"])
    pick <- function(sheets) sheets[[track]][[column]][match(ids, sheets[[track]]$paper_id)]
    final <- pick(run); one <- pick(first); ref <- pick(reference)
    resolved <- vapply(run[[track]]$labels_resolved[match(ids, run[[track]]$paper_id)],
                       function(x) name %in% split_values(x), logical(1))
    cat(sprintf("%-12s %-18s n=%d | first reader alone %.0f%% | pipeline %.0f%% | sent to tie-break %d, of which final equals reference %d\n",
                track, name, length(ids), 100 * mean(one == ref, na.rm = TRUE), 100 * mean(final == ref, na.rm = TRUE),
                sum(resolved), sum(final[resolved] == ref[resolved], na.rm = TRUE)))
  }
}

cat("\n== Regression assertions\n")
assertions <- utils::read.csv(file.path("analysis", "bakeoff", "regression_assertions.csv"), stringsAsFactors = FALSE)
assertions$paper_id <- sample$paper_id[match(assertions$review_row, as.integer(sample$review_row))]
scored <- score_assertions(assertions, run)
assertions$passed <- scored$passed
print(assertions[c("review_row", "track", "field", "assertion", "value", "strength", "passed")], row.names = FALSE)
for (strength in unique(assertions$strength)) {
  rows <- assertions$strength == strength
  cat(sprintf("%s: %d of %d passed\n", strength, sum(assertions$passed[rows], na.rm = TRUE), sum(rows)))
}
