# Bake-off step 8: summarise the pilot run on papers from the new queries.
#
# Usage (from the app directory):
#   Rscript analysis/bakeoff/08_pilot_summary.R [--run pilot] [--sample pilot_sample.csv]
# Prints scope decisions by group, cost, label checks, and for every label
# with a catch-all option the terms the model wrote when nothing listed fit.
# Nothing here calls a model.

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

spec <- spec_load()
args <- parse_cli_args(commandArgs(trailingOnly = TRUE), defaults = list(run = "pilot", sample = "pilot_sample.csv"))
local_dir <- file.path("analysis", "bakeoff", "local")
sample <- read_factsheet(file.path(local_dir, args$sample))
filled <- function(x) lengths(lapply(x, split_values)) > 0L

total <- 0
for (track in TRACK_IDS) {
  sheet <- read_factsheet(file.path(local_dir, "runs", args$run, paste0(track, "_factsheet.csv")))
  if (is.null(sheet)) next
  sheet <- coerce_factsheet(sheet, spec, track)
  sheet$group <- sample$pilot_group[match(paste(track, sheet$paper_id), paste(sample$track, sample$paper_id))]
  ok <- sheet[sheet$status == "ok", ]
  total <- total + sum(sheet$cost_usd, na.rm = TRUE)
  cat("\n==", track, "==", nrow(sheet), "papers, USD", sprintf("%.3f", sum(sheet$cost_usd, na.rm = TRUE)), "\n")
  print(table(group = sheet$group, status = sheet$status))
  out <- sheet[sheet$status == "out_of_scope", ]
  if (nrow(out) > 0L) { cat("Reasons for screening out:\n"); print(table(out$group, out$scope_category)) }
  failed <- sheet[sheet$status == "failed", ]
  if (nrow(failed) > 0L) { cat("Failures:\n"); print(table(failed$error_class)) }
  if (nrow(ok) == 0L) next
  cat(sprintf("In scope: tie-break on %d of %d, label changed on %d, cost per in-scope paper %.4f\n",
              sum(filled(ok$labels_resolved)), nrow(ok), sum(filled(ok$labels_changed)), mean(ok$cost_usd)))
  flags <- table(unlist(lapply(sheet$qa_flags, split_values)))
  if (length(flags) > 0L) cat("Flags:", paste(names(flags), flags, sep = "=", collapse = ", "), "\n")
  cat("Labels where nothing listed fit (share of in-scope papers, then the terms written):\n")
  for (field in spec_fields(spec, track)) {
    if (!field_flag(field, "other")) next
    column <- if (field$kind == "primary_additional") paste0(field$name, "_primary") else field$name
    none <- vapply(ok[[column]], function(x) NONE_OF_LISTED %in% split_values(x), logical(1))
    if (!any(none)) next
    terms <- ok[[paste0(field$name, "_other_term")]][none]
    cat(sprintf("  %-22s %2d of %d: %s\n", field$name, sum(none), nrow(ok), paste(sort(terms), collapse = "; ")))
  }
}
cat(sprintf("\nTotal USD %.3f\n", total))
