# Bake-off step 4: a side-by-side page of narrative fields with the model
# names hidden, for judging summaries, results and equations.
#
# Usage (from the app directory):
#   Rscript analysis/bakeoff/04_blind_page.R [--runs luna,sol] [--n 15]
# Output: analysis/bakeoff/local/blind_review.html (open in a browser) and
# blind_key.csv (which side was which; open only after judging).

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

spec <- spec_load()
args <- parse_cli_args(commandArgs(trailingOnly = TRUE), defaults = list(runs = "luna,sol", n = "15"))
local_dir <- file.path("analysis", "bakeoff", "local")
labels <- strsplit(args$runs, ",", fixed = TRUE)[[1]]
stopifnot(length(labels) == 2L)
set.seed(401)

sample <- read_factsheet(file.path(local_dir, "sample.csv"))
fields <- c(summary = "Summary", key_results = "Key results", key_equations = "Key equations",
            limitations_unstated = "Possible limitations", glossary = "Glossary")
esc <- function(x) htmltools::htmlEscape(ifelse(is.na(x), "(empty)", x))

cards <- character(); key <- list()
per_track <- ceiling(as.integer(args$n) / length(TRACK_IDS))
for (track in TRACK_IDS) {
  sheets <- lapply(labels, function(label) {
    coerce_factsheet(read_factsheet(file.path(local_dir, "runs", label, paste0(track, "_factsheet.csv"))), spec, track)
  })
  both <- intersect(sheets[[1]]$paper_id[sheets[[1]]$status %in% "ok"],
                    sheets[[2]]$paper_id[sheets[[2]]$status %in% "ok"])
  for (id in sample(both, min(per_track, length(both)))) {
    order <- sample(1:2)
    rows <- lapply(order, function(k) sheets[[k]][sheets[[k]]$paper_id == id, ])
    title <- sample$title[match(id, sample$paper_id)]
    body <- vapply(names(fields), function(field) {
      paste0("<tr><th>", fields[[field]], "</th><td>", gsub("\n", "<br>", esc(rows[[1]][[field]])),
             "</td><td>", gsub("\n", "<br>", esc(rows[[2]][[field]])), "</td></tr>")
    }, character(1))
    cards <- c(cards, paste0(
      "<section><h2>", esc(title), "</h2><p><a href='https://arxiv.org/abs/", id, "' target='_blank' rel='noopener'>arXiv:",
      id, "</a> &middot; ", spec_track(spec, track)$short_label, "</p><table><thead><tr><th></th><th>Version A</th>",
      "<th>Version B</th></tr></thead><tbody>", paste(body, collapse = ""), "</tbody></table></section>"))
    key[[length(key) + 1L]] <- data.frame(paper_id = id, track = track, version_a = labels[order[1]],
                                          version_b = labels[order[2]], stringsAsFactors = FALSE)
  }
}

page <- paste0(
  "<!doctype html><html lang='en'><head><meta charset='utf-8'><title>Blind factsheet comparison</title>",
  "<meta name='viewport' content='width=device-width, initial-scale=1'>",
  "<script>window.MathJax={tex:{inlineMath:[['\\\\(','\\\\)']],displayMath:[['\\\\[','\\\\]']]}};</script>",
  "<script src='https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-chtml.js'></script>",
  "<style>body{font-family:Georgia,serif;max-width:1200px;margin:2rem auto;padding:0 1rem;color:#222;line-height:1.5}",
  "h1{font-size:1.5rem}h2{font-size:1.1rem;margin-bottom:.2rem}section{border-top:1px solid #ccc;padding:1rem 0}",
  "table{border-collapse:collapse;width:100%;table-layout:fixed}th,td{border:1px solid #ddd;padding:.5rem;vertical-align:top;",
  "text-align:left;overflow-wrap:anywhere}tbody th{width:9rem;background:#f6f6f6;font-weight:normal}thead th{background:#f0f0f0}",
  "</style></head><body><h1>Blind comparison of two extraction models</h1>",
  "<p>Each paper shows the same fields written by two models. Which side is which model changes from paper to paper. ",
  "Judge each side against the paper itself (link under each title): are the statements correct, are the numbers ",
  "the paper's own, are terms defined, do the equations render and match the method?</p>",
  paste(cards, collapse = "\n"), "</body></html>")
writeLines(page, file.path(local_dir, "blind_review.html"), useBytes = TRUE)
write_factsheet_atomic(do.call(rbind, key), file.path(local_dir, "blind_key.csv"))
cat("Wrote", length(cards), "papers to", file.path(local_dir, "blind_review.html"), "\n")
