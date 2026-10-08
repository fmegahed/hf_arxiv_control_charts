# Bake-off step 3: compare the runs and write a report.
#
# Usage (from the app directory):
#   Rscript analysis/bakeoff/03_score.R [--runs luna,sol] [--repeat luna_b] [--corpus 1641]
# Output: analysis/bakeoff/local/REPORT.md and disagreement lists for manual
# adjudication. Nothing here calls a model.

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

spec <- spec_load()
args <- parse_cli_args(commandArgs(trailingOnly = TRUE),
                       defaults = list(runs = "luna,sol", `repeat` = "luna_b", corpus = "1641",
                                       pdf_cache = "pdf_cache"))
local_dir <- file.path("analysis", "bakeoff", "local")
labels <- strsplit(args$runs, ",", fixed = TRUE)[[1]]
sample <- read_factsheet(file.path(local_dir, "sample.csv"))

load_run <- function(label) {
  sheets <- lapply(stats::setNames(TRACK_IDS, TRACK_IDS), function(track) {
    path <- file.path(local_dir, "runs", label, paste0(track, "_factsheet.csv"))
    sheet <- read_factsheet(path)
    if (is.null(sheet)) NULL else coerce_factsheet(sheet, spec, track)
  })
  raw <- unlist(lapply(TRACK_IDS, function(track) {
    read_raw(file.path(local_dir, "runs", label, paste0(track, ".jsonl")))
  }), recursive = FALSE)
  list(sheets = sheets, raw = raw)
}
runs <- lapply(stats::setNames(labels, labels), load_run)
has_repeat <- dir.exists(file.path(local_dir, "runs", args$`repeat`))
if (has_repeat) repeat_run <- load_run(args$`repeat`)

pct <- function(x) if (is.na(x)) "n/a" else sprintf("%.0f%%", 100 * x)
out <- character()
say <- function(...) out <<- c(out, paste0(...))
table_md <- function(df) {
  say("| ", paste(names(df), collapse = " | "), " |")
  say("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  for (i in seq_len(nrow(df))) say("| ", paste(format(unlist(df[i, ])), collapse = " | "), " |")
  say("")
}
all_rows <- function(run) dplyr::bind_rows(lapply(run$sheets, function(s) {
  s[c("paper_id", "track", "status", "error_class", "input_tokens", "cached_input_tokens",
      "output_tokens", "cost_usd", "n_calls", "qa_flags")]
}))
# What the screen itself said, before any --force-in-scope override.
screen_decisions <- function(run) {
  ids <- vapply(run$raw, function(e) e$paper_id, character(1))
  said <- vapply(run$raw, function(e) {
    s <- e$raw$screen
    if (is.null(s)) NA_character_ else if (!is.null(s$screen_said)) s$screen_said else s$scope_decision
  }, character(1))
  stats::setNames(said, ids)
}

say("# Factsheet model bake-off"); say("")
say("Sample: ", nrow(sample), " papers (", sum(sample$reviewed == "TRUE"), " author-reviewed). ",
    "Runs: ", paste(labels, collapse = ", "), if (has_repeat) paste0("; repeat run: ", args$`repeat`) else "", ".")
say("")

say("## Completion, tokens and cost"); say("")
cost <- do.call(rbind, lapply(labels, function(label) {
  rows <- all_rows(runs[[label]])
  done <- rows[rows$status %in% c("ok", "out_of_scope"), ]
  ok <- rows[rows$status %in% "ok", ]
  data.frame(
    run = label, papers = nrow(rows), ok = nrow(ok), out_of_scope = sum(rows$status %in% "out_of_scope"),
    failed = sum(rows$status %in% "failed"),
    input_per_ok = round(mean(ok$input_tokens)), cached_share = pct(sum(ok$cached_input_tokens) / sum(ok$input_tokens)),
    output_per_ok = round(mean(ok$output_tokens)),
    usd_per_ok = sprintf("%.4f", mean(ok$cost_usd)), usd_total = sprintf("%.2f", sum(rows$cost_usd, na.rm = TRUE)),
    stringsAsFactors = FALSE)
}))
table_md(cost)
say("Projected cost of a full run assumes every paper in a corpus of ", args$corpus,
    " is extracted at the mean cost of an ok paper in this sample; screened-out papers cost far less, ",
    "so this is an upper bound."); say("")
for (label in labels) {
  ok <- all_rows(runs[[label]]); ok <- ok[ok$status %in% "ok", ]
  say("- ", label, ": about USD ", sprintf("%.0f", mean(ok$cost_usd) * as.numeric(args$corpus)))
}
say("")
failures <- do.call(rbind, lapply(labels, function(label) {
  rows <- all_rows(runs[[label]]); rows <- rows[rows$status %in% "failed", ]
  if (nrow(rows) == 0L) NULL else data.frame(run = label, as.data.frame(table(error = rows$error_class)))
}))
if (!is.null(failures)) { say("Failures by class:"); say(""); table_md(failures) }

say("## Regression assertions from author feedback"); say("")
assertions <- utils::read.csv(file.path("analysis", "bakeoff", "regression_assertions.csv"), stringsAsFactors = FALSE)
assertions$paper_id <- sample$paper_id[match(assertions$review_row, as.integer(sample$review_row))]
for (label in labels) {
  scored <- score_assertions(assertions, runs[[label]]$sheets)
  assertions[[label]] <- scored$passed
  for (strength in c("hard", "soft")) {
    part <- scored$passed[scored$strength == strength]
    say("- ", label, ", ", strength, ": ", sum(part %in% TRUE), " of ", sum(!is.na(part)), " passed",
        if (any(is.na(part))) paste0(" (", sum(is.na(part)), " not evaluable)") else "")
  }
}
say("")
shown <- assertions[c("review_row", "track", "field", "assertion", "value", "strength", labels)]
table_md(shown)

say("## Scope screen"); say("")
decisions <- lapply(runs, screen_decisions)
v1 <- stats::setNames(sample$v1_scope, sample$paper_id)
for (label in labels) {
  d <- decisions[[label]]; d <- d[!is.na(d)]
  model_out <- d == "out_of_scope"
  ref <- v1[names(d)]
  keep <- ref %in% c("in", "out")
  say("- ", label, " versus the v1 full-PDF flag (", sum(keep), " papers): agreement ",
      pct(mean(model_out[keep] == (ref[keep] == "out"))), "; screened out ", sum(model_out[keep] & ref[keep] == "in"),
      " that v1 kept; kept ", sum(!model_out[keep] & ref[keep] == "out"), " that v1 excluded; borderline ",
      sum(d == "borderline"), ".")
}
say("")
if (length(labels) >= 2L) {
  a <- decisions[[labels[1]]]; b <- decisions[[labels[2]]]
  common <- intersect(names(a)[!is.na(a)], names(b)[!is.na(b)])
  differ <- common[(a[common] == "out_of_scope") != (b[common] == "out_of_scope")]
  say("The two models disagree on in or out for ", length(differ), " of ", length(common), " papers. ",
      "See scope_disagreements.csv.")
  write_factsheet_atomic(
    data.frame(paper_id = differ, track = sample$track[match(differ, sample$paper_id)],
               title = sample$title[match(differ, sample$paper_id)], v1 = unname(v1[differ]),
               a = unname(a[differ]), b = unname(b[differ]), stringsAsFactors = FALSE) |>
      stats::setNames(c("paper_id", "track", "title", "v1_scope", labels[1], labels[2])),
    file.path(local_dir, "scope_disagreements.csv"))
  say("")
}

say("## Classification"); say("")
main_column <- function(field) if (field$kind == "primary_additional") paste0(field$name, "_primary") else field$name
label_rows <- list(); disagreements <- list()
for (track in TRACK_IDS) for (field in spec_fields(spec, track)) {
  if (!field$kind %in% c("single", "primary_additional", "multi")) next
  column <- main_column(field)
  row <- data.frame(track = track, field = column, stringsAsFactors = FALSE)
  for (label in labels) {
    row[[paste0("other_", label)]] <- if (field_flag(field, "other")) pct(catch_all_rate(runs[[label]]$sheets[[track]], column)) else "-"
  }
  if (length(labels) >= 2L) {
    ag <- agreement(runs[[labels[1]]]$sheets[[track]], runs[[labels[2]]]$sheets[[track]], column)
    row$n_both <- ag$n; row$models_agree <- pct(ag$agree)
    if (length(ag$differing) > 0L) {
      sa <- runs[[labels[1]]]$sheets[[track]]; sb <- runs[[labels[2]]]$sheets[[track]]
      evidence <- paste0(field$name, "_evidence")
      disagreements[[length(disagreements) + 1L]] <- data.frame(
        track = track, field = column, paper_id = ag$differing,
        title = sample$title[match(ag$differing, sample$paper_id)],
        a = sa[[column]][match(ag$differing, sa$paper_id)], b = sb[[column]][match(ag$differing, sb$paper_id)],
        a_evidence = if (evidence %in% names(sa)) sa[[evidence]][match(ag$differing, sa$paper_id)] else NA,
        b_evidence = if (evidence %in% names(sb)) sb[[evidence]][match(ag$differing, sb$paper_id)] else NA,
        stringsAsFactors = FALSE)
    }
  }
  if (has_repeat) {
    rep_ag <- agreement(runs[[labels[1]]]$sheets[[track]], repeat_run$sheets[[track]], column)
    row$self_consistency <- pct(rep_ag$agree)
  }
  label_rows[[length(label_rows) + 1L]] <- row
}
table_md(do.call(rbind, label_rows))
say("other_*: share of ok records labelled '", NONE_OF_LISTED, "'. models_agree: same main label in both runs. ",
    if (has_repeat) paste0("self_consistency: ", labels[1], " against its own repeat run.") else "")
say("")
if (length(disagreements) > 0L) {
  d <- do.call(rbind, disagreements); names(d)[5:8] <- c(labels[1], labels[2], paste0(labels[1:2], "_evidence"))
  write_factsheet_atomic(d, file.path(local_dir, "label_disagreements.csv"))
  say(nrow(d), " label disagreements written to label_disagreements.csv for adjudication against the PDFs."); say("")
}

say("## Quality flags and evidence"); say("")
flags <- c("truncated_additional", "other_term_missing", "math_repaired", "latex_invalid",
           "undefined_acronym", "pdf_truncated")
flag_rows <- do.call(rbind, lapply(labels, function(label) {
  rows <- all_rows(runs[[label]])
  data.frame(run = label, t(vapply(flags, function(f) pct(flag_rate(rows, f)), character(1))),
             stringsAsFactors = FALSE)
}))
table_md(flag_rows)

pdf_text <- function(id) {
  path <- file.path(args$pdf_cache, paste0(gsub("/", "_", id, fixed = TRUE), ".pdf"))
  if (!file.exists(path)) return(NA_character_)
  paste(tryCatch(pdftools::pdf_text(path), error = function(e) ""), collapse = " ")
}
texts <- new.env()
for (label in labels) {
  found <- logical()
  for (track in TRACK_IDS) {
    sheet <- runs[[label]]$sheets[[track]]; sheet <- sheet[sheet$status %in% "ok", ]
    for (column in grep("_evidence$", names(sheet), value = TRUE)) for (i in seq_len(nrow(sheet))) {
      id <- sheet$id[i]
      if (is.null(texts[[id]])) texts[[id]] <- pdf_text(id)
      found <- c(found, evidence_found(sheet[[column]][i], texts[[id]]))
    }
  }
  say("- ", label, ": ", sum(found %in% TRUE), " of ", sum(!is.na(found)),
      " checkable evidence quotes found verbatim in the PDF text (", sum(is.na(found)),
      " were section pointers or too short to check).")
}
say("")
say("Manual steps still required: adjudicate scope_disagreements.csv and label_disagreements.csv against ",
    "the papers, and score the blind narrative page (04_blind_page.R).")

writeLines(out, file.path(local_dir, "REPORT.md"), useBytes = TRUE)
cat("Wrote", file.path(local_dir, "REPORT.md"), "\n")
