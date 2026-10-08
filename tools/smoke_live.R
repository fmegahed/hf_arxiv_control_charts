# Live smoke test of the two model services behind "Ask a question".
#
# Usage (from the app directory):
#   QEW_DATA_DIR=data/dev_v2 Rscript tools/smoke_live.R            # both services
#   QEW_DATA_DIR=data/dev_v2 Rscript tools/smoke_live.R jev        # ranking only
#   QEW_DATA_DIR=data/dev_v2 Rscript tools/smoke_live.R question   # translation only
#
# Needs OPENAI_API_KEY (translation) and JEV_API_KEY (ranking) in the
# environment or in a .env file. Keys are never printed.

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
load_dotenv()

args <- commandArgs(trailingOnly = TRUE)
what <- if (length(args) >= 1L) args[[1]] else "both"

spec <- spec_load()
settings <- app_settings_load()
data <- load_app_data(spec)
if (is.null(data$papers)) stop(paste(data$problems, collapse = "\n"))

if (what %in% c("both", "question")) {
  question <- "Which SPC papers submitted on arXiv in 2025 use nonparametric methods and provide public code?"
  cat("\n== Translation with", spec$models$question, "==\nQuestion:", question, "\n")
  started <- Sys.time()
  result <- interpret_question(question, spec, data$year_range)
  cat(sprintf("Mode: %s (%.1f s)\n", result$mode, as.numeric(Sys.time() - started, units = "secs")))
  cat("Message:", result$message, "\n")
  if (length(result$dropped) > 0L) cat("Dropped:", paste(result$dropped, collapse = "; "), "\n")
  for (chip in state_chips(result$state, spec)) cat("  chip:", chip$label, "\n")
  cat("Matching papers:", sum(filter_mask(data$papers, result$state, spec, settings)), "\n")
}

if (what %in% c("both", "jev")) {
  residual <- "wind turbines"
  state <- new_filter_state("reliability")
  state$year_from <- data$year_range[2] - 1L
  papers <- apply_filters(data$papers, state, spec, settings)
  cat("\n== Ranking with", settings$jev$model, "==\n")
  cat(sprintf("Residual: '%s' over %d reliability papers since %d\n", residual, nrow(papers), state$year_from))
  started <- Sys.time()
  ranking <- rank_papers(papers, residual, settings)
  elapsed <- as.numeric(Sys.time() - started, units = "secs")
  cat(sprintf("Source: %s; %d of %d scored; %s requests; %.1f s\n", ranking$source,
              sum(!is.na(ranking$scores$score)), ranking$considered,
              ranking$n_requests %||% 0L, elapsed))
  if (nzchar(ranking$message)) cat("Message:", ranking$message, "\n")
  if (!is.null(ranking$usage)) cat("Tokens in/out:", ranking$usage[["input_tokens"]], "/", ranking$usage[["output_tokens"]], "\n")
  groups <- group_by_relevance(papers, ranking$scores, ranking$threshold)
  cat(sprintf("At or above %.2f: %d; less likely: %d\n", ranking$threshold, nrow(groups$likely),
              nrow(groups$less_likely)))
  top <- utils::head(rbind(groups$likely, groups$less_likely), 8L)
  for (i in seq_len(nrow(top))) cat(sprintf("  %.3f  %s  %s\n", top$relevance[i], top$paper_id[i], substr(top$title[i], 1, 90)))
}
