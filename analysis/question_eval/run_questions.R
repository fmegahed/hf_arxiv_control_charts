# Live check of the question box on thirty kinds of question.
#
# Each question goes through the same steps as in the app: the language model
# fills the form, the form is checked, the filters are applied to the
# published data, and the decision model judges the relevance criteria. The
# outcome is compared with what a careful reader would expect. This calls the
# model services (about thirty short calls to each), so it runs only on
# request:
#   Rscript analysis/question_eval/run_questions.R [--only 6,12] [--data-dir data]
# It writes analysis/question_eval/results.csv and prints one line per question.

for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f)
load_dotenv()
args <- parse_cli_args(commandArgs(trailingOnly = TRUE), defaults = list(only = "", data_dir = "data"))

spec <- spec_load()
settings <- jsonlite::fromJSON(file.path("config", "app_settings.json"), simplifyVector = FALSE)
data <- load_app_data(spec, args$data_dir)
papers <- data$papers
model_fn <- question_model_fn(spec)

# intent: expected intent. chips: patterns that must each match some chip.
# no_chips: a pattern no chip may match. source: allowed ranking outcomes
# ("none" = nothing was ranked). likely: allowed range of likely matches.
# must: an arXiv id that has to be among the likely matches (or the shown
# papers when nothing is ranked). check: a further test on the result.
case <- function(id, track, question, intent = "find_papers", chips = character(0), no_chips = NULL,
                 source = NULL, likely = NULL, must = NULL, check = NULL, note = "") {
  list(id = id, track = track, question = question, intent = intent, chips = chips, no_chips = no_chips,
       source = source, likely = likely, must = must, check = check, note = note)
}
cases <- list(
  case(1, "spc", "Nonparametric charts with public code since 2023",
       chips = c("Approach: Nonparametric", "Code: public", "Year: 2023"), source = "none"),
  case(2, "spc", "Phase I methods only", chips = "Phase: Phase I", source = "none"),
  case(3, "exp_design", "Reviews and tutorials on DOE", chips = "Review", source = "none"),
  case(4, "spc", "EWMA or CUSUM charts", chips = "Charting statistic: .*(EWMA.*CUSUM|CUSUM.*EWMA)", source = "none"),
  case(5, "reliability", "Papers that use real data in healthcare", chips = c("real data", "Healthcare")),
  case(6, "spc", "Papers by Fadel Megahed", chips = "Author: Fadel Megahed", source = "none",
       check = function(r) r$shown == 0L && r$hidden >= 1L, note = "both of his papers are screened out"),
  case(7, "spc", "Papers that Sven Knoth and Fadel Megahed wrote together",
       chips = c("Author: .*Knoth", "Author: .*Megahed"), source = "none",
       check = function(r) r$shown == 0L && r$hidden == 1L),
  case(8, NULL, "Papers published in Technometrics", chips = "Journal reference contains: Technometrics",
       source = "none", check = function(r) r$shown >= 5L),
  case(9, NULL, "Show me arXiv 2302.10916", chips = "arXiv id: 2302.10916", source = "none",
       check = function(r) r$hidden >= 1L),
  case(10, "exp_design", "Papers submitted in the last month", chips = "Submitted since: 2026-0[89]", source = "none",
       check = function(r) r$shown >= 1L),
  case(11, "spc", "stat.ME papers only", chips = "arXiv category: stat.ME", source = "none",
       check = function(r) r$shown >= 20L),
  case(12, "spc", "Control charts for wind turbines", chips = "wind turbines", source = "jev", likely = c(1L, 25L)),
  case(13, "spc", "Papers using conformal prediction", chips = "conformal", source = "jev", likely = c(1L, 25L)),
  case(14, "reliability", "Papers that use the C-MAPSS dataset", chips = "C-MAPSS", source = "jev",
       likely = c(35L, 55L), must = "2609.31338",
       note = "46 in-scope reliability papers mention C-MAPSS or turbofan in their abstract or factsheet"),
  case(15, "reliability", "Degradation models for lithium-ion batteries", chips = "batter", source = "jev",
       likely = c(1L, 30L), no_chips = "Application domain: Semiconductor"),
  case(16, "exp_design", "Bayesian optimization for additive manufacturing with small budgets",
       chips = "additive manufacturing", source = "jev", likely = c(0L, 25L)),
  case(17, "spc", "Control charts that do not use machine learning", chips = "Not about: machine.learning",
       source = "jev", check = function(r) r$likely >= 50L),
  case(18, "spc", "Papers that report an in-control ARL of 370", chips = "370", source = "jev", likely = c(1L, 120L)),
  case(19, "reliability", "Papers whose authors admit a small sample as a limitation", chips = "small", source = "jev",
       likely = c(1L, 150L)),
  case(20, "reliability", "Papers similar to 2609.31338", chips = "Similar to: 2609.31338", source = "jev",
       likely = c(1L, 60L), must = "2609.31338"),
  case(21, NULL, "Design of experiments for reliability testing", source = c("jev", "none"),
       check = function(r) r$shown >= 5L),
  case(22, "spc", "How many papers per year are on profile monitoring?", intent = "count_or_trend",
       chips = "[Pp]rofile"),
  case(23, "reliability", "Who publishes most on accelerated life tests?", intent = "find_people",
       chips = "[Aa]ccelerated"),
  case(24, "spc", "Which methods are rising in statistical process monitoring?", intent = "count_or_trend",
       source = "none"),
  case(25, "spc", "What is Phase I?", intent = "definition", check = function(r) r$n_definitions >= 1L),
  case(26, "spc", "The most cited papers on EWMA charts", chips = "EWMA",
       check = function(r) nzchar(r$cannot_answer) && r$source != "jev"),
  case(27, NULL, "New papers this week", chips = "Submitted since: 2026-10-0", source = "none"),
  case(28, "spc", "Interesting papers", check = function(r) r$source != "jev" || r$likely <= 25L,
       note = "too vague to rank; must not list most papers as likely"),
  case(29, NULL, "What is the weather tomorrow?", intent = "cannot_answer",
       check = function(r) nzchar(r$cannot_answer) && r$source != "jev"),
  case(30, "exp_design", "Experiments with microarrays", chips = "microarray", source = "jev",
       likely = c(2L, 60L), check = function(r) r$shown > JEV_MAX_PAPERS && r$oldest_likely_year <= 2012L,
       note = "more than 300 papers pass the filters; older matching papers must still be read"),
  case(31, "exp_design", "Optimal designs for mixture experiments", chips = "[Mm]ixture",
       check = function(r) r$shown <= 120L, note = "two requirements in one field must not become alternatives")
)
only <- suppressWarnings(as.integer(strsplit(args$only, ",", fixed = TRUE)[[1]]))
if (length(only) > 0L && !anyNA(only)) cases <- Filter(function(x) x$id %in% only, cases)

run_case <- function(x) {
  started <- Sys.time()
  read <- interpret_question(x$question, spec, data$year_range, context_track = x$track, model_fn = model_fn,
                             check_fn = function(question, conditions) check_question_filters(question, conditions, spec, settings))
  state <- read$state
  shown <- apply_filters(papers, state, spec, settings)
  counts <- scope_counts(papers, state, spec, settings)
  chips <- vapply(state_chips(state, spec), function(chip) chip$label, character(1))
  source <- "none"; likely <- NA_integer_; top <- ""; message <- ""; requests <- 0L; second <- 0L
  likely_ids <- shown$paper_id; oldest <- NA_integer_
  ranks <- nzchar(state$residual) && identical(read$mode, "model") &&
    !identical(read$intent, "cannot_answer") && !identical(read$intent, "definition")
  if (ranks) {
    with_references <- c(settings, list(references = criteria_references(state$residual, papers)))
    ranking <- rank_papers(shown, state$residual, with_references)
    source <- ranking$source
    message <- ranking$message
    requests <- ranking$n_requests %||% 0L
    second <- ranking$n_second_reading %||% 0L
    if (identical(source, "jev")) {
      groups <- group_by_relevance(shown, ranking$scores, ranking$threshold)
      likely <- nrow(groups$likely)
      likely_ids <- groups$likely$paper_id
      oldest <- suppressWarnings(min(groups$likely$year))
      top <- paste(substr(utils::head(groups$likely$title, 3L), 1L, 60L), collapse = " | ")
    }
  }
  result <- list(intent = read$intent %||% "", mode = read$mode, chips = chips, shown = nrow(shown),
                 hidden = counts$hidden_matches, source = source, likely = likely, likely_ids = likely_ids,
                 oldest_likely_year = oldest, cannot_answer = read$cannot_answer %||% "",
                 n_definitions = length(read$definitions), n_suggestions = length(read$suggestions))
  problems <- character(0)
  fail <- function(text) problems <<- c(problems, text)
  if (!identical(result$mode, "model")) fail(paste0("mode ", result$mode))
  if (!identical(result$intent, x$intent)) fail(paste0("intent ", result$intent))
  for (pattern in x$chips) if (!any(grepl(pattern, chips))) fail(paste0("no chip /", pattern, "/"))
  if (!is.null(x$no_chips) && any(grepl(x$no_chips, chips))) fail(paste0("chip /", x$no_chips, "/ applied"))
  if (!is.null(x$source) && !result$source %in% x$source) fail(paste0("ranking ", result$source))
  if (!is.null(x$likely) && (is.na(likely) || likely < x$likely[1] || likely > x$likely[2])) fail(paste0("likely ", likely))
  if (!is.null(x$must) && !x$must %in% likely_ids) fail(paste0(x$must, " not found"))
  if (!is.null(x$check) && !isTRUE(tryCatch(x$check(result), error = function(e) FALSE))) fail("check failed")
  data.frame(
    id = x$id, track = x$track %||% "all", question = x$question, pass = length(problems) == 0L,
    problems = paste(problems, collapse = "; "), intent = result$intent,
    chips = paste(chips, collapse = " | "), shown = result$shown, screened_out_matches = result$hidden,
    ranking = source, likely = likely, second_reading = second, requests = requests,
    seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1),
    suggestions = result$n_suggestions, cannot_answer = result$cannot_answer, ranking_message = message,
    top = top, stringsAsFactors = FALSE)
}

results <- do.call(rbind, lapply(cases, function(x) {
  row <- run_case(x)
  cat(sprintf("%2d %s %-58s %5.1fs shown=%-4d likely=%-4s %s\n", row$id, if (row$pass) "ok  " else "FAIL",
              substr(row$question, 1L, 58L), row$seconds, row$shown, format(row$likely), row$problems))
  row
}))
cat(sprintf("\n%d of %d passed. Median %.1f s, longest %.1f s.\n", sum(results$pass), nrow(results),
            stats::median(results$seconds), max(results$seconds)))
if (length(only) == 0L || anyNA(only)) {
  readr::write_csv(results, file.path("analysis", "question_eval", "results.csv"), na = "")
}
