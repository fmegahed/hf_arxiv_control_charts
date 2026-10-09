# Shared helpers: key loading, the labelling rubric, and a cached Luna labeller.
# Sourced by the numbered R scripts. Never prints or writes the API key.

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(tidyr); library(purrr); library(ellmer)
})

qd_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) == 1) normalizePath(dirname(f), winslash = "/") else normalizePath(".", winslash = "/")
}
QD <- qd_dir()
CACHE <- file.path(QD, "cache")
FROZEN <- normalizePath(file.path(QD, "..", "..", "data", "frozen", "v1"), winslash = "/")
LABEL_MODEL <- "gpt-6-luna"

load_openai_key <- function() {
  env_file <- normalizePath(file.path(QD, "..", "..", "..", "..", ".env"), winslash = "/", mustWork = TRUE)
  lines <- readLines(env_file, warn = FALSE)
  hit <- lines[grepl("^\\s*OPENAI_API_KEY\\s*=", lines)]
  if (length(hit) == 0) stop("OPENAI_API_KEY not found in .env")
  val <- sub("^\\s*OPENAI_API_KEY\\s*=\\s*", "", hit[[1]])
  val <- gsub("^['\"]|['\"]$", "", trimws(val))
  Sys.setenv(OPENAI_API_KEY = val)
  invisible(TRUE)
}

RUBRIC <- paste(
  "You classify research papers for a quality-engineering literature monitor.",
  "You are given a title and, when available, an abstract and keywords. Decide three independent yes/no labels.",
  "A paper can receive more than one label, or none. Judge by the paper's main contribution, not passing mentions.",
  "",
  "RELIABILITY = TRUE when the paper is about reliability engineering of physical or engineered items or systems:",
  "failure-time / lifetime / survival modelling of components, products or systems; degradation modelling;",
  "remaining useful life and prognostics; maintenance, inspection or replacement policies; warranty analysis;",
  "accelerated life or degradation testing and reliability test plans; reliability growth; system, network (as an",
  "engineered system), structural or software reliability; repairable systems; failure mode analysis (FMEA, fault trees);",
  "stress-strength reliability; lifetime distributions when framed for reliability or life testing (e.g. censored life tests).",
  "RELIABILITY = FALSE for: reliability of a measurement, test score, rater or questionnaire (psychometrics, Cronbach alpha,",
  "inter-rater agreement); reliability, robustness, calibration or trustworthiness of machine-learning models or predictions;",
  "'reliable' used as a general adjective; statistical 'reliability' of estimates; degradation of image, signal or model",
  "performance; biomedical survival analysis of patients with no engineering reliability framing; model or software",
  "'maintenance' in the sense of upkeep of code or ML models.",
  "",
  "DOE = TRUE when the paper is about the design and analysis of experiments: construction or evaluation of experimental",
  "designs (factorial, fractional factorial, screening, supersaturated, split-plot, blocked, mixture, optimal, space-filling,",
  "Latin hypercube, orthogonal arrays, computer experiments, sequential or adaptive designs, Bayesian experimental design,",
  "design for A/B or online experiments); response surface methodology; robust parameter design; analysis methods specific",
  "to designed experiments; or an applied study whose central method is a designed experiment or response surface study.",
  "DOE = FALSE for: papers that merely run simulations or 'experiments' to evaluate a method; observational studies;",
  "'experimental design' in the sense of an apparatus, detector or lab setup in physics, chemistry or biology; machine-",
  "learning benchmark setups; 'response surface' meaning a physical surface or a potential energy surface with no design",
  "of experiments content; sample surveys.",
  "",
  "SPM = TRUE when the paper is about statistical process monitoring / statistical process control: control charts of any",
  "kind (Shewhart, CUSUM, EWMA, multivariate, profile, nonparametric, Bayesian, attribute), Phase I / Phase II analysis,",
  "run length properties, statistical change-point or anomaly detection framed as sequential monitoring or surveillance of",
  "a process, stream, network or health indicator; process capability analysis; monitoring-oriented fault detection and",
  "diagnosis in industrial processes; acceptance sampling plans are NOT SPM unless tied to process monitoring.",
  "SPM = FALSE for: automatic control / feedback control theory with no statistical monitoring; generic offline change-point",
  "estimation with no monitoring framing; generic outlier detection in ML with no process-monitoring framing; 'monitoring' of",
  "patients, networks or software in a non-statistical sense; papers that only mention a control chart in passing.",
  "",
  "If the item is not a research paper (editorial, book review, erratum, index) set all three to FALSE.",
  "Return the three labels and a reason of at most 15 words.",
  sep = "\n"
)

label_type <- function() {
  type_object(
    reliability = type_boolean("TRUE if the paper meets the RELIABILITY definition"),
    doe = type_boolean("TRUE if the paper meets the DOE definition"),
    spm = type_boolean("TRUE if the paper meets the SPM definition"),
    reason = type_string("At most 15 words justifying the labels")
  )
}

make_prompt <- function(title, abstract, keywords = "") {
  abstract <- ifelse(is.na(abstract) | nchar(abstract) < 30, "(no abstract available)", str_trunc(abstract, 3000))
  kw <- ifelse(is.na(keywords) | keywords == "", "", paste0("\nKeywords: ", str_trunc(keywords, 400)))
  paste0("Title: ", title, "\nAbstract: ", abstract, kw)
}

# Label a data frame with columns key, title, abstract, keywords (optional).
# Results are appended to cache_file (keyed on `key`) so reruns only label what is missing.
# Token usage per run is appended to cache/token_usage.csv.
label_items <- function(df, cache_file, tag, chunk = 400) {
  load_openai_key()
  if (!"keywords" %in% names(df)) df$keywords <- ""
  done <- if (file.exists(cache_file)) read_csv(cache_file, col_types = cols(.default = "c")) else tibble(key = character())
  todo <- df %>% filter(!key %in% done$key) %>% distinct(key, .keep_all = TRUE)
  message(tag, ": ", nrow(done), " cached, ", nrow(todo), " to label")
  if (nrow(todo) == 0) return(invisible(done))
  chunks <- split(todo, ceiling(seq_len(nrow(todo)) / chunk))
  for (i in seq_along(chunks)) {
    ch <- chunks[[i]]
    chat <- chat_openai(model = LABEL_MODEL, system_prompt = RUBRIC,
                        credentials = function() Sys.getenv("OPENAI_API_KEY"), echo = "none")
    prompts <- as.list(make_prompt(ch$title, ch$abstract, ch$keywords))
    res <- parallel_chat_structured(chat, prompts, type = label_type(), include_tokens = TRUE,
                                    on_error = "continue", max_active = 8, rpm = 400)
    res <- as_tibble(res)
    out <- tibble(key = ch$key,
                  reliability = as.character(res$reliability), doe = as.character(res$doe),
                  spm = as.character(res$spm), reason = as.character(res$reason)) %>%
      filter(!is.na(reliability))
    write_csv(out, cache_file, append = file.exists(cache_file))
    use <- tibble(when = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), tag = tag, model = LABEL_MODEL,
                  n_items = nrow(out),
                  input_tokens = sum(res$input_tokens, na.rm = TRUE),
                  output_tokens = sum(res$output_tokens, na.rm = TRUE),
                  cached_input_tokens = if ("cached_input_tokens" %in% names(res)) sum(res$cached_input_tokens, na.rm = TRUE) else NA_real_)
    uf <- file.path(CACHE, "token_usage.csv")
    write_csv(use, uf, append = file.exists(uf))
    message(tag, " chunk ", i, "/", length(chunks), ": ", nrow(out), " labelled, in=", use$input_tokens, " out=", use$output_tokens)
  }
  invisible(read_csv(cache_file, col_types = cols(.default = "c")))
}
