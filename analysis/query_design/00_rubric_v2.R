# Rubric v2 (follow-up analysis) and its labeller. Source 00_common.R first (it sources this file).
# Stricter than v1: each topic is defined by what the reference journals publish, and sequential
# change-detection THEORY gets its own flag instead of counting as SPM.

RUBRIC_V2 <- paste(
  "You classify research papers for a quality-engineering literature monitor. The reference point is what is",
  "published in Journal of Quality Technology, Technometrics, Quality Engineering, Quality and Reliability",
  "Engineering International, IISE Transactions and IEEE Transactions on Reliability.",
  "You get a title and, when available, an abstract and keywords. Decide four independent yes/no labels.",
  "A label is TRUE only when the paper's MAIN contribution is a method, model, analysis or case study framed",
  "around an engineered product, a production or service process, a quality characteristic, an engineered",
  "system, or an experiment. General statistical, probabilistic or machine-learning theory that could be applied",
  "to such problems but is not framed around them is FALSE. Passing mentions do not count. A paper can get",
  "several labels or none. If the item is an editorial, book review, erratum or index, all labels are FALSE.",
  "",
  "RELIABILITY = TRUE: failure-time or lifetime modelling of components, products or systems; degradation",
  "modelling; remaining useful life and prognostics of equipment; maintenance, inspection or replacement",
  "policies; warranty; accelerated life or degradation tests and reliability test plans; reliability growth;",
  "system, structural, network (as an engineered system) or software reliability; repairable systems; failure",
  "mode analysis; stress-strength; failure-probability estimation for an engineered system; lifetime",
  "distributions only when the paper frames them for reliability or life-test data.",
  "RELIABILITY = FALSE: reliability of a measurement, rater, questionnaire or test score; reliability,",
  "robustness, calibration or trustworthiness of machine-learning predictions or of statistical estimates;",
  "'reliable' as a general adjective; new distribution families or survival models with a biomedical or generic",
  "framing; rare-event simulation not tied to failure of an engineered system; link or transmission reliability",
  "as a communication-theory quantity; degradation of images, signals or model performance.",
  "",
  "DOE = TRUE: how to choose the runs, points or allocations of an experiment (factorial, fractional factorial,",
  "screening, supersaturated, split-plot, blocked, mixture, optimal, space-filling, Latin hypercube, orthogonal",
  "arrays, order-of-addition, design of computer experiments, sequential design or active learning for",
  "simulation experiments, design of online controlled experiments / A-B tests); response surface methodology;",
  "robust parameter design; analysis methods specific to data from designed experiments; applied studies whose",
  "central method is a designed experiment.",
  "DOE = FALSE: papers that merely run simulations or experiments to evaluate something; clinical-trial",
  "designs (dose finding, group-sequential, adaptive randomisation); bandit or best-arm-identification theory;",
  "causal-inference estimators for observational data; surrogate / Gaussian-process modelling or calibration",
  "with no design component; 'experimental design' as an apparatus or lab setup; sample surveys; 'response",
  "surface' as a physical surface.",
  "",
  "SPM = TRUE: statistical process monitoring / statistical process control: control charts of any kind,",
  "Phase I and Phase II analysis, monitoring or surveillance of a process, profile, data stream, network or",
  "health indicator with a signalling rule evaluated by run length or false-alarm performance; process",
  "capability analysis; fault detection and diagnosis in industrial processes with a statistical monitoring",
  "procedure.",
  "SPM = FALSE: sequential or quickest change-detection THEORY that is not framed around a process or quality",
  "characteristic (see CHANGE_THEORY); offline / retrospective change-point estimation or testing; anomaly or",
  "outlier detection with no monitoring procedure; predictive business process monitoring (process mining);",
  "sensor-based condition classification by machine learning with no statistical monitoring scheme; feedback",
  "control; acceptance sampling; a control chart mentioned only in passing.",
  "",
  "CHANGE_THEORY = TRUE: the paper is sequential or quickest change-detection theory or sequential-testing",
  "theory: optimality, minimax or asymptotic results for stopping rules (CUSUM, Shiryaev-Roberts, detection",
  "delay bounds), e-detectors or e-processes, conformal test martingales, anytime-valid inference. It can be",
  "TRUE whatever the SPM label is. Otherwise FALSE.",
  sep = "\n"
)

label_type_v2 <- function() {
  type_object(
    reliability = type_boolean("TRUE if the paper meets the RELIABILITY definition"),
    doe = type_boolean("TRUE if the paper meets the DOE definition"),
    spm = type_boolean("TRUE if the paper meets the SPM definition"),
    change_theory = type_boolean("TRUE if the paper meets the CHANGE_THEORY definition")
  )
}

# Label a data frame with columns key, title, abstract, keywords (optional) using rubric v2.
# Resumable (cache keyed on `key`), modest parallelism, and a hard stop when most of a chunk fails
# (for example HTTP 429 with no credit): the function then returns what it has and does not retry.
label_items_v2 <- function(df, cache_file, tag, chunk = 200, max_active = 4) {
  load_openai_key()
  options(ellmer_max_tries = 2)
  if (!"keywords" %in% names(df)) df$keywords <- ""
  done <- if (file.exists(cache_file)) read_csv(cache_file, col_types = cols(.default = "c")) else tibble(key = character())
  todo <- df %>% filter(!key %in% done$key) %>% distinct(key, .keep_all = TRUE)
  message(tag, ": ", nrow(done), " cached, ", nrow(todo), " to label")
  if (nrow(todo) == 0) return(invisible(done))
  chunks <- split(todo, ceiling(seq_len(nrow(todo)) / chunk))
  for (i in seq_along(chunks)) {
    ch <- chunks[[i]]
    chat <- chat_openai(model = LABEL_MODEL, system_prompt = RUBRIC_V2,
                        credentials = function() Sys.getenv("OPENAI_API_KEY"), echo = "none")
    prompts <- as.list(make_prompt(ch$title, ch$abstract, ch$keywords))
    res <- tryCatch(
      parallel_chat_structured(chat, prompts, type = label_type_v2(), include_tokens = TRUE,
                               on_error = "continue", max_active = max_active),
      error = function(e) { message("chunk failed: ", conditionMessage(e)); NULL })
    if (is.null(res)) { message("HARD STOP: API error, not retrying. Rerun later to resume."); break }
    res <- as_tibble(res)
    out <- tibble(key = ch$key, reliability = as.character(res$reliability), doe = as.character(res$doe),
                  spm = as.character(res$spm), change_theory = as.character(res$change_theory)) %>%
      filter(!is.na(reliability))
    if (nrow(out) > 0) write_csv(out, cache_file, append = file.exists(cache_file))
    use <- tibble(when = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), tag = tag, model = LABEL_MODEL, n_items = nrow(out),
                  input_tokens = sum(res$input_tokens, na.rm = TRUE), output_tokens = sum(res$output_tokens, na.rm = TRUE),
                  cached_input_tokens = if ("cached_input_tokens" %in% names(res)) sum(res$cached_input_tokens, na.rm = TRUE) else NA_real_)
    uf <- file.path(CACHE, "token_usage.csv")
    write_csv(use, uf, append = file.exists(uf))
    message(tag, " chunk ", i, "/", length(chunks), ": ", nrow(out), " of ", nrow(ch), " labelled, in=", use$input_tokens,
            " out=", use$output_tokens)
    if (nrow(out) < 0.5 * nrow(ch)) {
      message("HARD STOP: more than half of this chunk failed (rate limit or no credit). Rerun later to resume.")
      break
    }
  }
  invisible(read_csv(cache_file, col_types = cols(.default = "c")))
}
