# =============================================================================
# arXiv Multi-Track Paper Search and Factsheet Extractor
# =============================================================================
#
# Purpose:
#   Search arXiv for research papers across multiple tracks (SPC, DOE, Reliability)
#   and extract structured metadata using LLMs.
#
# Tracks:
#   - SPC: Statistical Process Control and control chart research
#   - DOE: Experimental Design and response surface methodology
#   - Reliability: Reliability engineering, degradation modeling, maintenance
#
# Workflow:
#   1. For each track, query arXiv API for papers matching track-specific criteria
#   2. Save metadata to track-specific CSV for reproducibility
#   3. Extract structured factsheets from PDFs using ellmer (via URL)
#   4. Write track configuration to JSON for app consumption
#
# Requirements:
#   - R packages: aRxiv, dplyr, tidyr, lubridate, purrr, readr, fs,
#                 tibble, progressr, ellmer, jsonlite
#   - API credentials for chosen LLM provider (if using cloud providers)
#
# Notes:
#   - PDF extraction uses ellmer::content_pdf_url() for URL-based extraction
#   - No local PDF downloads required (works in GitHub Actions)
#   - Results are cached in CSV to avoid redundant API calls
#   - List columns are stored as "|"-delimited strings in CSV
# =============================================================================


# Configuration ---------------------------------------------------------------

OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY")
get_openai_api_key = function(){return(OPENAI_API_KEY)}

config <- list(
  # arXiv search parameters (legacy - now per-track)
  limit = 10000L,

  # Directory paths
  output_dir = "data",

  # LLM extraction settings
  extraction_enabled = TRUE,
  extraction_delay_sec = 0L,
  llm_provider = "openai",
  llm_model = "gpt-5.2-2025-12-11",
  n_extraction_repeats = 1L
)

# Track Configuration --------------------------------------------------------

tracks <- list(
  spc = list(
    id = "spc",
    label = "Control Charts (SPC)",
    short_label = "SPC",
    query = '(ti:"control chart" OR abs:"control chart")',
    metadata_csv = "data/spc_arxiv_metadata.csv",
    factsheet_csv = "data/spc_factsheet.csv",
    schema = "schema_spc_factsheet",
    relevance_field = "is_spc_paper",
    icon = "chart-bar",
    color = "#C41230",
    description = "Statistical Process Control and control chart research"
  ),
  exp_design = list(
    id = "exp_design",
    label = "Experimental Design (DOE)",
    short_label = "DOE",
    query = '(ti:"experimental design" OR ti:"designed experiment" OR ti:"design of experiment" OR ti:"response surface" OR ti:"supersaturated design")',
    metadata_csv = "data/exp_design_arxiv_metadata.csv",
    factsheet_csv = "data/exp_design_factsheet.csv",
    schema = "schema_exp_design_factsheet",
    relevance_field = "is_exp_design_paper",
    icon = "flask",
    color = "#1B9E77",
    description = "Design of experiments and response surface methodology research"
  ),
  reliability = list(
    id = "reliability",
    label = "Reliability Engineering",
    short_label = "Reliability",
    query = '((ti:reliability OR ti:degradation OR ti:maintenance OR ti:"remaining useful life" OR ti:"failure analysis") AND (cat:stat.ME OR cat:stat.AP OR cat:stat.ML))',
    metadata_csv = "data/reliability_arxiv_metadata.csv",
    factsheet_csv = "data/reliability_factsheet.csv",
    schema = "schema_reliability_factsheet",
    relevance_field = "is_reliability_paper",
    icon = "cogs",
    color = "#D95F02",
    description = "Reliability engineering, degradation modeling, and maintenance optimization research"
  )
)

# Track-specific list column fields for normalization
track_list_col_fields <- list(
  spc = c(
    "chart_family", "chart_statistic", "phase", "application_domain",
    "evaluation_type", "performance_metrics", "software_platform",
    "code_availability_source", "software_urls"
  ),
  exp_design = c(
    "design_type", "design_objective", "optimality_criterion",
    "application_domain", "evaluation_type", "software_platform",
    "code_availability_source", "software_urls"
  ),
  reliability = c(
    "reliability_topic", "modeling_approach", "data_type", "maintenance_policy",
    "application_domain", "evaluation_type", "software_platform",
    "code_availability_source", "software_urls"
  )
)


# Helper Functions ------------------------------------------------------------

#' Create LLM Chat Object
#'
#' Factory function for creating ellmer chat objects based on provider.
#' We currently support only OpenAI.
#' 
#' @param provider Character. One of "ollama", "anthropic", or "openai".
#' @param model Character. Model identifier for the chosen provider.
#'
#' @return An ellmer chat object.
create_chat <- function(provider, model) {
  ellmer::chat_openai(
    model = model,
    credentials = get_openai_api_key
  )
}


#' Coerce Values to Character List-Column
#'
#' Normalizes various input types to a list of character vectors suitable
#' for list-columns in tibbles.
#'
#' @param x Input value (NULL, list, or atomic vector).
#'
#' @return A list of character vectors.
as_list_chr <- function(x) {
  if (is.null(x)) {
    return(list(NA_character_))
  }
  
  if (is.list(x)) {
    return(
      lapply(x, function(el) {
        if (is.null(el) || length(el) == 0L) NA_character_ else as.character(el)
      })
    )
  }
  
  if (length(x) == 0L) {
    return(list(NA_character_))
  }
  
  list(as.character(x))
}


#' Collapse List Column to Delimited String
#'
#' Converts a list column to a character column with values separated by a delimiter.
#'
#' @param x A list column.
#' @param sep Character. Delimiter to use (default "|").
#'
#' @return A character vector.
collapse_list_col <- function(x, sep = "|") {
  vapply(x, function(el) {
    if (all(is.na(el))) NA_character_ else paste(el, collapse = sep)
  }, character(1L))
}


#' Expand Delimited String to List Column
#'
#' Converts a character column with delimiter-separated values to a list column.
#'
#' @param x A character vector.
#' @param sep Character. Delimiter to split on (default "|").
#'
#' @return A list column.
expand_list_col <- function(x, sep = "|") {
  lapply(x, function(el) {
    if (is.na(el) || el == "") {
      NA_character_
    } else {
      strsplit(el, split = sep, fixed = TRUE)[[1L]]
    }
  })
}


#' Normalize Factsheet Output
#'
#' Ensures consistent structure and types for extracted factsheet data.
#' Supports track-specific fields based on track_id parameter.
#'
#' @param raw_output Raw output from LLM extraction (list or tibble).
#' @param track_id Character. One of "spc", "exp_design", or "reliability".
#'
#' @return A single-row tibble with standardized columns.
normalize_factsheet <- function(raw_output, track_id = "spc") {
  # Common list column fields across all tracks
  common_list_cols <- c(
    "application_domain",
    "evaluation_type",
    "software_platform",
    "code_availability_source",
    "software_urls"
  )

  # Track-specific list column fields
  track_specific_list_cols <- switch(track_id,
    spc = c("chart_family", "chart_statistic", "phase", "performance_metrics"),
    exp_design = c("design_type", "design_objective", "optimality_criterion"),
    reliability = c("reliability_topic", "modeling_approach", "data_type", "maintenance_policy"),
    c()  # default empty
  )

  list_col_fields <- c(common_list_cols, track_specific_list_cols)

  # Common boolean fields
  common_boolean_fields <- c("code_used")

  # Track-specific boolean fields
  track_specific_boolean_fields <- switch(track_id,
    spc = c("is_spc_paper", "assumes_normality", "handles_autocorrelation", "handles_missing_data"),
    exp_design = c("is_exp_design_paper"),
    reliability = c("is_reliability_paper"),
    c()
  )

  boolean_fields <- c(common_boolean_fields, track_specific_boolean_fields)

  # Common text fields
  common_text_fields <- c(
    "summary",
    "key_equations",
    "key_results",
    "limitations_stated",
    "limitations_unstated",
    "future_work_stated",
    "future_work_unstated"
  )

  # Track-specific text fields
  track_specific_text_fields <- switch(track_id,
    spc = c("sample_size_requirements"),
    exp_design = c("number_of_factors"),
    reliability = c(),
    c()
  )

  text_fields <- c(common_text_fields, track_specific_text_fields)

  # If raw_output is a list (not already a data.frame), wrap array fields
  if (is.list(raw_output) && !inherits(raw_output, "data.frame")) {
    for (field in list_col_fields) {
      if (field %in% names(raw_output)) {
        val <- raw_output[[field]]
        raw_output[[field]] <- list(if (is.null(val)) NA_character_ else as.character(val))
      }
    }
    for (field in c(boolean_fields, text_fields)) {
      if (field %in% names(raw_output) && is.null(raw_output[[field]])) {
        raw_output[[field]] <- NA
      }
    }
  }

  result <- tibble::as_tibble(raw_output)

  # Ensure all required columns exist
  all_fields <- c(list_col_fields, boolean_fields, text_fields)

  for (col in all_fields) {
    if (!col %in% names(result)) {
      if (col %in% list_col_fields) {
        result[[col]] <- list(NA_character_)
      } else {
        result[[col]] <- NA
      }
    }
  }

  # Normalize list columns to consistent format
  for (col in list_col_fields) {
    if (col %in% names(result)) {
      result[[col]] <- as_list_chr(result[[col]])
    }
  }

  # Normalize boolean columns
  for (col in boolean_fields) {
    if (col %in% names(result)) {
      result[[col]] <- as.logical(result[[col]])
    }
  }

  # Normalize text columns
  for (col in text_fields) {
    if (col %in% names(result)) {
      val <- result[[col]]
      result[[col]] <- if (is.na(val)) NA_character_ else as.character(val)
    }
  }

  result |> dplyr::slice(1L)
}


#' Extract Factsheet from PDF URL
#'
#' Uses an LLM to extract structured metadata from a PDF document via URL.
#' The PDF is downloaded internally by ellmer to a temp file, processed,
#' and cleaned up automatically.
#'
#' @param pdf_url Character. URL of the PDF to process.
#' @param chat_object An ellmer chat object.
#' @param type_object An ellmer type specification.
#' @param track_id Character. One of "spc", "exp_design", or "reliability".
#'
#' @return A normalized single-row tibble.
extract_factsheet_from_pdf <- function(pdf_url, chat_object, type_object, track_id = "spc") {
  pdf_content <- ellmer::content_pdf_url(pdf_url)

  # Track-specific prompts
  prompt <- switch(track_id,
    spc = paste(
      "Extract the SPC factsheet fields from the attached PDF.",
      "Select ALL applicable categories for multi-label fields.",
      "If a URL exists, return only the URL string.",
      "If code is implied but not shared, choose 'Not provided'.",
      sep = "\n"
    ),
    exp_design = paste(
      "Extract the experimental design (DOE) factsheet fields from the attached PDF.",
      "Select ALL applicable categories for multi-label fields.",
      "If a URL exists, return only the URL string.",
      "If code is implied but not shared, choose 'Not provided'.",
      sep = "\n"
    ),
    reliability = paste(
      "Extract the reliability engineering factsheet fields from the attached PDF.",
      "Select ALL applicable categories for multi-label fields.",
      "If a URL exists, return only the URL string.",
      "If code is implied but not shared, choose 'Not provided'.",
      sep = "\n"
    ),
    # default
    paste(
      "Extract the factsheet fields from the attached PDF.",
      "Select ALL applicable categories for multi-label fields.",
      sep = "\n"
    )
  )

  raw_output <- chat_object$chat_structured(prompt, pdf_content, type = type_object)

  normalize_factsheet(raw_output, track_id)
}


# Schema Definitions ----------------------------------------------------------

# --- Relevance Filter ---

schema_is_spc_paper <- ellmer::type_boolean(
  description = paste(
    "TRUE if this paper is substantively about Statistical Process Control (SPC) or control charts.",
    "",
    "Return TRUE if the paper:",
    "- Proposes, develops, or evaluates a control chart or monitoring procedure",
    "- Focuses on change-point detection in a process monitoring context",
    "- Addresses Phase I estimation or Phase II monitoring",
    "- Discusses run length properties, ARL, or false alarm rates for monitoring",
    "",
    "Return FALSE if:",
    "- 'Control chart' is only mentioned in passing or as background",
    "- The paper is primarily about a different topic (e.g., general time series, machine learning)",
    "- SPC methods are used as a minor tool rather than being the focus",
    sep = "\n"
  ),
  required = TRUE
)

# --- Classification Fields ---

schema_chart_family <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "Univariate",
      "Multivariate",
      "Self-starting",
      "Profile monitoring",
      "Image-based monitoring",
      "Functional data analysis",
      "Bayesian",
      "Nonparametric",
      "High-dimensional",
      "Other"
    ),
    description = paste(
      "The broad family or class of control chart proposed or evaluated.",
      "Select ALL applicable categories.",
      "",
      "Definitions:",
      "- Univariate: Monitors a single quality characteristic (scalar).",
      "- Multivariate: Monitors multiple correlated characteristics (vectors, p > 1).",
      "- Self-starting: Runs without Phase I reference sample; parameters updated sequentially.",
      "- Profile monitoring: Monitors stability of response-predictor relationships (regression profiles).",
      "- Image-based monitoring: Monitors images, video streams, or spatial point clouds.",
      "- Functional data analysis: Monitors curves/functions using FDA techniques.",
      "- Bayesian: Uses posterior/predictive distributions rather than frequentist parameters.",
      "- Nonparametric: Distribution-free charts (e.g., rank-based, sign-based).",
      "- High-dimensional: Targets scenarios where p is large, often exceeding n.",
      "- Other: Specialized types not listed above (e.g., spatial, network monitoring).",
      sep = "\n"
    ),
    required = TRUE
  )
)

schema_chart_statistic <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "Shewhart",
      "CUSUM",
      "EWMA",
      "Hotelling T-squared",
      "MEWMA",
      "MCUSUM",
      "GLR (Generalized Likelihood Ratio)",
      "Change-point",
      "Machine learning-based",
      "Other"
    ),
    description = paste(
      "The specific charting statistic or method used.",
      "Select ALL that apply.",
      "",
      "Definitions:",
      "- Shewhart: Memoryless charts based on current observation only (X-bar, R, S, p, c, u, etc.).",
      "- CUSUM: Cumulative sum charts that accumulate deviations from target.",
      "- EWMA: Exponentially weighted moving average charts.",
      "- Hotelling T-squared: Multivariate generalization of Shewhart for mean vectors.",
      "- MEWMA: Multivariate EWMA for mean vectors.",
      "- MCUSUM: Multivariate CUSUM for mean vectors.",
      "- GLR: Generalized likelihood ratio-based detection procedures.",
      "- Change-point: Methods focused on detecting structural breaks or changes.",
      "- Machine learning-based: Charts using neural networks, random forests, SVMs, etc.",
      "- Other: Specialized statistics not listed above.",
      sep = "\n"
    ),
    required = FALSE
  )
)

schema_phase <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "Phase I",
      "Phase II",
      "Both"
    ),
    description = paste(
      "Which phase of SPC implementation does the paper address?",
      "",
      "Definitions:",
      "- Phase I: Retrospective analysis to estimate in-control parameters and identify outliers.",
      "- Phase II: Prospective monitoring with parameters assumed known or estimated from Phase I.",
      "- Both: Paper addresses both phases or the transition between them.",
      sep = "\n"
    ),
    required = FALSE
  )
)

schema_application_domain <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "Manufacturing (general)",
      "Semiconductor/electronics",
      "Healthcare/medical",
      "Pharmaceutical",
      "Finance/economics",
      "Environmental monitoring",
      "Network/cybersecurity",
      "Service industry",
      "Food/agriculture",
      "Energy/utilities",
      "Transportation/logistics",
      "Theoretical/simulation only",
      "Other"
    ),
    description = paste(
      "The application domain or industry context.",
      "Select ALL that apply.",
      "",
      "Select 'Theoretical/simulation only' if the paper uses only simulated data",
      "and does not apply methods to any real-world domain.",
      sep = "\n"
    ),
    required = FALSE
  )
)

# --- Data Assumptions ---

schema_assumes_normality <- ellmer::type_boolean(
  description = paste(
    "TRUE if the method assumes normally distributed data.",
    "FALSE if the method is designed for non-normal or distribution-free settings.",
    "NULL if unclear or not applicable.",
    sep = "\n"
  ),
  required = FALSE
)

schema_handles_autocorrelation <- ellmer::type_boolean(
  description = paste(
    "TRUE if the method explicitly accounts for autocorrelated (serially dependent) data.",
    "FALSE if the method assumes independent observations.",
    "NULL if unclear or not applicable.",
    sep = "\n"
  ),
  required = FALSE
)

schema_handles_missing_data <- ellmer::type_boolean(
  description = paste(
    "TRUE if the method explicitly handles missing observations or irregular sampling.",
    "FALSE if the method assumes complete data.",
    "NULL if unclear or not applicable.",
    sep = "\n"
  ),
  required = FALSE
)

# --- Evaluation Methodology ---

schema_evaluation_type <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "Exact distribution theory",
      "Approximation methods",
      "Simulation study",
      "Markov chain",
      "Integral equation",
      "Economic design",
      "Case study (real dataset)",
      "Other"
    ),
    description = paste(
      "Methodology used to evaluate control chart performance.",
      "Select all that apply.",
      "",
      "Definitions:",
      "- Exact distribution theory: Closed-form probability formulas for run length.",
      "- Approximation methods: Saddlepoint, Edgeworth, or other analytical approximations.",
      "- Simulation study: Monte Carlo methods to estimate ARL/run length.",
      "- Markov chain: Discretized state space with transition probability matrix.",
      "- Integral equation: Solves Fredholm equations via numerical quadrature.",
      "- Economic design: Optimizes parameters by minimizing cost functions.",
      "- Case study (real dataset): Applied to real-world industrial/service data.",
      "- Other: Methods not listed above.",
      sep = "\n"
    ),
    required = FALSE
  )
)

schema_performance_metrics <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "ARL (Average Run Length)",
      "SDRL (Standard Deviation of Run Length)",
      "MRL (Median Run Length)",
      "ATS (Average Time to Signal)",
      "ANOS (Average Number of Observations to Signal)",
      "Detection probability",
      "False alarm rate",
      "Expected detection delay",
      "Steady-state ARL",
      "Conditional expected delay",
      "Other"
    ),
    description = paste(
      "Performance metrics reported in the paper.",
      "Select ALL that apply.",
      "",
      "These metrics characterize how quickly the chart detects shifts (out-of-control ARL)",
      "and how often it produces false alarms (in-control ARL).",
      sep = "\n"
    ),
    required = FALSE
  )
)

schema_sample_size_requirements <- ellmer::type_string(
  description = paste(
    "Phase I sample size requirements or recommendations (1-2 sentences).",
    "",
    "Report specific numbers if stated (e.g., 'm = 100 subgroups of size n = 5').",
    "Note any guidance on minimum sample sizes for reliable parameter estimation.",
    "Return 'Not discussed' if the paper does not address sample size requirements.",
    sep = "\n"
  ),
  required = FALSE
)

# --- Software and Code ---

schema_code_used <- ellmer::type_boolean(
  description = paste(
    "TRUE if computational experiments or simulations were performed.",
    "FALSE if purely theoretical or a review with no original computations."
  ),
  required = FALSE
)

schema_software_platform <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "R",
      "Python",
      "MATLAB",
      "Julia",
      "SAS",
      "C/C++",
      "Fortran",
      "None / Not applicable",
      "Other"
    ),
    description = paste(
      "Programming language used for experiments or implementation.",
      "Select ALL that apply.",
      "Select 'None / Not applicable' for purely theoretical or review papers."
    ),
    required = FALSE
  )
)

schema_code_availability <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "Public repository (GitHub/GitLab)",
      "Package registry (CRAN/PyPI)",
      "Supplementary material (Journal/Publisher)",
      "In text/Appendix",
      "Personal website",
      "Upon request",
      "Not provided",
      "Not applicable (No code used)"
    ),
    description = paste(
      "How the code is made available.",
      "",
      "- Not provided: Code was used but not shared.",
      "- Not applicable: Purely theoretical/review; no code generated."
    ),
    required = FALSE
  )
)

schema_software_urls <- ellmer::type_array(
  ellmer::type_string(
    description = "A URL for code or software."
  ),
  description = paste(
    "URLs for code/software if explicitly provided.",
    "Return an empty array if no URLs are given.",
    "Include all URLs mentioned (e.g., GitHub repo, CRAN package, personal site)."
  ),
  required = FALSE
)

# --- Content Summary and Analysis ---

schema_summary <- ellmer::type_string(
  description = paste(
    "A concise summary (4-6 sentences) of the paper's main contributions and findings.",
    "",
    "Include:",
    "- The primary methodological contribution (e.g., new chart, new estimator, new design).",
    "- The type of shift or change the method is designed to detect (mean, variance, both, shape, etc.).",
    "- Key theoretical results (e.g., derivation of run length distribution, optimality proofs).",
    "- Main empirical findings (e.g., performance improvements, ARL comparisons vs. competitors).",
    "- How this work relates to or advances the existing SPC literature.",
    "- Practical implications or recommendations for practitioners.",
    "",
    "Focus on what is novel and significant. Avoid generic statements.",
    "Situate the contribution within the broader SPC literature.",
    sep = "\n"
  ),
  required = TRUE
)

schema_key_equations <- ellmer::type_string(
  description = paste(
    "Key equations or formulas that define the proposed method (2-4 sentences).",
    "",
    "Describe in words the main mathematical expressions:",
    "- The charting statistic formula and how it is computed.",
    "- Control limit expressions if provided.",
    "- Run length distribution or ARL formulas if derived.",
    "",
    "Use LaTeX notation where helpful (e.g., '$\\bar{X}_t$', '$\\sum_{i=1}^n$').",
    "Return 'Not applicable' for review papers or purely empirical studies.",
    sep = "\n"
  ),
  required = FALSE
)

schema_key_results <- ellmer::type_string(
  description = paste(
    "Key quantitative results and findings (3-5 sentences).",
    "",
    "Include specific numbers where available:",
    "- ARL improvements (e.g., 'ARL1 reduced from 45 to 32 for a 1-sigma shift').",
    "- Comparison results (e.g., 'Proposed method outperforms EWMA by 20% for small shifts').",
    "- Optimal parameter values or design recommendations.",
    "- Key findings from case studies or real data applications.",
    "",
    "Focus on the most important quantitative takeaways.",
    sep = "\n"
  ),
  required = FALSE
)

# --- Limitations ---

schema_limitations_stated <- ellmer::type_string(
  description = paste(
    "Limitations the authors explicitly acknowledge (2-4 sentences).",
    "",
    "Only include weaknesses or constraints the authors themselves mention.",
    "Quote or closely paraphrase their statements.",
    "If no limitations are explicitly stated, return 'None stated'.",
    sep = "\n"
  ),
  required = TRUE
)

schema_limitations_unstated <- ellmer::type_string(
  description = paste(
    "Limitations you identify that the authors do not mention (2-4 sentences).",
    "",
    "Draw on your knowledge of the SPC literature to identify gaps or weaknesses.",
    "",
    "Consider these dimensions:",
    "- Assumptions: Are distributional assumptions (normality, independence) realistic?",
    "- Scope: Is the method limited to specific scenarios (e.g., known parameters, Phase II only)?",
    "- Evaluation: Are simulations comprehensive? Are comparisons fair and complete?",
    "- Practicality: Is the method computationally feasible? Easy to implement?",
    "- Generalizability: Do results depend on specific parameter choices or data characteristics?",
    "- Sample size: Are the Phase I requirements realistic for practitioners?",
    "- Comparison: Are important competing methods missing from the comparison?",
    "",
    "Be specific and constructive. These are gaps YOU identify based on SPC best practices.",
    sep = "\n"
  ),
  required = TRUE
)

# --- Future Work ---

schema_future_work_stated <- ellmer::type_string(
  description = paste(
    "Future research directions the authors explicitly suggest (2-4 sentences).",
    "",
    "Only include extensions or directions the authors themselves propose.",
    "Typically found in the conclusion or discussion section.",
    "If no future work is explicitly stated, return 'None stated'.",
    sep = "\n"
  ),
  required = TRUE
)

schema_future_work_unstated <- ellmer::type_string(
  description = paste(
    "Promising research directions you identify that the authors do not mention (2-4 sentences).",
    "",
    "Draw on your knowledge of the SPC literature to suggest valuable extensions.",
    "",
    "Consider:",
    "- Relaxing assumptions (e.g., non-normality, autocorrelation, unknown parameters).",
    "- Extending to other settings (e.g., multivariate, high-dimensional, profiles).",
    "- Methodological improvements (e.g., adaptive designs, robust estimation, self-starting versions).",
    "- Practical enhancements (e.g., diagnostic tools, software implementation, interpretability).",
    "- Theoretical gaps (e.g., exact ARL derivations, optimality properties, robustness analysis).",
    "- Empirical validation (e.g., real-world case studies, comparative benchmarks).",
    "- Integration with modern methods (e.g., machine learning, Bayesian approaches).",
    "",
    "Be specific about what future research should address and why it matters.",
    "These are directions YOU identify based on current trends and gaps in SPC research.",
    sep = "\n"
  ),
  required = TRUE
)

# --- Combined Schema Object ---

schema_spc_factsheet <- ellmer::type_object(
  # Relevance

  is_spc_paper = schema_is_spc_paper,

  # Classification
  chart_family = schema_chart_family,
  chart_statistic = schema_chart_statistic,
  phase = schema_phase,
  application_domain = schema_application_domain,

  # Data assumptions
  assumes_normality = schema_assumes_normality,
  handles_autocorrelation = schema_handles_autocorrelation,
  handles_missing_data = schema_handles_missing_data,

  # Evaluation
  evaluation_type = schema_evaluation_type,
  performance_metrics = schema_performance_metrics,
  sample_size_requirements = schema_sample_size_requirements,

  # Software
  code_used = schema_code_used,
  software_platform = schema_software_platform,
  code_availability_source = schema_code_availability,
  software_urls = schema_software_urls,

  # Content
  summary = schema_summary,
  key_equations = schema_key_equations,
  key_results = schema_key_results,

  # Limitations
  limitations_stated = schema_limitations_stated,
  limitations_unstated = schema_limitations_unstated,

  # Future work
  future_work_stated = schema_future_work_stated,
  future_work_unstated = schema_future_work_unstated
)


# =============================================================================
# Experimental Design (DOE) Schema Definitions
# =============================================================================

# --- Relevance Filter ---

schema_is_exp_design_paper <- ellmer::type_boolean(
  description = paste(
    "TRUE if this paper is substantively about experimental design (DOE) or designed experiments.",
    "",
    "Return TRUE if the paper:",
    "- Proposes, develops, or evaluates an experimental design methodology",
    "- Focuses on factorial designs, response surface methodology, or optimal designs",
    "- Addresses screening experiments, mixture designs, or split-plot designs",
    "- Discusses optimality criteria (D-optimal, A-optimal, etc.) for design construction",
    "",
    "Return FALSE if:",
    "- 'Experimental design' is only mentioned in passing or as a minor tool",
    "- The paper is primarily about data analysis without design contributions",
    "- DOE methods are used but not the focus of the paper",
    sep = "\n"
  ),
  required = TRUE
)

# --- DOE Classification Fields ---

schema_design_type <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "Factorial (full)",
      "Factorial (fractional)",
      "Response surface",
      "Mixture",
      "Split-plot",
      "Optimal design",
      "Screening",
      "Definitive screening",
      "Supersaturated",
      "Robust parameter design",
      "Sequential/adaptive",
      "Computer experiment",
      "Bayesian design",
      "Other"
    ),
    description = paste(
      "The type of experimental design proposed or evaluated.",
      "Select ALL applicable categories.",
      "",
      "Definitions:",
      "- Factorial (full): Complete factorial experiments at all level combinations.",
      "- Factorial (fractional): Subset of factorial runs using aliasing structure.",
      "- Response surface: Second-order designs for optimization (CCD, Box-Behnken, etc.).",
      "- Mixture: Designs where factors are proportions summing to 1.",
      "- Split-plot: Designs with hard-to-change and easy-to-change factors.",
      "- Optimal design: Designs constructed by optimizing a criterion (D, A, I, etc.).",
      "- Screening: Designs for identifying important factors among many.",
      "- Definitive screening: Designs that screen and estimate some two-factor interactions.",
      "- Supersaturated: Designs with more factors than runs.",
      "- Robust parameter design: Taguchi-style designs separating control and noise factors.",
      "- Sequential/adaptive: Designs that adapt based on observed responses.",
      "- Computer experiment: Space-filling designs for deterministic simulations.",
      "- Bayesian design: Designs optimized using Bayesian criteria.",
      "- Other: Specialized designs not listed above.",
      sep = "\n"
    ),
    required = TRUE
  )
)

schema_design_objective <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "Parameter estimation",
      "Screening",
      "Optimization",
      "Model discrimination",
      "Prediction",
      "Robustness",
      "Cost reduction",
      "Other"
    ),
    description = paste(
      "The primary objective of the experimental design.",
      "Select ALL applicable objectives.",
      "",
      "- Parameter estimation: Precise estimation of model coefficients.",
      "- Screening: Identifying important factors from many candidates.",
      "- Optimization: Finding factor settings that optimize response.",
      "- Model discrimination: Distinguishing between competing models.",
      "- Prediction: Accurate prediction at untested factor combinations.",
      "- Robustness: Minimizing sensitivity to noise factors.",
      "- Cost reduction: Minimizing experimental resources.",
      sep = "\n"
    ),
    required = FALSE
  )
)

schema_optimality_criterion <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "D-optimal",
      "A-optimal",
      "I-optimal (IV-optimal)",
      "G-optimal",
      "E-optimal",
      "V-optimal",
      "Bayesian D-optimal",
      "Bayesian A-optimal",
      "Compound criterion",
      "Space-filling",
      "Minimax/Maximin",
      "Not applicable",
      "Other"
    ),
    description = paste(
      "The optimality criterion used for design construction.",
      "Select ALL applicable criteria.",
      "",
      "- D-optimal: Maximizes determinant of information matrix.",
      "- A-optimal: Minimizes average variance of parameter estimates.",
      "- I-optimal: Minimizes average prediction variance over design region.",
      "- G-optimal: Minimizes maximum prediction variance.",
      "- E-optimal: Maximizes minimum eigenvalue of information matrix.",
      "- V-optimal: Minimizes average prediction variance at specified points.",
      "- Space-filling: Spreads points uniformly (e.g., Latin hypercube).",
      "- Minimax/Maximin: Optimizes worst-case distances between points.",
      "- Not applicable: Paper does not involve optimal design construction.",
      sep = "\n"
    ),
    required = FALSE
  )
)

schema_number_of_factors <- ellmer::type_string(
  description = paste(
    "The number of factors considered in the experimental design (brief description).",
    "",
    "Examples: '3 factors', '5-10 factors', 'Up to 100 factors for screening'.",
    "Return 'Variable/General' if the paper considers general settings.",
    "Return 'Not specified' if unclear.",
    sep = "\n"
  ),
  required = FALSE
)

# --- DOE Combined Schema Object ---

schema_exp_design_factsheet <- ellmer::type_object(
  # Relevance
  is_exp_design_paper = schema_is_exp_design_paper,

  # Classification
  design_type = schema_design_type,
  design_objective = schema_design_objective,
  optimality_criterion = schema_optimality_criterion,
  number_of_factors = schema_number_of_factors,
  application_domain = schema_application_domain,

  # Evaluation
  evaluation_type = schema_evaluation_type,

  # Software
  code_used = schema_code_used,
  software_platform = schema_software_platform,
  code_availability_source = schema_code_availability,
  software_urls = schema_software_urls,

  # Content
  summary = schema_summary,
  key_equations = schema_key_equations,
  key_results = schema_key_results,

  # Limitations
  limitations_stated = schema_limitations_stated,
  limitations_unstated = schema_limitations_unstated,

  # Future work
  future_work_stated = schema_future_work_stated,
  future_work_unstated = schema_future_work_unstated
)


# =============================================================================
# Reliability Engineering Schema Definitions
# =============================================================================

# --- Relevance Filter ---

schema_is_reliability_paper <- ellmer::type_boolean(
  description = paste(
    "TRUE if this paper is substantively about reliability engineering or related topics.",
    "",
    "Return TRUE if the paper:",
    "- Proposes, develops, or evaluates reliability models or methods",
    "- Focuses on degradation modeling, remaining useful life prediction, or failure analysis",
    "- Addresses maintenance optimization or condition-based maintenance",
    "- Discusses life distributions, survival analysis in engineering contexts",
    "",
    "Return FALSE if:",
    "- 'Reliability' is only mentioned in passing or as background",
    "- The paper is primarily about a different topic (e.g., pure statistics)",
    "- Reliability methods are used as a minor tool rather than being the focus",
    sep = "\n"
  ),
  required = TRUE
)

# --- Reliability Classification Fields ---

schema_reliability_topic <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "Life distribution modeling",
      "Degradation modeling",
      "RUL prediction",
      "Failure mode analysis",
      "Accelerated testing",
      "Maintenance optimization",
      "System reliability",
      "Warranty analysis",
      "Reliability growth",
      "Software reliability",
      "Network/infrastructure reliability",
      "Other"
    ),
    description = paste(
      "The primary reliability topic addressed in the paper.",
      "Select ALL applicable topics.",
      "",
      "Definitions:",
      "- Life distribution modeling: Parametric or nonparametric lifetime modeling.",
      "- Degradation modeling: Models for gradual deterioration over time.",
      "- RUL prediction: Remaining useful life estimation for equipment.",
      "- Failure mode analysis: FMEA, fault trees, root cause analysis.",
      "- Accelerated testing: ALT, HALT, step-stress testing.",
      "- Maintenance optimization: Optimal maintenance scheduling and policies.",
      "- System reliability: Reliability of systems with multiple components.",
      "- Warranty analysis: Warranty cost modeling and prediction.",
      "- Reliability growth: Tracking reliability improvement during development.",
      "- Software reliability: Reliability models for software systems.",
      "- Network/infrastructure reliability: Reliability of networks or critical infrastructure.",
      "- Other: Specialized reliability topics not listed above.",
      sep = "\n"
    ),
    required = TRUE
  )
)

schema_modeling_approach <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "Parametric (Weibull, etc.)",
      "Nonparametric/Semi-parametric",
      "Stochastic process",
      "Physics-based",
      "ML-based",
      "Bayesian",
      "Hybrid/Ensemble",
      "Simulation-based",
      "Other"
    ),
    description = paste(
      "The modeling approach used for reliability analysis.",
      "Select ALL applicable approaches.",
      "",
      "- Parametric: Classical distributions (Weibull, lognormal, exponential, etc.).",
      "- Nonparametric/Semi-parametric: Cox model, Kaplan-Meier, kernel methods.",
      "- Stochastic process: Wiener, Gamma, inverse Gaussian processes.",
      "- Physics-based: Models derived from failure physics/mechanisms.",
      "- ML-based: Neural networks, random forests, SVMs for prediction.",
      "- Bayesian: Bayesian inference for reliability parameters.",
      "- Hybrid/Ensemble: Combining multiple approaches.",
      "- Simulation-based: Monte Carlo or discrete event simulation.",
      sep = "\n"
    ),
    required = FALSE
  )
)

schema_data_type <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "Complete lifetime data",
      "Right-censored",
      "Interval-censored",
      "Left-censored",
      "Degradation measurements",
      "Event/count data",
      "Sensor/condition monitoring",
      "Mixture of types",
      "Simulated only",
      "Other"
    ),
    description = paste(
      "The type of reliability data used or modeled in the paper.",
      "Select ALL applicable types.",
      "",
      "- Complete lifetime data: Exact failure times observed for all units.",
      "- Right-censored: Some units have not yet failed at observation end.",
      "- Interval-censored: Failures known to occur within time intervals.",
      "- Left-censored: Failures occurred before observation began.",
      "- Degradation measurements: Repeated measurements of a degrading characteristic.",
      "- Event/count data: Recurrent events or failure counts.",
      "- Sensor/condition monitoring: Real-time monitoring signals.",
      "- Mixture of types: Combination of different data types.",
      "- Simulated only: Only simulated data used in the paper.",
      sep = "\n"
    ),
    required = FALSE
  )
)

schema_maintenance_policy <- ellmer::type_array(
  ellmer::type_enum(
    values = c(
      "Age-based",
      "Block replacement",
      "Condition-based",
      "Predictive",
      "Opportunistic",
      "Group replacement",
      "Imperfect maintenance",
      "Not applicable",
      "Other"
    ),
    description = paste(
      "The type of maintenance policy considered in the paper.",
      "Select ALL applicable policies.",
      "",
      "- Age-based: Replacement based on unit age/usage.",
      "- Block replacement: Replacement at fixed time intervals.",
      "- Condition-based: Maintenance triggered by condition thresholds.",
      "- Predictive: Maintenance based on predicted future state.",
      "- Opportunistic: Maintenance during natural downtime or dependencies.",
      "- Group replacement: Replacing multiple components together.",
      "- Imperfect maintenance: Maintenance that partially restores reliability.",
      "- Not applicable: Paper does not address maintenance policies.",
      sep = "\n"
    ),
    required = FALSE
  )
)

# --- Reliability Combined Schema Object ---

schema_reliability_factsheet <- ellmer::type_object(
  # Relevance
  is_reliability_paper = schema_is_reliability_paper,

  # Classification
  reliability_topic = schema_reliability_topic,
  modeling_approach = schema_modeling_approach,
  data_type = schema_data_type,
  maintenance_policy = schema_maintenance_policy,
  application_domain = schema_application_domain,

  # Evaluation
  evaluation_type = schema_evaluation_type,

  # Software
  code_used = schema_code_used,
  software_platform = schema_software_platform,
  code_availability_source = schema_code_availability,
  software_urls = schema_software_urls,

  # Content
  summary = schema_summary,
  key_equations = schema_key_equations,
  key_results = schema_key_results,

  # Limitations
  limitations_stated = schema_limitations_stated,
  limitations_unstated = schema_limitations_unstated,

  # Future work
  future_work_stated = schema_future_work_stated,
  future_work_unstated = schema_future_work_unstated
)


# Main Pipeline ---------------------------------------------------------------

# Initialize directories
fs::dir_create(config$output_dir)

# Set up progress reporting
progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")


#' Expand list columns from CSV for a specific track
#'
#' @param df Data frame read from CSV
#' @param track_id Character. One of "spc", "exp_design", or "reliability".
#' @return Data frame with list columns expanded
expand_track_list_cols <- function(df, track_id) {
  list_cols <- track_list_col_fields[[track_id]]
  for (col in list_cols) {
    if (col %in% names(df)) {
      df[[col]] <- expand_list_col(df[[col]])
    }
  }
  df
}


#' Collapse list columns to CSV format for a specific track
#'
#' @param df Data frame to save
#' @param track_id Character. One of "spc", "exp_design", or "reliability".
#' @return Data frame with list columns collapsed
collapse_track_list_cols <- function(df, track_id) {
  list_cols <- track_list_col_fields[[track_id]]
  for (col in list_cols) {
    if (col %in% names(df)) {
      df[[col]] <- collapse_list_col(df[[col]])
    }
  }
  df
}


#' Create empty error factsheet for a track
#'
#' @param track_id Character. One of "spc", "exp_design", or "reliability".
#' @return An empty tibble with NA values for all track fields
create_empty_factsheet <- function(track_id) {
  # Common fields
  common <- tibble::tibble(
    application_domain = list(NA_character_),
    evaluation_type = list(NA_character_),
    code_used = NA,
    software_platform = list(NA_character_),
    code_availability_source = list(NA_character_),
    software_urls = list(NA_character_),
    summary = NA_character_,
    key_equations = NA_character_,
    key_results = NA_character_,
    limitations_stated = NA_character_,
    limitations_unstated = NA_character_,
    future_work_stated = NA_character_,
    future_work_unstated = NA_character_
  )

  # Track-specific fields
  track_specific <- switch(track_id,
    spc = tibble::tibble(
      is_spc_paper = NA,
      chart_family = list(NA_character_),
      chart_statistic = list(NA_character_),
      phase = list(NA_character_),
      assumes_normality = NA,
      handles_autocorrelation = NA,
      handles_missing_data = NA,
      performance_metrics = list(NA_character_),
      sample_size_requirements = NA_character_
    ),
    exp_design = tibble::tibble(
      is_exp_design_paper = NA,
      design_type = list(NA_character_),
      design_objective = list(NA_character_),
      optimality_criterion = list(NA_character_),
      number_of_factors = NA_character_
    ),
    reliability = tibble::tibble(
      is_reliability_paper = NA,
      reliability_topic = list(NA_character_),
      modeling_approach = list(NA_character_),
      data_type = list(NA_character_),
      maintenance_policy = list(NA_character_)
    ),
    tibble::tibble()
  )

  dplyr::bind_cols(track_specific, common)
}


# =============================================================================
# Process Each Track
# =============================================================================

message("Starting multi-track arXiv extraction pipeline...")
message(sprintf("Tracks to process: %s", paste(names(tracks), collapse = ", ")))

for (track_id in names(tracks)) {
  track <- tracks[[track_id]]

  message("")
  message(strrep("=", 70))
  message(sprintf("Processing Track: %s (%s)", track$label, track_id))
  message(strrep("=", 70))
  

  # -------------------------------------------------------------------------
  # Step 1: Search arXiv for this track
  # -------------------------------------------------------------------------

  message(sprintf("[%s] Searching arXiv with query: %s", track_id, track$query))

  metadata <- tryCatch({
    aRxiv::arxiv_search(query = track$query, limit = config$limit) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        pdf_url = paste0("https://arxiv.org/pdf/", id, ".pdf"),
        submitted = lubridate::ymd_hms(submitted)
      ) |>
      dplyr::arrange(dplyr::desc(submitted))
  }, error = function(e) {
    warning(sprintf("[%s] arXiv search failed: %s", track_id, conditionMessage(e)))
    tibble::tibble()
  })

  if (nrow(metadata) == 0L) {
    message(sprintf("[%s] No papers found. Skipping track.", track_id))
    next
  }

  readr::write_csv(metadata, track$metadata_csv)
  message(sprintf("[%s] Found %d papers. Saved to: %s", track_id, nrow(metadata), track$metadata_csv))

  # -------------------------------------------------------------------------
  # Step 2: Extract Factsheets for this track
  # -------------------------------------------------------------------------

  if (!config$extraction_enabled) {
    message(sprintf("[%s] Structured extraction disabled.", track_id))
    next
  }

  # Get papers available for extraction
  papers_for_extraction <- metadata |>
    dplyr::select(id, pdf_url) |>
    dplyr::distinct()

  # Load existing cache
  factsheet_cache <- if (fs::file_exists(track$factsheet_csv)) {
    readr::read_csv(track$factsheet_csv, show_col_types = FALSE) |>
      expand_track_list_cols(track_id)
  } else {
    tibble::tibble()
  }

  extracted_ids <- if (nrow(factsheet_cache) > 0L && "id" %in% names(factsheet_cache)) {
    unique(dplyr::pull(factsheet_cache, id))
  } else {
    character(0L)
  }

  papers_to_extract <- papers_for_extraction |>
    dplyr::filter(!id %in% extracted_ids)

  message(sprintf(
    "[%s] Extraction status: %d in metadata, %d cached, %d remaining",
    track_id,
    nrow(papers_for_extraction),
    length(extracted_ids),
    nrow(papers_to_extract)
  ))

  if (nrow(papers_to_extract) == 0L) {
    message(sprintf("[%s] All factsheets already extracted.", track_id))
    next
  }

  # Get the schema object for this track
  schema_object <- get(track$schema)

  # Safe extraction wrapper with error logging
  safe_extract <- function(pdf_url, chat_object, type_object, track_id) {
    tryCatch(
      extract_factsheet_from_pdf(pdf_url, chat_object, type_object, track_id),
      error = function(e) {
        warning(
          sprintf("[%s] Extraction failed for %s: %s", track_id, pdf_url, conditionMessage(e)),
          call. = FALSE
        )
        normalize_factsheet(create_empty_factsheet(track_id), track_id)
      }
    )
  }

  # Build extraction grid (supports multiple repeats)
  extraction_grid <- tidyr::crossing(
    papers_to_extract,
    repeat_id = seq_len(config$n_extraction_repeats)
  )

  # Process each paper and save incrementally
  for (i in seq_len(nrow(extraction_grid))) {
    row <- extraction_grid[i, ]
    pdf_url <- row$pdf_url
    paper_id <- row$id
    repeat_id <- row$repeat_id

    message(sprintf("[%s] [%d/%d] %s | repeat %d", track_id, i, nrow(extraction_grid), paper_id, repeat_id))

    chat_clean <- create_chat(config$llm_provider, config$llm_model)

    result <- safe_extract(pdf_url, chat_clean, schema_object, track_id) |>
      dplyr::mutate(
        id = paper_id,
        pdf_url = pdf_url,
        pdf_path = NA_character_,
        llm_provider = config$llm_provider,
        llm_model = config$llm_model,
        repeat_id = repeat_id,
        extracted_at = Sys.time()
      )

    # Update cache incrementally
    factsheet_cache <- dplyr::bind_rows(factsheet_cache, result)

    # Save to CSV with list columns collapsed
    factsheet_cache |>
      collapse_track_list_cols(track_id) |>
      readr::write_csv(track$factsheet_csv)

    Sys.sleep(config$extraction_delay_sec)
  }

  message(sprintf("[%s] Extraction complete. Output: %s", track_id, track$factsheet_csv))
}


# =============================================================================
# Write Track Configuration JSON
# =============================================================================

message("")
message("Writing track configuration to data/tracks.json...")

tracks_for_json <- lapply(tracks, function(t) {
  t[c("id", "label", "short_label", "query", "icon", "color", "description",
      "metadata_csv", "factsheet_csv", "relevance_field")]
})

jsonlite::write_json(
  tracks_for_json,
  "data/tracks.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

message("Track configuration saved.")
message("")
message("Pipeline complete.")