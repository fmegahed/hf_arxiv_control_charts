# =============================================================================
# arXiv Paper Downloader and SPC Factsheet Extractor
# =============================================================================
#
# Purpose:
#   Search arXiv for control chart papers, download PDFs, and extract
#   structured Statistical Process Control (SPC) metadata using LLMs.
#
# Workflow:
#   1. Query arXiv API for papers matching search criteria
#   2. Save metadata to CSV for reproducibility
#   3. Download PDFs with polite rate limiting
#   4. Extract structured factsheets from PDFs using ellmer
#
# Requirements:
#   - R packages: aRxiv, dplyr, tidyr, lubridate, purrr, readr, fs, httr,
#                 tibble, progressr, ellmer
#   - API credentials for chosen LLM provider (if using cloud providers)
#
# Notes:
#   - Downloads respect arXiv rate limits with configurable delays
#   - Uses httr::RETRY() for resilient network requests
#   - PDF extraction requires LLM provider with PDF attachment support
#   - Results are cached to avoid redundant API calls
# =============================================================================


# Configuration ---------------------------------------------------------------

OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY")
get_openai_api_key = function(){return(OPENAI_API_KEY)}

config <- list(
  # arXiv search parameters
  query = '(ti:"control chart" OR abs:"control chart")',
  limit = 10000L,
  
  # Directory paths
  pdf_dir = "local_papers/arxiv",
  output_dir = "data",
  
  # Download settings
  download_delay_sec = 3L,
  user_agent = "arXiv control-chart downloader (polite; academic use)",
  
  # LLM extraction settings
  extraction_enabled = TRUE,
  extraction_delay_sec = 0L,
  llm_provider = "openai",
  llm_model = "gpt-5.2-2025-12-11",
  n_extraction_repeats = 1L,
  
  # Output file paths
  metadata_csv = "data/arxiv_metadata.csv",
  factsheet_cache_rds = "data/spc_factsheet_cache.rds",
  factsheet_csv = "data/spc_factsheet.csv"
)


# Helper Functions ------------------------------------------------------------

#' Download a Single PDF from arXiv
#'
#' Downloads a PDF file with retry logic and polite error handling.
#' Skips download if the file already exists.
#'
#' @param pdf_url Character. URL of the PDF to download.
#' @param output_path Character. Local file path for saving the PDF.
#' @param user_agent Character. User agent string for the request.
#'
#' @return Logical (invisible). TRUE if successful or file exists, FALSE on failure.
download_pdf <- function(pdf_url, output_path, user_agent) {
  if (fs::file_exists(output_path)) {
    return(invisible(TRUE))
  }
  
  fs::dir_create(fs::path_dir(output_path))
  
  response <- httr::RETRY(
    verb = "GET",
    url = pdf_url,
    times = 5L,
    pause_min = 1,
    pause_cap = 10,
    terminate_on = c(400L, 401L, 403L, 404L),
    httr::user_agent(user_agent)
  )
  
  if (httr::http_error(response)) {
    warning(
      sprintf("Download failed [%d]: %s", httr::status_code(response), pdf_url),
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  
  writeBin(httr::content(response, as = "raw"), output_path)
  invisible(TRUE)
}


#' Create LLM Chat Object
#'
#' Factory function for creating ellmer chat objects based on provider.
#'
#' @param provider Character. One of "ollama", "anthropic", or "openai".
#' @param model Character. Model identifier for the chosen provider.
#'
#' @return An ellmer chat object.
create_chat <- function(provider, model) {
  provider <- tolower(provider)
  
  chat_fn <- switch(
    provider,
    ollama = ellmer::chat_ollama,
    anthropic = ellmer::chat_anthropic,
    openai = ellmer::chat_openai,
    stop(sprintf("Unknown provider: '%s'. Use 'ollama', 'anthropic', or 'openai'.", provider))
  )
  
  chat_fn(
    model = model, 
    credentials = list(api_key = if (provider == "openai") get_openai_api_key() else NULL)
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


#' Normalize SPC Factsheet Output
#'
#' Ensures consistent structure and types for extracted factsheet data.
#'
#' @param raw_output Raw output from LLM extraction (list or tibble).
#'
#' @return A single-row tibble with standardized columns.
normalize_factsheet <- function(raw_output) {
  # Fields that should become list-columns (multi-select)
  list_col_fields <- c(
    "chart_family",
    "chart_statistic",
    "phase",
    "application_domain",
    "evaluation_type",
    "performance_metrics",
    "software_platform",
    "code_availability_source",
    "software_urls"
  )
  
  # Boolean fields
  boolean_fields <- c(
    "is_spc_paper",
    "assumes_normality",
    "handles_autocorrelation",
    "handles_missing_data",
    "code_used"
  )
  
  # Text fields
  text_fields <- c(
    "sample_size_requirements",
    "summary",
    "key_equations",
    "key_results",
    "limitations_stated",
    "limitations_unstated",
    "future_work_stated",
    "future_work_unstated"
  )
  
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
  
  # Normalize types
  result <- result |>
    dplyr::mutate(
      # List columns
      chart_family = as_list_chr(chart_family),
      chart_statistic = as_list_chr(chart_statistic),
      phase = as_list_chr(phase),
      application_domain = as_list_chr(application_domain),
      evaluation_type = as_list_chr(evaluation_type),
      performance_metrics = as_list_chr(performance_metrics),
      software_platform = as_list_chr(software_platform),
      code_availability_source = as_list_chr(code_availability_source),
      software_urls = as_list_chr(software_urls),
      
      # Boolean columns
      is_spc_paper = as.logical(is_spc_paper),
      assumes_normality = as.logical(assumes_normality),
      handles_autocorrelation = as.logical(handles_autocorrelation),
      handles_missing_data = as.logical(handles_missing_data),
      code_used = as.logical(code_used),
      
      # Text columns
      sample_size_requirements = if (is.na(sample_size_requirements)) NA_character_ else as.character(sample_size_requirements),
      summary = if (is.na(summary)) NA_character_ else as.character(summary),
      key_equations = if (is.na(key_equations)) NA_character_ else as.character(key_equations),
      key_results = if (is.na(key_results)) NA_character_ else as.character(key_results),
      limitations_stated = if (is.na(limitations_stated)) NA_character_ else as.character(limitations_stated),
      limitations_unstated = if (is.na(limitations_unstated)) NA_character_ else as.character(limitations_unstated),
      future_work_stated = if (is.na(future_work_stated)) NA_character_ else as.character(future_work_stated),
      future_work_unstated = if (is.na(future_work_unstated)) NA_character_ else as.character(future_work_unstated)
    ) |>
    dplyr::slice(1L)
  
  result
}


#' Extract SPC Factsheet from PDF
#'
#' Uses an LLM to extract structured metadata from a PDF document.
#'
#' @param pdf_path Character. Path to the PDF file.
#' @param chat_object An ellmer chat object.
#' @param type_object An ellmer type specification.
#'
#' @return A normalized single-row tibble.
extract_factsheet_from_pdf <- function(pdf_path, chat_object, type_object) {
  pdf_content <- ellmer::content_pdf_file(pdf_path)
  
  prompt <- paste(
    "Extract the SPC factsheet fields from the attached PDF.",
    "Select ALL applicable categories for multi-label fields.",
    "If a URL exists, return only the URL string.",
    "If code is implied but not shared, choose 'Not provided'.",
    sep = "\n"
  )
  
  raw_output <- chat_object$chat_structured(prompt, pdf_content, type = type_object)
  
  normalize_factsheet(raw_output)
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


# Main Pipeline ---------------------------------------------------------------

# Initialize directories
fs::dir_create(config$pdf_dir)
fs::dir_create(config$output_dir)

# Set up progress reporting
progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")


# Step 1: Search arXiv --------------------------------------------------------

message("Searching arXiv...")

metadata <- aRxiv::arxiv_search(query = config$query, limit = config$limit) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    pdf_url = paste0("https://arxiv.org/pdf/", id, ".pdf"),
    output_path = fs::path(config$pdf_dir, paste0(id, ".pdf")),
    submitted = lubridate::ymd_hms(submitted)
    ) |> 
  dplyr::arrange(dplyr::desc(submitted))

readr::write_csv(metadata, config$metadata_csv)
message(sprintf("Found %d papers. Metadata saved to: %s", nrow(metadata), config$metadata_csv))


# Step 2: Download PDFs -------------------------------------------------------

papers_to_download <- metadata |>
  dplyr::filter(!fs::file_exists(output_path))

message(sprintf("Papers to download: %d of %d", nrow(papers_to_download), nrow(metadata)))

if (nrow(papers_to_download) > 0L) {
  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(papers_to_download))
    
    purrr::pwalk(
      papers_to_download,
      function(pdf_url, output_path, ...) {
        p(message = fs::path_file(output_path))
        download_pdf(pdf_url, output_path, config$user_agent)
        Sys.sleep(config$download_delay_sec)
      }
    )
  })
  
  message("PDF downloads complete.")
} else {
  message("All PDFs already downloaded.")
}


# Step 3: Extract Factsheets --------------------------------------------------

if (!config$extraction_enabled) {
  message("Structured extraction disabled.")
} else {
  available_pdfs <- tibble::tibble(
    pdf_path = fs::dir_ls(config$pdf_dir, glob = "*.pdf"),
    id = fs::path_ext_remove(fs::path_file(pdf_path))
  )
  
  if (nrow(available_pdfs) == 0L) {
    message(sprintf("No PDFs found in: %s", config$pdf_dir))
  } else {
    # Load existing cache
    factsheet_cache <- if (fs::file_exists(config$factsheet_cache_rds)) {
      readRDS(config$factsheet_cache_rds)
    } else {
      tibble::tibble()
    }
    
    extracted_ids <- if (nrow(factsheet_cache) > 0L && "id" %in% names(factsheet_cache)) {
      unique(dplyr::pull(factsheet_cache, id))
    } else {
      character(0L)
    }
    
    pdfs_to_extract <- available_pdfs |>
      dplyr::filter(!id %in% extracted_ids)
    
    message(sprintf(
      "Extraction status: %d available, %d cached, %d remaining",
      nrow(available_pdfs),
      length(extracted_ids),
      nrow(pdfs_to_extract)
    ))
    
    if (nrow(pdfs_to_extract) == 0L) {
      message("All factsheets already extracted.")
    } else {
      # Safe extraction wrapper with error logging
      safe_extract <- function(pdf_path, chat_object, type_object) {
        tryCatch(
          extract_factsheet_from_pdf(pdf_path, chat_object, type_object),
          error = function(e) {
            warning(
              sprintf("Extraction failed for %s: %s", basename(pdf_path), conditionMessage(e)),
              call. = FALSE
            )
            normalize_factsheet(tibble::tibble(
              # Relevance
              is_spc_paper = NA,
              
              # Classification
              chart_family = list(NA_character_),
              chart_statistic = list(NA_character_),
              phase = list(NA_character_),
              application_domain = list(NA_character_),
              
              # Data assumptions
              assumes_normality = NA,
              handles_autocorrelation = NA,
              handles_missing_data = NA,
              
              # Evaluation
              evaluation_type = list(NA_character_),
              performance_metrics = list(NA_character_),
              sample_size_requirements = NA_character_,
              
              # Software
              code_used = NA,
              software_platform = list(NA_character_),
              code_availability_source = list(NA_character_),
              software_urls = list(NA_character_),
              
              # Content
              summary = NA_character_,
              key_equations = NA_character_,
              key_results = NA_character_,
              
              # Limitations
              limitations_stated = NA_character_,
              limitations_unstated = NA_character_,
              
              # Future work
              future_work_stated = NA_character_,
              future_work_unstated = NA_character_
            ))
          }
        )
      }
      
      # Build extraction grid (supports multiple repeats)
      extraction_grid <- tidyr::crossing(
        pdfs_to_extract,
        repeat_id = seq_len(config$n_extraction_repeats)
      )

      # Process each PDF and save incrementally
      for (i in seq_len(nrow(extraction_grid))) {
        row <- extraction_grid[i, ]
        pdf_path <- row$pdf_path
        id <- row$id
        repeat_id <- row$repeat_id

        message(sprintf("[%d/%d] %s | repeat %d", i, nrow(extraction_grid), id, repeat_id))

        chat_clean <- create_chat(config$llm_provider, config$llm_model)

        result <- safe_extract(pdf_path, chat_clean, schema_spc_factsheet) |>
          dplyr::mutate(
            id = id,
            pdf_path = as.character(pdf_path),
            llm_provider = config$llm_provider,
            llm_model = config$llm_model,
            repeat_id = repeat_id,
            extracted_at = Sys.time()
          )

        # Update cache incrementally
        factsheet_cache <- dplyr::bind_rows(factsheet_cache, result)
        saveRDS(factsheet_cache, config$factsheet_cache_rds)

        # Export CSV with list columns collapsed to delimited strings
        factsheet_csv <- factsheet_cache |>
          dplyr::mutate(
            chart_family = collapse_list_col(chart_family),
            chart_statistic = collapse_list_col(chart_statistic),
            phase = collapse_list_col(phase),
            application_domain = collapse_list_col(application_domain),
            evaluation_type = collapse_list_col(evaluation_type),
            performance_metrics = collapse_list_col(performance_metrics),
            software_platform = collapse_list_col(software_platform),
            code_availability_source = collapse_list_col(code_availability_source),
            software_urls = collapse_list_col(software_urls)
          )
        readr::write_csv(factsheet_csv, config$factsheet_csv)

        Sys.sleep(config$extraction_delay_sec)
      }

      message(sprintf("Extraction complete. Cache: %s", config$factsheet_cache_rds))
      message(sprintf("CSV: %s", config$factsheet_csv))
    }
  }
}

message("Pipeline complete.")