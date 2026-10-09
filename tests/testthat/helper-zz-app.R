# Fixture rows carry the current schema version so they read as up to date.
FIXTURE_SCHEMA <- jsonlite::read_json(app_path("config", "factsheet_spec.json"))$schema_version

# Fixtures for the app tests: a small dataset in the version 2 layout, written
# to a temporary directory and read back with the app's own loader.

TEST_SPEC <- spec_load(app_path("config", "factsheet_spec.json"))
TEST_SETTINGS <- app_settings_load(app_path("config", "app_settings.json"))

fixture_factsheet_row <- function(track, id, status = "ok", ...) {
  columns <- factsheet_columns(TEST_SPEC, track)
  row <- stats::setNames(as.list(rep(NA_character_, length(columns))), columns)
  base <- list(paper_id = id, arxiv_version = "2", id = paste0(id, "v2"), track = track, status = status,
               schema_version = FIXTURE_SCHEMA, llm_model = "test-extractor", extracted_at = "2026-03-01T10:00:00Z")
  if (status == "ok") {
    base <- c(base, list(scope_decision = "in_scope", scope_category = "in_scope",
                         summary = paste("Summary of paper", id), key_results = "Results.",
                         key_equations = "Statistic: \\[z_t = \\lambda x_t\\] where x is the observation."))
  }
  values <- utils::modifyList(base, list(...))
  for (name in names(values)) row[[name]] <- as.character(values[[name]])
  as.data.frame(row, stringsAsFactors = FALSE)
}

fixture_metadata_row <- function(id, year, title = paste("Paper", id), authors = "Ann Author|Bo Writer",
                                 version = 2L, abstract = paste("Abstract of", id), category = "stat.ME") {
  data.frame(id = paste0(id, "v", version), submitted = sprintf("%d-05-01T00:00:00Z", year),
             updated = sprintf("%d-06-01 00:00:00", year), title = title, abstract = abstract,
             authors = authors, link_abstract = paste0("https://arxiv.org/abs/", id, "v", version),
             link_pdf = paste0("https://arxiv.org/pdf/", id, "v", version), comment = NA, journal_ref = NA,
             doi = NA, primary_category = category, categories = category, stringsAsFactors = FALSE)
}

# Eleven rows over three tracks. Ids starting 25 are from 2025, and so on.
fixture_data_dir <- function() {
  dir <- tempfile("qew_fixture_")
  dir.create(dir)
  spc <- rbind(
    fixture_factsheet_row("spc", "2501.00001", paper_type = "New method", labels_confirmed = "chart_family", labels_disputed = "paper_type",
                          labels_resolved = "chart_statistic|phase", labels_changed = "phase",
                          chart_family = "Univariate",
                          chart_approach = "Nonparametric (distribution-free)|Bayesian",
                          chart_approach_evidence = "a distribution-free <b>EWMA</b> chart",
                          chart_statistic_primary = "EWMA", chart_statistic_additional = "CUSUM",
                          chart_statistic = "EWMA|CUSUM", phase = "Phase II", assumes_normality = "FALSE",
                          evaluation_type = "Simulation study|Real-data example",
                          application_domain_primary = "Manufacturing", application_domain = "Manufacturing",
                          data_source = "Simulated data|Real data: field or operational",
                          software_platform = "R|Python", code_availability = "Public repository",
                          software_urls = "https://example.org/code", code_public = "TRUE",
                          glossary = "EWMA = exponentially weighted moving average|ARL = average run length",
                          limitations_stated = "Assumes independence.",
                          limitations_unstated = "No autocorrelation study."),
    fixture_factsheet_row("spc", "2401.00002", paper_type = "Extension of an existing method",
                          chart_family = "Multivariate", chart_approach = "Bayesian",
                          chart_statistic_primary = "MEWMA", chart_statistic = "MEWMA", phase = "Phase I",
                          assumes_normality = "TRUE", evaluation_type = "Simulation study",
                          application_domain_primary = "Healthcare and medical",
                          application_domain = "Healthcare and medical", data_source = "Simulated data",
                          software_platform = "MATLAB", code_availability = "Not shared", code_public = "FALSE"),
    fixture_factsheet_row("spc", "2001.00003", paper_type = "Review or tutorial",
                          chart_family = "None of the listed", chart_family_other_term = "compositional data",
                          chart_approach = "None", chart_statistic_primary = "Shewhart-type",
                          chart_statistic = "Shewhart-type", phase = "Not applicable",
                          evaluation_type = "None",
                          application_domain_primary = "No specific application (general methodology)",
                          application_domain = "No specific application (general methodology)",
                          data_source = "No data (theory only)", software_platform = "Not stated",
                          code_availability = "No code used", code_public = "FALSE"),
    fixture_factsheet_row("spc", "2501.00004", status = "out_of_scope", scope_decision = "out_of_scope",
                          scope_category = "keyword_in_passing", scope_reason = "Control charts are background."),
    fixture_factsheet_row("spc", "2501.00005", status = "failed", error_class = "timeout"),
    fixture_factsheet_row("spc", "2501.00099", paper_type = "New method"))   # no metadata row
  doe <- rbind(
    fixture_factsheet_row("exp_design", "2502.00006", paper_type = "New method",
                          design_type_primary = "Bayesian design", design_type_additional = "Optimal design",
                          design_type = "Bayesian design|Optimal design",
                          design_objective_primary = "Parameter estimation",
                          design_objective = "Parameter estimation", optimality_criterion = "D-optimal",
                          evaluation_type = "Simulation study",
                          application_domain_primary = "Manufacturing", application_domain = "Manufacturing",
                          data_source = "Public benchmark dataset", software_platform = "R",
                          code_availability = "Package registry", code_public = "TRUE"),
    fixture_factsheet_row("exp_design", "1902.00007", paper_type = "Theory",
                          design_type_primary = "Optimal design", design_type = "Optimal design",
                          design_objective_primary = "Prediction", design_objective = "Prediction",
                          optimality_criterion = "Not applicable", evaluation_type = "Theoretical derivation",
                          application_domain_primary = "Pharmaceutical", application_domain = "Pharmaceutical",
                          data_source = "Simulated data", software_platform = "Not stated",
                          code_availability = "Not shared", code_public = "FALSE"))
  rel <- rbind(
    fixture_factsheet_row("reliability", "2503.00008", paper_type = "Application or case study",
                          reliability_topic_primary = "Maintenance optimization",
                          reliability_topic = "Maintenance optimization",
                          model_family_primary = "Machine learning or deep learning",
                          model_family = "Machine learning or deep learning",
                          evaluation_type = "Real-data case study",
                          application_domain_primary = "Energy and utilities",
                          application_domain = "Energy and utilities",
                          data_source = "Real data: field or operational", software_platform = "Python",
                          code_availability = "Not shared", code_public = "FALSE",
                          summary = "Maintenance of offshore wind turbines with sensor data."),
    fixture_factsheet_row("reliability", "2203.00009", paper_type = "New method",
                          reliability_topic_primary = "Degradation modeling",
                          reliability_topic = "Degradation modeling",
                          model_family_primary = "Stochastic degradation process",
                          model_family = "Stochastic degradation process",
                          evaluation_type = "Simulation study",
                          application_domain_primary = "Manufacturing", application_domain = "Manufacturing",
                          data_source = "Simulated data", software_platform = "R",
                          code_availability = "Public repository", code_public = "TRUE"),
    fixture_factsheet_row("reliability", "2503.00010", status = "out_of_scope", scope_decision = "out_of_scope",
                          scope_category = "different_meaning_of_keyword"))
  write_factsheet_atomic(spc, file.path(dir, TEST_SPEC$tracks$spc$factsheet_csv))
  write_factsheet_atomic(doe, file.path(dir, TEST_SPEC$tracks$exp_design$factsheet_csv))
  write_factsheet_atomic(rel, file.path(dir, TEST_SPEC$tracks$reliability$factsheet_csv))

  meta_spc <- rbind(
    fixture_metadata_row("2501.00001", 2025, version = 1L, title = "Old title"),
    fixture_metadata_row("2501.00001", 2025, title = "A chart for $\\bar{X}$ with <script>alert(1)</script>",
                         authors = "Ann Author|Bo Writer|Cy Third|Di Fourth", category = "stat.AP"),
    fixture_metadata_row("2401.00002", 2024, authors = "Ann Author"),
    fixture_metadata_row("2001.00003", 2020),
    fixture_metadata_row("2501.00004", 2025),
    fixture_metadata_row("2501.00005", 2025),
    fixture_metadata_row("2501.00077", 2025))                                 # no factsheet yet
  meta_doe <- rbind(fixture_metadata_row("2502.00006", 2025), fixture_metadata_row("1902.00007", 2019))
  meta_rel <- rbind(fixture_metadata_row("2503.00008", 2025, title = "Wind turbine maintenance"),
                    fixture_metadata_row("2203.00009", 2022), fixture_metadata_row("2503.00010", 2025))
  readr::write_csv(meta_spc, file.path(dir, TEST_SPEC$tracks$spc$metadata_csv), na = "")
  readr::write_csv(meta_doe, file.path(dir, TEST_SPEC$tracks$exp_design$metadata_csv), na = "")
  readr::write_csv(meta_rel, file.path(dir, TEST_SPEC$tracks$reliability$metadata_csv), na = "")
  dir
}

fixture_data <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) cached <<- load_app_data(TEST_SPEC, fixture_data_dir())
    cached
  }
})

fixture_papers <- function() fixture_data()$papers

ids_of <- function(papers, state) {
  sort(apply_filters(papers, state, TEST_SPEC, TEST_SETTINGS)$paper_id)
}

dev_data_dir <- function() app_path("data", "dev_v2")
skip_without_dev_data <- function() {
  testthat::skip_if_not(file.exists(file.path(dev_data_dir(), TEST_SPEC$tracks$spc$factsheet_csv)),
                        "development dataset data/dev_v2 is not present")
}

# HTML as one line of text with single spaces, for matching text that spans tags.
flat <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", as.character(x)))
