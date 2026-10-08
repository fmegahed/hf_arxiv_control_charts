spec <- spec_load(app_path("config", "factsheet_spec.json"))
field <- function(track, name) spec_field(spec, track, name)

test_that("a single label is kept when valid and dropped when not an option", {
  out <- postprocess_field(field("spc", "phase"), list(phase = "Phase II", phase_evidence = "we monitor online"))
  expect_identical(out$columns$phase, "Phase II")
  expect_identical(out$columns$phase_evidence, "we monitor online")
  expect_identical(postprocess_field(field("spc", "phase"), list(phase = "Both"))$columns$phase,
                   NA_character_)
})

test_that("additional labels drop the primary, the 'None' marker and unknown values", {
  raw <- list(chart_statistic_primary = "EWMA",
              chart_statistic_additional = list("EWMA", "None", "CUSUM", "Made up"),
              chart_statistic_other_term = "n/a", chart_statistic_evidence = "Section 2")
  out <- postprocess_field(field("spc", "chart_statistic"), raw)$columns
  expect_identical(out$chart_statistic_primary, "EWMA")
  expect_identical(out$chart_statistic_additional, "CUSUM")
  expect_identical(out$chart_statistic, "EWMA|CUSUM")
  expect_identical(out$chart_statistic_other_term, NA_character_)
})

test_that("additional labels are capped and the cap is flagged", {
  raw <- list(chart_statistic_primary = "EWMA",
              chart_statistic_additional = list("CUSUM", "MEWMA", "MCUSUM", "Shewhart-type"))
  out <- postprocess_field(field("spc", "chart_statistic"), raw, max_additional = 2L)
  expect_identical(out$columns$chart_statistic_additional, "CUSUM|MEWMA")
  expect_identical(out$columns$chart_statistic, "EWMA|CUSUM|MEWMA")
  expect_identical(out$flags, "truncated_additional")
})

test_that("no additional labels gives NA additional and a combined column equal to the primary", {
  raw <- list(reliability_topic_primary = "Degradation modeling",
              reliability_topic_additional = list("None"))
  out <- postprocess_field(field("reliability", "reliability_topic"), raw)$columns
  expect_identical(out$reliability_topic_additional, NA_character_)
  expect_identical(out$reliability_topic, "Degradation modeling")
})

test_that("a primary of 'Not applicable' clears additional labels", {
  raw <- list(design_type_primary = "Not applicable", design_type_additional = list("Optimal design"))
  out <- postprocess_field(field("exp_design", "design_type"), raw)$columns
  expect_identical(out$design_type_additional, NA_character_)
  expect_identical(out$design_type, "Not applicable")
})

test_that("the catch-all keeps its term; a listed label discards any term", {
  f <- field("reliability", "model_family")
  other <- postprocess_field(f, list(model_family_primary = NONE_OF_LISTED,
                                     model_family_additional = list("None"),
                                     model_family_other_term = "copula dependence model"))
  expect_identical(other$columns$model_family_primary, NONE_OF_LISTED)
  expect_identical(other$columns$model_family_other_term, "copula dependence model")

  listed <- postprocess_field(f, list(model_family_primary = "Surrogate model",
                                      model_family_other_term = "polynomial chaos"))
  expect_identical(listed$columns$model_family_other_term, NA_character_)

  unnamed <- postprocess_field(f, list(model_family_primary = NONE_OF_LISTED,
                                       model_family_other_term = "n/a"))
  expect_identical(unnamed$columns$model_family_other_term, "unspecified")
  expect_identical(unnamed$flags, "other_term_missing")
})

test_that("a sentinel never sits beside a real value, and an empty list becomes the sentinel", {
  f <- field("reliability", "maintenance_policy")
  mixed <- postprocess_field(f, list(maintenance_policy = list("No maintenance policy", "Condition-based")))
  expect_identical(mixed$columns$maintenance_policy, "Condition-based")
  empty <- postprocess_field(f, list(maintenance_policy = list()))
  expect_identical(empty$columns$maintenance_policy, "No maintenance policy")
  junk <- postprocess_field(f, list(maintenance_policy = list("Not a policy")))
  expect_identical(junk$columns$maintenance_policy, "No maintenance policy")
  both <- postprocess_field(f, list(maintenance_policy = list("Predictive", "Predictive", "Age-based")))
  expect_identical(both$columns$maintenance_policy, "Predictive|Age-based")
})

test_that("tristate answers map to logical with NA for unclear", {
  f <- field("spc", "assumes_normality")
  expect_true(postprocess_field(f, list(assumes_normality = "Yes"))$columns$assumes_normality)
  expect_false(postprocess_field(f, list(assumes_normality = "No"))$columns$assumes_normality)
  expect_true(is.na(postprocess_field(f, list(assumes_normality = "Unclear or not applicable"))$columns$assumes_normality))
  expect_true(is.na(postprocess_field(f, list())$columns$assumes_normality))
})

test_that("only well-formed URLs are kept", {
  f <- field("spc", "software_urls")
  out <- postprocess_field(f, list(software_urls = list("https://github.com/a/b", "none", "see appendix",
                                                        "http://cran.r-project.org/package=x")))
  expect_identical(out$columns$software_urls, "https://github.com/a/b|http://cran.r-project.org/package=x")
  expect_identical(postprocess_field(f, list(software_urls = list()))$columns$software_urls, NA_character_)
})

test_that("evidence and text fields are sanitized and length-limited", {
  long <- paste(rep("word", 200), collapse = " ")
  out <- postprocess_field(field("spc", "phase"), list(phase = "Phase II", phase_evidence = long))
  expect_lte(nchar(out$columns$phase_evidence), EVIDENCE_MAX_CHARS)
  expect_match(out$columns$phase_evidence, "\\.\\.\\.$")
  text <- postprocess_field(field("spc", "sample_size_requirements"),
                            list(sample_size_requirements = "Use $m = 100$ subgroups costing $5 each"))
  expect_identical(text$columns$sample_size_requirements,
                   "Use \\(m = 100\\) subgroups costing USD 5 each")
})

test_that("code_public is derived from code availability", {
  raw_public <- list(code_availability = "Public repository")
  raw_private <- list(code_availability = "Not shared")
  expect_true(postprocess_classification(raw_public, spec, "spc")$columns$code_public)
  expect_false(postprocess_classification(raw_private, spec, "spc")$columns$code_public)
  expect_false(postprocess_classification(list(code_availability = "Available on request"), spec, "spc")$columns$code_public)
  expect_true(is.na(postprocess_classification(list(), spec, "spc")$columns$code_public))
})

test_that("classification output has exactly the spec's label columns plus code_public", {
  for (track in TRACK_IDS) {
    out <- postprocess_classification(list(), spec, track)
    expect_setequal(names(out$columns), c(spec_label_columns(spec, track), "code_public"))
  }
})

test_that("narrative output is sanitized, equations rendered and problems flagged", {
  raw <- list(
    glossary = list(list(term = "ARL", meaning = "average run length"),
                    list(term = "", meaning = "ignored")),
    summary = "The ARL improves by $20\\%$ and costs $5. EWMA is used.",
    key_results = "ARL of 370.",
    key_equations = list(list(name = "Statistic", latex = "Z_t = \\lambda X_t", explanation = "Z_t is the statistic."),
                         list(name = "Bad", latex = "\\frac{a}{b", explanation = "broken")),
    limitations_stated = "None stated", limitations_unstated = "Assumes independence.",
    future_work_stated = "None stated", future_work_unstated = "Study autocorrelation."
  )
  out <- postprocess_narrative(raw)
  expect_identical(out$columns$glossary, "ARL = average run length")
  expect_identical(out$columns$summary, "The ARL improves by \\(20\\%\\) and costs USD 5. EWMA is used.")
  expect_match(out$columns$key_equations, "Statistic: \\[Z_t = \\lambda X_t\\]", fixed = TRUE)
  expect_setequal(out$flags, c("math_repaired", "latex_invalid", "undefined_acronym"))
  for (col in out$columns) expect_true(validate_stored_text(col)$ok)
})

test_that("a paper with no equations stores 'Not applicable', and clean text raises no flags", {
  raw <- list(glossary = list(), summary = "A plain summary.", key_results = "Plain results.",
              key_equations = list(), limitations_stated = "None stated",
              limitations_unstated = "x", future_work_stated = "None stated", future_work_unstated = "y")
  out <- postprocess_narrative(raw)
  expect_identical(out$columns$key_equations, "Not applicable")
  expect_identical(out$columns$glossary, NA_character_)
  expect_identical(out$flags, character(0))
})

test_that("arrays of objects are accepted as data frames as well as lists", {
  raw <- list(
    glossary = data.frame(term = c("ARL", ""), meaning = c("average run length", "x"), stringsAsFactors = FALSE),
    summary = "The average run length (ARL) is reported.", key_results = "r",
    key_equations = data.frame(name = "Statistic", latex = "Z_t = \\lambda X_t",
                               explanation = "Z_t is the statistic.", stringsAsFactors = FALSE),
    limitations_stated = "None stated", limitations_unstated = "u",
    future_work_stated = "None stated", future_work_unstated = "f")
  out <- postprocess_narrative(raw)
  expect_identical(out$columns$glossary, "ARL = average run length")
  expect_match(out$columns$key_equations, "Statistic: \\[Z_t = \\lambda X_t\\]", fixed = TRUE)
  expect_identical(as_records(NULL), list())
  expect_identical(as_records(data.frame()), list())
  expect_identical(length(as_records(list(list(term = "a"), "stray"))), 1L)
})
