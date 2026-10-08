help_ctx <- function(spec = TEST_SPEC, settings = TEST_SETTINGS, data = fixture_data()) {
  help_context(spec, settings, data, reliability_load(app_path("config", "field_reliability_v1.json")))
}

test_that("every topic has a title and a body, and unknown topics say so", {
  ctx <- help_ctx()
  for (topic in HELP_TOPICS) {
    content <- help_content(topic, ctx, track = "spc")
    expect_true(nzchar(content$title), info = topic)
    expect_gt(nchar(help_text(topic, ctx, "spc")), 80L)
  }
  expect_match(help_text("no_such_topic", ctx), "No help is available")
})

test_that("help names the models, limits and query that are configured", {
  ctx <- help_ctx()
  ask <- help_text("ask", ctx)
  expect_match(ask, TEST_SPEC$models$question, fixed = TRUE)
  expect_match(ask, TEST_SETTINGS$jev$model, fixed = TRUE)
  expect_match(ask, paste0(QUESTION_MAX_CHARS, " characters"), fixed = TRUE)
  expect_match(ask, paste0(QUESTION_RATE_LIMIT, " questions per ", QUESTION_RATE_WINDOW_SEC / 60, " minutes"), fixed = TRUE)
  relevance <- help_text("relevance", ctx)
  expect_match(relevance, TEST_SETTINGS$jev$model, fixed = TRUE)
  expect_match(relevance, as.character(JEV_MAX_PAPERS), fixed = TRUE)
  expect_match(relevance, as.character(JEV_THRESHOLD), fixed = TRUE)
  chat <- help_text("chat", ctx)
  expect_match(chat, TEST_SPEC$models$chat, fixed = TRUE)
  expect_match(chat, paste0("at most ", COLLECTION_CHAT_MAX_PDFS), fixed = TRUE)
  scope <- help_text("scope", ctx, track = "spc")
  expect_match(scope, htmltools::htmlEscape(TEST_SPEC$tracks$spc$query), fixed = TRUE)
  expect_match(scope, htmltools::htmlEscape(TEST_SPEC$tracks$spc$scope$include), fixed = TRUE)
  expect_false(grepl(TEST_SPEC$tracks$reliability$query, scope, fixed = TRUE))
  expect_match(scope, "01 March 2026", fixed = TRUE)                       # the data date, from the data
  expect_match(help_text("rising", ctx), paste0("last ", RISING_RECENT_YEARS), fixed = TRUE)
})

test_that("help follows the spec when the spec changes", {
  changed <- TEST_SPEC
  changed$models$question <- "question-model-under-test"
  changed$models$chat <- "chat-model-under-test"
  changed$models$extraction <- "extraction-model-under-test"
  changed$schema_version <- "9.9.9"
  changed$tracks$spc$query <- "ti:\"a different query\""
  changed$fields$spc[[1]]$values[[1]][[2]] <- "A definition written for this test."
  ctx <- help_ctx(spec = changed)
  expect_match(help_text("ask", ctx), "question-model-under-test", fixed = TRUE)
  expect_false(grepl(TEST_SPEC$models$question, help_text("ask", ctx), fixed = TRUE))
  expect_match(help_text("chat", ctx), "chat-model-under-test", fixed = TRUE)
  factsheet <- help_text("factsheet", ctx, "spc")
  expect_match(factsheet, "extraction-model-under-test", fixed = TRUE)
  expect_match(factsheet, "9.9.9", fixed = TRUE)
  expect_match(help_text("scope", ctx, "spc"), "a different query", fixed = TRUE)
  expect_match(help_text("field:spc:chart_family", ctx), "A definition written for this test.", fixed = TRUE)

  settings <- TEST_SETTINGS
  settings$jev$model <- "ranking-model-under-test"
  expect_match(help_text("relevance", help_ctx(settings = settings)), "ranking-model-under-test", fixed = TRUE)
})

test_that("help reports the models and schema versions actually present in the data", {
  data <- fixture_data()
  factsheet <- help_text("factsheet", help_ctx())
  expect_match(factsheet, "test-extractor", fixed = TRUE)
  data$provenance$llm_model <- "another-extractor"
  data$provenance$schema_version <- "1.0.0-bridged"
  changed <- help_text("factsheet", help_ctx(data = data))
  expect_match(changed, "another-extractor", fixed = TRUE)
  expect_match(changed, "1.0.0-bridged", fixed = TRUE)
  expect_false(grepl("test-extractor", changed, fixed = TRUE))
})

test_that("the reliability section shows n per field and warns about the smallest track", {
  ctx <- help_ctx()
  rel <- ctx$reliability
  html <- as.character(help_reliability(ctx))
  text <- flat(html)
  expect_match(text, rel$measured_on$model, fixed = TRUE)
  expect_match(text, rel$measured_on$schema_version, fixed = TRUE)
  expect_match(text, paste0("DOE has only ", rel$by_track$exp_design, " rated papers"), fixed = TRUE)
  expect_match(html, "<th>n</th>", fixed = TRUE)
  doe_only <- as.character(help_reliability(ctx, "exp_design"))
  expect_false(grepl(">SPM<", doe_only, fixed = TRUE))
  expect_match(as.character(help_reliability(help_context(TEST_SPEC, TEST_SETTINGS, fixture_data(), NULL))),
               "No rating study")
})

test_that("field help gives the question and every value definition from the spec", {
  ctx <- help_ctx()
  field <- spec_field(TEST_SPEC, "spc", "chart_statistic")
  text <- help_text("field:spc:chart_statistic", ctx)
  expect_match(text, htmltools::htmlEscape(field$question), fixed = TRUE)
  for (value in field_values(field)) expect_match(text, htmltools::htmlEscape(value), fixed = TRUE)
  expect_match(text, htmltools::htmlEscape(field_definitions(field)[["CUSUM"]]), fixed = TRUE)
  expect_match(help_text("field::data_source", ctx), "Public benchmark dataset", fixed = TRUE)
  expect_match(help_text("field:spc:nope", ctx), "not in the specification")
  with_fields <- help_text("gap_map", ctx, "spc", fields = list(field))
  expect_match(with_fields, htmltools::htmlEscape(field$question), fixed = TRUE)
})

test_that("the help control is a labelled button", {
  html <- as.character(help_button("ask", "How asking a question works"))
  expect_match(html, "^<button type=\"button\"")
  expect_match(html, "data-help=\"ask\"", fixed = TRUE)
  expect_match(html, "aria-label=\"How asking a question works\"", fixed = TRUE)
})
