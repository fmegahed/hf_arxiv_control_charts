paper_row <- function(id) find_paper(fixture_papers(), id)
view_html <- function(id, ...) as.character(paper_view(paper_row(id), TEST_SPEC, TEST_SETTINGS, ...))

test_that("the paper page has every section of an in-scope factsheet", {
  html <- view_html("2501.00001", chat = htmltools::tags$div(id = "chat-slot"))
  for (section in c("glance", "summary", "results", "equations", "methods", "evaluation", "software",
                    "limitations", "glossary", "abstract", "chat", "provenance")) {
    expect_match(html, paste0("id=\"factsheet-", section, "\""), fixed = TRUE, info = section)
  }
  expect_match(html, "id=\"chat-slot\"", fixed = TRUE)
  expect_match(html, "https://arxiv.org/abs/2501.00001", fixed = TRUE)
  expect_match(html, "https://arxiv.org/pdf/2501.00001v2", fixed = TRUE)
  expect_match(html, "https://example.org/code", fixed = TRUE)
  expect_match(html, "exponentially weighted moving average", fixed = TRUE)
  expect_false(grepl("id=\"factsheet-future\"", html, fixed = TRUE))       # no future work recorded: no empty section
})

test_that("paper text is escaped and math delimiters are left for the browser", {
  html <- view_html("2501.00001")
  expect_false(grepl("<script>alert", html, fixed = TRUE))
  expect_false(grepl("<b>EWMA</b>", html, fixed = TRUE))
  expect_match(html, "&lt;b&gt;EWMA&lt;/b&gt;", fixed = TRUE)          # evidence shown as text
  expect_match(html, "\\[z_t = \\lambda x_t\\]", fixed = TRUE)
  expect_match(html, "\\(\\bar{X}\\)", fixed = TRUE)
})

test_that("tags are buttons that carry the filter they apply; the primary one is marked", {
  html <- view_html("2501.00001")
  expect_match(html, "data-field=\"chart_statistic\" data-value=\"EWMA\" data-track=\"spc\" data-role=\"primary\"",
               fixed = TRUE)
  expect_match(html, "data-field=\"chart_statistic\" data-value=\"CUSUM\" data-track=\"spc\" data-role=\"any\"",
               fixed = TRUE)
  expect_match(html, "data-field=\"application_domain\" data-value=\"Manufacturing\" data-track=\"\"", fixed = TRUE)
  expect_match(html, "tag-role", fixed = TRUE)
  field <- spec_field(TEST_SPEC, "spc", "chart_statistic")
  chip <- as.character(tag_chip(field, "CUSUM", "spc"))
  expect_match(chip, htmltools::htmlEscape(field_definitions(field)[["CUSUM"]], attribute = TRUE), fixed = TRUE)
  static <- as.character(tag_chip(spec_field(TEST_SPEC, "spc", "handles_missing_data"), TRISTATE_VALUES[1], "spc"))
  expect_match(static, "^<span")                                        # not a filter field: not clickable
})

test_that("evidence is labelled as model-cited and absent evidence leaves no empty box", {
  html <- view_html("2501.00001")
  expect_match(html, "Model-cited evidence", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("<details class=\"evidence\"", html, fixed = TRUE))), 1L)
  expect_null(evidence_block(NA_character_))
})

test_that("what the model inferred is separated from what the authors wrote", {
  html <- view_html("2501.00001")
  expect_match(html, "class=\"attributed by-authors\"", fixed = TRUE)
  expect_match(html, "class=\"attributed by-model\"", fixed = TRUE)
  expect_match(html, "Stated by the authors", fixed = TRUE)
  expect_match(html, "Model-identified", fixed = TRUE)
  expect_lt(regexpr("Assumes independence", html), regexpr("No autocorrelation study", html))
})

test_that("the catch-all label and missing values are shown honestly", {
  html <- view_html("2001.00003")
  expect_match(html, "Other: compositional data", fixed = TRUE)
  expect_match(html, "Not recorded", fixed = TRUE)
  expect_false(grepl(">None of the listed<", html, fixed = TRUE))
})

test_that("provenance states model, date and schema, and flags an older schema", {
  html <- view_html("2501.00001")
  expect_match(html, "test-extractor", fixed = TRUE)
  expect_match(html, "01 March 2026", fixed = TRUE)
  expect_match(html, paste0("<code>", FIXTURE_SCHEMA, "</code>"), fixed = TRUE)
  expect_false(grepl("earlier definition of the fields", html, fixed = TRUE))
  old <- paper_row("2501.00001")
  old$schema_version <- "1.0.0-bridged"
  old$qa_flags <- "pdf_truncated|something_new"
  html <- as.character(paper_view(old, TEST_SPEC, TEST_SETTINGS))
  expect_match(html, "1.0.0-bridged", fixed = TRUE)
  expect_match(html, "earlier definition of the fields", fixed = TRUE)
  expect_match(html, "only its first pages were read", fixed = TRUE)
  expect_match(html, "something_new", fixed = TRUE)
  expect_equal(qa_flag_text(NA_character_), character(0))
})

test_that("a screened-out paper shows why and has no factsheet sections", {
  html <- view_html("2501.00004")
  expect_match(html, "screened out and has no factsheet", fixed = TRUE)
  expect_match(html, "mentioned only as background", fixed = TRUE)
  expect_match(html, "Control charts are background.", fixed = TRUE)
  expect_match(html, "id=\"factsheet-abstract\"", fixed = TRUE)
  expect_false(grepl("id=\"factsheet-summary\"", html, fixed = TRUE))
})

test_that("the report link opens a prefilled issue in the right repository", {
  url <- report_issue_url(as.list(paper_row("2501.00001")), TEST_SPEC, TEST_SETTINGS)
  expect_match(url, "^https://github.com/fmegahed/hf_arxiv_control_charts/issues/new\\?title=")
  decoded <- utils::URLdecode(url)
  expect_match(decoded, "title=Factsheet problem: arXiv 2501.00001", fixed = TRUE)
  expect_match(decoded, "https://arxiv.org/abs/2501.00001", fixed = TRUE)
  expect_match(decoded, "- [ ] Charting statistic:", fixed = TRUE)
  expect_match(decoded, "- [ ] Summary:", fixed = TRUE)
  expect_match(decoded, paste0("model test-extractor, schema ", FIXTURE_SCHEMA), fixed = TRUE)
  expect_match(view_html("2501.00001"), "Report a problem with this factsheet", fixed = TRUE)
})

test_that("text blocks keep paragraphs and never emit raw HTML", {
  html <- as.character(text_block("First <i>line</i>\nsecond line\n\nNew paragraph"))
  expect_equal(lengths(regmatches(html, gregexpr("<p>", html, fixed = TRUE))), 2L)
  expect_match(html, "<br/>", fixed = TRUE)
  expect_match(html, "&lt;i&gt;line&lt;/i&gt;", fixed = TRUE)
  expect_null(text_block(NA_character_))
  expect_null(text_block("  "))
})

test_that("the results table is plain text with the lean default columns", {
  papers <- apply_filters(fixture_papers(), new_filter_state("spc"), TEST_SPEC, TEST_SETTINGS)
  table <- results_table(papers, TEST_SPEC, "spc")
  expect_equal(names(table), c("paper_id", "row_track", "status", "href", "Saved", "Title", "Year", "Authors",
                               "Data structure", "Charting statistic", "Code"))
  expect_equal(table$Code, c("Public", "Not public", "Not public"))
  expect_equal(table$href[1], "?track=spc&paper=2501.00001")
  expect_false(any(grepl("<a |<span|<button", unlist(table))))          # markup is added in the browser
  expect_equal(table$`Data structure`[3], "Other: compositional data")

  extra <- results_table(papers, TEST_SPEC, "spc", extra = c("chart_approach", "paper_id", "assumes_normality", "bogus"))
  expect_equal(extra$Approach[1], "Nonparametric (distribution-free); Bayesian")
  expect_equal(extra$`arXiv id`[1], "2501.00001")
  expect_equal(extra$`Assumes normality`[1:2], TRISTATE_VALUES[2:1])
  expect_false("bogus" %in% names(extra))

  scores <- data.frame(paper_id = "2501.00001", score = 0.91234)
  ranked <- results_table(papers, TEST_SPEC, "spc", scores = scores)
  expect_equal(ranked$Relevance, c(0.912, NA, NA))

  all_tracks <- results_table(apply_filters(fixture_papers(), new_filter_state(), TEST_SPEC, TEST_SETTINGS), TEST_SPEC)
  expect_true(all(c("Track", "Topic", "Method") %in% names(all_tracks)))
  expect_setequal(unique(all_tracks$Track), c("SPM", "DOE", "Reliability"))
  expect_equal(nrow(results_table(papers[0, ], TEST_SPEC, "spc")), 0L)
  expect_s3_class(results_widget(table), "datatables")
})

test_that("the column picker offers the spec's fields for the scope", {
  spc <- extra_column_choices(TEST_SPEC, "spc")
  expect_true(all(c("phase", "chart_approach", "data_source", "paper_id") %in% spc))
  expect_false(any(c("chart_family", "chart_statistic") %in% spc))      # already in the table
  expect_false("phase" %in% extra_column_choices(TEST_SPEC))
})

test_that("the CSV export has one row per paper and every label column", {
  papers <- apply_filters(fixture_papers(), new_filter_state(), TEST_SPEC, TEST_SETTINGS)
  export <- export_table(papers, TEST_SPEC, data.frame(paper_id = "2501.00001", score = 0.9))
  expect_equal(nrow(export), nrow(papers))
  expect_true(all(c("arxiv_id", "track", "title", "arxiv_url", "relevance", "chart_statistic", "design_type",
                    "reliability_topic", "code_public", "llm_model", "schema_version") %in% names(export)))
  expect_equal(export$authors[export$arxiv_id == "2501.00001"], "Ann Author; Bo Writer; Cy Third; Di Fourth")
})

test_that("BibTeX uses the paper's own arXiv category and base id", {
  entry <- bibtex_entry(paper_row("2501.00001"))
  expect_match(entry, "primaryClass  = {stat.AP}", fixed = TRUE)
  expect_false(grepl("stat.ME", entry, fixed = TRUE))
  expect_match(entry, "eprint        = {2501.00001}", fixed = TRUE)
  expect_match(entry, "author        = {Ann Author and Bo Writer and Cy Third and Di Fourth}", fixed = TRUE)
  expect_match(entry, "^@misc\\{author2025_250100001,")
  expect_false(grepl("journal", entry))
  with_journal <- paper_row("2501.00001")
  with_journal$journal_ref <- "Quality Engineering 38(1) & more"
  with_journal$doi <- "10.1000/xyz"
  entry <- bibtex_entry(with_journal)
  expect_match(entry, "Quality Engineering 38(1) \\& more", fixed = TRUE)
  expect_match(entry, "doi           = {10.1000/xyz}", fixed = TRUE)
  export <- bibtex_export(fixture_papers()[1:2, ], TEST_SETTINGS, now = as.POSIXct("2026-02-03", tz = "UTC"))
  expect_match(export[3], "Exported 2026-02-03; 2 papers", fixed = TRUE)
  expect_equal(bibtex_export(fixture_papers()[0, ], TEST_SETTINGS), "% No bookmarked papers to export")
})

test_that("chat prompts and limits come from the spec and named constants", {
  prompt <- collection_chat_system_prompt(TEST_SPEC)
  for (track in TRACK_IDS) expect_match(prompt, TEST_SPEC$tracks[[track]]$topic_name, fixed = TRUE)
  expect_match(paper_chat_system_prompt(as.list(paper_row("2501.00001"))), "Ann Author, Bo Writer", fixed = TRUE)
  many <- fixture_papers()[rep(1:7, 3), ]
  expect_equal(nrow(collection_chat_papers(many)), COLLECTION_CHAT_MAX_PDFS)
  expect_match(chat_error_text("HTTP 429 insufficient quota"), "no credit")
})

test_that("section ids cannot collide with the ids of the paper module's inputs", {
  # The chat element is "paper-chat" (module "paper", input "chat"); a section
  # with the same id would swallow the messages sent to the chat.
  html <- view_html("2501.00001", chat = paper_chat_panel(shiny::NS("paper"), TEST_SPEC))
  ids <- regmatches(html, gregexpr("id=\"[^\"]+\"", html))[[1]]
  expect_false(any(duplicated(ids)))
  expect_true("id=\"paper-chat\"" %in% ids)
  expect_true("id=\"factsheet-chat\"" %in% ids)
})

# What a markdown renderer does to backslash escapes: "\_" becomes "_".
markdown_unescape <- function(x) gsub("\\\\([[:punct:]])", "\\1", x)

test_that("math in a chat answer survives markdown: punctuation inside delimiters is escaped", {
  answer <- "The statistic is \\(z_t = \\lambda x_t + (1-\\lambda) z_{t-1}\\) with *weights*. \\[a_i * b_j \\\\[2pt] c < d\\] Done_now."
  guarded <- guard_math_text(answer)
  expect_equal(markdown_unescape(guarded), answer)                    # the formula comes out exactly
  expect_match(guarded, "with *weights*.", fixed = TRUE)              # text outside math is untouched
  expect_match(guarded, "Done_now.", fixed = TRUE)
  expect_match(guarded, "z\\_t", fixed = TRUE)                         # underscores cannot become emphasis
  expect_match(guarded, "a\\_i \\* b\\_j", fixed = TRUE)
  expect_match(guarded, "c \\< d", fixed = TRUE)                       # nor "<" a tag
  expect_match(guarded, "\\\\\\(z", fixed = TRUE)                      # the delimiter keeps its backslash
  plain <- "No math here, just (parentheses), a_b and [brackets]."
  expect_equal(guard_math_text(plain), plain)
  # a line break inside displayed math would end the paragraph and split the formula
  expect_equal(markdown_unescape(guard_math_text("\\[\na = b\n\\]")), "\\[ a = b \\]")
})

test_that("the math guard gives the same result however the answer is cut into pieces", {
  answer <- "Let \\(n\\) be the index and \\[T_{n} = \\frac{a_n}{b_n} \\\\ x\\] the statistic; \\(w\\) is the window."
  whole <- guard_math_text(answer)
  expect_equal(markdown_unescape(whole), answer)
  for (size in c(1L, 2L, 3L, 7L)) {
    starts <- seq(1L, nchar(answer), by = size)
    pieces <- substring(answer, starts, pmin(starts + size - 1L, nchar(answer)))
    guard <- math_guard()
    streamed <- paste0(paste(vapply(pieces, guard, character(1)), collapse = ""), guard(NULL))
    expect_equal(streamed, whole, info = paste("piece size", size))
  }
})

test_that("a checked label says how the readers fared, and an unchecked one says nothing", {
  paper <- paper_row("2501.00001")
  expect_identical(label_check_status(paper, "chart_family"), "confirmed")
  expect_identical(label_check_status(paper, "chart_statistic"), "kept")
  expect_identical(label_check_status(paper, "phase"), "changed")
  expect_identical(label_check_status(paper, "paper_type"), "disputed")
  expect_true(is.na(label_check_status(paper, "chart_approach")))
  html <- view_html("2501.00001")
  for (text in LABEL_CHECK_TEXT) expect_match(html, htmltools::htmlEscape(text), fixed = TRUE)
  expect_match(html, "data-help=\"label_check\"|label_check")
  expect_null(label_check_note(paper_row("2401.00002"), "chart_family"))   # written before the check existed
  expect_false(grepl("label-check", view_html("2401.00002"), fixed = TRUE))
})
