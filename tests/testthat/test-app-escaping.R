# The browser-side cell renderers in www/qew.js place cell text inside HTML and
# rely on DT escaping that text on the server. Titles and abstracts come from
# arXiv and summaries from a model, so that escaping must never be switched off.

app_sources <- function() {
  files <- c(app_path("app.R"), list.files(app_path("R"), pattern = "^app_.*\\.R$", full.names = TRUE))
  stats::setNames(lapply(files, function(f) paste(readLines(f, warn = FALSE), collapse = "\n")), basename(files))
}

test_that("every data table escapes its cell text on the server", {
  sources <- app_sources()
  calls <- 0L
  for (name in names(sources)) {
    starts <- gregexpr("DT::datatable\\(", sources[[name]])[[1]]
    if (starts[1] == -1L) next
    for (start in starts) {
      calls <- calls + 1L
      call_text <- substr(sources[[name]], start, start + 600L)
      expect_match(call_text, "escape\\s*=\\s*TRUE", info = paste(name, "datatable call"))
    }
  }
  expect_gt(calls, 0L)
  expect_false(any(grepl("escape\\s*=\\s*(FALSE|F\\b|c\\(|-)", unlist(sources))))
})

test_that("model and arXiv text is never inserted as raw HTML", {
  sources <- app_sources()
  fields <- c("summary", "key_results", "key_equations", "abstract", "title", "limitations_stated",
              "limitations_unstated", "future_work_stated", "future_work_unstated", "glossary",
              "scope_reason", "_evidence", "_other_term")
  pattern <- paste0("HTML\\([^)]*\\$(", paste(fields, collapse = "|"), ")")
  for (name in names(sources)) {
    expect_false(grepl(pattern, sources[[name]]), info = name)
  }
})
