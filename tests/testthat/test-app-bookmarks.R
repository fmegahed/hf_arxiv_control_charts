# Bookmarks live in the browser. The id migration is a pure JavaScript
# function (www/bookmark_ids.js) tested with Node; the R side only has to
# treat whatever arrives as base ids.

test_that("bookmark ids are migrated from versioned to base ids (Node test)", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node is not installed; run tests/js/test-bookmark-ids.js where it is")
  output <- suppressWarnings(system2(node, shQuote(app_path("tests", "js", "test-bookmark-ids.js")),
                                     stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status") %||% 0L
  expect_equal(status, 0L, info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "bookmark id tests passed")
})

test_that("the page loads the id helper before the bookmark code and reads through the migration", {
  head <- htmltools::renderTags(app_head())$head
  expect_lt(regexpr("bookmark_ids.js", head, fixed = TRUE), regexpr("personalization.js", head, fixed = TRUE))
  script <- paste(readLines(app_path("www", "personalization.js"), warn = FALSE), collapse = "\n")
  expect_match(script, "QEBookmarkIds.migrateStorage", fixed = TRUE)
  expect_match(script, "QEBookmarkIds.baseId", fixed = TRUE)
})

test_that("MathJax is configured for the two delimiter pairs of the storage contract only", {
  expect_match(MATHJAX_CONFIG, "inlineMath: [['\\\\(', '\\\\)']]", fixed = TRUE)
  expect_match(MATHJAX_CONFIG, "displayMath: [['\\\\[', '\\\\]']]", fixed = TRUE)
  expect_false(grepl("$", MATHJAX_CONFIG, fixed = TRUE))
})
