# Relevance criteria, the metadata filters a question can set, and author names.

test_that("criteria are parsed from, and written back to, one string", {
  criteria <- parse_criteria(" wind turbines ; -machine learning; like:2609.31338v2;; wind turbines")
  expect_length(criteria, 3L)
  expect_equal(criteria[[1]], new_criterion("wind turbines"))
  expect_true(criteria[[2]]$exclude)
  expect_equal(criteria[[3]]$similar_to, "2609.31338")
  expect_equal(format_criteria(criteria), "wind turbines; -machine learning; like:2609.31338")
  expect_equal(parse_criteria(format_criteria(criteria)), criteria)
  expect_length(parse_criteria(""), 0L)
  expect_length(parse_criteria(NULL), 0L)
  expect_length(parse_criteria(paste(letters, collapse = ";")), CRITERIA_MAX)
  expect_equal(vapply(criteria, criterion_label, character(1)),
               c("Ranked by relevance to: wind turbines", "Not about: machine learning", "Similar to: 2609.31338"))
  expect_equal(clean_criterion_text("a; b"), "a, b")
  expect_equal(criteria_terms(criteria, list(`2609.31338` = list(title = "Causal degradation models"))),
               c("wind", "turbin", "causal", "degradation", "model"))
  expect_equal(criteria_terms(parse_criteria("microarrays; batteries; -lasers")), c("microarray", "batter"))
})

test_that("each criterion is a chip, and removing one keeps the others", {
  state <- new_filter_state("spc")
  state$residual <- "wind turbines; -machine learning"
  chips <- state_chips(state, TEST_SPEC)
  expect_equal(vapply(chips, function(chip) chip$id, character(1)), c("track", "crit:1", "crit:2"))
  expect_equal(remove_chip(state, "crit:1")$residual, "-machine learning")
  expect_equal(remove_chip(state, "crit:9")$residual, state$residual)
})

test_that("an author filter matches whole words of a name within one author", {
  authors <- c("Fadel M. Megahed|Ying-Ju Chen", "Megan Hedley|Fadel Smith", NA, "F. Megahed")
  expect_equal(author_mask(authors, "Fadel Megahed"), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(author_mask(authors, "megahed"), c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(author_mask(authors, "Megahed, Fadel M."), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(author_mask(authors, "M."), rep(TRUE, 4L))            # nothing but an initial: no filter
  papers <- fixture_papers()
  someone <- split_values(papers$authors[papers$status == "ok"][1])[1]
  state <- new_filter_state()
  state$authors <- someone
  found <- apply_filters(papers, state, TEST_SPEC, TEST_SETTINGS)
  expect_gt(nrow(found), 0L)
  expect_true(all(author_mask(found$authors, someone)))
  expect_equal(state_chips(state, TEST_SPEC)[[1]]$label, paste0("Author: ", someone))
  expect_length(remove_chip(state, "author:1")$authors, 0L)
})

test_that("journal, category, identifier, title and date filters select by the arXiv record", {
  papers <- fixture_papers()
  papers$journal_ref <- NA_character_
  papers$journal_ref[1] <- "Technometrics 67(1), 2025"
  papers$primary_category <- "stat.ME"
  papers$categories <- "stat.ME|cs.LG"
  papers$primary_category[2] <- "math.ST"
  papers$categories[2] <- "math.ST"
  pick <- function(...) {
    state <- utils::modifyList(new_filter_state(), list(include_screened = TRUE, ...))
    papers$paper_id[filter_mask(papers, state, TEST_SPEC, TEST_SETTINGS)]
  }
  everything <- pick()
  expect_equal(pick(venue = "technometrics"), intersect(everything, papers$paper_id[1]))
  expect_false(papers$paper_id[2] %in% pick(category = "cs.LG"))
  expect_true(papers$paper_id[2] %in% pick(category = "math.ST"))
  expect_equal(pick(ids = papers$paper_id[3]), intersect(everything, papers$paper_id[3]))
  word <- name_words(papers$title[1])[1]
  expect_true(papers$paper_id[1] %in% pick(title = word))
  cutoff <- sort(papers$submitted_date, decreasing = TRUE)[2]
  expect_true(all(papers$submitted_date[papers$paper_id %in% pick(since = format(cutoff))] >= cutoff))

  labels <- vapply(state_chips(utils::modifyList(new_filter_state(), list(
    venue = "Technometrics", category = "stat.ME", ids = "2501.00001", title = "control chart",
    since = "2026-09-01")), TEST_SPEC), function(chip) chip$label, character(1))
  expect_equal(labels, c("Journal reference contains: Technometrics", "arXiv category: stat.ME",
                         "arXiv id: 2501.00001", "Title contains: control chart", "Submitted since: 2026-09-01"))
})

test_that("values from a link or a model are checked before they reach the filters", {
  dirty <- utils::modifyList(new_filter_state(), list(
    authors = c(" Fadel Megahed ", "", "M."), venue = "  Journal   of Quality Technology ",
    category = "stat.ME; drop", ids = c("2501.00001v3", "not-an-id"), title = strrep("x", 500L),
    since = "next week", residual = "wind;; -ml"))
  clean <- sanitize_state(dirty, TEST_SPEC, c(2002L, 2026L))$state
  expect_equal(clean$authors, "Fadel Megahed")
  expect_equal(clean$venue, "Journal of Quality Technology")
  expect_equal(clean$category, "")
  expect_equal(clean$ids, "2501.00001")
  expect_equal(nchar(clean$title), AUTHOR_NAME_MAX_CHARS)
  expect_null(clean$since)
  expect_equal(clean$residual, "wind; -ml")
  expect_equal(sanitize_state(utils::modifyList(new_filter_state(), list(since = "2026-09-01")), TEST_SPEC,
                              c(2002L, 2026L))$state$since, "2026-09-01")
})

test_that("the new filters survive a round trip through the address of the page", {
  state <- utils::modifyList(new_filter_state("spc"), list(
    authors = c("Fadel Megahed", "Inez Zwetsloot"), venue = "Technometrics", category = "stat.ME",
    ids = c("2501.00001", "2502.00002"), title = "control chart", since = "2026-09-01",
    residual = "wind turbines; -machine learning", sort = "relevance"))
  query <- encode_view(new_view("browse", filters = state))
  back <- decode_view(query, TEST_SPEC, c(2002L, 2026L))$filters
  expect_equal(back, normalize_state(state))
})

test_that("spellings of one person's name are counted as one", {
  counts <- c("Inez M. Zwetsloot" = 4L, "Inez Maria Zwetsloot" = 7L, "Inez Zwetsloot" = 1L,
              "Maria L. Weese" = 3L, "Maria Weese" = 2L,
              "Wei X. Zhang" = 2L, "Wei Y. Zhang" = 2L, "Wei Zhang" = 5L,
              "L. Allison Jones-Farmer" = 1L, "L. Allision Jones-Farmer" = 1L,
              "Mar\u00eda Jaenada" = 1L, "Maria Jaenada" = 2L, "Sven Knoth" = 9L)
  map <- author_merge_map(counts, aliases = c("L. Allision Jones-Farmer" = "L. Allison Jones-Farmer"))
  expect_equal(unname(map[c("Inez M. Zwetsloot", "Inez Zwetsloot")]), rep("Inez Maria Zwetsloot", 2L))
  expect_equal(map[["Maria Weese"]], "Maria L. Weese")
  expect_false(any(grepl("Zhang", names(map))))                  # contradictory middle names: left alone
  expect_equal(map[["L. Allision Jones-Farmer"]], "L. Allison Jones-Farmer")
  expect_equal(map[["Mar\u00eda Jaenada"]], "Maria Jaenada")
  expect_false("Sven Knoth" %in% names(map))
  expect_true(middle_names_agree(c("m"), c("maria")))
  expect_false(middle_names_agree(c("m"), c("l")))

  merged <- merge_author_names(c("Inez M. Zwetsloot|Sven Knoth", "Inez Maria Zwetsloot", "Inez Maria Zwetsloot", NA),
                               aliases = character(0))
  expect_equal(merged, c("Inez Maria Zwetsloot|Sven Knoth", "Inez Maria Zwetsloot", "Inez Maria Zwetsloot", NA))
  table <- author_counts(data.frame(paper_id = c("a", "b", "c"), year = 2025L, stringsAsFactors = FALSE,
                                    authors = c("Inez M. Zwetsloot", "Inez Maria Zwetsloot", "Inez Maria Zwetsloot"),
                                    authors_merged = merged[1:3]))
  expect_equal(table$n[table$author == "Inez Maria Zwetsloot"], 3L)
  aliases <- load_author_aliases(app_path("config", "author_aliases.json"))
  expect_true(all(nzchar(aliases)) && !is.null(names(aliases)))
})

test_that("the Code cell links to the code, or to the paper when the code is in it", {
  papers <- data.frame(
    status = c("ok", "ok", "ok", "ok", "out_of_scope"),
    code_public = c(TRUE, TRUE, FALSE, NA, TRUE),
    software_urls = c("https://github.com/a/b|https://cran.r-project.org/package=x", NA,
                      "https://github.com/private/repo", NA, "https://github.com/c/d"),
    link_abstract = paste0("https://arxiv.org/abs/2501.0000", 1:5), stringsAsFactors = FALSE)
  link <- code_link(papers)
  expect_equal(link$href, c("https://github.com/a/b", "https://arxiv.org/abs/2501.00002", "", "", ""))
  expect_equal(link$kind, c("code", "paper", "", "", ""))
  papers$software_urls[1] <- "javascript:alert(1)|see the appendix"
  expect_equal(code_link(papers)$kind[1], "paper")                # nothing but a web address becomes a link
})

test_that("the author box has no choices, and no error, when no paper is selected", {
  expect_length(author_choices(author_counts(fixture_papers()[0, ])), 0L)
  some <- author_choices(data.frame(author = c("A B", "C D"), n = c(2L, 1L), stringsAsFactors = FALSE))
  expect_equal(some, c("A B (2)" = "A B", "C D (1)" = "C D"))
})
