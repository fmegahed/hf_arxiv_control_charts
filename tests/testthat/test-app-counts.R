in_scope <- function(track = NULL) apply_filters(fixture_papers(), new_filter_state(track), TEST_SPEC, TEST_SETTINGS)

test_that("papers per year fills empty years with zero", {
  counts <- papers_per_year(in_scope("spc"))
  expect_equal(counts$year, 2020:2025)
  expect_equal(counts$n, c(1L, 0L, 0L, 0L, 1L, 1L))
  expect_equal(nrow(papers_per_year(in_scope("spc")[0, ])), 0L)
  both <- papers_per_year(in_scope())
  expect_equal(sum(both$n), 7L)
  expect_setequal(unique(both$track), TRACK_IDS)
})

test_that("composition counts whole elements and reports its denominator", {
  papers <- in_scope("spc")
  primary <- composition(papers, "chart_statistic_primary")
  expect_equal(sum(primary$n), 3L)
  expect_equal(unique(primary$total), 3L)
  expect_equal(primary$share, primary$n / 3)
  any_label <- composition(papers, "chart_statistic")
  expect_equal(any_label$n[any_label$value == "CUSUM"], 1L)
  expect_equal(any_label$n[any_label$value == "EWMA"], 1L)        # MEWMA is not counted as EWMA
  approach <- composition(papers, "chart_approach", exclude = "None")
  expect_false("None" %in% approach$value)
  expect_equal(approach$n[approach$value == "Bayesian"], 2L)
  family <- composition(papers, "chart_family")
  expect_equal(family$label[family$value == NONE_OF_LISTED], OTHER_LABEL)
  expect_equal(nrow(composition(papers, "no_such_column")), 0L)
  logical_field <- composition(papers, "assumes_normality")
  expect_equal(sum(logical_field$n), 2L)
})

test_that("the gap map counts papers that carry both labels", {
  gap <- gap_map(in_scope("spc"), "chart_approach", "chart_statistic", exclude = "None")
  cell <- function(a, b) gap$cells$n[gap$cells$a == a & gap$cells$b == b]
  expect_equal(gap$papers, 2L)
  expect_equal(cell("Bayesian", "EWMA"), 1L)
  expect_equal(cell("Bayesian", "MEWMA"), 1L)
  expect_equal(cell("Nonparametric (distribution-free)", "CUSUM"), 1L)
  expect_equal(cell("Nonparametric (distribution-free)", "MEWMA"), 0L)
  expect_equal(nrow(gap$cells), length(gap$a_values) * length(gap$b_values))
  expect_equal(gap$a_values[1], "Bayesian")                        # most frequent first
  expect_equal(gap_map(in_scope("spc")[0, ], "chart_approach", "chart_statistic")$papers, 0L)
  expect_s3_class(chart_gap_map(gap), "plotly")
})

test_that("a click on a heat-map cell is read as the two labels of that cell", {
  gap <- gap_map(in_scope("spc"), "chart_approach", "chart_statistic", exclude = "None")
  # plotly reports zero-based (row, column)
  expect_equal(gap_map_click(list(pointNumber = list(0L, 0L)), gap), c(gap$a_values[1], gap$b_values[1]))
  expect_equal(gap_map_click(list(pointNumber = list(1L, 2L)), gap), c(gap$a_values[2], gap$b_values[3]))
  expect_null(gap_map_click(list(pointNumber = list(9L, 0L)), gap))
  expect_null(gap_map_click(list(pointNumber = 3L), gap))
  expect_null(gap_map_click(list(), gap))
  expect_null(gap_map_click(list(pointNumber = list(0L, 1L)), gap, max_values = 1L))
})

rising_fixture <- function() {
  data.frame(
    year = c(rep(2020L, 10), rep(2025L, 10)),
    tag = c(rep("A", 2), rep("B", 6), "A|C", "B",          # earlier: A 3, B 7, C 1
            rep("A", 7), rep("B", 2), "C"),                # recent:  A 7, B 2, C 1
    stringsAsFactors = FALSE)
}

test_that("rising tags compare shares, keep the counts and suppress small tags", {
  rising <- rising_tags(rising_fixture(), c(Tag = "tag"), latest_year = 2025L, recent_years = 3L, min_papers = 5L)
  expect_equal(rising$value, c("A", "B"))                 # C is on 2 papers only: suppressed
  a <- rising[rising$value == "A", ]
  expect_equal(c(a$n_recent, a$n_earlier, a$papers_recent, a$papers_earlier), c(7L, 3L, 10L, 10L))
  expect_equal(a$change, 0.4)
  expect_equal(rising$change[rising$value == "B"], -0.5)
  expect_equal(rising$field, c("Tag", "Tag"))
  with_c <- rising_tags(rising_fixture(), c(Tag = "tag"), 2025L, min_papers = 1L)
  expect_true("C" %in% with_c$value)
  expect_match(rising_label(a), "70% recently (7 of 10) vs 30% earlier (3 of 10)", fixed = TRUE)
})

test_that("rising tags need papers on both sides of the cut", {
  only_recent <- rising_fixture()[11:20, ]
  expect_equal(nrow(rising_tags(only_recent, c(Tag = "tag"), 2025L)), 0L)
  expect_equal(nrow(rising_tags(rising_fixture(), c(Tag = "missing_column"), 2025L)), 0L)
})

test_that("the share of public code leaves papers with unknown status out", {
  papers <- data.frame(year = c(2024L, 2024L, 2024L, 2025L, 2025L),
                       code_public = c(TRUE, FALSE, NA, TRUE, TRUE))
  share <- code_share_by_year(papers)
  expect_equal(share$papers, c(2L, 2L))
  expect_equal(share$public, c(1L, 2L))
  expect_equal(share$share, c(0.5, 1))
  expect_equal(nrow(code_share_by_year(papers[0, ])), 0L)
})

test_that("each paper falls in exactly one data group", {
  papers <- in_scope()
  split <- data_use_split(papers, TEST_SETTINGS, TEST_SPEC)
  count <- function(group) split$n[split$group == group]
  expect_equal(sum(split$n), nrow(papers))
  expect_equal(count("Real and simulated data"), 1L)
  expect_equal(count("Real data only"), 2L)            # field data, and a public benchmark
  expect_equal(count("Simulated data only"), 3L)
  expect_equal(count("No data (theory only)"), 1L)
  expect_equal(sum(split$n[split$uses_real]), sum(filter_mask(fixture_papers(), utils::modifyList(
    new_filter_state(), list(real_data = TRUE)), TEST_SPEC, TEST_SETTINGS)))
})

test_that("trends count a label per year with the year's total", {
  trend <- tag_trend(in_scope("spc"), "chart_approach", "Bayesian")
  expect_equal(trend$n[trend$year %in% c(2024, 2025)], c(1L, 1L))
  expect_equal(trend$papers[trend$year == 2021], 0L)
  expect_true(is.na(trend$share[trend$year == 2021]))
  expect_equal(trend$share[trend$year == 2025], 1)
})

test_that("author counts and team sizes follow the selection", {
  papers <- in_scope("spc")
  counts <- author_counts(papers)
  expect_equal(counts$n[counts$author == "Ann Author"], 3L)
  expect_equal(counts$n[counts$author == "Di Fourth"], 1L)
  sizes <- team_sizes(papers)
  expect_equal(sizes$n[sizes$size == 4L], 1L)
  expect_equal(sum(sizes$n), 3L)
  expect_equal(nrow(author_counts(papers[0, ])), 0L)
})

test_that("charts are built from the counts without error", {
  papers <- in_scope()
  expect_s3_class(chart_per_year(papers_per_year(papers), TEST_SPEC, ALL_TRACKS_ACCENT), "plotly")
  expect_s3_class(chart_bars(composition(papers, "application_domain_primary"), "#1b9e77"), "plotly")
  expect_s3_class(chart_code_share(code_share_by_year(papers), "#1b9e77"), "plotly")
  expect_s3_class(chart_trends(tag_trend(papers, "data_source", "Simulated data")), "plotly")
  rising <- rising_tags(rising_fixture(), c(Tag = "tag"), 2025L)
  expect_s3_class(chart_rising(rising, "#1b9e77"), "plotly")
  expect_equal(toupper(darken("#ffffff", 0.5)), "#7F7F7F")
})

test_that("chart fields come from the spec for the scope", {
  expect_setequal(unname(chartable_fields(TEST_SPEC)),
                  c("paper_type", "application_domain", "data_source", "software_platform", "code_availability"))
  expect_true(all(c("chart_family", "chart_statistic", "phase") %in% chartable_fields(TEST_SPEC, "spc")))
  expect_false("assumes_normality" %in% chartable_fields(TEST_SPEC, "spc"))
  expect_equal(default_chart_fields(TEST_SPEC, TEST_SETTINGS, "exp_design"), c("design_type", "design_objective"))
})
