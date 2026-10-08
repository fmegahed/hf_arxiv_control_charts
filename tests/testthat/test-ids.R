test_that("base id and version are parsed for new and old style ids", {
  expect_identical(arxiv_base_id(c("2403.01234v2", "math/0406049v1", "2403.01234")),
                   c("2403.01234", "math/0406049", "2403.01234"))
  expect_identical(arxiv_version(c("2403.01234v2", "math/0406049v1", "2403.01234", "1201.3935v12")),
                   c(2L, 1L, NA, 12L))
})

test_that("an id whose base contains the letter v is not truncated", {
  expect_identical(arxiv_base_id("solv-int/9901001v3"), "solv-int/9901001")
  expect_identical(arxiv_version("solv-int/9901001v3"), 3L)
})

test_that("keep_latest_version keeps the numerically highest version per paper", {
  df <- data.frame(id = c("1.1v9", "1.1v10", "2.2v1", "3.3", "3.3v1", "1.1v2"),
                   x = 1:6, stringsAsFactors = FALSE)
  out <- keep_latest_version(df)
  expect_identical(out$id, c("1.1v10", "2.2v1", "3.3v1"))
  expect_identical(out$x, c(2L, 3L, 5L))
})

test_that("keep_latest_version drops exact duplicates and handles empty input", {
  df <- data.frame(id = c("1.1v2", "1.1v2"), stringsAsFactors = FALSE)
  expect_identical(nrow(keep_latest_version(df)), 1L)
  expect_identical(nrow(keep_latest_version(df[0, , drop = FALSE])), 0L)
})
