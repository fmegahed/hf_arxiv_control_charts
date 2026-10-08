# The app must take model names and label values from the specification and
# the settings, never from text typed into the code. This test fails when a
# model id or a value of a specification field appears as a string literal.

string_literals <- function(path) {
  parsed <- utils::getParseData(parse(path, keep.source = TRUE))
  tokens <- parsed$text[parsed$token == "STR_CONST"]
  # Each token is a quoted string constant from this repository's own source;
  # evaluating it only resolves the escapes to give the string's value.
  vapply(tokens, function(token) tryCatch(eval(parse(text = token)), error = function(e) token),
         character(1), USE.NAMES = FALSE)
}

spec_enum_values <- function(spec) {
  values <- character(0)
  for (track in names(spec$tracks)) {
    for (field in spec_fields(spec, track)) {
      if (!is.null(field$values)) values <- c(values, field_values(field))
    }
  }
  unique(c(values, NONE_OF_LISTED, TRISTATE_VALUES, scope_category_values(spec)))
}

app_code_files <- function() {
  c(app_path("app.R"),
    list.files(app_path("R"), pattern = "^app_.*\\.R$", full.names = TRUE),
    app_path("R", "question.R"), app_path("R", "jev.R"))
}

# Each exception is one literal in one file, with the reason it is not a label.
LITERAL_EXCEPTIONS <- list(
  list(file = "app.R", literal = "R", reason = "name of the directory holding the R files")
)

is_exception <- function(file, literal) {
  any(vapply(LITERAL_EXCEPTIONS, function(e) identical(e$file, basename(file)) && identical(e$literal, literal),
             logical(1)))
}

test_that("the files under test exist and are parsed", {
  files <- app_code_files()
  expect_true(all(file.exists(files)))
  expect_gte(length(files), 14L)
  expect_true("R" %in% string_literals(app_path("app.R")))          # the scanner sees literals
})

test_that("no model id is written into the app code", {
  pattern <- "(gpt-|claude-|jev-)"
  for (file in c(app_code_files(), app_path("02_weekly_synthesis.r"))) {
    found <- grep(pattern, string_literals(file), value = TRUE)
    expect_identical(found, character(0), info = basename(file))
  }
})

test_that("no value of a specification field is written into the app code", {
  enums <- spec_enum_values(TEST_SPEC)
  expect_gt(length(enums), 150L)
  for (file in app_code_files()) {
    literals <- unique(string_literals(file))
    found <- literals[literals %in% enums]
    found <- found[!vapply(found, function(literal) is_exception(file, literal), logical(1))]
    expect_identical(found, character(0), info = basename(file))
  }
})

test_that("every exception is still needed", {
  for (e in LITERAL_EXCEPTIONS) {
    expect_true(e$literal %in% string_literals(app_path(e$file)), info = paste(e$file, e$literal))
    expect_true(nzchar(e$reason))
  }
})

test_that("the weekly digest script takes its tracks and model from the spec", {
  code <- paste(readLines(app_path("02_weekly_synthesis.r"), warn = FALSE), collapse = "\n")
  expect_match(code, "spec_load(", fixed = TRUE)
  expect_match(code, "spec$models$", fixed = TRUE)
  expect_false(grepl("Control Charts (SPC)", code, fixed = TRUE))
})

test_that("user-facing text in the app code has no em dashes", {
  for (file in c(app_code_files(), app_path("www", "qew.js"), app_path("www", "personalization.js"))) {
    text <- readLines(file, warn = FALSE, encoding = "UTF-8")
    expect_false(any(grepl("—", text, fixed = TRUE)), info = basename(file))
  }
})
