# Tests run against the modules in R/, with paths resolved from the app root.
APP_ROOT <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")

app_path <- function(...) file.path(APP_ROOT, ...)

for (f in sort(list.files(app_path("R"), pattern = "\\.R$", full.names = TRUE))) {
  source(f, local = FALSE)
}
