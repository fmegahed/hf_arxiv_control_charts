BS <- "\b"; FF <- "\f"; CR <- "\r"; TAB <- "\t"; NL <- "\n"

test_that("TeX commands damaged by JSON escapes are restored in equations", {
  expect_identical(repair_control_chars(paste0(BS, "ig( x ", CR, "ho"), latex = TRUE)$text,
                   "\\big( x \\rho")
  expect_identical(repair_control_chars(paste0(FF, "rac{a}{b}"), latex = TRUE)$text, "\\frac{a}{b}")
  expect_identical(repair_control_chars(paste0(TAB, "heta_0"), latex = TRUE)$text, "\\theta_0")
  expect_identical(repair_control_chars(paste0(BS, "eta_1 + ", BS, "ar{X}"), latex = TRUE)$text,
                   "\\beta_1 + \\bar{X}")
  expect_identical(repair_control_chars(paste0("a ", NL, "eq b"), latex = TRUE)$text, "a \\neq b")
  expect_true(repair_control_chars(paste0(BS, "ig("), latex = TRUE)$repaired)
})

test_that("the longest matching command wins", {
  expect_identical(repair_control_chars(paste0(BS, "igg("), latex = TRUE)$text, "\\bigg(")
  expect_identical(repair_control_chars(paste0(CR, "ightarrow"), latex = TRUE)$text, "\\rightarrow")
})

test_that("in narrative text tabs and newlines are whitespace, not TeX", {
  expect_identical(repair_control_chars(paste0("a", TAB, "heta"), latex = FALSE)$text, "a heta")
  expect_identical(repair_control_chars(paste0("line one", NL, "eq two"), latex = FALSE)$text,
                   paste0("line one", NL, "eq two"))
  expect_identical(repair_control_chars(paste0("a", CR, NL, "b"), latex = FALSE)$text,
                   paste0("a", NL, "b"))
})

test_that("other control characters are removed and clean text is untouched", {
  dirty <- paste0("a", rawToChar(as.raw(0x03)), "b", rawToChar(as.raw(0x0e)), "c",
                  rawToChar(as.raw(0x1d)), "d")
  out <- repair_control_chars(dirty)
  expect_identical(out$text, "abcd")
  expect_true(out$repaired)
  expect_false(repair_control_chars("plain text")$repaired)
  expect_identical(repair_control_chars(NA_character_)$text, NA_character_)
})

test_that("sanitize_latex strips outer delimiters and accepts valid equations", {
  for (wrapped in c("$\\bar{X}_t$", "$$\\bar{X}_t$$", "\\(\\bar{X}_t\\)", "\\[\\bar{X}_t\\]", " \\bar{X}_t ")) {
    out <- sanitize_latex(wrapped)
    expect_identical(out$latex, "\\bar{X}_t")
    expect_true(out$ok)
  }
  expect_true(sanitize_latex("\\left( \\frac{a}{b} \\right) \\{1,2\\}")$ok)
  expect_true(sanitize_latex("\\begin{cases} a \\\\ b \\end{cases}")$ok)
})

test_that("sanitize_latex flags equations that cannot be typeset safely", {
  expect_match(sanitize_latex("\\frac{a}{b")$problems, "unbalanced braces")
  expect_match(sanitize_latex("\\left( x")$problems, "left/")
  expect_match(sanitize_latex("\\begin{cases} a")$problems, "begin/")
  expect_match(sanitize_latex("\\href{http://x}{y}")$problems, "disallowed command")
  expect_match(sanitize_latex("a $ b")$problems, "dollar sign")
  expect_false(sanitize_latex("   ")$ok)
  expect_false(sanitize_latex(NA_character_)$ok)
  expect_false(sanitize_latex(NULL)$ok)
})

test_that("a less-than sign cannot open an HTML tag", {
  expect_identical(sanitize_latex("I(X_i<Y_j)")$latex, "I(X_i< Y_j)")
  expect_identical(sanitize_narrative("with |rho|<1 and <script>x")$text,
                   "with |rho|<1 and < script>x")
  expect_identical(sanitize_latex("a < b")$latex, "a < b")
})

test_that("sanitize_narrative converts math, currency and stray dollars", {
  expect_identical(sanitize_narrative("costs $5 and $10 per unit")$text,
                   "costs USD 5 and USD 10 per unit")
  expect_identical(sanitize_narrative("shift $\\delta = 1$ detected")$text,
                   "shift \\(\\delta = 1\\) detected")
  expect_identical(sanitize_narrative("a $3\\sigma$ limit")$text, "a \\(3\\sigma\\) limit")
  expect_identical(sanitize_narrative("ARL $= 370")$text, "ARL = 370")
  expect_identical(sanitize_narrative("$$x^2$$ holds")$text, "\\[x^2\\] holds")
  expect_identical(sanitize_narrative("saves $1,200")$text, "saves USD 1,200")
  expect_identical(sanitize_narrative("$n$ runs cost $40 each")$text, "\\(n\\) runs cost USD 40 each")
  expect_identical(sanitize_narrative("ends with $")$text, "ends with")
  expect_identical(sanitize_narrative(NA_character_)$text, NA_character_)
})

test_that("math delimiters without a partner are removed, matched ones kept", {
  expect_identical(sanitize_narrative("ok \\(a\\) then \\(b never closed.")$text,
                   "ok \\(a\\) then b never closed.")
  expect_identical(sanitize_narrative("stray b\\) and \\(c\\)")$text, "stray b and \\(c\\)")
  expect_identical(sanitize_narrative("\\(a \\(b\\) c")$text, "a \\(b\\) c")
  expect_identical(sanitize_narrative("\\[x\\] and \\[y")$text, "\\[x\\] and y")
  expect_identical(drop_unmatched_delims("no math", "\\(", "\\)"), "no math")
})

test_that("sanitized text meets the storage contract and sanitizing is idempotent", {
  samples <- c(
    "costs $5 and $10 per unit", "shift $\\delta = 1$ detected", "ARL $= 370", "$$x^2$$",
    "already \\(x\\) clean \\[y\\]", "$a$ and $b$ and $c", paste0("bad", rawToChar(as.raw(0x03)), " $x_1$"),
    "plain sentence.", "$", "$$", "a $ b $ c $ d", "USD 5 is $5"
  )
  for (s in samples) {
    once <- sanitize_narrative(s)$text
    check <- validate_stored_text(once)
    expect_true(check$ok, info = paste(s, "->", once, ":", paste(check$problems, collapse = ",")))
    expect_identical(sanitize_narrative(once)$text, once, info = s)
  }
})

test_that("validate_stored_text reports each kind of violation", {
  expect_match(validate_stored_text("a $ b")$problems, "bare dollar")
  expect_match(validate_stored_text(paste0("a", rawToChar(as.raw(0x08))))$problems, "control")
  expect_match(validate_stored_text("\\(x")$problems, "inline")
  expect_match(validate_stored_text("\\[x")$problems, "display")
  expect_true(validate_stored_text(NA_character_)$ok)
  expect_true(validate_stored_text(paste0("two", NL, "lines"))$ok)
})

test_that("undefined acronyms are those with neither a glossary entry nor an expansion", {
  text <- "The ARL of the EWMA chart beats CUSUM; the average run length (ARL0) is 370 in Phase II."
  expect_setequal(find_undefined_acronyms(text, glossary_terms = "ARL"), c("EWMA", "CUSUM"))
  expect_identical(find_undefined_acronyms("Costs USD 5 in Phase II"), character(0))
  expect_identical(find_undefined_acronyms("math \\(ARL_0\\) only"), character(0))
  expect_identical(find_undefined_acronyms(NA_character_), character(0))
})

test_that("equations render with display delimiters and invalid ones are shown as code", {
  eqs <- list(
    list(name = "EWMA statistic", latex = "Z_t = \\lambda X_t + (1-\\lambda) Z_{t-1}",
         explanation = "Z_t is the chart statistic and $\\lambda$ the smoothing constant."),
    list(name = "Broken", latex = "\\frac{a}{b", explanation = "Not valid.")
  )
  out <- render_equations(eqs)
  expect_match(out, "EWMA statistic: \\[Z_t = \\lambda X_t + (1-\\lambda) Z_{t-1}\\]", fixed = TRUE)
  expect_match(out, "\\(\\lambda\\) the smoothing constant", fixed = TRUE)
  expect_match(out, "Broken: `\\frac{a}{b`", fixed = TRUE)
  expect_true(validate_stored_text(out)$ok)
  expect_identical(render_equations(list()), NA_character_)
})

test_that("every narrative field in the frozen v1 factsheets sanitizes to the contract", {
  frozen <- app_path("data", "frozen", "v1")
  skip_if_not(dir.exists(frozen), "v1 snapshot not present")
  fields <- c("summary", "key_results", "key_equations", "limitations_stated",
              "limitations_unstated", "future_work_stated", "future_work_unstated")
  for (track in c("spc", "exp_design", "reliability")) {
    df <- read_factsheet(file.path(frozen, paste0(track, "_factsheet.csv")))
    for (field in fields) {
      values <- stats::na.omit(df[[field]])
      cleaned <- vapply(values, function(v) sanitize_narrative(v)$text, character(1))
      bad <- !vapply(cleaned, function(v) validate_stored_text(v)$ok, logical(1))
      expect_identical(sum(bad), 0L, info = paste(track, field))
      again <- vapply(cleaned, function(v) sanitize_narrative(v)$text, character(1))
      expect_identical(unname(again), unname(cleaned), info = paste(track, field, "idempotence"))
    }
  }
})
