# Token usage and cost.
#
# ellmer's built-in price table does not know every model, so prices are kept
# here. USD per one million tokens, standard tier. Requests above
# LONG_CONTEXT_TOKENS input tokens are billed at the long-context rates.
# Source: https://developers.openai.com/api/docs/pricing (checked 2026-10-08).

LONG_CONTEXT_TOKENS <- 272000

PRICES <- list(
  "gpt-6-luna"  = list(input = 0.10, cached = 0.01, output = 0.50,
                       long_input = 0.20, long_cached = 0.02, long_output = 0.75),
  "gpt-6.1-sol" = list(input = 2.00, cached = 0.10, output = 10.00,
                       long_input = 4.00, long_cached = 0.20, long_output = 15.00),
  # TypeSafe decision model: input only, output free.
  # Source: https://docs.typesafe.ai/models (checked 2026-10-08).
  "jev-latest"  = list(input = 0.042, cached = 0.042, output = 0,
                       long_input = 0.042, long_cached = 0.042, long_output = 0)
)

# input_tokens counts all input, of which cached_tokens were read from cache.
compute_cost <- function(model, input_tokens, cached_tokens = 0, output_tokens = 0) {
  price <- PRICES[[model]]
  if (is.null(price)) {
    warning("No price for model '", model, "'; cost not computed.")
    return(NA_real_)
  }
  long <- input_tokens > LONG_CONTEXT_TOKENS
  p_in <- if (long) price$long_input else price$input
  p_cached <- if (long) price$long_cached else price$cached
  p_out <- if (long) price$long_output else price$output
  fresh <- max(input_tokens - cached_tokens, 0)
  (fresh * p_in + cached_tokens * p_cached + output_tokens * p_out) / 1e6
}

empty_usage <- function() {
  list(input_tokens = 0, cached_input_tokens = 0, output_tokens = 0, cost_usd = 0, n_calls = 0L)
}

add_usage <- function(total, call) {
  if (is.null(call)) return(total)
  list(
    input_tokens = total$input_tokens + call$input_tokens,
    cached_input_tokens = total$cached_input_tokens + call$cached_input_tokens,
    output_tokens = total$output_tokens + call$output_tokens,
    cost_usd = total$cost_usd + call$cost_usd,
    n_calls = total$n_calls + 1L
  )
}

# One row per model call, appended to a CSV log.
append_usage_log <- function(path, paper_id, track, stage, model, usage, status,
                             at = Sys.time()) {
  row <- data.frame(
    at = format(at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    paper_id = paper_id, track = track, stage = stage, model = model,
    input_tokens = usage$input_tokens, cached_input_tokens = usage$cached_input_tokens,
    output_tokens = usage$output_tokens, cost_usd = usage$cost_usd, status = status,
    stringsAsFactors = FALSE
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(row, path, sep = ",", row.names = FALSE, qmethod = "double",
                     col.names = !file.exists(path), append = file.exists(path))
  invisible(row)
}
