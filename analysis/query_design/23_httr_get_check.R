# Step 23. Send every follow-up candidate the way aRxiv 0.18 does: httr::GET on the http endpoint with the
# query passed as a URL parameter. Confirms that the app's R client can deliver the long queries.
# (aRxiv itself is not installed in this R library, so its request is reproduced, not called.)
# Input:  cache/v2/score_{track}.csv (column query)
# Output: cache/httr_get_check.csv
# Run:    Rscript --vanilla 23_httr_get_check.R
suppressPackageStartupMessages({library(readr); library(dplyr); library(purrr)})
qd <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))
if (!requireNamespace("httr", quietly = TRUE)) stop("package httr is not installed")
files <- list.files(file.path(qd, "cache", "v2"), pattern = "^score_.*\\.csv$", full.names = TRUE)
cands <- map_dfr(files, ~ read_csv(.x, col_types = cols(.default = "c")) %>% select(candidate, query))
out <- map_dfr(seq_len(nrow(cands)), function(i) {
  Sys.sleep(4)
  r <- tryCatch(httr::GET("http://export.arxiv.org/api/query",
                          query = list(search_query = cands$query[i], start = 0, max_results = 1),
                          httr::timeout(120)), error = function(e) NULL)
  if (is.null(r)) return(tibble(candidate = cands$candidate[i], status = NA, total_results = NA, url_chars = NA))
  txt <- httr::content(r, as = "text", encoding = "UTF-8")
  tibble(candidate = cands$candidate[i], status = httr::status_code(r),
         total_results = sub(".*<opensearch:totalResults[^>]*>([0-9]+)<.*", "\\1", txt),
         url_chars = nchar(r$url))
})
write_csv(out, file.path(qd, "cache", "httr_get_check.csv"))
print(as.data.frame(out))
