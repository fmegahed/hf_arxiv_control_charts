# Step 15. Label sampled arXiv papers with gpt-6-luna and rubric v2 (00_rubric_v2.R).
# Supersedes 08_label_new_samples.R. Resumable; stops hard if the API refuses most of a chunk.
# Input:  every cache/terms/*_sample.csv and cache/v2/*_sample.csv that exists
#         (columns key = "<track>:<arxiv id>", title, abstract)
# Output: cache/arxiv_luna_labels_v2.csv
# Run:    Rscript --vanilla 15_label_arxiv_samples.R
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))), "00_common.R"))
source(file.path(QD, "00_rubric_v2.R"))

files <- c(list.files(file.path(CACHE, "terms"), pattern = "_sample\\.csv$", full.names = TRUE),
           list.files(file.path(CACHE, "v2"), pattern = "_sample\\.csv$", full.names = TRUE))
df <- map_dfr(files, function(f) read_csv(f, col_types = cols(.default = "c")) %>%
                transmute(key, title, abstract = coalesce(abstract, ""), keywords = "")) %>%
  distinct(key, .keep_all = TRUE)
cat("papers in samples:", nrow(df), "\n")
lab <- label_items_v2(df, file.path(CACHE, "arxiv_luna_labels_v2.csv"), tag = "arxiv_v2")
cat("labelled so far:", nrow(lab), "of", nrow(df), "\n")
