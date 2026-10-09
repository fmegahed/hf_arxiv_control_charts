# Step 8. Label the random samples of newly retrieved arXiv papers (up to 150 per candidate) with the
# same Luna rubric used for the journal articles. Resumable: already-labelled keys are skipped.
# Input:  cache/new_sample_{track}.csv (written by 07_score_candidates.py)
# Output: cache/new_sample_luna_labels.csv (key = "<track>:<arxiv id>")
# Run:    Rscript --vanilla 08_label_new_samples.R      then rerun 07 and 09 to fold the labels in.
# NOT RUN in the October 2026 analysis: the OpenAI account had no credit (HTTP 429).
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))), "00_common.R"))

tracks <- c("reliability", "exp_design", "spc")
df <- map_dfr(tracks, function(t) {
  f <- file.path(CACHE, paste0("new_sample_", t, ".csv"))
  if (!file.exists(f)) return(tibble())
  read_csv(f, col_types = cols(.default = "c")) %>%
    transmute(key = paste0(t, ":", id), title, abstract = coalesce(abstract, ""), keywords = "")
})
cat("papers to label (all tracks):", nrow(df), "\n")
lab <- label_items(df, file.path(CACHE, "new_sample_luna_labels.csv"), tag = "new_arxiv_sample")
cat("labelled so far:", nrow(lab), "of", nrow(df), "\n")
