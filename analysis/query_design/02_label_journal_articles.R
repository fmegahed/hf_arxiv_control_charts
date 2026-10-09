# Step 2. Label every journal article (title + best available abstract + OpenAlex keywords/topics) with
# gpt-6-luna and rubric v2 (00_rubric_v2.R). Resumable; stops hard if the API refuses most of a chunk.
# Input:  cache/journal_works.csv, cache/journal_text.csv (from step 03)
# Output: cache/journal_labels_v2.csv (key = openalex_id), cache/journal_labelled.csv (joined)
# Run:    Rscript --vanilla 02_label_journal_articles.R [n_test]
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))), "00_common.R"))
source(file.path(QD, "00_rubric_v2.R"))

args <- commandArgs(trailingOnly = TRUE)
works <- read_csv(file.path(CACHE, "journal_works.csv"), col_types = cols(.default = "c")) %>%
  left_join(read_csv(file.path(CACHE, "journal_text.csv"), col_types = cols(.default = "c")), by = "openalex_id")
df <- works %>% transmute(
  key = openalex_id, title, abstract = coalesce(abstract_best, ""),
  # OpenAlex topics are added only when there is no abstract (mostly Elsevier articles)
  keywords = str_replace_all(paste0(coalesce(keywords, ""),
                                    if_else(nchar(coalesce(abstract_best, "")) < 100, paste0("|", coalesce(topics, "")), "")),
                             "\\|", "; "))
if (length(args) >= 1) { set.seed(1); df <- head(df[sample(nrow(df)), ], as.integer(args[[1]])) }

lab <- label_items_v2(df, file.path(CACHE, "journal_labels_v2.csv"), tag = "journal_v2")
out <- works %>% inner_join(lab %>% distinct(key, .keep_all = TRUE), by = c("openalex_id" = "key")) %>%
  mutate(across(c(reliability, doe, spm, change_theory), as.logical))
write_csv(out, file.path(CACHE, "journal_labelled.csv"))
cat("labelled:", nrow(out), "of", nrow(works), "\n")
print(out %>% group_by(journal) %>% summarise(n = n(), reliability = sum(reliability), doe = sum(doe), spm = sum(spm),
                                             change_theory = sum(change_theory)))
