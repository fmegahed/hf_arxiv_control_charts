# Step 25. Run the app's own SPM scope screen (title, abstract, categories) on
# the sampled new papers and the hand-checked papers, and compare its decision
# with the hand label and with the rubric's change-detection-theory flag.
# Used to check a change to the SPM scope rule in config/factsheet_spec.json.
# Run from the app directory:
#   Rscript analysis/query_design/25_spm_screen_rule.R --tag before

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
load_dotenv()

spec <- spec_load()
args <- parse_cli_args(commandArgs(trailingOnly = TRUE), defaults = list(tag = "run"))
qd <- file.path("analysis", "query_design")
read <- function(...) utils::read.csv(file.path(qd, ...), stringsAsFactors = FALSE, colClasses = "character")

sampled <- read("cache", "v2", "spc_sample.csv")
checked <- read("cache", "v2", "handcheck_arxiv_v2.csv")
checked <- checked[checked$track == "spc", ]
checked$categories <- ""
papers <- rbind(sampled[c("key", "title", "abstract", "categories")],
                checked[c("key", "title", "abstract", "categories")])
papers <- papers[!duplicated(papers$key), ]

prompts <- lapply(seq_len(nrow(papers)), function(i) {
  screen_user_prompt(spec, "spc", papers$title[i], papers$abstract[i], papers$categories[i])
})
chat <- ellmer::chat_openai(model = spec$models$extraction, system_prompt = SCREEN_SYSTEM_PROMPT,
                            credentials = get_openai_api_key, echo = "none")
decisions <- ellmer::parallel_chat_structured(chat, prompts, type = build_screen_type(spec),
                                              max_active = 6, on_error = "continue")
out <- cbind(papers[c("key", "title")], decisions)

rubric <- read("cache", "arxiv_luna_labels_v2.csv")
hand <- read("hand_labels", "handcheck_v2_arxiv.csv")
out$rubric_spm <- rubric$spm[match(out$key, rubric$key)]
out$rubric_theory <- rubric$change_theory[match(out$key, rubric$key)]
out$hand <- hand$hand[match(out$key, hand$key)]
utils::write.csv(out, file.path(qd, "cache", paste0("spm_screen_rule_", args$tag, ".csv")), row.names = FALSE)

kept <- out$scope_decision %in% c("in_scope", "borderline")
cat("papers:", nrow(out), " kept by the screen:", sum(kept), "\n")
cat("\nHand label (1 = SPM by a strict reading) against the screen:\n")
print(table(hand = out$hand, kept = kept, useNA = "no"))
cat("\nRubric labels against the screen (rows: SPM / theory flag):\n")
print(table(rubric = paste0("spm=", out$rubric_spm, " theory=", out$rubric_theory), kept = kept))
