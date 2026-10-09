# Screen the sampled new papers for each candidate SPM term with the app's own
# scope screen (title, abstract, categories), and report the in-scope share per
# term. Run from the app directory after 11_spm_term_marginals.py:
#   Rscript analysis/query_design/12_spm_term_screen.R

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
load_dotenv()

spec <- spec_load()
cache <- file.path("analysis", "query_design", "cache")
input <- jsonlite::read_json(file.path(cache, "spm_term_new_papers.json"))
papers <- input$papers
model <- spec$models$extraction

prompts <- lapply(papers, function(p) {
  screen_user_prompt(spec, "spc", p$title, p$abstract, p$categories)
})
chat <- ellmer::chat_openai(model = model, system_prompt = SCREEN_SYSTEM_PROMPT,
                            credentials = get_openai_api_key, echo = "none")
decisions <- ellmer::parallel_chat_structured(chat, unname(prompts), type = build_screen_type(spec),
                                              max_active = 6, on_error = "continue")
decisions$id <- names(papers)
decisions$title <- vapply(papers, function(p) p$title, character(1))
decisions$primary <- vapply(papers, function(p) p$primary, character(1))
utils::write.csv(decisions, file.path(cache, "spm_term_screen.csv"), row.names = FALSE)

kept <- stats::setNames(decisions$scope_decision, decisions$id)
cat(sprintf("%-24s %7s %5s %6s %9s %10s %12s\n", "term", "matches", "new", "gold+", "sampled", "in scope", "borderline"))
for (term in names(input$terms)) {
  info <- input$terms[[term]]
  d <- kept[unlist(info$sample)]
  cat(sprintf("%-24s %7d %5d %6d %9d %10d %12d\n", term, info$matches, info$new, length(info$gold_new),
              length(d), sum(d %in% "in_scope"), sum(d %in% "borderline")))
}
