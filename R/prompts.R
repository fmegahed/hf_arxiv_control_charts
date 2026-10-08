# Prompts for the three extraction stages.
#
# Written for a small model: one task per call, short numbered rules, and the
# decision rules for each field kept next to that field in the schema
# (config/factsheet_spec.json), not here.
#
# The classify and narrate stages share one system prompt and send the PDF
# before the task text, so the paper is an identical prefix in both calls.

PAPER_SYSTEM_PROMPT <- paste(
  "You read one research paper and fill in structured fields about it for a literature",
  "database used by quality engineers.",
  "The paper comes first. The task comes after the paper. Follow the task exactly.",
  "Use only what is in the paper. Never leave a field empty.",
  sep = "\n"
)

SCREEN_SYSTEM_PROMPT <- paste(
  "You decide whether a paper belongs in a literature database on one topic.",
  "You see only the title, the abstract and the arXiv categories.",
  "Choose out_of_scope only when an exclusion clearly applies.",
  "If you are unsure, choose borderline.",
  sep = "\n"
)

screen_user_prompt <- function(spec, track, title, abstract, categories = NA_character_) {
  info <- spec_track(spec, track)
  paste0(
    "Topic of the database: ", info$topic_name, ".\n",
    "Include a paper when: ", info$scope$include, "\n",
    "Exclude a paper when: ", info$scope$exclude, "\n",
    "Example to include: ", info$scope$example_in, "\n",
    "Example to exclude: ", info$scope$example_out, "\n\n",
    "Title: ", title, "\n",
    "arXiv categories: ", if (is.na(categories)) "not given" else gsub("|", ", ", categories, fixed = TRUE), "\n",
    "Abstract: ", if (is.na(abstract)) "not available" else abstract
  )
}

classify_user_prompt <- function(spec, track) {
  info <- spec_track(spec, track)
  max_additional <- spec$limits$max_additional_labels
  paste0(
    "Task: label this paper for a database on ", info$topic_name, ".\n",
    "Rules:\n",
    "1. Label only what THIS paper proposes or evaluates with its own results (a derivation, a ",
    "simulation or a data analysis). Do not label methods that appear only in the introduction, ",
    "in the literature review, or as a competitor in a comparison.\n",
    "2. Where a field has an evidence field before it, fill the evidence first, then choose the ",
    "label that the evidence supports.\n",
    "3. A primary label is the one the title and abstract are about. Add at most ", max_additional,
    " additional labels, and only if rule 1 holds for each. When in doubt, leave it out.\n",
    "4. Choose '", NONE_OF_LISTED, "' only when no listed option fits, and then name what the paper ",
    "uses in the matching other-term field.\n",
    "5. Use the exact option text. Use the 'None' or 'Not applicable' option when nothing applies."
  )
}

# One line per classified field, for the narrate stage.
labels_as_text <- function(labels, spec, track) {
  lines <- character()
  for (field in spec_fields(spec, track)) {
    if (!field$kind %in% c("single", "primary_additional", "multi")) next
    value <- labels[[field$name]]
    if (is.null(value) || is.na(value)) next
    lines <- c(lines, paste0(field$label, ": ", gsub(LIST_SEP, "; ", value, fixed = TRUE)))
  }
  paste(lines, collapse = "\n")
}

narrate_user_prompt <- function(spec, track, labels_text = "") {
  info <- spec_track(spec, track)
  paste0(
    "Task: describe this paper for a database on ", info$topic_name, ".\n",
    "Rules:\n",
    "1. Write for a reader who has not read the paper. Use plain sentences.\n",
    "2. Fill the glossary first. The first time an acronym or symbol appears in a field, spell it ",
    "out, for example 'average run length (ARL)'.\n",
    "3. Do not use LaTeX, backslashes or dollar signs anywhere except the latex part of an ",
    "equation. Write Greek letters by name, for example 'lambda'. Write money as 'USD 1,200'.\n",
    "4. Report only numbers that appear in the paper.\n",
    "5. Keep what the authors say (stated fields) separate from what you infer (unstated fields).",
    if (nzchar(labels_text)) paste0("\n\nThe paper was labelled as follows:\n", labels_text) else ""
  )
}
