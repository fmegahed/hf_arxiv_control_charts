# Second reader and tie-break for the single-answer labels.
#
# The first reader (a generative model) fills the whole factsheet from the PDF.
# A second, independent reader (a decision model that returns one option and a
# confidence per question) answers the single-answer labels from the paper's
# text. Where the two agree, the label stands. Where they disagree on a label
# the spec marks "arbitrate", a stronger model reads the PDF and decides.
#
# This file has no network code: questions, text preparation, comparison and
# resolution only.

SECOND_READER_GATE <- paste(
  "Judge only what THIS paper proposes or evaluates with its own results.",
  "Methods that appear only as background, in the literature review, or as a competitor do not count."
)

# Fields with exactly one answer: `single` fields and the primary of
# `primary_additional` fields.
single_answer_fields <- function(spec, track) {
  Filter(function(f) f$kind %in% c("single", "primary_additional"), spec_fields(spec, track))
}

# Column holding a field's single answer.
answer_column <- function(field) {
  if (field$kind == "primary_additional") paste0(field$name, "_primary") else field$name
}

arbitrated_fields <- function(spec, track) {
  fields <- Filter(function(f) field_flag(f, "arbitrate"), single_answer_fields(spec, track))
  vapply(fields, function(f) f$name, character(1))
}

# One choice question per single-answer field, built from the spec.
second_reader_questions <- function(spec, track) {
  fields <- single_answer_fields(spec, track)
  questions <- lapply(fields, function(field) {
    values <- field_choices(field)
    definitions <- field_definitions(field)[values]
    definitions[values == NONE_OF_LISTED] <- "No listed option fits this paper."
    criteria <- stats::setNames(lapply(definitions, function(d) if (is.na(d)) NULL else d), values)
    list(type = "choice", instructions = paste(field$question, SECOND_READER_GATE), criteria = criteria)
  })
  stats::setNames(questions, vapply(fields, function(f) f$name, character(1)))
}

# Paper text for a reader with a size limit: the reference list is removed
# and, if the text is still too long, the start and the closing part are kept.
prepare_paper_text <- function(pages, max_chars) {
  text <- paste(pages, collapse = "\n")
  text <- gsub("[ \t]+", " ", text)
  text <- gsub("\n{3,}", "\n\n", text)
  headings <- gregexpr("\n\\s*(References|REFERENCES|Bibliography|BIBLIOGRAPHY)\\s*\n", text)[[1]]
  if (headings[1] != -1L && utils::tail(headings, 1) > 0.5 * nchar(text)) {
    text <- substr(text, 1L, utils::tail(headings, 1))
  }
  if (nchar(text) <= max_chars) return(list(text = text, cut = FALSE))
  head_chars <- floor(0.72 * max_chars)
  tail_chars <- max_chars - head_chars
  list(text = paste0(substr(text, 1L, head_chars), "\n[...middle of the paper omitted...]\n",
                     substr(text, nchar(text) - tail_chars + 1L, nchar(text))),
       cut = TRUE)
}

# Answers from the decision model -> named character vector of labels and a
# named numeric vector of confidences. Answers outside the allowed options
# are dropped.
parse_second_reader <- function(answers, spec, track) {
  labels <- character()
  confidence <- numeric()
  for (field in single_answer_fields(spec, track)) {
    answer <- answers[[field$name]]
    if (is.null(answer) || is.null(answer$choice) || !answer$choice %in% field_choices(field)) next
    labels[[field$name]] <- answer$choice
    confidence[[field$name]] <- if (is.null(answer$confidence)) NA_real_ else answer$confidence
  }
  list(labels = labels, confidence = confidence)
}

# Compare the first reader's labels (factsheet columns) with the second
# reader's. Returns the field names in three groups.
compare_readers <- function(columns, second_labels, spec, track) {
  confirmed <- character()
  disputed <- character()
  for (field in single_answer_fields(spec, track)) {
    second <- second_labels[field$name]
    first <- columns[[answer_column(field)]]
    if (is.na(second) || is.null(first) || is.na(first)) next
    if (identical(unname(second), first)) confirmed <- c(confirmed, field$name) else disputed <- c(disputed, field$name)
  }
  to_arbitrate <- intersect(disputed, arbitrated_fields(spec, track))
  list(confirmed = confirmed, disputed = setdiff(disputed, to_arbitrate), to_arbitrate = to_arbitrate)
}

# Schema for the tie-break call: only the disputed fields, each with its
# evidence and, where the field has one, its other-term.
build_tiebreak_type <- function(spec, track, field_names) {
  props <- list()
  for (name in field_names) {
    field <- spec_field(spec, track, name)
    props[[paste0(name, "_evidence")]] <- ellmer::type_string(evidence_description(field))
    props[[answer_column(field)]] <- ellmer::type_enum(field_choices(field), field_description(field))
    if (field_flag(field, "other")) {
      props[[paste0(name, "_other_term")]] <- ellmer::type_string(other_term_description(field))
    }
  }
  do.call(ellmer::type_object, props)
}

tiebreak_user_prompt <- function(spec, track, field_names) {
  labels <- vapply(field_names, function(name) spec_field(spec, track, name)$label, character(1))
  paste0(
    "Task: label this paper for a database on ", spec_track(spec, track)$topic_name, ". ",
    "Decide only these fields: ", paste(labels, collapse = "; "), ".\n",
    "Rules:\n",
    "1. Label only what THIS paper proposes or evaluates with its own results (a derivation, a ",
    "simulation or a data analysis). Do not label methods that appear only in the introduction, ",
    "in the literature review, or as a competitor in a comparison.\n",
    "2. Fill the evidence first, then choose the label that the evidence supports.\n",
    "3. Choose '", NONE_OF_LISTED, "' only when no listed option fits, and then name what the paper ",
    "uses in the matching other-term field.\n",
    "4. Use the exact option text."
  )
}

# Put the tie-break's decisions into the factsheet columns. Returns
# list(columns, changed): `changed` names the fields whose label now differs
# from the first reader's.
apply_tiebreak <- function(columns, tiebreak_raw, field_names, spec, track) {
  changed <- character()
  for (name in field_names) {
    field <- spec_field(spec, track, name)
    column <- answer_column(field)
    decided <- as_values(tiebreak_raw[[column]])
    decided <- decided[decided %in% field_choices(field)]
    if (length(decided) == 0L) next
    decided <- decided[1]
    if (!identical(decided, columns[[column]])) changed <- c(changed, name)
    columns[[column]] <- decided

    if (field$kind == "primary_additional") {
      additional <- setdiff(split_values(columns[[paste0(name, "_additional")]]), decided)
      if (identical(decided, "Not applicable")) additional <- character(0)
      columns[[paste0(name, "_additional")]] <- collapse_values(additional)
      columns[[name]] <- collapse_values(c(decided, additional))
    }
    if (field_flag(field, "other")) {
      term <- clean_short_text(tiebreak_raw[[paste0(name, "_other_term")]], OTHER_TERM_MAX_CHARS)
      columns[[paste0(name, "_other_term")]] <- if (!identical(decided, NONE_OF_LISTED)) NA_character_ else
        if (is.na(term) || tolower(term) %in% c(NOT_APPLICABLE_TERM, "na", "none")) "unspecified" else term
    }
    if (field_flag(field, "evidence")) {
      columns[[paste0(name, "_evidence")]] <- clean_short_text(tiebreak_raw[[paste0(name, "_evidence")]],
                                                               EVIDENCE_MAX_CHARS)
    }
  }
  list(columns = columns, changed = changed)
}
