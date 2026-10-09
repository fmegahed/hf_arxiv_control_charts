# The page for one paper: its whole factsheet, built with htmltools only.
#
# All paper text is inserted as text nodes (htmltools escapes it) and never
# through HTML() or a markdown renderer. Math is typeset in the browser from
# the \( \) and \[ \] delimiters that the storage contract guarantees.

QA_FLAG_TEXT <- c(
  truncated_additional = "The model listed more additional labels than allowed; the extra ones were dropped.",
  other_term_missing = "The model chose \"none of the listed\" for a field without naming what the paper uses.",
  math_repaired = "Some mathematical notation was repaired after extraction.",
  latex_invalid = "An equation could not be typeset and is shown as plain code.",
  undefined_acronym = "The text uses an acronym that the glossary does not explain.",
  pdf_truncated = "The PDF was longer than the page limit; only its first pages were read."
)

qa_flag_text <- function(flags) {
  flags <- split_values(flags)
  if (length(flags) == 0L) return(character(0))
  known <- QA_FLAG_TEXT[flags]
  ifelse(is.na(known), paste0("Flag recorded by the pipeline: ", flags), known)
}

# Paragraphs of plain text. Blank lines separate paragraphs.
text_block <- function(x, class = "math-content") {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(trimws(x))) return(NULL)
  paragraphs <- strsplit(x, "\n[ \t]*\n+")[[1]]
  htmltools::tags$div(class = class, lapply(paragraphs[nzchar(trimws(paragraphs))], function(paragraph) {
    lines <- strsplit(paragraph, "\n", fixed = TRUE)[[1]]
    htmltools::tags$p(utils::head(unlist(lapply(lines, function(line) list(line, htmltools::tags$br())),
                                         recursive = FALSE), -1L))
  }))
}

is_blank <- function(x) is.null(x) || length(x) == 0L || is.na(x) || !nzchar(trimws(as.character(x)))

# A label a reader can click to filter by it.
tag_chip <- function(field, value, track, other_term = NA_character_, primary = FALSE, muted = FALSE) {
  label <- display_value(value, other_term)
  definition <- if (field$kind %in% c("single", "primary_additional", "multi")) {
    unname(field_definitions(field)[value])
  } else NA_character_
  can_filter <- isTRUE(field$filter %in% c("core", "more"))
  classes <- paste(c("tag-chip", if (primary) "tag-primary", if (muted) "tag-muted",
                     if (!can_filter) "tag-static"), collapse = " ")
  content <- list(label, if (primary) htmltools::tags$span(class = "tag-role", "primary"))
  title <- paste0(if (!is.na(definition)) paste0(definition, " ") else "",
                  if (can_filter) "Click to show papers with this label." else "")
  if (!can_filter) return(htmltools::tags$span(class = classes, title = trimws(title), content))
  htmltools::tags$button(type = "button", class = classes, title = trimws(title),
                         `data-field` = field$name, `data-value` = value, `data-track` = track %||% "",
                         `data-role` = if (primary) "primary" else "any", content)
}

evidence_block <- function(evidence) {
  if (is_blank(evidence)) return(NULL)
  htmltools::tags$details(class = "evidence",
                          htmltools::tags$summary("Model-cited evidence"),
                          htmltools::tags$blockquote(class = "math-content", evidence))
}

# How a single-answer label fared when a second reader checked it. One of
# "confirmed", "kept", "changed", "disputed", or NA when it was not checked.
label_check_status <- function(paper, name) {
  listed <- function(column) name %in% split_values(paper[[column]] %||% NA_character_)
  if (listed("labels_changed")) "changed"
  else if (listed("labels_resolved")) "kept"
  else if (listed("labels_disputed")) "disputed"
  else if (listed("labels_confirmed")) "confirmed"
  else NA_character_
}

LABEL_CHECK_TEXT <- c(
  confirmed = "Two models agreed",
  kept = "Models disagreed; a third model kept this label",
  changed = "Models disagreed; a third model chose this label",
  disputed = "Models disagreed; not reviewed"
)

label_check_note <- function(paper, name) {
  status <- label_check_status(paper, name)
  if (is.na(status)) return(NULL)
  htmltools::tags$span(class = paste("label-check", paste0("label-check-", status)),
                       LABEL_CHECK_TEXT[[status]], help_button("label_check", "How labels are checked"))
}

# One field of the factsheet: label, values as chips or text, evidence.
field_row <- function(paper, field, spec, shared) {
  name <- field$name
  track <- if (name %in% shared) NULL else paper$track
  other_term <- paper[[paste0(name, "_other_term")]] %||% NA_character_
  value_part <- NULL

  if (identical(field$kind, "primary_additional")) {
    primary <- paper[[paste0(name, "_primary")]] %||% NA_character_
    additional <- split_values(paper[[paste0(name, "_additional")]] %||% NA_character_)
    if (!is.na(primary)) {
      value_part <- htmltools::tagList(
        tag_chip(field, primary, track, other_term, primary = TRUE),
        lapply(additional, function(value) tag_chip(field, value, track)))
    }
  } else if (field$kind %in% c("single", "multi")) {
    values <- split_values(paper[[name]] %||% NA_character_)
    if (length(values) > 0L) {
      value_part <- htmltools::tagList(lapply(values, function(value) {
        tag_chip(field, value, track, other_term, muted = identical(value, field$sentinel))
      }))
    }
  } else if (identical(field$kind, "tristate")) {
    value <- paper[[name]] %||% NA
    text <- if (is.na(value)) TRISTATE_VALUES[3] else if (isTRUE(as.logical(value))) TRISTATE_VALUES[1] else TRISTATE_VALUES[2]
    value_part <- htmltools::tagList(
      tag_chip(field, text, track, muted = is.na(value)),
      if (is.na(value)) htmltools::tags$span(class = "not-recorded", "(or not recorded)"))
  } else if (identical(field$kind, "urls")) {
    urls <- split_values(paper[[name]] %||% NA_character_)
    urls <- urls[grepl("^https?://", urls)]
    if (length(urls) > 0L) {
      value_part <- htmltools::tags$ul(class = "link-list", lapply(urls, function(url) {
        htmltools::tags$li(htmltools::tags$a(href = url, target = "_blank", rel = "noopener noreferrer", url))
      }))
    }
  } else {
    if (!is_blank(paper[[name]])) value_part <- htmltools::tags$span(class = "math-content", paper[[name]])
  }

  htmltools::tags$div(
    class = "fact-row",
    htmltools::tags$div(class = "fact-label", field$label,
                        help_button(paste0("field:", track %||% "", ":", name),
                                    paste0("Definition of ", field$label))),
    htmltools::tags$div(class = "fact-value",
                        if (is.null(value_part)) htmltools::tags$span(class = "not-recorded", "Not recorded") else value_part,
                        if (!is.null(value_part)) label_check_note(paper, name),
                        evidence_block(paper[[paste0(name, "_evidence")]] %||% NA_character_)))
}

group_rows <- function(paper, spec, groups, shared, skip = character(0)) {
  fields <- Filter(function(f) f$group %in% groups && !f$name %in% skip, spec_fields(spec, paper$track))
  lapply(fields, function(field) field_row(paper, field, spec, shared))
}

glossary_block <- function(glossary) {
  items <- split_values(glossary)
  if (length(items) == 0L) return(NULL)
  htmltools::tags$dl(class = "glossary math-content", lapply(items, function(item) {
    term <- trimws(sub("=.*$", "", item))
    meaning <- if (grepl("=", item, fixed = TRUE)) trimws(sub("^[^=]*=", "", item)) else ""
    htmltools::tagList(htmltools::tags$dt(term), htmltools::tags$dd(meaning))
  }))
}

section <- function(id, title, ..., help = NULL) {
  content <- Filter(Negate(is.null), list(...))
  if (length(content) == 0L) return(NULL)
  htmltools::tags$section(class = "paper-section", id = paste0("factsheet-", id),
                          htmltools::tags$h4(title, help), content)
}

# A block of text with a line saying who wrote it.
attributed_block <- function(text, source = c("authors", "model"), label) {
  source <- match.arg(source)
  if (is_blank(text)) return(NULL)
  htmltools::tags$div(
    class = paste("attributed", paste0("by-", source)),
    htmltools::tags$div(class = "attribution", label),
    text_block(text))
}

# Prefilled GitHub issue for a problem with one factsheet.
report_issue_url <- function(paper, spec, settings) {
  fields <- if (identical(paper$status, "ok")) {
    c(vapply(spec_fields(spec, paper$track), function(f) f$label, character(1)),
      vapply(spec$narrative_fields, function(f) f$label, character(1)))
  } else "Screening decision"
  body <- paste0(
    "Paper: https://arxiv.org/abs/", paper$paper_id, "\n",
    "Track: ", spec_track(spec, paper$track)$label, "\n",
    "Factsheet: model ", paper$llm_model %||% NA, ", schema ", paper$schema_version %||% NA,
    ", extracted ", paper$extracted_at %||% NA, "\n\n",
    "Which fields are wrong? Delete the lines that are fine and say what is wrong on the others.\n\n",
    paste0("- [ ] ", fields, ": ", collapse = "\n"), "\n\n",
    "Anything else:\n")
  paste0(settings$issue_url, "?title=", url_escape(paste0("Factsheet problem: arXiv ", paper$paper_id)),
         "&body=", url_escape(body))
}

format_extracted_at <- function(x) {
  stamp <- parse_utc(x)
  if (is.na(stamp)) "an unrecorded date" else format(stamp, "%d %B %Y")
}

provenance_block <- function(paper, spec) {
  current <- identical(paper$schema_version, spec$schema_version)
  flags <- qa_flag_text(paper$qa_flags %||% NA_character_)
  htmltools::tagList(
    htmltools::tags$p(
      "This factsheet was written by the model ", htmltools::tags$code(paper$llm_model %||% "not recorded"),
      " on ", format_extracted_at(paper$extracted_at), " from version ", paper$arxiv_version %||% "?",
      " of the paper, under schema version ", htmltools::tags$code(paper$schema_version %||% "not recorded"), "."),
    if (!current) htmltools::tags$p(
      class = "provenance-note",
      "The current schema version is ", spec$schema_version,
      ". This factsheet was written under an earlier definition of the fields and mapped to the current labels, so it has no evidence quotes and some fields are not recorded."),
    if (length(flags) > 0L) htmltools::tagList(
      htmltools::tags$p("Automatic checks noted:"),
      htmltools::tags$ul(lapply(flags, htmltools::tags$li)))
    else htmltools::tags$p("Automatic checks noted nothing for this factsheet.")
  )
}

metadata_line <- function(paper) {
  htmltools::tags$p(
    class = "paper-meta",
    gsub(LIST_SEP, ", ", paper$authors %||% "", fixed = TRUE), htmltools::tags$br(),
    "Submitted ", format(paper$submitted_date, "%d %B %Y"),
    if (!is_blank(paper$primary_category)) paste0(" | arXiv category ", paper$primary_category),
    if (!is_blank(paper$journal_ref)) htmltools::tagList(htmltools::tags$br(), "Journal reference: ", paper$journal_ref),
    if (!is_blank(paper$doi)) htmltools::tagList(
      htmltools::tags$br(), "DOI: ",
      htmltools::tags$a(href = paste0("https://doi.org/", paper$doi), target = "_blank",
                        rel = "noopener noreferrer", paper$doi))
  )
}

paper_links <- function(paper, spec, settings) {
  htmltools::tags$div(
    class = "paper-links",
    htmltools::tags$a(class = "btn btn-primary btn-sm", href = paper$link_abstract, target = "_blank",
                      rel = "noopener noreferrer", "arXiv page"),
    htmltools::tags$a(class = "btn btn-info btn-sm", href = paper$link_pdf, target = "_blank",
                      rel = "noopener noreferrer", "PDF"),
    bookmark_button(paper$paper_id, with_label = TRUE),
    htmltools::tags$a(class = "report-link", href = report_issue_url(paper, spec, settings), target = "_blank",
                      rel = "noopener noreferrer", "Report a problem with this factsheet")
  )
}

bookmark_button <- function(paper_id, with_label = FALSE) {
  htmltools::tags$button(type = "button", class = paste("bookmark-icon", if (with_label) "with-label"),
                         `data-paper-id` = paper_id, `aria-pressed` = "false",
                         `aria-label` = "Bookmark this paper", title = "Bookmark this paper",
                         htmltools::tags$span(class = "bookmark-glyph", `aria-hidden` = "true", "☆"),
                         if (with_label) htmltools::tags$span(class = "bookmark-text", "Bookmark"))
}

# The whole page. `chat` is the chat panel (built by the Shiny module).
paper_view <- function(paper, spec, settings, chat = NULL, also_in = character(0)) {
  paper <- as.list(paper)
  info <- spec_track(spec, paper$track)
  shared <- spec_shared_fields(spec)
  head <- htmltools::tagList(
    htmltools::tags$div(class = "paper-track", style = paste0("border-color:", info$color, ";"), info$label),
    if (length(also_in) > 0L) htmltools::tags$div(
      class = "paper-also-in",
      "This paper was also found by another track's search and has a separate factsheet there: ",
      lapply(also_in, function(track) {
        htmltools::tags$a(href = paper_href(paper$paper_id, track), `data-open-paper` = paper$paper_id,
                          `data-paper-track` = track, spec_track(spec, track)$label)
      })),
    htmltools::tags$h3(class = "paper-title math-content", paper$title),
    metadata_line(paper),
    paper_links(paper, spec, settings))
  abstract <- section("abstract", "Abstract (from arXiv)", text_block(paper$abstract))

  if (!identical(paper$status, "ok")) {
    category <- paper$scope_category %||% NA_character_
    definition <- NA_character_
    for (item in spec$scope_categories) if (identical(item[[1]], category)) definition <- item[[2]]
    return(htmltools::tags$article(
      class = "paper-view", head,
      htmltools::tags$div(
        class = "screened-note",
        htmltools::tags$strong("This paper was screened out and has no factsheet. "),
        if (!is.na(definition)) definition else "The reason was not recorded.",
        if (!is_blank(paper$scope_reason)) htmltools::tagList(" ", htmltools::tags$em("Model's note: "), paper$scope_reason),
        help_button("scope", "How papers are screened")),
      abstract))
  }

  core <- c(info$core_filters$topic, info$core_filters$method)
  glance_names <- unique(c(settings$paper_type_field, core, settings$domain_field, settings$code_field))
  glance <- lapply(glance_names, function(name) field_row(paper, spec_field(spec, paper$track, name), spec, shared))

  htmltools::tags$article(
    class = "paper-view",
    head,
    section("glance", "At a glance", htmltools::tags$div(class = "fact-table", glance),
            help = help_button("factsheet", "How factsheets are made and how good they are")),
    section("summary", "Summary", text_block(paper$summary)),
    section("results", "Key results", text_block(paper$key_results)),
    section("equations", "Equations", text_block(paper$key_equations)),
    section("methods", "Methods and data",
            htmltools::tags$div(class = "fact-table",
                                group_rows(paper, spec, c("Method", "Data"), shared, skip = glance_names))),
    section("evaluation", "Evaluation",
            htmltools::tags$div(class = "fact-table", group_rows(paper, spec, "Evaluation", shared))),
    section("software", "Software and code",
            htmltools::tags$div(class = "fact-table",
                                group_rows(paper, spec, "Software", shared, skip = glance_names))),
    section("limitations", "Limitations",
            attributed_block(paper$limitations_stated, "authors", "Stated by the authors"),
            attributed_block(paper$limitations_unstated, "model",
                             "Model-identified: not stated in the paper, suggested by the model")),
    section("future", "Future work",
            attributed_block(paper$future_work_stated, "authors", "Stated by the authors"),
            attributed_block(paper$future_work_unstated, "model",
                             "Model-identified: not stated in the paper, suggested by the model")),
    section("glossary", "Glossary", glossary_block(paper$glossary)),
    abstract,
    if (!is.null(chat)) section("chat", "Chat with this paper", chat, help = help_button("chat", "About the chat")),
    section("provenance", "Provenance", provenance_block(paper, spec),
            help = help_button("factsheet", "How factsheets are made and how good they are"))
  )
}
