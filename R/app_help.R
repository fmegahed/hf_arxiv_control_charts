# Text behind every "?" in the app.
#
# Nothing here is written from memory: model names come from the
# specification and the settings, field and value definitions from the
# specification, limits from the named constants, and the "which model wrote
# this" statements from the data that are actually loaded. Change any of those
# and the help changes with it.

help_context <- function(spec, settings, data, reliability = NULL) {
  list(spec = spec, settings = settings, provenance = data$provenance, year_range = data$year_range,
       data_date = data$data_date, reliability = reliability,
       n_without_metadata = data$n_without_metadata)
}

help_tracks <- function(ctx, track = NULL) if (is.null(track)) names(ctx$spec$tracks) else track

p <- function(...) htmltools::tags$p(...)

definition_list <- function(values, definitions) {
  htmltools::tags$dl(class = "help-definitions", lapply(seq_along(values), function(i) {
    htmltools::tagList(
      htmltools::tags$dt(values[i]),
      htmltools::tags$dd(if (is.na(definitions[i])) "No further definition." else definitions[i]))
  }))
}

# Definition of one field as the model was given it.
help_field_block <- function(spec, field) {
  parts <- list(htmltools::tags$h5(field$label), p(htmltools::tags$em("Question put to the model: "), field$question))
  if (field$kind %in% c("single", "primary_additional", "multi")) {
    values <- field_values(field)
    parts <- c(parts, list(definition_list(values, unname(field_definitions(field)[values]))))
    if (field_flag(field, "other")) {
      parts <- c(parts, list(p("When no option fits, the model names what the paper uses; the app shows it as \"Other: ...\".")))
    }
  } else if (identical(field$kind, "tristate")) {
    parts <- c(parts, list(p("Recorded as ", paste(TRISTATE_VALUES, collapse = ", "), ".")))
  }
  if (identical(field$kind, "primary_additional")) {
    parts <- c(parts, list(p("A paper has one primary label and at most ", spec$limits$max_additional_labels,
                             " additional labels. The primary label is marked.")))
  }
  htmltools::tagList(parts)
}

help_provenance_table <- function(ctx, track = NULL) {
  prov <- ctx$provenance
  if (!is.null(track)) prov <- prov[prov$track == track, , drop = FALSE]
  if (is.null(prov) || nrow(prov) == 0L) return(p("No factsheets are loaded."))
  htmltools::tags$table(
    class = "table table-condensed help-table",
    htmltools::tags$thead(htmltools::tags$tr(lapply(
      c("Track", "Model", "Schema version", "Factsheets", "Extracted between"), htmltools::tags$th))),
    htmltools::tags$tbody(lapply(seq_len(nrow(prov)), function(i) {
      htmltools::tags$tr(
        htmltools::tags$td(ctx$spec$tracks[[prov$track[i]]]$short_label),
        htmltools::tags$td(prov$llm_model[i]),
        htmltools::tags$td(prov$schema_version[i]),
        htmltools::tags$td(format(prov$n[i], big.mark = ",")),
        htmltools::tags$td(paste(prov$first[i], "and", prov$last[i])))
    })))
}

help_reliability <- function(ctx, track = NULL) {
  rel <- ctx$reliability
  if (is.null(rel)) return(p("No rating study is available for these factsheets."))
  fields <- rel$fields
  if (!is.null(track)) fields <- fields[fields$track %in% c("all", track), , drop = FALSE]
  track_label <- function(id) if (id == "all") "All tracks" else ctx$spec$tracks[[id]]$short_label %||% id
  field_label <- function(name) {
    for (narrative in ctx$spec$narrative_fields) if (identical(narrative$name, name)) return(narrative$label)
    tracks <- spec_field_tracks(ctx$spec, name)
    if (length(tracks) > 0L) spec_field(ctx$spec, tracks[[1]], name)$label else paste0(name, " (earlier schema)")
  }
  counts <- unlist(rel$by_track)
  smallest <- names(counts)[which.min(counts)]
  htmltools::tagList(
    p(rel$description),
    p("Collected ", rel$measured_on$collected, " on factsheets written by ", rel$measured_on$model,
      " under schema version ", rel$measured_on$schema_version, ": ", rel$papers, " papers rated by ",
      rel$authors, " authors (",
      paste(vapply(names(counts), function(id) paste0(track_label(id), " ", counts[[id]]), character(1)),
            collapse = ", "), ")."),
    p(htmltools::tags$strong("Read the n column. "), track_label(smallest), " has only ", counts[[smallest]],
      " rated papers, so its percentages move by ", round(100 / counts[[smallest]]),
      " points per paper and say little. The ratings describe the schema and model named above; ",
      "factsheets from another schema or model were not part of the study."),
    htmltools::tags$table(
      class = "table table-condensed help-table",
      htmltools::tags$thead(htmltools::tags$tr(lapply(
        c("Track", "Field", "n", "Mean rating (1 to 5)", "Rated 4 or 5"), htmltools::tags$th))),
      htmltools::tags$tbody(lapply(seq_len(nrow(fields)), function(i) {
        htmltools::tags$tr(
          htmltools::tags$td(track_label(fields$track[i])),
          htmltools::tags$td(field_label(fields$field[i])),
          htmltools::tags$td(fields$n[i]),
          htmltools::tags$td(format(fields$mean[i], nsmall = 2)),
          htmltools::tags$td(paste0(fields$pct_rated_4_or_5[i], "%")))
      })))
  )
}

help_scope_rules <- function(ctx, track) {
  info <- spec_track(ctx$spec, track)
  htmltools::tagList(
    htmltools::tags$h5(info$label),
    p("arXiv search that finds candidate papers: ", htmltools::tags$code(info$query)),
    p(htmltools::tags$strong("Kept: "), info$scope$include),
    p(htmltools::tags$strong("Screened out: "), info$scope$exclude),
    p("For example, kept: \"", info$scope$example_in, "\" Screened out: \"", info$scope$example_out, "\""))
}

HELP_TOPICS <- c("ask", "scope", "factsheet", "chat", "relevance", "explore", "per_year", "composition",
                 "trends", "gap_map", "rising", "reuse", "tested", "authors", "library")

# Content of one help topic: list(title, body). `fields` are field
# definitions (from spec_field) of what a chart currently shows.
help_content <- function(topic, ctx, track = NULL, fields = list()) {
  spec <- ctx$spec
  settings <- ctx$settings
  field_blocks <- lapply(fields, function(field) help_field_block(spec, field))
  shared_labels <- vapply(spec_shared_fields(spec), function(name) spec_field_any(spec, name)$label, character(1))
  shared_filterable <- shared_labels[vapply(spec_shared_fields(spec), function(name) {
    isTRUE(spec_field_any(spec, name)$filter %in% c("core", "more"))
  }, logical(1))]

  if (startsWith(topic, "field:")) {
    parts <- strsplit(topic, ":", fixed = TRUE)[[1]]
    field <- tryCatch(spec_field_any(spec, parts[3], if (nzchar(parts[2])) parts[2] else NULL),
                      error = function(e) NULL)
    if (is.null(field)) return(list(title = "Field", body = p("This field is not in the specification.")))
    return(list(title = field$label, body = help_field_block(spec, field)))
  }

  switch(
    topic,
    ask = list(title = "Asking a question", body = htmltools::tagList(
      p("Type what you are looking for in plain words. A language model (", htmltools::tags$code(spec$models$question),
        ") translates the question into the filters shown as chips under the box, together with one sentence saying how it read the question. ",
        "It does not answer the question and it cannot invent a filter value: every value is checked against the list of allowed values, and anything else is dropped and reported."),
      p("Chips can be removed and filters added by hand. Neither calls the model again."),
      p("Inside a track the first chip is the track. Remove it to search all tracks. Across tracks, only the fields that every track has can filter all papers (year, ",
        paste(shared_filterable, collapse = ", "),
        "). A filter on a field that belongs to one track narrows that track's papers only, and its chip says so."),
      p("Whatever part of the question no field covers becomes a phrase. A second model (", htmltools::tags$code(settings$jev$model),
        ") then judges, for each paper that passed the filters, how likely it is to be about that phrase. See the help next to the relevance column."),
      p("If the language model cannot be reached, the app searches the words of the question in titles, abstracts and summaries, and says that it did."),
      p("Limits: ", QUESTION_MAX_CHARS, " characters per question, and ", QUESTION_RATE_LIMIT, " questions per ",
        QUESTION_RATE_WINDOW_SEC / 60, " minutes in one session."))),

    relevance = list(title = "Relevance ranking", body = htmltools::tagList(
      p("The bar shows a probability between 0 and 1 that the paper is about the phrase left over from your question. It comes from a decision model (",
        htmltools::tags$code(settings$jev$model), ") that reads the paper's title and factsheet summary (the abstract when there is no summary) and returns a number. It writes no text."),
      p("Papers at ", JEV_THRESHOLD, " or above are listed first. Papers below it are kept under \"Less likely matches\" and never removed."),
      p("Only papers that passed the filters are scored, at most the ", JEV_MAX_PAPERS, " most recent, in batches of ", JEV_BATCH_SIZE,
        ". When more papers pass the filters, the app says so and the rest are listed with the less likely matches."),
      p("If the service is unavailable, papers are ordered by the share of the phrase's words they contain, and the bar is labelled as a keyword match. The same threshold idea applies at ",
        KEYWORD_THRESHOLD, "."))),

    scope = list(title = "Which papers are counted", body = htmltools::tagList(
      p("Papers are found by a keyword search on arXiv, so some of them use the keywords in another sense. Each paper's title and abstract are screened against the rules below. ",
        "Papers that fail the screen are hidden and left out of every count and chart unless you choose to include them."),
      lapply(help_tracks(ctx, track), function(id) help_scope_rules(ctx, id)),
      htmltools::tags$h5("Reasons a paper is screened out"),
      definition_list(vapply(spec$scope_categories, function(v) v[[1]], character(1)),
                      vapply(spec$scope_categories, function(v) v[[2]], character(1))),
      p("Papers whose factsheet could not be produced are not listed. The line under the question box says how many there are."),
      p("A paper found by the searches of two tracks has a factsheet in each of them, and is counted in each when all tracks are shown."),
      p("Data current as of ", format(ctx$data_date, "%d %B %Y"), ". Submission years in the data: ",
        ctx$year_range[1], " to ", ctx$year_range[2], "."))),

    factsheet = list(title = "How the factsheets are made, and how good they are", body = htmltools::tagList(
      p("A factsheet is a structured reading of one paper's PDF by a language model. The model chooses labels from fixed lists, quotes the passage it relied on where the field asks for evidence, and writes the summary, results, equations, limitations and future work."),
      p("Parts marked \"model-identified\" (limitations and future work that the authors did not state) are the model's own suggestions. Evidence quotes are labelled \"model-cited\": they are what the model says the paper states, and should be checked against the PDF."),
      htmltools::tags$h5("What produced the factsheets you are looking at"),
      help_provenance_table(ctx, track),
      p("New papers are extracted with ", htmltools::tags$code(spec$models$extraction), " under schema version ",
        spec$schema_version, ". Factsheets listed above with another schema version were written under an earlier definition of the fields and mapped to the current labels; they carry no evidence quotes, and some fields are empty."),
      htmltools::tags$h5("How good are these factsheets"),
      help_reliability(ctx, track),
      p("Each paper's own page shows the model, date and schema version of its factsheet, and has a link to report a problem."))),

    chat = list(title = "Chat", body = htmltools::tagList(
      p("The chat sends your message and the paper's PDF (fetched from arXiv) to a language model (",
        htmltools::tags$code(spec$models$chat), "), which writes the answer. Answers can be wrong; check them against the paper."),
      p("Chat with a collection sends the PDFs of your bookmarked papers, at most ", COLLECTION_CHAT_MAX_PDFS,
        " (the most recent ones when you have more)."),
      p("Nothing you type is stored by this app. It is sent to the model provider to produce the answer."))),

    explore = list(title = "Explore", body = htmltools::tagList(
      p("The filters here and the chips above are the same thing: changing one changes the other, and every tab uses them."),
      p("Within one filter, choosing several values means any of them. Different filters all have to hold."),
      p("A filter on a field with a primary and additional labels matches either. Clicking a bar in a chart of primary labels filters on the primary label only, and the chip says so."),
      p("\"Uses real data\" means the factsheet lists one of: ", paste(settings$real_data_values, collapse = "; "),
        ". \"Reviews and tutorials only\" means the paper type is: ", paste(settings$review_paper_types, collapse = "; "), "."),
      p("\"Public code\" means the code can be obtained without asking the authors: ",
        paste(PUBLIC_CODE_SOURCES, collapse = "; "), "."),
      field_blocks)),

    per_year = list(title = "Papers per year", body = htmltools::tagList(
      p("Number of papers in the current selection by the year they were first submitted to arXiv. The latest year is incomplete: the data end on ",
        format(ctx$data_date, "%d %B %Y"), "."),
      p("Click a bar to filter to that year."))),

    composition = list(title = "Composition", body = htmltools::tagList(
      p("Each bar counts the papers in the current selection whose primary label is that value. The percentage is the share of papers that have a label for the field. A paper is counted once per chart."),
      p("Click a bar to filter to those papers."),
      field_blocks)),

    trends = list(title = "Trends", body = htmltools::tagList(
      p("For each chosen value, the number of papers per submission year that carry it as a primary or additional label. Hover to see the count and the share of that year's papers. Years with few papers make shares jump, so read the counts."),
      field_blocks)),

    gap_map = list(title = "Gap map", body = htmltools::tagList(
      p("Each cell counts the papers in the current selection that carry both labels. A paper with several labels in a field is counted in every cell it belongs to, so cells can add up to more than the number of papers."),
      p("An empty cell means no paper in this database combines the two. It may be a gap in the literature, a combination that makes no sense, or a combination arXiv authors publish elsewhere."),
      p("Click a cell to filter to those papers."),
      field_blocks)),

    rising = list(title = "What is rising", body = htmltools::tagList(
      p("For each label, the share of papers carrying it in the last ", RISING_RECENT_YEARS,
        " submission years is compared with its share in all earlier years. Labels are ordered by the change in share, in percentage points."),
      p("Counts are shown next to each share. Labels carried by fewer than ", RISING_MIN_PAPERS,
        " papers in the current selection are left out because their shares are noise."),
      p("The comparison uses the current selection, so a year filter changes or empties it."),
      field_blocks)),

    reuse = list(title = "Can I reuse it", body = htmltools::tagList(
      p("Left: of the papers submitted each year, the share whose code is public, with the counts on hover. Papers with no recorded code status are not in the denominator."),
      p("Public means one of: ", paste(PUBLIC_CODE_SOURCES, collapse = "; "), "."),
      p("Right: the software named in the papers. A paper naming several is counted for each."),
      field_blocks)),

    tested = list(title = "How is it tested", body = htmltools::tagList(
      p("Left: each paper in exactly one group by the data it analyses. Real data means: ",
        paste(settings$real_data_values, collapse = "; "), "."),
      p("Right: the kinds of evaluation the papers use. A paper using several is counted for each. This field differs by track, so it is shown when one track is selected."),
      field_blocks)),

    authors = list(title = "Authors", body = htmltools::tagList(
      p("Counts use the current selection of papers. Author names are taken from arXiv as written, so one person who writes their name in two ways appears twice, and two people with the same name appear as one."))),

    library = list(title = "Library", body = htmltools::tagList(
      p("Bookmarks are kept in this browser's local storage, not on a server. They are not affected by the question or the filters, and they cover all tracks."),
      p("The BibTeX export describes the arXiv preprints. When arXiv records a journal reference or DOI, it is included."))),

    list(title = "Help", body = p("No help is available for this element."))
  )
}

help_text <- function(topic, ctx, track = NULL, fields = list()) {
  content <- help_content(topic, ctx, track, fields)
  # One line, so the text can be searched without regard to how tags wrap.
  gsub("\\s+", " ", paste(content$title, as.character(htmltools::tagList(content$body))))
}

# The "?" control: a real button, so it is reachable by keyboard.
help_button <- function(topic, label = "Explain this") {
  htmltools::tags$button(type = "button", class = "help-q", `data-help` = topic, `aria-label` = label,
                         title = label, "?")
}
