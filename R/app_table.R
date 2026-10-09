# Data behind the results table, the CSV export and the BibTeX export.
#
# Tables hold plain text only. DT escapes every cell, and the links, badges
# and bars are drawn in the browser by the renderers in www/qew.js from the
# plain values, so no paper text is ever pasted into HTML here.

RESULTS_PAGE_LENGTH <- 15L

readable_list <- function(x) gsub(LIST_SEP, "; ", x, fixed = TRUE)

# Columns a reader can add to the results table: named vector, label -> column.
extra_column_choices <- function(spec, track = NULL) {
  shared <- spec_shared_fields(spec)
  fields <- if (is.null(track)) lapply(shared, function(name) spec_field_any(spec, name)) else spec_fields(spec, track)
  shown <- if (is.null(track)) character(0) else unlist(spec_track(spec, track)$core_filters)
  fields <- Filter(function(f) !f$name %in% shown, fields)
  choices <- stats::setNames(vapply(fields, function(f) f$name, character(1)),
                             vapply(fields, function(f) f$label, character(1)))
  c(choices, "arXiv id" = "paper_id", "Submitted" = "submitted_date", "arXiv category" = "primary_category")
}

code_status <- function(code_public) {
  ifelse(is.na(code_public), "Not recorded", ifelse(code_public, "Public", "Not public"))
}

# Where a click on the Code cell leads: the first code link the factsheet
# records (a repository, CRAN, PyPI and so on), or the paper's arXiv page when
# the code is public but only described or attached there. "" for no link.
code_link <- function(papers) {
  urls <- vapply(papers$software_urls %||% rep(NA_character_, nrow(papers)), function(cell) {
    found <- grep("^https?://[^[:space:]\"<>]+$", trimws(split_values(cell)), value = TRUE)
    if (length(found) > 0L) found[1] else ""
  }, character(1), USE.NAMES = FALSE)
  public <- !is.na(papers$code_public) & papers$code_public
  in_paper <- public & !nzchar(urls)
  href <- ifelse(nzchar(urls) & public, urls, ifelse(in_paper, papers$link_abstract, ""))
  href[is.na(href) | papers$status != "ok"] <- ""
  list(href = href, kind = ifelse(!nzchar(href), "", ifelse(in_paper, "paper", "code")))
}

column_as_text <- function(papers, column) {
  cells <- papers[[column]]
  if (is.null(cells)) return(rep(NA_character_, nrow(papers)))
  if (is.logical(cells)) return(ifelse(is.na(cells), NA, ifelse(cells, TRISTATE_VALUES[1], TRISTATE_VALUES[2])))
  if (inherits(cells, "Date")) return(format(cells, "%Y-%m-%d"))
  term <- papers[[paste0(column, "_other_term")]]
  cells <- as.character(cells)
  if (!is.null(term)) {
    other <- !is.na(cells) & !is.na(term) & grepl(NONE_OF_LISTED, cells, fixed = TRUE)
    cells[other] <- mapply(function(cell, t) sub(NONE_OF_LISTED, paste0("Other: ", t), cell, fixed = TRUE),
                           cells[other], term[other])
  }
  readable_list(cells)
}

# Display table: one row per paper, plain text. The first six columns
# (paper_id, row_track, status, href, code_href, code_kind) are hidden in the
# browser and used by the renderers.
results_table <- function(papers, spec, track = NULL, extra = character(0), scores = NULL) {
  out <- data.frame(
    paper_id = papers$paper_id,
    row_track = papers$track,
    status = papers$status,
    href = if (nrow(papers) > 0L) paper_href(papers$paper_id, track, papers$track) else character(0),
    code_href = code_link(papers)$href,
    code_kind = code_link(papers)$kind,
    Saved = papers$paper_id,
    Title = papers$title,
    Year = papers$year,
    Authors = papers$authors_short,
    stringsAsFactors = FALSE, check.names = FALSE)
  if (is.null(track)) {
    out$Track <- vapply(papers$track, function(id) spec$tracks[[id]]$short_label, character(1), USE.NAMES = FALSE)
    out$Topic <- papers$topic
    out$Method <- papers$method
  } else {
    info <- spec_track(spec, track)
    out[[spec_field(spec, track, info$core_filters$topic)$label]] <- papers$topic
    out[[spec_field(spec, track, info$core_filters$method)$label]] <- papers$method
  }
  out$Code <- code_status(papers$code_public)
  out$Code[papers$status != "ok"] <- ""
  if (!is.null(scores)) out$Relevance <- round(score_for(papers, scores), 3)
  choices <- extra_column_choices(spec, track)
  for (column in intersect(extra, choices)) {
    out[[names(choices)[match(column, choices)]]] <- column_as_text(papers, column)
  }
  out
}

# Full export of a result list: identifiers, links and every label column.
export_table <- function(papers, spec, scores = NULL) {
  out <- data.frame(
    arxiv_id = papers$paper_id,
    track = papers$track,
    title = papers$title,
    authors = readable_list(papers$authors),
    year = papers$year,
    submitted = format(papers$submitted_date, "%Y-%m-%d"),
    arxiv_url = papers$link_abstract,
    in_scope = papers$status == "ok",
    stringsAsFactors = FALSE)
  if (!is.null(scores)) out$relevance <- score_for(papers, scores)
  tracks <- intersect(names(spec$tracks), unique(papers$track))
  columns <- unique(unlist(lapply(tracks, function(track) {
    vapply(spec_fields(spec, track), function(f) f$name, character(1))
  })))
  for (column in columns) out[[column]] <- column_as_text(papers, column)
  out$code_public <- papers$code_public
  for (column in intersect(c("llm_model", "schema_version", "extracted_at"), names(papers))) {
    out[[column]] <- papers[[column]]
  }
  out
}

# ---- BibTeX ------------------------------------------------------------------

bibtex_escape <- function(x) gsub("([&%#_])", "\\\\\\1", x)

bibtex_key <- function(paper) {
  first <- split_values(paper$authors)[1]
  last <- utils::tail(strsplit(trimws(first %||% "anon"), " ", fixed = TRUE)[[1]], 1L)
  last <- iconv(last, to = "ASCII//TRANSLIT", sub = "")
  paste0(tolower(gsub("[^A-Za-z]", "", last %||% "anon")), paper$year, "_", gsub("[^0-9A-Za-z]", "", paper$paper_id))
}

bibtex_entry <- function(paper) {
  paper <- as.list(paper)
  # primaryClass is the paper's own arXiv category, not a fixed value.
  fields <- c(
    title = paste0("{", gsub("[{}]", "", paper$title), "}"),
    author = paste(split_values(paper$authors), collapse = " and "),
    year = as.character(paper$year),
    eprint = paper$paper_id,
    archivePrefix = "arXiv",
    primaryClass = if (is_blank(paper$primary_category)) NA_character_ else paper$primary_category,
    journal = if (is_blank(paper$journal_ref)) NA_character_ else bibtex_escape(paper$journal_ref),
    doi = if (is_blank(paper$doi)) NA_character_ else paper$doi,
    url = paper$link_abstract)
  fields <- fields[!is.na(fields)]
  paste0("@misc{", bibtex_key(paper), ",\n",
         paste0("  ", format(names(fields), width = 13), " = {", fields, "}", collapse = ",\n"), "\n}")
}

bibtex_export <- function(papers, settings, now = Sys.time()) {
  if (nrow(papers) == 0L) return("% No bookmarked papers to export")
  header <- c("% Bibliography exported from QE ArXiv Watch",
              paste0("% ", settings$app_url),
              paste0("% Exported ", format(now, "%Y-%m-%d"), "; ", nrow(papers), " papers"), "")
  entries <- vapply(seq_len(nrow(papers)), function(i) bibtex_entry(papers[i, , drop = FALSE]), character(1))
  c(header, paste(entries, collapse = "\n\n"))
}
