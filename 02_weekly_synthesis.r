# =============================================================================
# Weekly Research Digest - AI-Synthesized Literature Summary
# =============================================================================
#
# Purpose:
#   Generate a professional weekly digest of quality engineering research
#   from arXiv with AI-synthesized insights for subscribers.
#
# Schedule:
#   Runs every Monday at 12:00 UTC via GitHub Actions
#
# Output:
#   - data/weekly_digest.xml: RSS feed with curated weekly summary
#   - data/weekly_digest.json: Structured digest data
#
# Target Audience:
#   - Quality engineering researchers and academics
#   - Industrial practitioners in manufacturing, healthcare, reliability
#   - Graduate students in statistics, industrial engineering, operations research
#
# =============================================================================

library(dplyr)
library(readr)
library(lubridate)
library(jsonlite)
library(ellmer)

# Configuration ---------------------------------------------------------------

OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
get_openai_api_key <- function() { return(OPENAI_API_KEY) }

# Track configuration (must match main app)
tracks <- list(
  spc = list(
    id = "spc",
    label = "Control Charts (SPC)",
    short_label = "Control Charts",
    metadata_csv = "data/spc_arxiv_metadata.csv",
    factsheet_csv = "data/spc_factsheet.csv",
    color = "#1b9e77"
  ),
  exp_design = list(
    id = "exp_design",
    label = "Experimental Design (DOE)",
    short_label = "Experimental Design",
    metadata_csv = "data/exp_design_arxiv_metadata.csv",
    factsheet_csv = "data/exp_design_factsheet.csv",
    color = "#d95f02"
  ),
  reliability = list(
    id = "reliability",
    label = "Reliability Engineering",
    short_label = "Reliability",
    metadata_csv = "data/reliability_arxiv_metadata.csv",
    factsheet_csv = "data/reliability_factsheet.csv",
    color = "#7570b3"
  )
)

# =============================================================================
# System Prompt for Literature Synthesis
# =============================================================================

WEEKLY_SYNTHESIS_SYSTEM_PROMPT <- '
You are the editor of "QE ArXiv Watch Weekly," a premier research digest for
quality engineering professionals. Your readers include:

- University professors and researchers in statistics, industrial engineering,
  and operations research
- Quality engineers and Six Sigma practitioners in manufacturing and healthcare
- Reliability engineers in aerospace, automotive, and electronics industries
- Graduate students seeking to understand the research landscape
- R&D professionals evaluating new methodologies for their organizations

YOUR MISSION:
Transform a week of academic papers into an insightful, actionable digest that
saves readers hours of literature review while keeping them at the forefront
of their field.

WRITING STYLE:
- Professional yet accessible - avoid unnecessary jargon
- Synthesize, don\'t summarize - find connections between papers
- Highlight practical implications alongside theoretical contributions
- Be specific when citing papers - use first author names
- Maintain scholarly credibility while being engaging

OUTPUT FORMAT:
Structure your digest with these sections:

## This Week in Quality Engineering
A compelling 2-3 sentence overview of the week\'s themes and significance.

## Spotlight: [Most Significant Development]
Highlight one paper or theme that represents a notable advance. Explain why
practitioners and researchers should pay attention.

## Research Roundup

### Control Charts & Statistical Process Monitoring
[If papers exist in this track]
- Key methodological advances
- Novel applications or domains
- Practical takeaways

### Experimental Design & Response Surface Methods
[If papers exist in this track]
- Key methodological advances
- Novel applications or domains
- Practical takeaways

### Reliability Engineering & Maintenance
[If papers exist in this track]
- Key methodological advances
- Novel applications or domains
- Practical takeaways

## Cross-Cutting Themes
Identify 2-3 themes that span multiple tracks (e.g., "machine learning integration,"
"healthcare applications," "high-dimensional methods").

## Practitioner\'s Corner
2-3 bullet points on papers with immediate industrial applicability. Focus on:
- Methods ready for implementation
- Software or code availability
- Case studies with real data

## Looking Ahead
Brief note on emerging trends or research gaps suggested by this week\'s papers.

IMPORTANT GUIDELINES:
1. If a track has no papers, briefly note "No new submissions this week" and move on
2. Be honest about limitations - if a week is light on papers, acknowledge it
3. Never fabricate paper details - only reference papers provided to you
4. Use author names (e.g., "Zhang et al.") when referencing specific papers
5. Keep the total length between 600-900 words for readability
6. End with an encouraging call-to-action to explore the full dashboard

Remember: Your readers chose to subscribe because they trust your curation.
Deliver value that justifies their time investment.
'

# =============================================================================
# Helper Functions
# =============================================================================

#' Collect papers from past week across all tracks
#'
#' @param tracks List of track configurations
#' @param days_back Number of days to look back (default 7)
#' @return Data frame of papers from the past week
collect_weekly_papers <- function(tracks, days_back = 7) {

  all_papers <- list()

  for (track_id in names(tracks)) {
    track <- tracks[[track_id]]

    if (!file.exists(track$metadata_csv) || !file.exists(track$factsheet_csv)) {
      message(sprintf("  Skipping %s - files not found", track_id))
      next
    }

    metadata <- readr::read_csv(track$metadata_csv, show_col_types = FALSE)
    factsheet <- readr::read_csv(track$factsheet_csv, show_col_types = FALSE)

    combined <- dplyr::left_join(metadata, factsheet, by = "id")
    combined$track_id <- track_id
    combined$track_label <- track$label
    combined$track_short <- track$short_label

    all_papers[[track_id]] <- combined
  }

  papers <- dplyr::bind_rows(all_papers)

  if (nrow(papers) == 0) {
    return(data.frame())
  }

  # Parse dates and filter
  papers$submitted_date <- as.Date(lubridate::ymd_hms(papers$submitted))
  cutoff_date <- Sys.Date() - days_back

  recent <- papers |>
    dplyr::filter(submitted_date >= cutoff_date) |>
    dplyr::arrange(desc(submitted_date))

  message(sprintf("  Found %d papers from the past %d days", nrow(recent), days_back))
  recent
}

#' Format papers for LLM consumption
#'
#' @param papers Data frame of papers
#' @return Character string with formatted paper information
format_papers_for_llm <- function(papers) {

  if (nrow(papers) == 0) {
    return("No new papers were submitted this week.")
  }

  # Group by track
  tracks_summary <- list()

  for (track_id in unique(papers$track_id)) {
    track_papers <- papers |> dplyr::filter(track_id == !!track_id)
    track_label <- track_papers$track_label[1]

    paper_texts <- sapply(seq_len(nrow(track_papers)), function(i) {
      p <- track_papers[i, ]

      # Get first author's last name for citation
      first_author <- strsplit(p$authors, "\\|")[[1]][1]
      author_parts <- strsplit(trimws(first_author), " ")[[1]]
      last_name <- tail(author_parts, 1)

      # Use AI summary if available, otherwise abstract
      content <- if (!is.na(p$summary) && p$summary != "") {
        p$summary
      } else if (!is.na(p$abstract) && p$abstract != "") {
        substr(p$abstract, 1, 400)
      } else {
        "No summary available."
      }

      # Include key results if available
      key_results <- if (!is.na(p$key_results) && p$key_results != "") {
        paste0("\nKey Results: ", p$key_results)
      } else {
        ""
      }

      sprintf(
        "**%s et al. (%s)**: \"%s\"\n%s%s\n",
        last_name, format(p$submitted_date, "%Y-%m-%d"),
        p$title, content, key_results
      )
    })

    tracks_summary[[track_id]] <- paste0(
      "\n### ", track_label, " (", nrow(track_papers), " papers)\n\n",
      paste(paper_texts, collapse = "\n---\n")
    )
  }

  paste0(
    "# Papers Submitted This Week\n",
    "Week ending: ", format(Sys.Date(), "%B %d, %Y"), "\n",
    "Total papers: ", nrow(papers), "\n\n",
    paste(tracks_summary, collapse = "\n")
  )
}

#' Generate AI synthesis using ellmer
#'
#' @param papers Data frame of papers
#' @return Character string with synthesized digest
generate_synthesis <- function(papers) {

  papers_text <- format_papers_for_llm(papers)

  if (nrow(papers) == 0) {
    return(paste0(
      "## This Week in Quality Engineering\n\n",
      "This was a quiet week on arXiv for quality engineering research, with no new ",
      "submissions in our monitored tracks. This occasionally happens, particularly ",
      "around academic holidays or conference deadlines.\n\n",
      "**Looking Ahead:** We encourage you to explore our archives in the ",
      "[QE ArXiv Watch dashboard](https://huggingface.co/spaces/fmegahed/arxiv_control_charts) ",
      "where you can browse over 1,000 papers across Control Charts, Experimental Design, ",
      "and Reliability Engineering."
    ))
  }

  # Create chat with synthesis model
  tryCatch({
    chat <- ellmer::chat_openai(
      model = "gpt-5.2-2025-12-11",
      system_prompt = WEEKLY_SYNTHESIS_SYSTEM_PROMPT,
      api_key = get_openai_api_key()
    )

    prompt <- paste0(
      "Please synthesize the following papers into our weekly digest. ",
      "Remember to follow the output format specified in your instructions.\n\n",
      papers_text
    )

    synthesis <- chat$chat(prompt)
    message("  Synthesis generated successfully")
    synthesis

  }, error = function(e) {
    message(sprintf("  Error generating synthesis: %s", e$message))

    # Fallback to basic summary
    paste0(
      "## This Week in Quality Engineering\n\n",
      "This week brought ", nrow(papers), " new papers across our research tracks: ",
      sum(papers$track_id == "spc"), " in Control Charts, ",
      sum(papers$track_id == "exp_design"), " in Experimental Design, and ",
      sum(papers$track_id == "reliability"), " in Reliability Engineering.\n\n",
      "Visit the [QE ArXiv Watch dashboard](https://huggingface.co/spaces/fmegahed/arxiv_control_charts) ",
      "to explore each paper with AI summaries and interactive chat."
    )
  })
}

#' Escape XML special characters
#'
#' @param text Character string to escape
#' @return Escaped string
escape_xml <- function(text) {
  if (is.na(text) || text == "") return("")
  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  text <- gsub("\"", "&quot;", text)
  text <- gsub("'", "&apos;", text)
  text
}

#' Convert markdown to HTML for RSS
#'
#' @param markdown Character string with markdown
#' @return HTML string
markdown_to_html <- function(markdown) {
  # Basic markdown to HTML conversion
  html <- markdown

  # Headers
  html <- gsub("^### (.+)$", "<h4>\\1</h4>", html, perl = TRUE)
  html <- gsub("^## (.+)$", "<h3>\\1</h3>", html, perl = TRUE)

  # Bold
  html <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", html)

  # Italic
  html <- gsub("\\*(.+?)\\*", "<em>\\1</em>", html)

  # Links
  html <- gsub("\\[(.+?)\\]\\((.+?)\\)", "<a href=\"\\2\">\\1</a>", html)

  # Line breaks for paragraphs
  html <- gsub("\n\n", "</p><p>", html)
  html <- paste0("<p>", html, "</p>")

  # Bullet points
  html <- gsub("<p>- ", "<li>", html)
  html <- gsub("\n- ", "</li><li>", html)

  html
}

#' Generate weekly digest RSS feed
#'
#' @param papers Data frame of papers
#' @param synthesis AI-generated synthesis text
#' @param output_file Path to output XML file
generate_weekly_rss <- function(papers, synthesis, output_file = "data/weekly_digest.xml") {

  build_date <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S +0000", tz = "UTC")
  week_start <- Sys.Date() - 7
  week_end <- Sys.Date()

  # Create week title
  week_title <- sprintf("Week of %s - %s",
    format(week_start, "%B %d"), format(week_end, "%B %d, %Y"))

  # Convert synthesis to HTML
  synthesis_html <- markdown_to_html(synthesis)

  # Build paper list for the digest
  paper_list_html <- ""
  if (nrow(papers) > 0) {
    paper_items <- sapply(seq_len(min(nrow(papers), 8)), function(i) {
      p <- papers[i, ]
      first_author <- strsplit(p$authors, "\\|")[[1]][1]
      author_parts <- strsplit(trimws(first_author), " ")[[1]]
      last_name <- tail(author_parts, 1)

      sprintf(
        "<li><strong>%s et al.</strong>: <a href=\"%s\">%s</a> <em>(%s)</em></li>",
        escape_xml(last_name), p$link_pdf, escape_xml(p$title), p$track_short
      )
    })

    paper_list_html <- paste0(
      "<h4>Featured Papers This Week</h4>",
      "<ul>", paste(paper_items, collapse = ""), "</ul>"
    )
  }

  # Dashboard call-to-action
  cta_html <- paste0(
    "<hr/>",
    "<p><strong>Explore More:</strong> Visit the ",
    "<a href=\"https://huggingface.co/spaces/fmegahed/arxiv_control_charts\">QE ArXiv Watch Dashboard</a> ",
    "to browse all papers with AI summaries, interactive filtering, and paper chat.</p>",
    "<p style=\"color: #666; font-size: 0.9em;\">",
    "This digest is automatically generated every Monday. ",
    "Questions or feedback? Open an issue on our ",
    "<a href=\"https://github.com/fmegahed/arxiv_control_charts\">GitHub repository</a>.",
    "</p>"
  )

  # Full description content
  description_content <- paste0(
    synthesis_html,
    paper_list_html,
    cta_html
  )

  # Build RSS item
  digest_item <- paste0(
    "    <item>\n",
    "      <title>QE ArXiv Watch: ", escape_xml(week_title), "</title>\n",
    "      <link>https://huggingface.co/spaces/fmegahed/arxiv_control_charts</link>\n",
    "      <guid isPermaLink=\"false\">qe-weekly-", format(Sys.Date(), "%Y-%m-%d"), "</guid>\n",
    "      <pubDate>", build_date, "</pubDate>\n",
    "      <description><![CDATA[\n",
    description_content,
    "\n]]></description>\n",
    "    </item>"
  )

  # Assemble RSS document
  rss_content <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">\n',
    '  <channel>\n',
    '    <title>QE ArXiv Watch Weekly</title>\n',
    '    <link>https://huggingface.co/spaces/fmegahed/arxiv_control_charts</link>\n',
    '    <description>Weekly AI-synthesized digest of quality engineering research from arXiv. Covering Control Charts, Experimental Design, and Reliability Engineering.</description>\n',
    '    <language>en-us</language>\n',
    '    <copyright>CC BY 4.0 - QE ArXiv Watch</copyright>\n',
    '    <managingEditor>noreply@example.com (QE ArXiv Watch)</managingEditor>\n',
    '    <lastBuildDate>', build_date, '</lastBuildDate>\n',
    '    <ttl>10080</ttl>\n',  # 7 days in minutes
    '    <image>\n',
    '      <url>https://huggingface.co/spaces/fmegahed/arxiv_control_charts/resolve/main/www/favicon.svg</url>\n',
    '      <title>QE ArXiv Watch Weekly</title>\n',
    '      <link>https://huggingface.co/spaces/fmegahed/arxiv_control_charts</link>\n',
    '    </image>\n',
    '    <atom:link href="https://huggingface.co/spaces/fmegahed/arxiv_control_charts/resolve/main/data/weekly_digest.xml" rel="self" type="application/rss+xml"/>\n',
    digest_item,
    '\n  </channel>\n',
    '</rss>\n'
  )

  writeLines(rss_content, output_file)
  message(sprintf("  Weekly digest RSS saved to: %s", output_file))

  invisible(output_file)
}

#' Save digest data as JSON
#'
#' @param papers Data frame of papers
#' @param synthesis AI-generated synthesis
#' @param output_file Path to output JSON file
save_digest_json <- function(papers, synthesis, output_file = "data/weekly_digest.json") {

  # Get paper counts by track
  track_counts <- list(
    spc = sum(papers$track_id == "spc"),
    exp_design = sum(papers$track_id == "exp_design"),
    reliability = sum(papers$track_id == "reliability")
  )

  digest <- list(
    metadata = list(
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      week_start = format(Sys.Date() - 7, "%Y-%m-%d"),
      week_end = format(Sys.Date(), "%Y-%m-%d"),
      version = "3.1.0"
    ),
    summary = list(
      total_papers = nrow(papers),
      papers_by_track = track_counts
    ),
    synthesis = synthesis,
    papers = lapply(seq_len(nrow(papers)), function(i) {
      p <- papers[i, ]
      list(
        id = p$id,
        title = p$title,
        authors = p$authors,
        submitted = as.character(p$submitted_date),
        track = p$track_id,
        link = p$link_pdf
      )
    })
  )

  jsonlite::write_json(digest, output_file, pretty = TRUE, auto_unbox = TRUE)
  message(sprintf("  Weekly digest JSON saved to: %s", output_file))

  invisible(output_file)
}

# =============================================================================
# Main Execution
# =============================================================================

message("=============================================================================")
message("QE ArXiv Watch - Weekly Research Digest")
message("=============================================================================")
message(sprintf("Run date: %s (Monday)", format(Sys.Date(), "%B %d, %Y")))
message("")

# Step 1: Collect papers from the past week
message("Step 1: Collecting papers from the past 7 days...")
papers <- collect_weekly_papers(tracks, days_back = 7)

message("")
message(sprintf("Papers by track:"))
message(sprintf("  - Control Charts (SPC): %d", sum(papers$track_id == "spc")))
message(sprintf("  - Experimental Design: %d", sum(papers$track_id == "exp_design")))
message(sprintf("  - Reliability: %d", sum(papers$track_id == "reliability")))

# Step 2: Generate AI synthesis
message("")
message("Step 2: Generating AI synthesis...")
synthesis <- generate_synthesis(papers)

# Step 3: Generate outputs
message("")
message("Step 3: Generating output files...")
generate_weekly_rss(papers, synthesis)
save_digest_json(papers, synthesis)

message("")
message("=============================================================================")
message("Weekly digest generation complete!")
message("=============================================================================")
