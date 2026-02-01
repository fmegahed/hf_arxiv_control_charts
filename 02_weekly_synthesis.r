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
You are the voice behind "QE ArXiv Watch Weekly" — a research digest that
quality engineering folks actually look forward to reading. Think of yourself
as the colleague who browses arXiv so others don\'t have to, then shares the
highlights over coffee with genuine enthusiasm for clever methods.

YOUR READERS: Practicing quality engineers, reliability folks, academics in
stats/IE/OR, and grad students trying to keep up. They\'re smart but busy.

VOICE & STYLE:
- Write like you talk to a respected colleague — conversational but substantive
- Lead with what\'s interesting, not what\'s comprehensive
- Use "you" and "we" — this is a conversation, not a lecture
- Short paragraphs. Let the page breathe.
- It\'s okay to be excited about a clever result or skeptical about a claim
- Analogies and "imagine if..." make abstract ideas stick
- Skip the throat-clearing ("This paper presents...") — get to the point

STRUCTURE (adapt as needed):
Open with a hook — the single most interesting thing from this week\'s papers.
What would make someone lean in?

Then cover what matters:
- What problems are researchers tackling? Why should practitioners care?
- Any methods that are ready to try? Be specific about applicability.
- Connections between papers or to broader trends worth noting.

Close with a forward look or a question worth pondering.

WHAT TO AVOID:
- Bullet-point exhaustion (not everything needs a list)
- Equal coverage for every paper (highlight what\'s highlight-worthy)
- Jargon without payoff (if you use a term, make it earn its place)
- The phrase "This paper" at the start of sentences
- Summaries that could apply to any paper ("makes important contributions...")

LENGTH: 400-600 words. Tight and valuable beats long and thorough.

Remember: Nobody *has* to read this. Make it worth their time.
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
      credentials = get_openai_api_key
    )

    prompt <- paste0(
      "Please synthesize the following papers into our weekly digest. ",
      "Remember to follow the output format specified in your instructions.\n\n",
      papers_text
    )

    synthesis <- chat$chat(prompt)
    message("  Synthesis generated successfully")
    # Convert ellmer_output to plain character string for JSON serialization
    as.character(synthesis)

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
  # Split into lines for processing
  lines <- strsplit(markdown, "\n")[[1]]

  # Track state for list processing
  in_list <- FALSE
  result_lines <- character(0)

  for (i in seq_along(lines)) {
    line <- lines[i]

    # Check if this is a bullet point
    is_bullet <- grepl("^\\s*-\\s+", line)

    # Close list if we were in one and this line isn't a bullet
    if (in_list && !is_bullet && trimws(line) != "") {
      result_lines <- c(result_lines, "</ul>")
      in_list <- FALSE
    }

    # Process headers (must be at start of line)
    if (grepl("^###\\s+", line)) {
      line <- sub("^###\\s+(.+)$", "<h4>\\1</h4>", line)
    } else if (grepl("^##\\s+", line)) {
      line <- sub("^##\\s+(.+)$", "<h3>\\1</h3>", line)
    } else if (is_bullet) {
      # Start list if not already in one
      if (!in_list) {
        result_lines <- c(result_lines, "<ul>")
        in_list <- TRUE
      }
      # Convert bullet to list item
      line <- sub("^\\s*-\\s+(.+)$", "<li>\\1</li>", line)
    }

    result_lines <- c(result_lines, line)
  }

  # Close list if still open at end

  if (in_list) {
    result_lines <- c(result_lines, "</ul>")
  }

  # Rejoin lines
  html <- paste(result_lines, collapse = "\n")

  # Bold (non-greedy match)
  html <- gsub("\\*\\*([^*]+?)\\*\\*", "<strong>\\1</strong>", html)

  # Italic (non-greedy match, but not if it's a bold marker)
  html <- gsub("(?<!\\*)\\*([^*]+?)\\*(?!\\*)", "<em>\\1</em>", html, perl = TRUE)

  # Links
  html <- gsub("\\[([^]]+?)\\]\\(([^)]+?)\\)", "<a href=\"\\2\">\\1</a>", html)

  # LaTeX inline math: \(...\) -> readable format with common Unicode substitutions
  # RSS readers don't execute JavaScript, so we make LaTeX human-readable
  html <- gsub("\\\\\\((.+?)\\\\\\)", "<code>\\1</code>", html)

  # LaTeX display math: \[...\] -> code block

  html <- gsub("\\\\\\[(.+?)\\\\\\]", "<pre><code>\\1</code></pre>", html)

  # Common LaTeX to Unicode conversions for readability
  html <- gsub("\\\\sqrt\\{?([^}]*)\\}?", "\u221A(\\1)", html)  # sqrt -> √
  html <- gsub("\\\\approx", "\u2248", html)                    # approx -> ≈
  html <- gsub("\\\\times", "\u00D7", html)                     # times -> ×
  html <- gsub("\\\\pm", "\u00B1", html)                        # pm -> ±
  html <- gsub("\\\\leq", "\u2264", html)                       # leq -> ≤
  html <- gsub("\\\\geq", "\u2265", html)                       # geq -> ≥
  html <- gsub("\\\\neq", "\u2260", html)                       # neq -> ≠
  html <- gsub("\\\\infty", "\u221E", html)                     # infty -> ∞
  html <- gsub("\\\\alpha", "\u03B1", html)                     # alpha -> α
  html <- gsub("\\\\beta", "\u03B2", html)                      # beta -> β
  html <- gsub("\\\\gamma", "\u03B3", html)                     # gamma -> γ
  html <- gsub("\\\\delta", "\u03B4", html)                     # delta -> δ
  html <- gsub("\\\\sigma", "\u03C3", html)                     # sigma -> σ
  html <- gsub("\\\\mu", "\u03BC", html)                        # mu -> μ
  html <- gsub("\\\\lambda", "\u03BB", html)                    # lambda -> λ
  html <- gsub("\\\\pi", "\u03C0", html)                        # pi -> π
  html <- gsub("\\\\sum", "\u2211", html)                       # sum -> ∑
  html <- gsub("\\\\prod", "\u220F", html)                      # prod -> ∏
  html <- gsub("\\\\rightarrow", "\u2192", html)                # rightarrow -> →
  html <- gsub("\\\\leftarrow", "\u2190", html)                 # leftarrow -> ←
  html <- gsub("\\\\Rightarrow", "\u21D2", html)                # Rightarrow -> ⇒
  html <- gsub("\\\\in", "\u2208", html)                        # in -> ∈
  html <- gsub("\\\\subset", "\u2282", html)                    # subset -> ⊂
  html <- gsub("\\\\cap", "\u2229", html)                       # cap -> ∩
  html <- gsub("\\\\cup", "\u222A", html)                       # cup -> ∪

  # Convert double newlines to paragraph breaks (but not inside lists)
  # Split by double newline, wrap non-tag content in <p>
  paragraphs <- strsplit(html, "\n\n+")[[1]]
  processed <- sapply(paragraphs, function(p) {
    p <- trimws(p)
    if (p == "") return("")
    # Don't wrap if already a block element

    if (grepl("^<(h[1-6]|ul|ol|li|div|p|hr)", p)) {
      return(p)
    }
    # Don't wrap if it's just closing tags
    if (grepl("^</(ul|ol)>$", p)) {
      return(p)
    }
    paste0("<p>", p, "</p>")
  })

  html <- paste(processed, collapse = "\n\n")

  # Clean up any empty paragraphs
  html <- gsub("<p>\\s*</p>", "", html)

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
