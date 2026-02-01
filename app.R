library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)
library(plotly)
library(DT)
library(ellmer)
library(jsonlite)
library(commonmark)

# Ensuring that the API key is being read from the app's secrets; ellmer requires a zero input function
OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY")
get_openai_api_key = function(){return(OPENAI_API_KEY)}

# Color palette for charts
# First 4 colors are for topic analytics (NOT track colors)
# Track colors: Teal (#1b9e77) = Control Charts, Orange (#d95f02) = Experimental Design, Purple (#7570b3) = Reliability
CHART_COLORS <- c(
  "#e7298a", # Magenta (for topic charts)
  "#66a61e", # Green (for topic charts)
  "#e6ab02", # Mustard (for topic charts)
  "#a6761d", # Brown (for topic charts)
  "#1b9e77", # Teal (track: Control Charts)
  "#d95f02", # Orange (track: Experimental Design)
  "#7570b3", # Purple (track: Reliability)
  "#666666", # Gray
  "#4E79A7", # Blue
  "#F28E2B"  # Lighter orange
)

# ============================================================================
# Track Configuration
# ============================================================================

#' Load track configuration from JSON
load_tracks_config <- function() {
  if (file.exists("data/tracks.json")) {
    jsonlite::fromJSON("data/tracks.json", simplifyVector = FALSE)
  } else {
    # Fallback for backward compatibility
    list(
      spc = list(
        id = "spc",
        label = "Control Charts (SPC)",
        short_label = "SPC",
        query = '(ti:"control chart" OR abs:"control chart")',
        metadata_csv = "data/spc_arxiv_metadata.csv",
        factsheet_csv = "data/spc_factsheet.csv",
        relevance_field = "is_spc_paper",
        icon = "chart-bar",
        color = "#C41230",
        description = "Statistical Process Control and control chart research"
      )
    )
  }
}

# Load tracks config at startup
TRACKS_CONFIG <- load_tracks_config()

#' Get filter column mapping for a track
get_track_filter_columns <- function(track_id) {
  switch(track_id,
    spc = list(
      primary = "chart_family", primary_label = "Chart Family",
      secondary = "chart_statistic", secondary_label = "Statistical Method",
      tertiary = "phase", tertiary_label = "Phase"
    ),
    exp_design = list(
      primary = "design_type", primary_label = "Design Type",
      secondary = "design_objective", secondary_label = "Design Objective",
      tertiary = "optimality_criterion", tertiary_label = "Optimality Criterion"
    ),
    reliability = list(
      primary = "reliability_topic", primary_label = "Reliability Topic",
      secondary = "modeling_approach", secondary_label = "Modeling Approach",
      tertiary = "data_type", tertiary_label = "Data Type"
    ),
    # default fallback
    list(
      primary = "chart_family", primary_label = "Primary Category",
      secondary = "chart_statistic", secondary_label = "Secondary Category",
      tertiary = "phase", tertiary_label = "Tertiary Category"
    )
  )
}

# ============================================================================
# Helper Functions
# ============================================================================

parse_arxiv_date <- function(date_str) {
  as.Date(lubridate::ymd_hms(date_str))
}

#' Load data for a specific track
load_track_data <- function(track_id) {
  track <- TRACKS_CONFIG[[track_id]]
  if (is.null(track)) return(list(metadata = NULL, factsheet = NULL, track = NULL))

  metadata <- NULL
  factsheet <- NULL

  if (file.exists(track$metadata_csv)) {
    metadata <- readr::read_csv(track$metadata_csv, show_col_types = FALSE) |>
      dplyr::mutate(
        submitted_date = parse_arxiv_date(submitted),
        year = lubridate::year(submitted_date),
        month = lubridate::month(submitted_date),
        year_month = paste0(year, "-", sprintf("%02d", month))
      )
  }

  if (file.exists(track$factsheet_csv)) {
    factsheet <- readr::read_csv(track$factsheet_csv, show_col_types = FALSE)
  }

  list(metadata = metadata, factsheet = factsheet, track = track)
}

#' Get paper count for a track (for landing page)
get_track_paper_count <- function(track_id) {
  track <- TRACKS_CONFIG[[track_id]]
  if (is.null(track)) return(0)
  if (!file.exists(track$metadata_csv)) return(0)

  tryCatch({
    nrow(readr::read_csv(track$metadata_csv, show_col_types = FALSE))
  }, error = function(e) 0)
}

# Count pipe-delimited values, with option to exclude non-informative categories
count_pipe_delimited <- function(data, column_name, top_n = 10, exclude_other = FALSE) {
  if (is.null(data) || is.null(column_name) || !column_name %in% names(data)) return(NULL)

  all_values <- unlist(strsplit(data[[column_name]], "\\|"))
  all_values <- trimws(all_values)
  all_values <- all_values[all_values != "" & !is.na(all_values)]

  if (exclude_other) {
    # Exclude non-informative categories like "Other", "Not Applicable", "N/A", etc.
    exclude_patterns <- c("other", "others", "n/a", "na", "none", "not applicable",
                          "not specified", "unspecified", "unknown", "general")
    all_values <- all_values[!tolower(all_values) %in% exclude_patterns]
  }

  if (length(all_values) == 0) return(NULL)

  counts <- as.data.frame(sort(table(all_values), decreasing = TRUE))
  names(counts) <- c("category", "count")
  if (!is.null(top_n)) {
    counts <- head(counts, top_n)
  }
  counts$category <- as.character(counts$category)
  counts
}

# Get top category excluding "Other"
get_top_category <- function(data, column_name, max_chars = 20) {
  counts <- count_pipe_delimited(data, column_name, top_n = 1, exclude_other = TRUE)
  if (is.null(counts) || nrow(counts) == 0) return("N/A")
  top <- counts$category[1]
  if (nchar(top) > max_chars) top <- paste0(substr(top, 1, max_chars - 3), "...")
  top
}

# Create system prompt for paper chat
create_paper_system_prompt <- function(paper) {
  paste0(
    "You are an expert research assistant helping users understand this academic paper. ",
    "Answer questions accurately based on the PDF content provided. ",
    "When referencing specific findings, cite the relevant section. ",
    "If information is not in the paper, say so clearly. ",
    "Be concise but thorough.\n\n",
    "Paper metadata:\n",
    "Title: ", paper$title, "\n",
    "Authors: ", paper$authors, "\n",
    "Date: ", as.character(paper$submitted_date)
  )
}

# ============================================================================
# UI Definition
# ============================================================================

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),

  shiny::tags$head(
    shiny::tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "miami-theme.css"),
    shiny::tags$script(src = "personalization.js"),
    shiny::tags$title("QE ArXiv Watch"),

    # MathJax config MUST come before the MathJax loader
    shiny::tags$script(shiny::HTML("
    window.MathJax = {
      tex: {
        inlineMath: [['$','$'], ['\\\\(','\\\\)']],
        displayMath: [['$$','$$'], ['\\\\[','\\\\]']]
      },
      options: {
        skipHtmlTags: ['script','noscript','style','textarea','pre','code']
      }
    };
  ")),

    shiny::tags$script(
      src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js",
      async = NA
    ),
    
    # Re-typeset when Bootstrap modals open
    shiny::tags$script(shiny::HTML("
    $(document).on('shown.bs.modal', function () {
      if (window.MathJax && MathJax.typesetPromise) {
        MathJax.typesetPromise();
      }
    });
  ")),

    # Enter key handler for chat input
    shiny::tags$script(shiny::HTML("
    $(document).on('keydown', '#chat_input', function(e) {
      if (e.key === 'Enter' && !e.shiftKey) {
        e.preventDefault();
        $('#send_chat_msg').click();
      }
    });
  "))
  ),

  # ==========================================================================
  # Landing Page (shown when no track selected)
  # ==========================================================================
  shiny::conditionalPanel(
    condition = "output.show_landing",
    shiny::div(
      class = "landing-page",
      shiny::div(
        class = "landing-hero",
        shiny::tags$h1("QE ArXiv Watch"),
        shiny::tags$p(class = "landing-subtitle", "Analytics insights across timelines, topics, and authors, plus AI summaries, paper chat, and PDF access")
      ),
      shiny::div(
        class = "track-selector-container",
        shiny::tags$h2("Select a Research Track"),
        shiny::fluidRow(
          shiny::column(4,
            shiny::div(
              class = "track-card",
              id = "track_card_spc",
              style = "border: 3px solid #1b9e77;",
              onclick = "Shiny.setInputValue('select_track', 'spc', {priority: 'event'})",
              onmouseover = "this.style.boxShadow='0 12px 40px rgba(27, 158, 119, 0.3)'; this.style.transform='translateY(-8px)';",
              onmouseout = "this.style.boxShadow=''; this.style.transform='';",
              shiny::div(class = "track-card-icon", style = "color: #1b9e77;", shiny::icon("chart-line")),
              shiny::tags$h3(style = "color: #1b9e77;", "Control Charts"),
              shiny::tags$p("Statistical process monitoring and control charting research"),
              shiny::div(class = "track-card-count", style = "background: rgba(27, 158, 119, 0.15); border: 1px solid #1b9e77;",
                shiny::textOutput("spc_paper_count", inline = TRUE), " papers")
            )
          ),
          shiny::column(4,
            shiny::div(
              class = "track-card",
              id = "track_card_exp_design",
              style = "border: 3px solid #d95f02;",
              onclick = "Shiny.setInputValue('select_track', 'exp_design', {priority: 'event'})",
              onmouseover = "this.style.boxShadow='0 12px 40px rgba(217, 95, 2, 0.3)'; this.style.transform='translateY(-8px)';",
              onmouseout = "this.style.boxShadow=''; this.style.transform='';",
              shiny::div(class = "track-card-icon", style = "color: #d95f02;", shiny::icon("flask")),
              shiny::tags$h3(style = "color: #d95f02;", "Experimental Design"),
              shiny::tags$p("Design of experiments and response surface methodology research"),
              shiny::div(class = "track-card-count", style = "background: rgba(217, 95, 2, 0.15); border: 1px solid #d95f02;",
                shiny::textOutput("exp_design_paper_count", inline = TRUE), " papers")
            )
          ),
          shiny::column(4,
            shiny::div(
              class = "track-card",
              id = "track_card_reliability",
              style = "border: 3px solid #7570b3;",
              onclick = "Shiny.setInputValue('select_track', 'reliability', {priority: 'event'})",
              onmouseover = "this.style.boxShadow='0 12px 40px rgba(117, 112, 179, 0.3)'; this.style.transform='translateY(-8px)';",
              onmouseout = "this.style.boxShadow=''; this.style.transform='';",
              shiny::div(class = "track-card-icon", style = "color: #7570b3;", shiny::icon("cogs")),
              shiny::tags$h3(style = "color: #7570b3;", "Reliability Engineering"),
              shiny::tags$p("Reliability engineering, degradation modeling, and maintenance optimization"),
              shiny::div(class = "track-card-count", style = "background: rgba(117, 112, 179, 0.15); border: 1px solid #7570b3;",
                shiny::textOutput("reliability_paper_count", inline = TRUE), " papers")
            )
          )
        )
      ),
      # Footer on landing page
      shiny::div(
        class = "landing-footer",
        shiny::tags$p(
          shiny::tags$strong("Authors: "),
          "Fadel M. Megahed, Ying-Ju (Tessa) Chen, Allison Jones-Farmer, Ibrahim Yousif, and Inez M. Zwetsloot"
        ),
        shiny::div(
          class = "landing-logo-container",
          shiny::tags$img(src = "miami-logo.png", alt = "Miami University", style = "height: 45px;"),
          shiny::tags$img(src = "university-of-dayton-vector-logo.png", alt = "University of Dayton", style = "height: 40px;"),
          shiny::tags$img(src = "uva-compacte-logo.png", alt = "University of Amsterdam", style = "height: 40px;")
        )
      )
    )
  ),

  # ==========================================================================
  # Main App (shown after track selection)
  # ==========================================================================
  shiny::conditionalPanel(
    condition = "!output.show_landing",

    # Header with dynamic title and track-specific color
    shiny::uiOutput("dynamic_header"),

    # Logo container
    shiny::div(
      class = "logo-container",
      shiny::tags$img(src = "miami-logo.png", alt = "Miami University", style = "height: 55px;"),
      shiny::div(class = "logo-divider"),
      shiny::tags$img(src = "university-of-dayton-vector-logo.png", alt = "University of Dayton", style = "height: 50px;"),
      shiny::div(class = "logo-divider"),
      shiny::tags$img(src = "uva-compacte-logo.png", alt = "University of Amsterdam", style = "height: 50px;")
    ),

    # Main content
    shiny::div(
      class = "main-content",
      shiny::tabsetPanel(
        id = "main_tabs",
        type = "tabs",

      # =======================================================================
      # Tab 1: Overview
      # =======================================================================
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("dashboard"), " Overview"),
        value = "overview",
        shiny::div(
          class = "tab-content-wrapper",

          # About section (moved to top)
          shiny::div(
            class = "note-section",
            shiny::tags$h4(shiny::icon("info-circle"), " About"),
            shiny::uiOutput("about_text")
          ),

          shiny::hr(),

          # Key metrics row
          shiny::fluidRow(
            shiny::column(3, shiny::div(class = "stat-card",
              shiny::div(class = "stat-icon", shiny::icon("database")),
              shiny::div(class = "stat-value", shiny::textOutput("total_papers")),
              shiny::div(class = "stat-label", "Total Papers in Database")
            )),
            shiny::column(3, shiny::div(class = "stat-card",
              shiny::div(class = "stat-icon", shiny::icon("calendar-alt")),
              shiny::div(class = "stat-value", shiny::textOutput("date_range_text")),
              shiny::div(class = "stat-label", "Date Coverage")
            )),
            shiny::column(3, shiny::div(class = "stat-card",
              shiny::div(class = "stat-icon", shiny::icon("chart-bar")),
              shiny::div(class = "stat-value", shiny::textOutput("top_primary_category")),
              shiny::uiOutput("top_primary_label")
            )),
            shiny::column(3, shiny::div(class = "stat-card",
              shiny::div(class = "stat-icon", shiny::icon("industry")),
              shiny::div(class = "stat-value", shiny::textOutput("top_domain")),
              shiny::div(class = "stat-label", "Top Application Domain")
            ))
          ),

          shiny::hr(),

          # Research landscape with period selector
          shiny::fluidRow(
            shiny::column(
              8,
              shiny::div(
                class = "info-card",
                shiny::fluidRow(
                  shiny::column(8,
                    shiny::tags$h4(class = "section-heading", shiny::icon("globe"), " Research Landscape")
                  ),
                  shiny::column(4,
                    shiny::selectInput(
                      "landscape_period", NULL,
                      choices = c("Last 12 Months" = "12", "Last 24 Months" = "24",
                                  "Last 36 Months" = "36", "All Time" = "all"),
                      selected = "12", width = "100%"
                    )
                  )
                ),
                shiny::uiOutput("research_landscape")
              )
            ),
            shiny::column(
              4,
              shiny::div(
                class = "info-card",
                shiny::tags$h4(class = "section-heading", shiny::icon("chart-line"), " Publication Trend"),
                plotly::plotlyOutput("mini_trend", height = "200px")
              )
            )
          ),

          # Papers table
          shiny::div(
            class = "info-card",
            shiny::tags$h4(class = "section-heading", shiny::icon("table"), " All Papers"),
            DT::dataTableOutput("overview_papers_table")
          )
        )
      ),

      # =======================================================================
      # Tab 2: Timeline
      # =======================================================================
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("chart-area"), " Timeline"),
        value = "timeline",
        shiny::div(
          class = "tab-content-wrapper",

          # Main timeline - all time first
          shiny::div(
            class = "info-card",
            shiny::tags$h3(class = "section-heading", shiny::icon("calendar-alt"), " Publication History"),
            shiny::fluidRow(
              shiny::column(3,
                shiny::selectInput("timeline_agg", "View by:",
                  choices = c("Year" = "year", "Month" = "month"), selected = "year")
              ),
              shiny::column(6,
                shiny::dateRangeInput("timeline_range", "Date Range:",
                  start = as.Date("2008-01-01"), end = Sys.Date())
              ),
              shiny::column(3,
                shiny::checkboxInput("show_cumulative", "Show Cumulative", FALSE)
              )
            ),
            plotly::plotlyOutput("main_timeline", height = "400px")
          ),

          # Topic trends
          shiny::div(
            class = "info-card",
            shiny::tags$h3(class = "section-heading", shiny::icon("chart-line"), " Topic Trends Over Time"),
            shiny::fluidRow(
              shiny::column(4,
                shiny::uiOutput("trend_category_ui")
              ),
              shiny::column(8,
                shiny::uiOutput("trend_selector_ui")
              )
            ),
            plotly::plotlyOutput("topic_trends", height = "350px")
          )
        )
      ),

      # =======================================================================
      # Tab 3: Topic Analytics
      # =======================================================================
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("chart-bar"), " Topic Analytics"),
        value = "topics",
        shiny::div(
          class = "tab-content-wrapper",

          # Filter controls
          shiny::div(
            class = "info-card",
            shiny::tags$h4(class = "section-heading", shiny::icon("filter"), " Filter Papers"),
            # Global search
            shiny::fluidRow(
              shiny::column(12,
                shiny::textInput("global_search", NULL,
                  placeholder = "Search across title, authors, abstract, summary, key results, chart family, application domain...",
                  width = "100%")
              )
            ),
            shiny::fluidRow(
              shiny::column(3,
                shiny::uiOutput("filter_primary_ui")
              ),
              shiny::column(3,
                shiny::uiOutput("filter_secondary_ui")
              ),
              shiny::column(3,
                shiny::selectInput("filter_domain", "Application Domain:",
                  choices = c("All" = ""), selected = "")
              ),
              shiny::column(3,
                shiny::uiOutput("filter_tertiary_ui")
              )
            ),
            shiny::fluidRow(
              shiny::column(6,
                shiny::uiOutput("year_range_ui")
              ),
              shiny::column(3,
                shiny::textOutput("filter_summary")
              ),
              shiny::column(3,
                shiny::actionButton("clear_filters", shiny::tagList(shiny::icon("times"), " Clear Filters"),
                  class = "btn-info", style = "width: 100%;")
              )
            )
          ),

          # Charts row 1
          shiny::fluidRow(
            shiny::column(6,
              shiny::div(class = "info-card",
                shiny::uiOutput("primary_chart_heading"),
                plotly::plotlyOutput("chart_family_plot", height = "320px")
              )
            ),
            shiny::column(6,
              shiny::div(class = "info-card",
                shiny::uiOutput("secondary_chart_heading"),
                plotly::plotlyOutput("statistic_plot", height = "320px")
              )
            )
          ),

          # Charts row 2
          shiny::fluidRow(
            shiny::column(6,
              shiny::div(class = "info-card",
                shiny::uiOutput("domain_chart_heading"),
                plotly::plotlyOutput("domain_plot", height = "320px")
              )
            ),
            shiny::column(6,
              shiny::div(class = "info-card",
                shiny::uiOutput("tertiary_chart_heading"),
                plotly::plotlyOutput("phase_plot", height = "320px")
              )
            )
          ),

          # Filtered papers table
          shiny::div(
            class = "info-card",
            shiny::tags$h4(class = "section-heading", shiny::icon("table"), " Papers ",
              shiny::tags$small(shiny::textOutput("papers_count_inline", inline = TRUE))
            ),
            DT::dataTableOutput("filtered_papers_table")
          ),

          # Paper details panel (hidden by default)
          shiny::div(
            id = "paper_detail_panel",
            style = "display: none;",
            shiny::uiOutput("paper_detail_view")
          )
        )
      ),

      # =======================================================================
      # Tab 4: Author Analytics
      # =======================================================================
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("users"), " Author Analytics"),
        value = "author_analytics",
        shiny::div(
          class = "tab-content-wrapper",

          shiny::fluidRow(
            shiny::column(6,
              shiny::div(class = "info-card",
                shiny::tags$h4(class = "section-heading", shiny::icon("user-graduate"), " Top Authors"),
                plotly::plotlyOutput("top_authors_plot", height = "400px")
              )
            ),
            shiny::column(6,
              shiny::div(class = "info-card",
                shiny::tags$h4(class = "section-heading", shiny::icon("users"), " Team Size Distribution"),
                plotly::plotlyOutput("team_size_plot", height = "400px")
              )
            )
          ),

          shiny::div(
            class = "info-card",
            shiny::tags$h4(class = "section-heading", shiny::icon("search"), " Author Lookup"),
            shiny::fluidRow(
              shiny::column(4,
                shiny::selectizeInput("author_select", "Select Author:",
                  choices = NULL, options = list(placeholder = "Type to search authors..."))
              ),
              shiny::column(8, shiny::uiOutput("author_info"))
            ),
            DT::dataTableOutput("author_papers")
          )
        )
      ),

      # =======================================================================
      # Tab 5: Paper Deep Dive
      # =======================================================================
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("microscope"), " Paper Deep Dive"),
        value = "deep_dive",
        shiny::div(
          class = "tab-content-wrapper",

          # Paper selection with filters
          shiny::div(
            class = "info-card",
            shiny::tags$h4(class = "section-heading", shiny::icon("filter"), " Select Paper"),
            shiny::fluidRow(
              shiny::column(3,
                shiny::selectInput("deep_dive_year", "Filter by Year:",
                  choices = c("All Years" = ""), selected = "")
              ),
              shiny::column(4,
                shiny::selectizeInput("deep_dive_author", "Filter by Author:",
                  choices = NULL, options = list(placeholder = "Type to search authors..."))
              ),
              shiny::column(5,
                shiny::selectizeInput("deep_dive_select", "Select Paper:",
                  choices = NULL, width = "100%",
                  options = list(placeholder = "Type to search papers..."))
              )
            )
          ),

          # Paper details
          shiny::div(
            class = "info-card",
            shiny::uiOutput("deep_dive_content")
          ),

          # Chat section (integrated)
          shiny::div(
            class = "info-card",
            shiny::tags$h4(class = "section-heading", shiny::icon("comments"), " Chat with Paper"),
            shiny::uiOutput("chat_status"),
            shiny::div(id = "chat_box",
              style = "height: 350px; overflow-y: auto; border: 1px solid #CCC9B8; border-radius: 6px; padding: 15px; background: #FAFAFA; margin-bottom: 15px;",
              shiny::div(id = "chat_thinking", style = "display: none; padding: 10px;",
                shiny::div(class = "chat-thinking-indicator",
                  shiny::tags$span(class = "typing-dot"),
                  shiny::tags$span(class = "typing-dot"),
                  shiny::tags$span(class = "typing-dot")
                )
              ),
              shiny::uiOutput("chat_history")
            ),
            shiny::div(
              shiny::tags$h5("Quick Questions:"),
              shiny::uiOutput("quick_questions")
            ),
            shiny::div(
              style = "display: flex; gap: 10px; align-items: flex-end; margin-top: 15px;",
              shiny::div(style = "flex: 1;",
                shiny::textAreaInput("chat_input", NULL,
                  placeholder = "Ask a question about this paper...", rows = 2, width = "100%")
              ),
              shiny::actionButton("send_chat_msg", shiny::icon("paper-plane"),
                class = "btn-primary", style = "height: 60px; min-width: 60px;")
            )
          )
        )
      ),

      # =======================================================================
      # Tab 6: My Library
      # =======================================================================
      shiny::tabPanel(
        title = shiny::tagList(shiny::icon("bookmark"), " My Library"),
        value = "my_library",
        shiny::div(
          class = "tab-content-wrapper",

          # Header with actions
          shiny::tags$div(
            class = "library-header",
            shiny::tags$h4(shiny::icon("star"), " Bookmarked Papers"),
            shiny::tags$div(
              class = "library-actions",
              shiny::downloadButton("download_bibtex", shiny::tagList(shiny::icon("file-export"), " Export BibTeX"),
                class = "btn-info"),
              shiny::actionButton("chat_all_papers", shiny::tagList(shiny::icon("comments"), " Chat with Collection"),
                class = "btn-primary")
            )
          ),

          # Info text
          shiny::tags$p(class = "help-block",
            "Bookmarks are stored in your browser's local storage. They work across all research tracks and persist between sessions."),

          # Bookmarked papers table
          DT::dataTableOutput("bookmarked_papers_table"),

          # RSS Feed Section - Weekly Digest Only
          shiny::div(
            class = "rss-section",
            shiny::tags$h4(shiny::icon("rss"), " Weekly Research Digest"),
            shiny::tags$p("Subscribe to receive a curated weekly summary of new quality engineering research with AI-synthesized insights."),

            shiny::div(class = "rss-feed-card", style = "max-width: 600px;",
              shiny::tags$h5(shiny::icon("newspaper"), " QE ArXiv Watch Weekly"),
              shiny::tags$p("Every Monday, receive our AI-generated synthesis of the week's new papers across Control Charts, Experimental Design, and Reliability Engineering. Our AI analyzes each paper to highlight key contributions, emerging trends, and practical implications for researchers and practitioners."),
              shiny::div(class = "rss-feed-url",
                shiny::tags$span(id = "weekly_feed_url", "https://huggingface.co/spaces/fmegahed/arxiv_control_charts/resolve/main/data/weekly_digest.xml"),
                shiny::tags$button(class = "btn btn-sm btn-info",
                  onclick = "navigator.clipboard.writeText(document.getElementById('weekly_feed_url').textContent); QEPersonalization.showToast('URL copied!', 'success');",
                  shiny::icon("copy"), " Copy URL")
              )
            ),

            # RSS Help
            shiny::div(style = "margin-top: 20px;",
              shiny::tags$span(class = "rss-help-tooltip",
                shiny::icon("question-circle"), " What is RSS?",
                shiny::tags$span(class = "tooltip-content",
                  "RSS (Really Simple Syndication) lets you subscribe to updates from websites. ",
                  "Copy the feed URL above and paste it into your RSS reader app to receive our weekly digest automatically."
                )
              )
            ),

            # Feed reader links
            shiny::tags$p(class = "help-block feed-reader-links", style = "margin-top: 15px;",
              "Popular RSS readers: ",
              shiny::tags$a(href = "https://feedly.com", target = "_blank", "Feedly"),
              " | ",
              shiny::tags$a(href = "https://www.inoreader.com", target = "_blank", "Inoreader"),
              " | ",
              shiny::tags$a(href = "https://netnewswire.com", target = "_blank", "NetNewsWire"),
              " (Mac/iOS) | ",
              shiny::tags$a(href = "https://newsblur.com", target = "_blank", "NewsBlur")
            )
          )
        )
      )

      )
    ),

    # Footer (inside conditional panel for main app)
    shiny::div(
      class = "app-footer",
      shiny::p(
        "Built with ", shiny::tags$a(href = "https://shiny.posit.co/", "Shiny"),
        " | Data source: ", shiny::tags$a(href = "https://arxiv.org/", "ArXiv")
      )
    )
  )  # End conditionalPanel for main app
)

# ============================================================================
# Server Logic
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # Reactive State
  # ==========================================================================

  selected_track <- shiny::reactiveVal(NULL)
  papers_data <- shiny::reactiveVal(NULL)
  factsheet_data <- shiny::reactiveVal(NULL)
  track_config <- shiny::reactiveVal(NULL)
  chat_messages <- shiny::reactiveVal(list())

  # Filter columns for current track
  filter_cols <- shiny::reactive({
    tid <- selected_track()
    if (is.null(tid)) return(NULL)
    get_track_filter_columns(tid)
  })

  # ==========================================================================
  # Landing Page Logic
  # ==========================================================================

  # Show landing page when no track selected
  output$show_landing <- shiny::reactive({
    is.null(selected_track())
  })
  shiny::outputOptions(output, "show_landing", suspendWhenHidden = FALSE)

  # Paper counts for landing page cards
  output$spc_paper_count <- shiny::renderText({
    format(get_track_paper_count("spc"), big.mark = ",")
  })

  output$exp_design_paper_count <- shiny::renderText({
    format(get_track_paper_count("exp_design"), big.mark = ",")
  })

  output$reliability_paper_count <- shiny::renderText({
    format(get_track_paper_count("reliability"), big.mark = ",")
  })

  # Track selection handler
  shiny::observeEvent(input$select_track, {
    track_id <- input$select_track
    if (is.null(track_id) || track_id == "") return()

    # Load data for selected track
    data <- load_track_data(track_id)

    selected_track(track_id)
    papers_data(data$metadata)
    factsheet_data(data$factsheet)
    track_config(data$track)

    # Reset chat messages
    chat_messages(list())
  })

  # Switch track button handler
  shiny::observeEvent(input$change_track, {
    selected_track(NULL)
    papers_data(NULL)
    factsheet_data(NULL)
    track_config(NULL)
    chat_messages(list())
  })

  # Dynamic header with track-specific colors

  output$dynamic_header <- shiny::renderUI({
    track <- track_config()

    # Default to Miami red if no track selected
    if (is.null(track) || is.null(track$color)) {
      header_color <- "#C41230"
      header_color_dark <- "#9E0F28"
      title_text <- "QE ArXiv Watch"
      subtitle_text <- "Analytics insights across timelines, topics, and authors, plus AI summaries, paper chat, and PDF access"
    } else {
      header_color <- track$color
      # Create darker shade for gradient
      header_color_dark <- switch(track$id,
        spc = "#147a5c",
        exp_design = "#a64a02",
        reliability = "#5a558a",
        header_color
      )
      title_text <- paste0("QE ArXiv Watch: ", track$label)
      subtitle_text <- track$description
    }

    # Dynamic CSS for active tabs to match track color
    tab_css <- shiny::tags$style(shiny::HTML(paste0("
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: ", header_color, " !important;
        color: #FFFFFF !important;
      }
      .nav-tabs > li > a:hover {
        color: ", header_color, " !important;
      }
      .section-heading {
        color: ", header_color, " !important;
      }
      .info-card h4 {
        color: ", header_color, " !important;
      }
      .stat-icon {
        color: ", header_color, " !important;
      }
      .stat-value {
        color: ", header_color, " !important;
      }
    ")))

    shiny::tagList(
      tab_css,
      shiny::div(
        class = "app-header",
        style = paste0("background: linear-gradient(135deg, ", header_color, " 0%, ", header_color_dark, " 100%);"),
        shiny::div(
          class = "header-content",
          shiny::div(
            class = "header-left",
            shiny::tags$h1(title_text),
            shiny::tags$p(class = "subtitle", subtitle_text)
          ),
          shiny::div(
            class = "header-right",
            shiny::actionButton("change_track", shiny::tagList(shiny::icon("exchange-alt"), " Switch Track"),
              class = "btn-header"),
            shiny::tags$p(style = "margin-top: 8px;", "Version 3.1.0 | February 2026")
          )
        )
      )
    )
  })

  # Combined data
  combined_data <- shiny::reactive({
    meta <- papers_data()
    fact <- factsheet_data()
    if (is.null(meta) || is.null(fact)) return(NULL)
    dplyr::left_join(meta, fact, by = "id")
  })

  # Filtered data based on topic filters
  filtered_data <- shiny::reactive({
    data <- combined_data()
    if (is.null(data)) return(NULL)

    # Global free text search
    search_term <- input$global_search
    # Use dynamic column names based on track
    cols <- filter_cols()
    primary_col_name <- if (!is.null(cols)) cols$primary else "chart_family"

    if (!is.null(search_term) && trimws(search_term) != "") {
      search_term <- tolower(trimws(search_term))
      # Search common fields plus track-specific primary category
      data <- data |> dplyr::filter(
        grepl(search_term, tolower(title), fixed = TRUE) |
        grepl(search_term, tolower(authors), fixed = TRUE) |
        grepl(search_term, tolower(ifelse(is.na(abstract), "", abstract)), fixed = TRUE) |
        grepl(search_term, tolower(ifelse(is.na(summary), "", summary)), fixed = TRUE) |
        grepl(search_term, tolower(ifelse(is.na(key_results), "", key_results)), fixed = TRUE) |
        grepl(search_term, tolower(ifelse(is.na(.data[[primary_col_name]]), "", .data[[primary_col_name]])), fixed = TRUE) |
        grepl(search_term, tolower(ifelse(is.na(application_domain), "", application_domain)), fixed = TRUE)
      )
    }
    if (!is.null(cols)) {
      # Primary filter (chart_family for SPC, design_type for DOE, etc.)
      if (!is.null(input$filter_chart_family) && input$filter_chart_family != "") {
        primary_col <- cols$primary
        if (primary_col %in% names(data)) {
          data <- data |> dplyr::filter(grepl(input$filter_chart_family, .data[[primary_col]], fixed = TRUE))
        }
      }

      # Secondary filter (chart_statistic for SPC, design_objective for DOE, etc.)
      if (!is.null(input$filter_statistic) && input$filter_statistic != "") {
        secondary_col <- cols$secondary
        if (secondary_col %in% names(data)) {
          data <- data |> dplyr::filter(grepl(input$filter_statistic, .data[[secondary_col]], fixed = TRUE))
        }
      }

      # Tertiary filter (phase for SPC, optimality_criterion for DOE, etc.)
      if (!is.null(input$filter_phase) && input$filter_phase != "") {
        tertiary_col <- cols$tertiary
        if (tertiary_col %in% names(data)) {
          data <- data |> dplyr::filter(grepl(input$filter_phase, .data[[tertiary_col]], fixed = TRUE))
        }
      }
    }

    # Domain filter (same for all tracks)
    if (!is.null(input$filter_domain) && input$filter_domain != "") {
      data <- data |> dplyr::filter(grepl(input$filter_domain, application_domain, fixed = TRUE))
    }

    # Year range filter
    if (!is.null(input$filter_year_range) && length(input$filter_year_range) == 2) {
      min_year <- input$filter_year_range[1]
      max_year <- input$filter_year_range[2]
      data <- data |> dplyr::filter(year >= min_year & year <= max_year)
    }

    data
  })

  # ===========================================================================
  # Overview Tab
  # ===========================================================================
  
  # About text with dynamic dates
  output$about_text <- shiny::renderUI({
    data <- papers_data()
    
    # Compute "data current as of" based on UTC time
    # If UTC hour >= 11, use today; otherwise use yesterday
    utc_now <- lubridate::with_tz(Sys.time(), "UTC")
    utc_hour <- lubridate::hour(utc_now)
    data_current_date <- if (utc_hour >= 11) {
      as.Date(utc_now)
    } else {
      as.Date(utc_now) - 1
    }
    data_current_str <- format(data_current_date, "%B %d, %Y")
    
    # Get latest arxiv paper submission date
    latest_paper_str <- "N/A"
    if (!is.null(data) && nrow(data) > 0) {
      latest_date <- max(data$submitted_date, na.rm = TRUE)
      latest_paper_str <- format(latest_date, "%B %d, %Y")
    }
    
    track <- track_config()
    track_label <- if (!is.null(track)) track$label else "research"
    track_query <- if (!is.null(track)) track$query else ""

    shiny::p(
      "This dashboard monitors ", shiny::tags$strong(track_label), " research from ArXiv using the query: ",
      shiny::tags$code(track_query),
      ". Each paper is analyzed using LLM extraction to identify methodology and findings. ",
      "Data current as of ", shiny::tags$strong(data_current_str), ". ",
      "Latest arXiv paper included is from ", shiny::tags$strong(latest_paper_str), "."
    )
  })
  
  output$total_papers <- shiny::renderText({
    data <- papers_data()
    if (is.null(data)) return("0")
    format(nrow(data), big.mark = ",")
  })

  output$date_range_text <- shiny::renderText({
    data <- papers_data()
    if (is.null(data)) return("N/A")
    range_dates <- range(data$submitted_date, na.rm = TRUE)
    paste0(format(range_dates[1], "%Y"), " - ", format(range_dates[2], "%Y"))
  })

  output$top_primary_category <- shiny::renderText({
    cols <- filter_cols()
    if (is.null(cols)) return("N/A")
    get_top_category(combined_data(), cols$primary, 15)
  })

  output$top_primary_label <- shiny::renderUI({
    cols <- filter_cols()
    label <- if (!is.null(cols)) paste0("Most Common ", cols$primary_label) else "Most Common Category"
    shiny::div(class = "stat-label", label)
  })

  output$top_domain <- shiny::renderText({
    get_top_category(combined_data(), "application_domain", 15)
  })

  output$research_landscape <- shiny::renderUI({
    data <- combined_data()
    if (is.null(data)) return(shiny::p("Loading..."))

    # Filter by period
    period <- if (!is.null(input$landscape_period)) input$landscape_period else "all"
    if (period != "all") {
      months <- as.numeric(period)
      cutoff <- Sys.Date() - (months * 30)
      data <- data |> dplyr::filter(submitted_date >= cutoff)
    }

    n_papers <- nrow(data)
    if (n_papers == 0) return(shiny::p("No papers in selected period."))

    cols <- filter_cols()
    track_id <- selected_track()
    period_label <- if (period == "all") "all time" else paste0("the last ", period, " months")

    # Common stats
    with_code <- sum(data$code_availability_source %in% c("GitHub", "Personal website", "Supplementary material"), na.rm = TRUE)

    # Build track-specific summary
    if (track_id == "spc") {
      # SPC-specific stats
      families <- count_pipe_delimited(data, "chart_family", NULL, TRUE)
      univariate <- sum(families$count[grepl("Univariate", families$category, ignore.case = TRUE)])
      multivariate <- sum(families$count[grepl("Multivariate", families$category, ignore.case = TRUE)])
      phase_both <- sum(data$phase == "Both", na.rm = TRUE)
      phase_i <- sum(data$phase == "Phase I", na.rm = TRUE)
      phase_ii <- sum(data$phase == "Phase II", na.rm = TRUE)
      nonparametric <- sum(data$assumes_normality == FALSE, na.rm = TRUE)

      shiny::tagList(
        shiny::tags$p(style = "line-height: 1.8;",
          "In ", shiny::tags$strong(period_label, .noWS = "after"), ", the database contains ",
          shiny::tags$strong(n_papers), " papers."
        ),
        shiny::tags$p(style = "line-height: 1.8;",
          shiny::tags$strong("Chart Types: "), univariate, " univariate, and ", multivariate, " multivariate methods."
        ),
        shiny::tags$p(style = "line-height: 1.8;",
          shiny::tags$strong("SPC Phases: "), phase_i, " Phase I, ", phase_ii, " Phase II, and ", phase_both, " both."
        ),
        shiny::tags$p(style = "line-height: 1.8;",
          shiny::tags$strong("Methods: "), nonparametric, " nonparametric/distribution-free, ", with_code, " with available code."
        )
      )
    } else if (track_id == "exp_design") {
      # DOE-specific stats
      designs <- count_pipe_delimited(data, "design_type", NULL, TRUE)
      top_designs <- head(designs, 3)
      objectives <- count_pipe_delimited(data, "design_objective", NULL, TRUE)
      top_objectives <- head(objectives, 3)

      shiny::tagList(
        shiny::tags$p(style = "line-height: 1.8;",
          "In ", shiny::tags$strong(period_label, .noWS = "after"), ", the database contains ",
          shiny::tags$strong(n_papers), " papers."
        ),
        shiny::tags$p(style = "line-height: 1.8;",
          shiny::tags$strong("Top Design Types: "),
          paste(paste0(top_designs$category, " (", top_designs$count, ")"), collapse = ", ")
        ),
        shiny::tags$p(style = "line-height: 1.8;",
          shiny::tags$strong("Top Objectives: "),
          paste(paste0(top_objectives$category, " (", top_objectives$count, ")"), collapse = ", ")
        ),
        shiny::tags$p(style = "line-height: 1.8;",
          shiny::tags$strong("Code Availability: "), with_code, " papers with available code."
        )
      )
    } else if (track_id == "reliability") {
      # Reliability-specific stats
      topics <- count_pipe_delimited(data, "reliability_topic", NULL, TRUE)
      top_topics <- head(topics, 3)
      approaches <- count_pipe_delimited(data, "modeling_approach", NULL, TRUE)
      top_approaches <- head(approaches, 3)

      shiny::tagList(
        shiny::tags$p(style = "line-height: 1.8;",
          "In ", shiny::tags$strong(period_label, .noWS = "after"), ", the database contains ",
          shiny::tags$strong(n_papers), " papers."
        ),
        shiny::tags$p(style = "line-height: 1.8;",
          shiny::tags$strong("Top Topics: "),
          paste(paste0(top_topics$category, " (", top_topics$count, ")"), collapse = ", ")
        ),
        shiny::tags$p(style = "line-height: 1.8;",
          shiny::tags$strong("Top Approaches: "),
          paste(paste0(top_approaches$category, " (", top_approaches$count, ")"), collapse = ", ")
        ),
        shiny::tags$p(style = "line-height: 1.8;",
          shiny::tags$strong("Code Availability: "), with_code, " papers with available code."
        )
      )
    } else {
      # Generic fallback
      primary_col <- if (!is.null(cols)) cols$primary else "chart_family"
      primaries <- count_pipe_delimited(data, primary_col, NULL, TRUE)
      top_primaries <- head(primaries, 3)

      shiny::tagList(
        shiny::tags$p(style = "line-height: 1.8;",
          "In ", shiny::tags$strong(period_label, .noWS = "after"), ", the database contains ",
          shiny::tags$strong(n_papers), " papers."
        ),
        shiny::tags$p(style = "line-height: 1.8;",
          shiny::tags$strong("Top Categories: "),
          paste(paste0(top_primaries$category, " (", top_primaries$count, ")"), collapse = ", ")
        ),
        shiny::tags$p(style = "line-height: 1.8;",
          shiny::tags$strong("Code Availability: "), with_code, " papers with available code."
        )
      )
    }
  })

  output$mini_trend <- plotly::renderPlotly({
    data <- papers_data()
    if (is.null(data)) return(NULL)

    # Get track color
    track <- track_config()
    track_color <- if (!is.null(track) && !is.null(track$color)) track$color else "#1b9e77"

    yearly <- data |> dplyr::count(year) |> dplyr::arrange(year)

    plotly::plot_ly(yearly, x = ~year, y = ~n, type = "bar", marker = list(color = track_color)) |>
      plotly::layout(
        xaxis = list(title = "", dtick = 2),
        yaxis = list(title = "Papers"),
        margin = list(l = 40, r = 10, t = 10, b = 40)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # Overview papers table
  output$overview_papers_table <- DT::renderDataTable({
    data <- combined_data()
    if (is.null(data)) return(NULL)

    # Parse first author from pipe-delimited authors
    parse_first_author <- function(authors_str) {
      if (is.na(authors_str) || authors_str == "") return("Unknown")
      first <- strsplit(as.character(authors_str), "\\|")[[1]][1]
      trimws(first)
    }

    # Extract short arXiv ID from full ID
    extract_short_id <- function(full_id) {
      if (is.na(full_id)) return("")
      # Extract just the numeric part (e.g., "2501.12345" from "http://arxiv.org/abs/2501.12345v1")
      match <- regmatches(full_id, regexpr("[0-9]+\\.[0-9]+", full_id))
      if (length(match) > 0) match else full_id
    }

    display <- data |>
      dplyr::arrange(desc(submitted_date)) |>
      dplyr::mutate(
        `Submitted` = format(submitted_date, "%Y-%m-%d"),
        `Title` = title,
        `First Author` = sapply(authors, parse_first_author),
        short_id = sapply(id, extract_short_id),
        ID = short_id,
        `Bookmark` = paste0('<span class="bookmark-icon" data-paper-id="', id, '" onclick="QEPersonalization.toggleBookmark(\'', id, '\')"><i class="fa fa-star-o"></i></span>'),
        `PDF` = paste0('<a href="', link_pdf, '" target="_blank" class="pdf-btn"><i class="fa fa-file-pdf"></i> ', short_id, '</a>'),
        `AI Summary` = paste0('<button class="summary-btn" onclick="Shiny.setInputValue(\'overview_summary_id\', \'', id, '\', {priority: \'event\'})"><i class="fa fa-robot"></i> View</button>')
      ) |>
      dplyr::select(ID, `Bookmark`, `Submitted`, `Title`, `First Author`, `PDF`, `AI Summary`)

    DT::datatable(display, escape = FALSE, selection = "none",
      options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
      class = "display compact")
  })

  # Modal for AI Summary from Overview table
  shiny::observeEvent(input$overview_summary_id, {
    pid <- input$overview_summary_id
    if (is.null(pid) || pid == "") return()

    data <- combined_data()
    paper <- data |> dplyr::filter(id == pid) |> dplyr::slice(1)
    if (nrow(paper) == 0) return()

    # Build modal content
    modal_content <- list(
      shiny::tags$p(shiny::tags$strong("Authors: "), paper$authors),
      shiny::tags$p(shiny::tags$strong("Date: "), as.character(paper$submitted_date)),
      shiny::hr()
    )

    if (!is.na(paper$summary) && paper$summary != "") {
      modal_content <- c(modal_content, list(
        shiny::tags$h5(shiny::icon("robot"), " AI Summary"),
        shiny::tags$div(class='math-content', style = "background: #EDECE2; padding: 15px; border-radius: 6px; margin-bottom: 15px;", paper$summary)
      ))
    }

    if (!is.na(paper$key_results) && paper$key_results != "") {
      modal_content <- c(modal_content, list(
        shiny::tags$h5(shiny::icon("chart-line"), " Key Results"),
        shiny::tags$div(style = "background: #F5FFF5; padding: 15px; border-radius: 6px; margin-bottom: 15px;", paper$key_results)
      ))
    }

    if (!is.na(paper$key_equations) && paper$key_equations != "") {
      modal_content <- c(modal_content, list(
        shiny::tags$h5(shiny::icon("square-root-alt"), " Key Equations"),
        shiny::tags$div(class = "math-content", style = "background: #F5F5FF; padding: 15px; border-radius: 6px;",
          shiny::HTML(paper$key_equations))
      ))
    }

    modal_content <- c(modal_content, list(
      shiny::tags$div(style = "margin-top: 15px;",
        shiny::tags$a(href = paper$link_pdf, target = "_blank", class = "btn btn-primary",
          shiny::icon("file-pdf"), " View PDF on ArXiv"))
    ))

    shiny::showModal(shiny::modalDialog(
      title = shiny::tags$div(style = "color: #C41230;", paper$title),
      size = "l",
      easyClose = TRUE,
      shiny::tagList(modal_content),
      footer = shiny::modalButton("Close")
    ))
  })

  # ===========================================================================
  # Timeline Tab
  # ===========================================================================

  output$main_timeline <- plotly::renderPlotly({
    data <- papers_data()
    if (is.null(data)) return(NULL)
    if (is.null(input$timeline_range) || length(input$timeline_range) < 2) return(NULL)

    data <- data |> dplyr::filter(
      submitted_date >= input$timeline_range[1],
      submitted_date <= input$timeline_range[2]
    )

    if (nrow(data) == 0) return(NULL)

    agg_mode <- if (!is.null(input$timeline_agg)) input$timeline_agg else "year"
    if (agg_mode == "year") {
      plot_data <- data |> dplyr::count(year) |> dplyr::arrange(year)
      x_var <- ~year
      x_title <- "Year"
    } else {
      plot_data <- data |> dplyr::count(year_month) |> dplyr::arrange(year_month)
      x_var <- ~year_month
      x_title <- "Month"
    }

    # Get track color for timeline charts
    track <- track_config()
    track_color <- if (!is.null(track) && !is.null(track$color)) track$color else "#1b9e77"

    if (isTRUE(input$show_cumulative)) {
      plot_data$cumulative <- cumsum(plot_data$n)

      plotly::plot_ly(plot_data, x = x_var) |>
        plotly::add_bars(y = ~n, name = "Per Period", marker = list(color = track_color)) |>
        plotly::add_lines(y = ~cumulative, name = "Cumulative", yaxis = "y2",
          line = list(color = "#EFDB72", width = 3)) |>
        plotly::layout(
          xaxis = list(title = x_title, tickangle = -45),
          yaxis = list(title = "Papers per Period"),
          yaxis2 = list(
            title = list(text = "Cumulative", standoff = 20),
            overlaying = "y",
            side = "right",
            automargin = TRUE
          ),
          legend = list(x = 0.02, y = 0.98),
          hovermode = "x unified",
          margin = list(r = 80)
        ) |>
        plotly::config(displayModeBar = FALSE)
    } else {
      plotly::plot_ly(plot_data, x = x_var, y = ~n, type = "scatter", mode = "lines+markers",
        fill = "tozeroy", line = list(color = track_color, width = 2),
        marker = list(color = track_color, size = 8), fillcolor = paste0(track_color, "33")) |>
        plotly::layout(
          xaxis = list(title = x_title, tickangle = -45),
          yaxis = list(title = "Number of Papers"),
          hovermode = "x unified"
        ) |>
        plotly::config(displayModeBar = FALSE)
    }
  })

  output$trend_selector_ui <- shiny::renderUI({
    data <- combined_data()
    if (is.null(data)) return(NULL)

    cat <- input$trend_category
    if (is.null(cat) || cat == "") return(NULL)
    counts <- count_pipe_delimited(data, cat, 10, TRUE)
    if (is.null(counts)) return(NULL)

    choices <- counts$category
    shiny::selectizeInput("trend_topics", "Select topics (max 5):", choices = choices,
      selected = head(choices, 3), multiple = TRUE, options = list(maxItems = 5))
  })

  output$topic_trends <- plotly::renderPlotly({
    data <- combined_data()
    topics <- input$trend_topics
    cat <- input$trend_category
    if (is.null(data) || is.null(topics) || length(topics) == 0) return(NULL)
    if (is.null(cat) || cat == "" || !cat %in% names(data)) return(NULL)

    trend_list <- lapply(topics, function(topic) {
      filtered <- data |> dplyr::filter(grepl(topic, .data[[cat]], fixed = TRUE))
      if (nrow(filtered) == 0) return(NULL)
      filtered |> dplyr::count(year) |> dplyr::mutate(topic = topic)
    })

    trend_df <- dplyr::bind_rows(trend_list)
    if (nrow(trend_df) == 0) return(NULL)

    p <- plotly::plot_ly()
    for (i in seq_along(topics)) {
      topic_data <- trend_df |> dplyr::filter(topic == topics[i])
      p <- p |> plotly::add_trace(data = topic_data, x = ~year, y = ~n, type = "scatter",
        mode = "lines+markers", name = topics[i],
        line = list(color = CHART_COLORS[i], width = 2),
        marker = list(color = CHART_COLORS[i], size = 8))
    }

    p |> plotly::layout(
      xaxis = list(title = "Year", dtick = 1),
      yaxis = list(title = "Papers"),
      legend = list(orientation = "h", y = -0.15),
      hovermode = "x unified"
    ) |> plotly::config(displayModeBar = FALSE)
  })

  # ===========================================================================
  # Topic Analytics Tab
  # ===========================================================================

  # ---------------------------------------------------------------------------
  # Dynamic Filter UI Elements
  # ---------------------------------------------------------------------------

  # Primary filter dropdown (chart_family for SPC, design_type for DOE, etc.)
  output$filter_primary_ui <- shiny::renderUI({
    cols <- filter_cols()
    data <- combined_data()
    if (is.null(cols) || is.null(data)) {
      return(shiny::selectInput("filter_chart_family", "Category:",
        choices = c("All" = ""), selected = ""))
    }

    primary_col <- cols$primary
    choices <- c("All" = "")
    if (primary_col %in% names(data)) {
      counts <- count_pipe_delimited(data, primary_col, NULL, TRUE)
      if (!is.null(counts)) {
        choices <- c("All" = "", setNames(counts$category, paste0(counts$category, " (", counts$count, ")")))
      }
    }

    shiny::selectInput("filter_chart_family", paste0(cols$primary_label, ":"),
      choices = choices, selected = "")
  })

  # Secondary filter dropdown (chart_statistic for SPC, design_objective for DOE, etc.)
  output$filter_secondary_ui <- shiny::renderUI({
    cols <- filter_cols()
    data <- combined_data()
    if (is.null(cols) || is.null(data)) {
      return(shiny::selectInput("filter_statistic", "Category:",
        choices = c("All" = ""), selected = ""))
    }

    secondary_col <- cols$secondary
    choices <- c("All" = "")
    if (secondary_col %in% names(data)) {
      counts <- count_pipe_delimited(data, secondary_col, NULL, TRUE)
      if (!is.null(counts)) {
        choices <- c("All" = "", setNames(counts$category, paste0(counts$category, " (", counts$count, ")")))
      }
    }

    shiny::selectInput("filter_statistic", paste0(cols$secondary_label, ":"),
      choices = choices, selected = "")
  })

  # Tertiary filter dropdown (phase for SPC, optimality_criterion for DOE, etc.)
  output$filter_tertiary_ui <- shiny::renderUI({
    cols <- filter_cols()
    data <- combined_data()
    if (is.null(cols) || is.null(data)) {
      return(shiny::selectInput("filter_phase", "Category:",
        choices = c("All" = ""), selected = ""))
    }

    tertiary_col <- cols$tertiary
    choices <- c("All" = "")
    if (tertiary_col %in% names(data)) {
      counts <- count_pipe_delimited(data, tertiary_col, NULL, TRUE)
      if (!is.null(counts)) {
        choices <- c("All" = "", setNames(counts$category, paste0(counts$category, " (", counts$count, ")")))
      }
    }

    shiny::selectInput("filter_phase", paste0(cols$tertiary_label, ":"),
      choices = choices, selected = "")
  })

  # Year range slider
  output$year_range_ui <- shiny::renderUI({
    data <- combined_data()
    if (is.null(data) || nrow(data) == 0) {
      return(shiny::sliderInput("filter_year_range", "Year Range:",
        min = 2000, max = 2025, value = c(2000, 2025), step = 1, sep = ""))
    }

    years <- data$year[!is.na(data$year)]
    if (length(years) == 0) {
      return(shiny::sliderInput("filter_year_range", "Year Range:",
        min = 2000, max = 2025, value = c(2000, 2025), step = 1, sep = ""))
    }

    min_year <- min(years)
    max_year <- max(years)

    shiny::sliderInput("filter_year_range", "Year Range:",
      min = min_year, max = max_year, value = c(min_year, max_year),
      step = 1, sep = "", width = "100%")
  })

  # Update domain filter choices (same for all tracks)
  shiny::observe({
    data <- combined_data()
    if (is.null(data)) return()

    dom <- count_pipe_delimited(data, "application_domain", NULL, TRUE)
    if (!is.null(dom)) {
      shiny::updateSelectInput(session, "filter_domain",
        choices = c("All" = "", setNames(dom$category, paste0(dom$category, " (", dom$count, ")"))))
    }
  })

  # ---------------------------------------------------------------------------
  # Dynamic Chart Headings
  # ---------------------------------------------------------------------------

  # Helper to create chart heading with multi-value note
  make_chart_heading <- function(label) {
    shiny::tagList(
      shiny::tags$h4(class = "section-heading",
        paste0(label, " Distribution"),
        shiny::tags$span(
          class = "chart-info-icon",
          title = "More than one category can apply to a single paper, so adding up the counts across the chart would be incorrect.",
          shiny::icon("info-circle")
        )
      ),
      shiny::tags$p(class = "chart-note", "More than one category can be present in a paper")
    )
  }

  output$primary_chart_heading <- shiny::renderUI({
    cols <- filter_cols()
    label <- if (!is.null(cols)) cols$primary_label else "Primary Category"
    make_chart_heading(label)
  })

  output$secondary_chart_heading <- shiny::renderUI({
    cols <- filter_cols()
    label <- if (!is.null(cols)) cols$secondary_label else "Secondary Category"
    make_chart_heading(label)
  })

  output$tertiary_chart_heading <- shiny::renderUI({
    cols <- filter_cols()
    label <- if (!is.null(cols)) cols$tertiary_label else "Tertiary Category"
    make_chart_heading(label)
  })

  output$domain_chart_heading <- shiny::renderUI({
    make_chart_heading("Application Domain")
  })

  # ---------------------------------------------------------------------------
  # Dynamic Timeline Category Dropdown
  # ---------------------------------------------------------------------------

  output$trend_category_ui <- shiny::renderUI({
    cols <- filter_cols()
    if (is.null(cols)) {
      return(shiny::selectInput("trend_category", "Category:",
        choices = c("Application Domain" = "application_domain")))
    }

    choices <- c(
      setNames(cols$primary, cols$primary_label),
      setNames(cols$secondary, cols$secondary_label),
      "Application Domain" = "application_domain"
    )

    shiny::selectInput("trend_category", "Category:", choices = choices)
  })

  # Clear filters
  shiny::observeEvent(input$clear_filters, {
    shiny::updateTextInput(session, "global_search", value = "")
    shiny::updateSelectInput(session, "filter_chart_family", selected = "")
    shiny::updateSelectInput(session, "filter_statistic", selected = "")
    shiny::updateSelectInput(session, "filter_domain", selected = "")
    shiny::updateSelectInput(session, "filter_phase", selected = "")

    # Reset year range to full range
    data <- combined_data()
    if (!is.null(data) && nrow(data) > 0) {
      years <- data$year[!is.na(data$year)]
      if (length(years) > 0) {
        shiny::updateSliderInput(session, "filter_year_range",
          value = c(min(years), max(years)))
      }
    }
  })

  # Filter summary
  output$filter_summary <- shiny::renderText({
    data <- combined_data()
    total <- nrow(data)
    filtered <- nrow(filtered_data())
    if (is.null(total) || is.null(filtered)) return("")

    filters <- c()
    search_term <- if (!is.null(input$global_search)) trimws(input$global_search) else ""
    if (search_term != "") filters <- c(filters, paste0("Search: '", search_term, "'"))
    if (!is.null(input$filter_chart_family) && input$filter_chart_family != "") filters <- c(filters, input$filter_chart_family)
    if (!is.null(input$filter_statistic) && input$filter_statistic != "") filters <- c(filters, input$filter_statistic)
    if (!is.null(input$filter_domain) && input$filter_domain != "") filters <- c(filters, input$filter_domain)
    if (!is.null(input$filter_phase) && input$filter_phase != "") filters <- c(filters, input$filter_phase)

    # Check if year filter is applied (not full range)
    if (!is.null(input$filter_year_range) && length(input$filter_year_range) == 2 && !is.null(data)) {
      years <- data$year[!is.na(data$year)]
      if (length(years) > 0) {
        data_min <- min(years)
        data_max <- max(years)
        if (input$filter_year_range[1] > data_min || input$filter_year_range[2] < data_max) {
          filters <- c(filters, paste0("Years: ", input$filter_year_range[1], "-", input$filter_year_range[2]))
        }
      }
    }

    if (length(filters) == 0) {
      paste0("Showing all ", total, " papers")
    } else {
      paste0("Showing ", filtered, " of ", total, " papers | Filters: ", paste(filters, collapse = ", "))
    }
  })

  output$papers_count_inline <- shiny::renderText({
    n <- nrow(filtered_data())
    if (is.null(n)) return("")
    paste0("(", n, " papers)")
  })

  # Helper for bar charts in Topic Analytics - excludes "Other" and "Not Applicable"
  make_bar_chart <- function(data, col_name, color) {
    # exclude_other = TRUE to filter out non-informative categories
    counts <- count_pipe_delimited(data, col_name, 10, exclude_other = TRUE)
    if (is.null(counts)) return(NULL)

    plotly::plot_ly(counts, x = ~count, y = ~reorder(category, count), type = "bar",
      orientation = "h", marker = list(color = color),
      text = ~paste0(count, " papers"), hoverinfo = "text") |>
      plotly::layout(
        xaxis = list(title = "Papers"),
        yaxis = list(title = "", ticksuffix = "  "),
        margin = list(l = 150)
      ) |>
      plotly::config(displayModeBar = FALSE)
  }

  output$chart_family_plot <- plotly::renderPlotly({
    cols <- filter_cols()
    col_name <- if (!is.null(cols)) cols$primary else "chart_family"
    make_bar_chart(filtered_data(), col_name, CHART_COLORS[1])
  })

  output$statistic_plot <- plotly::renderPlotly({
    cols <- filter_cols()
    col_name <- if (!is.null(cols)) cols$secondary else "chart_statistic"
    make_bar_chart(filtered_data(), col_name, CHART_COLORS[2])
  })

  output$domain_plot <- plotly::renderPlotly({
    make_bar_chart(filtered_data(), "application_domain", CHART_COLORS[3])
  })

  output$phase_plot <- plotly::renderPlotly({
    data <- filtered_data()
    cols <- filter_cols()
    if (is.null(data) || is.null(cols)) return(NULL)

    tertiary_col <- cols$tertiary
    if (!tertiary_col %in% names(data)) return(NULL)

    # Use the dynamic tertiary column
    make_bar_chart(data, tertiary_col, CHART_COLORS[4])
  })

  # Papers table
  output$filtered_papers_table <- DT::renderDataTable({
    data <- filtered_data()
    cols <- filter_cols()
    if (is.null(data) || is.null(cols)) return(NULL)

    # Get column names for this track
    primary_col <- cols$primary
    secondary_col <- cols$secondary

    # Safely extract column values
    get_col_value <- function(row, col_name, max_len = 30) {
      if (!col_name %in% names(data)) return("N/A")
      val <- row[[col_name]]
      if (is.na(val)) return("N/A")
      substr(as.character(val), 1, max_len)
    }

    display <- data |>
      dplyr::arrange(desc(submitted_date)) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        ID = id,
        Bookmark = paste0('<span class="bookmark-icon" data-paper-id="', id, '" onclick="QEPersonalization.toggleBookmark(\'', id, '\')"><i class="fa fa-star-o"></i></span>'),
        PDF = paste0('<a href="', link_pdf, '" target="_blank" class="pdf-btn"><i class="fa fa-file-pdf"></i></a>'),
        Summary = paste0('<button class="summary-btn" onclick="Shiny.setInputValue(\'topic_summary_id\', \'', id, '\', {priority: \'event\'})"><i class="fa fa-robot"></i></button>'),
        Title = title,
        Year = year,
        Primary = get_col_value(dplyr::cur_data(), primary_col, 30),
        Secondary = get_col_value(dplyr::cur_data(), secondary_col, 25),
        Domain = ifelse(is.na(application_domain), "N/A", substr(application_domain, 1, 25))
      ) |>
      dplyr::ungroup() |>
      dplyr::select(ID, Bookmark, PDF, Summary, Title, Year, Primary, Secondary, Domain)

    # Rename columns based on track
    names(display)[names(display) == "Primary"] <- cols$primary_label
    names(display)[names(display) == "Secondary"] <- cols$secondary_label

    DT::datatable(display, escape = FALSE, selection = "single",
      options = list(pageLength = 10, scrollX = TRUE, columnDefs = list(
        list(visible = FALSE, targets = 0)  # Hide ID column
      )),
      class = "display compact")
  })

  # Modal for AI Summary from Topic Analytics table
  shiny::observeEvent(input$topic_summary_id, {
    pid <- input$topic_summary_id
    if (is.null(pid) || pid == "") return()

    data <- combined_data()
    paper <- data |> dplyr::filter(id == pid) |> dplyr::slice(1)
    if (nrow(paper) == 0) return()

    # Build modal content
    modal_content <- list(
      shiny::tags$p(shiny::tags$strong("Authors: "), paper$authors),
      shiny::tags$p(shiny::tags$strong("Date: "), as.character(paper$submitted_date)),
      shiny::hr()
    )

    if (!is.na(paper$summary) && paper$summary != "") {
      modal_content <- c(modal_content, list(
        shiny::tags$h5(shiny::icon("robot"), " AI Summary"),
        shiny::tags$div(style = "background: #EDECE2; padding: 15px; border-radius: 6px; margin-bottom: 15px;", paper$summary)
      ))
    }

    if (!is.na(paper$key_results) && paper$key_results != "") {
      modal_content <- c(modal_content, list(
        shiny::tags$h5(shiny::icon("chart-line"), " Key Results"),
        shiny::tags$div(style = "background: #F5FFF5; padding: 15px; border-radius: 6px; margin-bottom: 15px;", paper$key_results)
      ))
    }

    if (!is.na(paper$key_equations) && paper$key_equations != "") {
      modal_content <- c(modal_content, list(
        shiny::tags$h5(shiny::icon("square-root-alt"), " Key Equations"),
        shiny::tags$div(class = "math-content", style = "background: #F5F5FF; padding: 15px; border-radius: 6px;",
          shiny::HTML(paper$key_equations))
      ))
    }

    modal_content <- c(modal_content, list(
      shiny::tags$div(style = "margin-top: 15px;",
        shiny::tags$a(href = paper$link_pdf, target = "_blank", class = "btn btn-primary",
          shiny::icon("file-pdf"), " View PDF on ArXiv"))
    ))

    shiny::showModal(shiny::modalDialog(
      title = shiny::tags$div(style = "color: #C41230;", paper$title),
      size = "l",
      easyClose = TRUE,
      shiny::tagList(modal_content),
      footer = shiny::modalButton("Close")
    ))
  })

  # Paper detail view when row selected
  shiny::observeEvent(input$filtered_papers_table_rows_selected, {
    idx <- input$filtered_papers_table_rows_selected
    if (is.null(idx) || length(idx) == 0) {
      shinyjs::hide("paper_detail_panel")
      return()
    }

    data <- filtered_data() |> dplyr::arrange(desc(submitted_date))
    paper <- data[idx, ]

    output$paper_detail_view <- shiny::renderUI({
      # Build content list
      content <- list(
        shiny::tags$h4(style = "color: #C41230;", paper$title),
        shiny::tags$p(shiny::tags$strong("Authors: "), paper$authors),
        shiny::tags$p(
          shiny::tags$strong("Date: "), as.character(paper$submitted_date), " | ",
          shiny::tags$strong("ArXiv: "), paper$id
        ),
        shiny::hr()
      )

      # AI Summary
      if (!is.na(paper$summary) && paper$summary != "") {
        content <- c(content, list(
          shiny::tags$h5(shiny::icon("robot"), " AI Summary"),
          shiny::tags$p(style = "background: #EDECE2; padding: 15px; border-radius: 6px;", paper$summary)
        ))
      }

      # Key Results
      if (!is.na(paper$key_results) && paper$key_results != "") {
        content <- c(content, list(
          shiny::tags$h5(shiny::icon("chart-line"), " Key Results"),
          shiny::tags$p(style = "background: #F5FFF5; padding: 15px; border-radius: 6px;", paper$key_results)
        ))
      }

      # Metadata and PDF link - use track-specific labels
      cols <- filter_cols()
      primary_col <- if (!is.null(cols)) cols$primary else "category1"
      secondary_col <- if (!is.null(cols)) cols$secondary else "category2"
      tertiary_col <- if (!is.null(cols)) cols$tertiary else "category3"
      primary_label <- if (!is.null(cols)) cols$primary_label else "Primary"
      secondary_label <- if (!is.null(cols)) cols$secondary_label else "Secondary"
      tertiary_label <- if (!is.null(cols)) cols$tertiary_label else "Tertiary"

      primary_val <- if (primary_col %in% names(paper)) paper[[primary_col]] else NA
      secondary_val <- if (secondary_col %in% names(paper)) paper[[secondary_col]] else NA
      tertiary_val <- if (tertiary_col %in% names(paper)) paper[[tertiary_col]] else NA

      content <- c(content, list(
        shiny::tags$div(
          style = "margin-top: 15px; padding: 15px; background: #F5F5F5; border-radius: 6px;",
          shiny::fluidRow(
            shiny::column(4, shiny::tags$strong(paste0(primary_label, ": ")), ifelse(is.na(primary_val), "N/A", primary_val)),
            shiny::column(4, shiny::tags$strong(paste0(secondary_label, ": ")), ifelse(is.na(secondary_val), "N/A", secondary_val)),
            shiny::column(4, shiny::tags$strong(paste0(tertiary_label, ": ")), ifelse(is.na(tertiary_val), "N/A", tertiary_val))
          )
        ),
        shiny::tags$div(
          style = "margin-top: 15px;",
          shiny::tags$a(href = paper$link_pdf, target = "_blank", class = "btn btn-primary",
            shiny::icon("file-pdf"), " View PDF on ArXiv")
        )
      ))

      shiny::div(class = "paper-detail-card", shiny::tagList(content))
    })

    shinyjs::show("paper_detail_panel")
  })

  # ===========================================================================
  # Author Analytics Tab
  # ===========================================================================

  all_authors <- shiny::reactive({
    data <- papers_data()
    if (is.null(data)) return(NULL)

    authors_list <- lapply(seq_len(nrow(data)), function(i) {
      auths <- unlist(strsplit(as.character(data$authors[i]), "\\|"))
      auths <- trimws(auths)
      auths <- auths[auths != "" & !is.na(auths)]
      if (length(auths) == 0) return(NULL)
      data.frame(author = auths, paper_id = data$id[i], title = data$title[i],
        date = data$submitted_date[i], stringsAsFactors = FALSE)
    })
    dplyr::bind_rows(authors_list)
  })

  output$top_authors_plot <- plotly::renderPlotly({
    auth_data <- all_authors()
    if (is.null(auth_data)) return(NULL)

    # Get track color
    track <- track_config()
    track_color <- if (!is.null(track) && !is.null(track$color)) track$color else "#1b9e77"

    top <- auth_data |> dplyr::count(author) |> dplyr::arrange(desc(n)) |> head(15)

    plotly::plot_ly(top, x = ~n, y = ~reorder(author, n), type = "bar",
      orientation = "h", marker = list(color = track_color)) |>
      plotly::layout(xaxis = list(title = "Papers"), yaxis = list(title = "", ticksuffix = "  "),
        margin = list(l = 180)) |>
      plotly::config(displayModeBar = FALSE)
  })

  output$team_size_plot <- plotly::renderPlotly({
    data <- papers_data()
    if (is.null(data)) return(NULL)

    sizes <- sapply(strsplit(as.character(data$authors), "\\|"), length)
    size_df <- data.frame(size = sizes) |> dplyr::count(size)

    plotly::plot_ly(size_df, x = ~size, y = ~n, type = "bar",
      marker = list(color = "#EFDB72")) |>
      plotly::layout(xaxis = list(title = "Authors per Paper", dtick = 1),
        yaxis = list(title = "Papers")) |>
      plotly::config(displayModeBar = FALSE)
  })

  shiny::observe({
    auth_data <- all_authors()
    if (is.null(auth_data)) return()

    counts <- auth_data |> dplyr::count(author) |> dplyr::arrange(desc(n))
    choices <- setNames(counts$author, paste0(counts$author, " (", counts$n, ")"))
    shiny::updateSelectizeInput(session, "author_select", choices = choices, server = TRUE)
  })

  output$author_info <- shiny::renderUI({
    auth <- input$author_select
    if (is.null(auth) || auth == "") return(NULL)

    auth_data <- all_authors()
    papers <- auth_data |> dplyr::filter(author == auth)
    n <- nrow(papers)
    years <- range(papers$date, na.rm = TRUE)

    shiny::tags$div(
      style = "background: #EDECE2; padding: 15px; border-radius: 6px;",
      shiny::tags$strong(auth), shiny::tags$br(),
      paste0(n, " papers (", format(years[1], "%Y"), "-", format(years[2], "%Y"), ")")
    )
  })

  output$author_papers <- DT::renderDataTable({
    auth <- input$author_select
    if (is.null(auth) || auth == "") return(NULL)

    auth_data <- all_authors()
    paper_ids <- auth_data |> dplyr::filter(author == auth) |> dplyr::pull(paper_id)

    data <- papers_data()
    papers <- data |>
      dplyr::filter(id %in% paper_ids) |>
      dplyr::arrange(desc(submitted_date)) |>
      dplyr::mutate(
        Title = paste0('<a href="', link_pdf, '" target="_blank">', title, '</a>'),
        Year = year
      ) |>
      dplyr::select(Title, Year)

    DT::datatable(papers, escape = FALSE, options = list(pageLength = 5, dom = "tip"))
  })

  # ===========================================================================
  # Paper Deep Dive Tab
  # ===========================================================================

  # Chat session management for ellmer
  chat_session <- shiny::reactiveVal(NULL)
  current_chat_paper_id <- shiny::reactiveVal(NULL)

  # Filtered papers for deep dive based on year and author
  deep_dive_papers <- shiny::reactive({
    data <- combined_data()
    if (is.null(data)) return(NULL)

    # Filter by year
    year_filter <- input$deep_dive_year
    if (!is.null(year_filter) && year_filter != "") {
      data <- data |> dplyr::filter(year == as.numeric(year_filter))
    }

    # Filter by author
    author_filter <- input$deep_dive_author
    if (!is.null(author_filter) && author_filter != "") {
      data <- data |> dplyr::filter(grepl(author_filter, authors, fixed = TRUE))
    }

    data
  })

  # Update year filter choices
  shiny::observe({
    data <- combined_data()
    if (is.null(data)) return()

    years <- sort(unique(data$year), decreasing = TRUE)
    shiny::updateSelectInput(session, "deep_dive_year",
      choices = c("All Years" = "", setNames(years, years)))
  })

  # Update author filter choices
  shiny::observe({
    auth_data <- all_authors()
    if (is.null(auth_data)) return()

    counts <- auth_data |> dplyr::count(author) |> dplyr::arrange(desc(n))
    choices <- c("All authors" = "", setNames(counts$author, paste0(counts$author, " (", counts$n, ")")))
    shiny::updateSelectizeInput(session, "deep_dive_author", choices = choices, server = TRUE)
    
  })

  # Update paper selector based on filters - show FULL titles
  shiny::observe({
    data <- deep_dive_papers()
    if (is.null(data) || nrow(data) == 0) {
      shiny::updateSelectizeInput(session, "deep_dive_select", choices = character(0), server = TRUE)
      return()
    }

    # Use full titles (no truncation)
    choices <- setNames(data$id, data$title)
    shiny::updateSelectizeInput(session, "deep_dive_select", choices = choices, server = TRUE)
  })

  # Store current paper data for chat
  current_paper <- shiny::reactiveVal(NULL)

  # Paper selection handler - initialize ellmer chat
  shiny::observeEvent(input$deep_dive_select, {
    pid <- input$deep_dive_select
    if (is.null(pid) || pid == "") {
      chat_session(NULL)
      current_chat_paper_id(NULL)
      current_paper(NULL)
      chat_messages(list())
      return()
    }

    # Check if same paper - skip if so
    if (!is.null(current_chat_paper_id()) && current_chat_paper_id() == pid) {
      return()
    }

    # Reset chat for new paper
    chat_messages(list())
    current_chat_paper_id(pid)

    data <- combined_data()
    paper <- data |> dplyr::filter(id == pid) |> dplyr::slice(1)
    if (nrow(paper) == 0) {
      chat_session(NULL)
      current_paper(NULL)
      return()
    }

    # Store paper data
    current_paper(paper)

    # Create ellmer chat session with system prompt
    tryCatch({
      system_prompt <- create_paper_system_prompt(paper)
      chat <- ellmer::chat_openai(
        model = "gpt-5-mini-2025-08-07",
        system_prompt = system_prompt,
        credentials = get_openai_api_key
      )
      chat_session(chat)
    }, error = function(e) {
      chat_session(NULL)
      shiny::showNotification(
        paste("Could not initialize chat:", e$message),
        type = "warning", duration = 5
      )
    })
  })

  # Chat status indicator
  output$chat_status <- shiny::renderUI({
    pid <- input$deep_dive_select
    chat <- chat_session()

    if (is.null(pid) || pid == "") {
      return(shiny::div(class = "chat-status chat-status-loading",
        shiny::icon("info-circle"), " Select a paper to start chatting"))
    }

    if (is.null(chat)) {
      return(shiny::div(class = "chat-status chat-status-loading",
        shiny::icon("spinner", class = "fa-spin"), " Loading paper for chat..."))
    }

    shiny::div(class = "chat-status chat-status-ready",
      shiny::icon("check-circle"), " Chat ready - ask questions about this paper")
  })

  output$deep_dive_content <- shiny::renderUI({
    pid <- input$deep_dive_select
    if (is.null(pid) || pid == "") return(shiny::p(class = "help-block", "Select a paper using the filters above."))

    data <- combined_data()
    paper <- data |> dplyr::filter(id == pid) |> dplyr::slice(1)
    if (nrow(paper) == 0) return(NULL)

    make_section <- function(title, content, icon_name, bg, is_math = FALSE) {
      if (is.na(content) || content == "") return(NULL)
      # Apply math-content class to AI Summary and Key Equations sections for consistent MathJax rendering
      content_div <- if (is_math || title == "AI Summary") {
        shiny::tags$div(class = "math-content", style = paste0("padding: 15px; background: ", bg, "; border-radius: 6px;"),
          shiny::HTML(commonmark::markdown_html(content)))
      } else {
        shiny::tags$div(style = paste0("padding: 15px; background: ", bg, "; border-radius: 6px;"), content)
      }
      shiny::tags$div(style = "margin-bottom: 15px;",
        shiny::tags$h5(shiny::icon(icon_name), " ", title),
        content_div
      )
    }

    shiny::tagList(
      shiny::tags$div(class = "paper-detail-card",
        shiny::tags$h4(style = "color: #C41230;", paper$title),
        shiny::tags$p(shiny::tags$strong("Authors: "), paper$authors),
        shiny::tags$p(shiny::tags$strong("Date: "), as.character(paper$submitted_date)),
        shiny::hr(),
        make_section("AI Summary", paper$summary, "robot", "#EDECE2"),
        make_section("Key Results", paper$key_results, "chart-line", "#F5FFF5"),
        make_section("Key Equations", paper$key_equations, "square-root-alt", "#F5F5FF", is_math = TRUE),
        shiny::tags$a(href = paper$link_pdf, target = "_blank", class = "btn btn-primary",
          shiny::icon("file-pdf"), " View PDF on ArXiv")
      )
    )
  })

  # Trigger MathJax after Deep Dive content renders
  shiny::observe({
    paper <- current_paper()
    shiny::req(paper)
    # Delay to ensure DOM is ready
    shinyjs::delay(100, {
      shinyjs::runjs("if (window.MathJax && MathJax.typesetPromise) { MathJax.typesetPromise(); }")
    })
  })

  # Quick questions
  output$quick_questions <- shiny::renderUI({
    pid <- input$deep_dive_select
    if (is.null(pid) || pid == "") return(NULL)

    questions <- c("What is the main contribution?", "Explain the methodology",
                   "What are the key equations?", "What are the limitations?",
                   "What future work is suggested?", "Compare to related methods")

    shiny::tagList(
      lapply(questions, function(q) {
        shiny::tags$button(class = "btn btn-sm quick-question-btn",
          onclick = paste0("Shiny.setInputValue('quick_q', '", q, "', {priority: 'event'});"), q)
      })
    )
  })

  shiny::observeEvent(input$quick_q, {
    shiny::updateTextAreaInput(session, "chat_input", value = input$quick_q)
  })

  # Chat history display
  output$chat_history <- shiny::renderUI({
    msgs <- chat_messages()
    if (length(msgs) == 0) {
      return(shiny::div(style = "text-align: center; color: #999; padding: 50px;",
        shiny::icon("comments", style = "font-size: 2em;"),
        shiny::tags$p("Select a paper and ask a question.")))
    }

    shiny::tagList(
      lapply(msgs, function(m) {
        if (m$role == "user") {
          shiny::div(style = "text-align: right; margin-bottom: 10px;",
            shiny::div(style = "display: inline-block; background: #C41230; color: white; padding: 10px 15px; border-radius: 15px 15px 0 15px; max-width: 75%;",
              m$content))
        } else {
          shiny::div(style = "text-align: left; margin-bottom: 10px;",
            shiny::div(class = "chat-response", style = "display: inline-block; background: #EDECE2; padding: 10px 15px; border-radius: 15px 15px 15px 0; max-width: 75%;",
              shiny::HTML(commonmark::markdown_html(m$content))))
        }
      })
    )
  })

  # Send chat message handler - uses ellmer with PDF support
  shiny::observeEvent(input$send_chat_msg, {
    chat <- chat_session()
    paper <- current_paper()
    msg <- trimws(input$chat_input)
    if (is.null(chat) || is.null(paper) || msg == "") return()

    # Check if this is the first message (need to include PDF)
    msgs <- chat_messages()
    is_first_message <- length(msgs) == 0

    # Add user message to display
    msgs <- append(msgs, list(list(role = "user", content = msg)))
    chat_messages(msgs)

    # Clear input
    shiny::updateTextAreaInput(session, "chat_input", value = "")

    # Show thinking indicator
    shinyjs::show("chat_thinking")

    # Get LLM response
    tryCatch({
      if (is_first_message) {
        # First message: include PDF via content_pdf_url
        response <- chat$chat(
          msg,
          ellmer::content_pdf_url(paper$link_pdf)
        )
      } else {
        # Subsequent messages: ellmer maintains history automatically
        response <- chat$chat(msg)
      }

      msgs <- chat_messages()
      msgs <- append(msgs, list(list(role = "assistant", content = response)))
      chat_messages(msgs)
    }, error = function(e) {
      msgs <- chat_messages()
      msgs <- append(msgs, list(list(role = "assistant",
        content = paste("<em>Error getting response:</em>", e$message))))
      chat_messages(msgs)
    })

    # Hide thinking indicator
    shinyjs::hide("chat_thinking")

    # Re-typeset MathJax for any equations in response
    shinyjs::runjs("if (window.MathJax && MathJax.typesetPromise) { MathJax.typesetPromise(); }")

    # Scroll to bottom
    shinyjs::runjs("document.getElementById('chat_box').scrollTop = document.getElementById('chat_box').scrollHeight;")
  })

  # ===========================================================================
  # My Library Tab
  # ===========================================================================

  # Collection chat system prompt (track-agnostic)
  COLLECTION_SYSTEM_PROMPT <- paste0(
    "You are a research synthesis assistant helping analyze a collection of academic papers. ",
    "Your expertise spans quality engineering domains including:\n",
    "- Statistical Process Control (SPC) and control charts\n",
    "- Design of Experiments (DOE) and experimental design\n",
    "- Reliability engineering and lifetime analysis\n\n",
    "When analyzing this collection, you should:\n",
    "1. Identify common themes, methodologies, and theoretical frameworks\n",
    "2. Compare and contrast approaches across papers\n",
    "3. Highlight complementary findings and potential contradictions\n",
    "4. Suggest research gaps or future directions\n",
    "5. Synthesize key takeaways for practitioners\n\n",
    "Be specific when referencing papers - cite by title or first author.\n",
    "Use clear, accessible language suitable for both researchers and practitioners."
  )

  # Reactive for collection chat session
  collection_chat_session <- shiny::reactiveVal(NULL)
  collection_chat_messages <- shiny::reactiveVal(list())
  collection_first_message <- shiny::reactiveVal(TRUE)
  collection_papers <- shiny::reactiveVal(NULL)

  # Bookmarked papers table
  output$bookmarked_papers_table <- DT::renderDataTable({
    bookmarks <- input$personalization_bookmarks
    if (is.null(bookmarks) || length(bookmarks) == 0) {
      return(DT::datatable(
        data.frame(Message = "No bookmarked papers yet. Click the star icon on any paper to bookmark it."),
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE,
        selection = "none"
      ))
    }

    # Get data from all tracks
    all_data <- list()
    for (track_id in names(TRACKS_CONFIG)) {
      track_data <- load_track_data(track_id)
      if (!is.null(track_data$metadata) && !is.null(track_data$factsheet)) {
        combined <- dplyr::left_join(track_data$metadata, track_data$factsheet, by = "id")
        combined$track <- track_id
        all_data[[track_id]] <- combined
      }
    }

    data <- dplyr::bind_rows(all_data)
    if (nrow(data) == 0) return(NULL)

    papers <- data |> dplyr::filter(id %in% bookmarks)
    if (nrow(papers) == 0) {
      return(DT::datatable(
        data.frame(Message = "No bookmarked papers found in current data."),
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE,
        selection = "none"
      ))
    }

    # Parse first author
    parse_first_author <- function(authors_str) {
      if (is.na(authors_str) || authors_str == "") return("Unknown")
      first <- strsplit(as.character(authors_str), "\\|")[[1]][1]
      trimws(first)
    }

    # Extract short arXiv ID
    extract_short_id <- function(full_id) {
      if (is.na(full_id)) return("")
      match <- regmatches(full_id, regexpr("[0-9]+\\.[0-9]+", full_id))
      if (length(match) > 0) match else full_id
    }

    display <- papers |>
      dplyr::arrange(desc(submitted_date)) |>
      dplyr::mutate(
        ID = sapply(id, extract_short_id),
        Title = title,
        `First Author` = sapply(authors, parse_first_author),
        Track = dplyr::case_when(
          track == "spc" ~ "SPC",
          track == "exp_design" ~ "DOE",
          track == "reliability" ~ "Reliability",
          TRUE ~ track
        ),
        Actions = paste0(
          '<a href="', link_pdf, '" target="_blank" class="btn btn-sm pdf-btn"><i class="fa fa-file-pdf"></i></a>',
          '<button class="btn btn-sm summary-btn" onclick="Shiny.setInputValue(\'library_summary_id\', \'', id, '\', {priority: \'event\'})"><i class="fa fa-robot"></i></button>',
          '<button class="btn btn-sm delete-btn" ',
          'onclick="QEPersonalization.toggleBookmark(\'', id, '\'); ',
          'Shiny.setInputValue(\'refresh_library\', Date.now());"><i class="fa fa-trash"></i></button>'
        )
      ) |>
      dplyr::select(Actions, ID, Title, `First Author`, Track)

    DT::datatable(display, escape = FALSE, selection = "none",
      options = list(
        pageLength = 10,
        dom = 'ftp',
        scrollX = TRUE,
        columnDefs = list(
          list(width = '100px', targets = 0),  # Actions
          list(width = '80px', targets = 1),   # ID
          list(width = '40%', targets = 2)     # Title
        )
      ),
      rownames = FALSE)
  })

  # Refresh library table when bookmark changes
  shiny::observeEvent(input$refresh_library, {
    # Trigger reactive invalidation - the table will re-render automatically
    # since it depends on input$personalization_bookmarks
  })

  # Modal for AI Summary from Library table
  shiny::observeEvent(input$library_summary_id, {
    pid <- input$library_summary_id
    if (is.null(pid) || pid == "") return()

    # Get data from all tracks
    all_data <- list()
    for (track_id in names(TRACKS_CONFIG)) {
      track_data <- load_track_data(track_id)
      if (!is.null(track_data$metadata) && !is.null(track_data$factsheet)) {
        combined <- dplyr::left_join(track_data$metadata, track_data$factsheet, by = "id")
        all_data[[track_id]] <- combined
      }
    }
    data <- dplyr::bind_rows(all_data)
    paper <- data |> dplyr::filter(id == pid) |> dplyr::slice(1)
    if (nrow(paper) == 0) return()

    # Build modal content
    modal_content <- list(
      shiny::tags$p(shiny::tags$strong("Authors: "), paper$authors),
      shiny::tags$p(shiny::tags$strong("Date: "), as.character(paper$submitted_date)),
      shiny::hr()
    )

    if (!is.na(paper$summary) && paper$summary != "") {
      modal_content <- c(modal_content, list(
        shiny::tags$h5(shiny::icon("robot"), " AI Summary"),
        shiny::tags$div(class = "math-content", style = "background: #EDECE2; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
          shiny::HTML(commonmark::markdown_html(paper$summary)))
      ))
    }

    if (!is.na(paper$key_results) && paper$key_results != "") {
      modal_content <- c(modal_content, list(
        shiny::tags$h5(shiny::icon("chart-line"), " Key Results"),
        shiny::tags$div(style = "background: #F5FFF5; padding: 15px; border-radius: 6px; margin-bottom: 15px;", paper$key_results)
      ))
    }

    if (!is.na(paper$key_equations) && paper$key_equations != "") {
      modal_content <- c(modal_content, list(
        shiny::tags$h5(shiny::icon("square-root-alt"), " Key Equations"),
        shiny::tags$div(class = "math-content", style = "background: #F5F5FF; padding: 15px; border-radius: 6px;",
          shiny::HTML(paper$key_equations))
      ))
    }

    modal_content <- c(modal_content, list(
      shiny::tags$div(style = "margin-top: 15px;",
        shiny::tags$a(href = paper$link_pdf, target = "_blank", class = "btn btn-primary",
          shiny::icon("file-pdf"), " View PDF on ArXiv"))
    ))

    shiny::showModal(shiny::modalDialog(
      title = shiny::tags$div(style = "color: #C41230;", paper$title),
      size = "l",
      easyClose = TRUE,
      shiny::tagList(modal_content),
      footer = shiny::modalButton("Close")
    ))
  })

  # BibTeX export download handler
  output$download_bibtex <- shiny::downloadHandler(
    filename = function() {
      paste0("qe_arxiv_papers_", format(Sys.Date(), "%Y%m%d"), ".bib")
    },
    content = function(file) {
      bookmarks <- input$personalization_bookmarks
      if (is.null(bookmarks) || length(bookmarks) == 0) {
        writeLines("% No bookmarked papers to export", file)
        return()
      }

      # Get data from all tracks
      all_data <- list()
      for (track_id in names(TRACKS_CONFIG)) {
        track_data <- load_track_data(track_id)
        if (!is.null(track_data$metadata) && !is.null(track_data$factsheet)) {
          combined <- dplyr::left_join(track_data$metadata, track_data$factsheet, by = "id")
          all_data[[track_id]] <- combined
        }
      }

      data <- dplyr::bind_rows(all_data)
      papers <- data |> dplyr::filter(id %in% bookmarks)

      if (nrow(papers) == 0) {
        writeLines("% No matching papers found", file)
        return()
      }

      # Header comment
      header <- paste0(
        "% Bibliography exported from QE ArXiv Watch\n",
        "% https://huggingface.co/spaces/fmegahed/arxiv_control_charts\n",
        "% Export date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n",
        "% Total papers: ", nrow(papers), "\n\n"
      )

      # Generate entries
      entries <- sapply(seq_len(nrow(papers)), function(i) {
        p <- papers[i, ]

        # Extract arXiv ID
        arxiv_id <- regmatches(p$id, regexpr("[0-9]+\\.[0-9]+", p$id))
        if (length(arxiv_id) == 0) arxiv_id <- p$id

        # Citation key: FirstAuthorYear
        first_author <- strsplit(p$authors, "\\|")[[1]][1]
        last_name <- trimws(tail(strsplit(trimws(first_author), " ")[[1]], 1))
        cite_key <- paste0(tolower(gsub("[^a-zA-Z]", "", last_name)), p$year, "_", substr(arxiv_id, 1, 4))

        # Format authors: "Last, First and Last, First"
        authors_list <- strsplit(p$authors, "\\|")[[1]]
        authors_bib <- paste(trimws(authors_list), collapse = " and ")

        # Clean title
        clean_title <- gsub("[{}]", "", p$title)

        # Clean abstract (truncate and escape)
        abstract_clean <- if (!is.na(p$abstract)) {
          gsub("\n", " ", substr(p$abstract, 1, 500))
        } else {
          ""
        }

        paste0(
          "@article{", cite_key, ",\n",
          "  title     = {{", clean_title, "}},\n",
          "  author    = {", authors_bib, "},\n",
          "  year      = {", p$year, "},\n",
          "  eprint    = {", arxiv_id, "},\n",
          "  archivePrefix = {arXiv},\n",
          "  primaryClass = {stat.ME},\n",
          "  url       = {", p$link_pdf, "},\n",
          "  abstract  = {", abstract_clean, "}\n",
          "}"
        )
      })

      writeLines(c(header, paste(entries, collapse = "\n\n")), file)
    }
  )

  # Initialize collection chat when button clicked
  shiny::observeEvent(input$chat_all_papers, {
    bookmarks <- input$personalization_bookmarks

    if (is.null(bookmarks) || length(bookmarks) == 0) {
      shiny::showNotification("No bookmarked papers to chat with. Bookmark some papers first!",
        type = "warning", duration = 5)
      return()
    }

    # Get data from all tracks
    all_data <- list()
    for (track_id in names(TRACKS_CONFIG)) {
      track_data <- load_track_data(track_id)
      if (!is.null(track_data$metadata) && !is.null(track_data$factsheet)) {
        combined <- dplyr::left_join(track_data$metadata, track_data$factsheet, by = "id")
        all_data[[track_id]] <- combined
      }
    }

    data <- dplyr::bind_rows(all_data)
    papers <- data |> dplyr::filter(id %in% bookmarks)

    if (nrow(papers) == 0) {
      shiny::showNotification("No bookmarked papers found.", type = "warning")
      return()
    }

    # Create chat with synthesis model
    tryCatch({
      chat <- ellmer::chat_openai(
        model = "gpt-5.2-2025-12-11",
        system_prompt = COLLECTION_SYSTEM_PROMPT,
        api_key = get_openai_api_key()
      )

      collection_chat_session(chat)
      collection_chat_messages(list())
      collection_first_message(TRUE)
      collection_papers(papers)

      # Show modal
      shiny::showModal(shiny::modalDialog(
        title = paste0("Chat with Your Collection (", nrow(papers), " papers)"),
        size = "l",
        easyClose = FALSE,

        # Warning/info banner
        shiny::div(class = "alert alert-info", style = "margin-bottom: 15px;",
          shiny::icon("info-circle"), " ",
          shiny::tags$strong(nrow(papers), " papers"), " will be analyzed. ",
          "Typically works well with up to ~10-15 papers. ",
          "Larger collections may take longer or hit token limits."
        ),

        # Chat history
        shiny::div(id = "collection_chat_box",
          style = "height: 300px; overflow-y: auto; border: 1px solid #CCC9B8; border-radius: 6px; padding: 15px; background: #FAFAFA; margin-bottom: 15px;",
          shiny::div(id = "collection_thinking", style = "display: none; padding: 10px;",
            shiny::div(class = "chat-thinking-indicator",
              shiny::tags$span(class = "typing-dot"),
              shiny::tags$span(class = "typing-dot"),
              shiny::tags$span(class = "typing-dot")
            )
          ),
          shiny::uiOutput("collection_chat_history")
        ),

        # Input area
        shiny::textAreaInput("collection_chat_input", NULL,
          placeholder = "Ask about your collection... (e.g., 'Compare the methodologies used' or 'What are the key findings?')",
          rows = 2, width = "100%"),

        footer = shiny::tagList(
          shiny::actionButton("send_collection_msg", shiny::tagList(shiny::icon("paper-plane"), " Send"),
            class = "btn-primary"),
          shiny::modalButton("Close")
        )
      ))
    }, error = function(e) {
      shiny::showNotification(paste("Could not initialize chat:", e$message),
        type = "error", duration = 5)
    })
  })

  # Send message handler for collection chat
  shiny::observeEvent(input$send_collection_msg, {
    chat <- collection_chat_session()
    papers <- collection_papers()
    msg <- trimws(input$collection_chat_input)
    if (is.null(chat) || is.null(papers) || msg == "") return()

    is_first <- collection_first_message()

    # Add user message to display
    msgs <- collection_chat_messages()
    msgs <- append(msgs, list(list(role = "user", content = msg)))
    collection_chat_messages(msgs)

    # Clear input and show thinking
    shiny::updateTextAreaInput(session, "collection_chat_input", value = "")
    shinyjs::show("collection_thinking")

    tryCatch({
      if (is_first) {
        # First message: include ALL PDFs via content_pdf_url
        pdf_urls <- papers$link_pdf
        pdf_contents <- lapply(pdf_urls, ellmer::content_pdf_url)

        # Use do.call to splice the pdf_contents list
        args <- c(list(msg), pdf_contents)
        response <- do.call(chat$chat, args)
        collection_first_message(FALSE)
      } else {
        # Subsequent messages: ellmer maintains history automatically
        response <- chat$chat(msg)
      }

      msgs <- collection_chat_messages()
      msgs <- append(msgs, list(list(role = "assistant", content = response)))
      collection_chat_messages(msgs)
    }, error = function(e) {
      msgs <- collection_chat_messages()
      error_msg <- paste0("<em>Error getting response:</em> ", e$message,
        "<br><small>This may occur with very large collections. Try with fewer papers.</small>")
      msgs <- append(msgs, list(list(role = "assistant", content = error_msg)))
      collection_chat_messages(msgs)
    })

    # Hide thinking, typeset MathJax, scroll
    shinyjs::hide("collection_thinking")
    shinyjs::runjs("if (window.MathJax && MathJax.typesetPromise) { MathJax.typesetPromise(); }")
    shinyjs::runjs("document.getElementById('collection_chat_box').scrollTop = document.getElementById('collection_chat_box').scrollHeight;")
  })

  # Render collection chat history
  output$collection_chat_history <- shiny::renderUI({
    msgs <- collection_chat_messages()
    if (length(msgs) == 0) {
      return(shiny::tags$p(class = "text-muted", style = "text-align: center; margin-top: 50px;",
        "Start by asking a question about your bookmarked papers..."))
    }

    shiny::tagList(
      lapply(msgs, function(m) {
        if (m$role == "user") {
          shiny::div(style = "text-align: right; margin-bottom: 10px;",
            shiny::div(class = "chat-user", style = "display: inline-block; background: #C41230; color: white; padding: 10px 15px; border-radius: 15px 15px 0 15px; max-width: 75%;",
              m$content))
        } else {
          shiny::div(style = "text-align: left; margin-bottom: 10px;",
            shiny::div(class = "chat-response math-content", style = "display: inline-block; background: #EDECE2; padding: 10px 15px; border-radius: 15px 15px 15px 0; max-width: 75%;",
              shiny::HTML(commonmark::markdown_html(m$content))))
        }
      })
    )
  })

}

# ============================================================================
# Run App
# ============================================================================

shiny::shinyApp(ui = ui, server = server)
