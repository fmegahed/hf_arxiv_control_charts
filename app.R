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

# Ensuring that the API key is being read from the app's secrets; ellmer requires a zero input function
openai_credentials <- function() {
  key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (!nzchar(key)) stop("OPENAI_API_KEY is not set (HF Space Settings -> Secrets).")
  key
}

# Color palette for charts
CHART_COLORS <- c(
  "#C41230", # Miami Red
  "#1B9E77", # teal
  "#D95F02", # orange
  "#7570B3", # purple
  "#66A61E", # green
  "#E6AB02", # mustard
  "#E7298A", # magenta
  "#A6761D", # brown
  "#666666", # gray
  "#4E79A7", # blue
  "#F28E2B", # lighter orange
  "#59A14F"  # medium green
)

# ============================================================================
# Helper Functions
# ============================================================================

parse_arxiv_date <- function(date_str) {

  as.Date(lubridate::ymd_hms(date_str))
}

load_initial_data <- function() {
 metadata_path <- "data/arxiv_metadata.csv"
  factsheet_path <- "data/spc_factsheet.csv"

  metadata <- NULL
  factsheet <- NULL

  if (file.exists(metadata_path)) {
    metadata <- readr::read_csv(metadata_path, show_col_types = FALSE) |>
      dplyr::mutate(
        submitted_date = parse_arxiv_date(submitted),
        year = lubridate::year(submitted_date),
        month = lubridate::month(submitted_date),
        year_month = paste0(year, "-", sprintf("%02d", month))
      )
  }

  if (file.exists(factsheet_path)) {
    factsheet <- readr::read_csv(factsheet_path, show_col_types = FALSE)
  }

  list(metadata = metadata, factsheet = factsheet)
}

# Count pipe-delimited values, excluding "Other"
count_pipe_delimited <- function(data, column_name, top_n = 10, exclude_other = FALSE) {
  if (is.null(data) || !column_name %in% names(data)) return(NULL)

  all_values <- unlist(strsplit(data[[column_name]], "\\|"))
  all_values <- trimws(all_values)
  all_values <- all_values[all_values != "" & !is.na(all_values)]

  if (exclude_other) {
    all_values <- all_values[!tolower(all_values) %in% c("other", "others", "n/a", "na", "none")]
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

# Generate chat response
generate_paper_response <- function(paper, question) {
  q <- tolower(question)

  format_section <- function(content, max_len = 800) {
    if (is.na(content) || content == "") return(NULL)
    if (nchar(content) > max_len) paste0(substr(content, 1, max_len), "...") else content
  }

  if (grepl("contribution|main|about|summary|overview|what is", q)) {
    summary_text <- format_section(paper$summary)
    if (!is.null(summary_text)) {
      return(paste0("<strong>Main Contribution:</strong><br><br>", summary_text))
    }
  }

  if (grepl("method|approach|statistical|technique|algorithm", q)) {
    return(paste0(
      "<strong>Methodology:</strong><br><br>",
      "<em>Chart Family:</em> ", ifelse(is.na(paper$chart_family), "Not specified", paper$chart_family), "<br>",
      "<em>Statistical Method:</em> ", ifelse(is.na(paper$chart_statistic), "Not specified", paper$chart_statistic), "<br>",
      "<em>Phase:</em> ", ifelse(is.na(paper$phase), "Not specified", paper$phase)
    ))
  }

  if (grepl("limit|weakness|drawback", q)) {
    stated <- format_section(paper$limitations_stated, 500)
    unstated <- format_section(paper$limitations_unstated, 500)
    stated_text <- if (!is.null(stated)) paste0("<em>Stated:</em> ", stated, "<br><br>") else ""
    unstated_text <- if (!is.null(unstated)) paste0("<em>Unstated:</em> ", unstated) else ""
    return(paste0("<strong>Limitations:</strong><br><br>", stated_text, unstated_text))
  }

  if (grepl("future|next|extend", q)) {
    stated <- format_section(paper$future_work_stated, 500)
    unstated <- format_section(paper$future_work_unstated, 500)
    stated_text <- if (!is.null(stated)) paste0("<em>Stated:</em> ", stated, "<br><br>") else ""
    unstated_text <- if (!is.null(unstated)) paste0("<em>Suggested:</em> ", unstated) else ""
    return(paste0("<strong>Future Directions:</strong><br><br>", stated_text, unstated_text))
  }

  # Default
  summary_text <- format_section(paper$summary, 600)
  default_text <- if (!is.null(summary_text)) summary_text else substr(paper$abstract, 1, 500)
  paste0(
    "Based on the paper:<br><br>",
    default_text,
    "<br><br><em>Try asking about: methodology, limitations, or future work.</em>"
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
    shiny::tags$title("ArXiv Control Chart Literature Monitor"),
    
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
  "))
  ),

  # Header
  shiny::div(
    class = "app-header",
    shiny::div(
      class = "header-content",
      shiny::div(
        class = "header-left",
        shiny::tags$h1("ArXiv Control Chart Literature Monitor"),
        shiny::p(class = "subtitle", "Real-time Tracking of Statistical Process Control Research")
      ),
      shiny::div(
        class = "header-right",
        shiny::p("Version 1.0.0 | January 2026"),
        shiny::p(
          shiny::tags$strong("Authors: "),
          "Fadel M. Megahed, Ying-Ju (Tessa) Chen, Allison Jones-Farmer, Ibrahim Yousif, and Inez M. Zwetsloot"
        )
      )
    )
  ),

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
            shiny::p(
              "This dashboard monitors control chart research from ArXiv using the query: ",
              shiny::tags$code('ti:"control chart" OR abs:"control chart"'),
              ". Each paper is analyzed using LLM extraction to identify methodology and findings. ",
              "Data current as of January 26, 2026. Latest arXiv paper included is from January 18, 2026."
            )
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
              shiny::div(class = "stat-value", shiny::textOutput("top_chart_type")),
              shiny::div(class = "stat-label", "Most Common Chart Type")
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
                shiny::selectInput("trend_category", "Category:",
                  choices = c("Chart Family" = "chart_family", "Statistical Method" = "chart_statistic",
                              "Application Domain" = "application_domain"))
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
                shiny::selectInput("filter_chart_family", "Chart Family:",
                  choices = c("All" = ""), selected = "")
              ),
              shiny::column(3,
                shiny::selectInput("filter_statistic", "Statistical Method:",
                  choices = c("All" = ""), selected = "")
              ),
              shiny::column(3,
                shiny::selectInput("filter_domain", "Application Domain:",
                  choices = c("All" = ""), selected = "")
              ),
              shiny::column(3,
                shiny::selectInput("filter_phase", "Phase:",
                  choices = c("All" = ""), selected = "")
              )
            ),
            shiny::fluidRow(
              shiny::column(9,
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
                shiny::tags$h4(class = "section-heading", "Chart Family Distribution"),
                plotly::plotlyOutput("chart_family_plot", height = "320px")
              )
            ),
            shiny::column(6,
              shiny::div(class = "info-card",
                shiny::tags$h4(class = "section-heading", "Statistical Method Distribution"),
                plotly::plotlyOutput("statistic_plot", height = "320px")
              )
            )
          ),

          # Charts row 2
          shiny::fluidRow(
            shiny::column(6,
              shiny::div(class = "info-card",
                shiny::tags$h4(class = "section-heading", "Application Domain Distribution"),
                plotly::plotlyOutput("domain_plot", height = "320px")
              )
            ),
            shiny::column(6,
              shiny::div(class = "info-card",
                shiny::tags$h4(class = "section-heading", "Phase Distribution"),
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
              shiny::uiOutput("chat_history")
            ),
            shiny::div(
              shiny::tags$h5("Quick Questions:"),
              shiny::uiOutput("quick_questions")
            ),
            shiny::fluidRow(style = "margin-top: 15px;",
              shiny::column(10,
                shiny::textAreaInput("chat_input", NULL, placeholder = "Ask a question about this paper...", rows = 2)
              ),
              shiny::column(2,
                shiny::actionButton("send_chat_msg", shiny::icon("paper-plane"), class = "btn-primary", style = "width:100%; height: 60px;")
              )
            )
          )
        )
      )

    )
  ),

  # Footer
  shiny::div(
    class = "app-footer",
    shiny::p(
      "Built with ", shiny::tags$a(href = "https://shiny.posit.co/", "Shiny"),
      " | Data source: ", shiny::tags$a(href = "https://arxiv.org/", "ArXiv")
    )
  )
)

# ============================================================================
# Server Logic
# ============================================================================

server <- function(input, output, session) {

  # Reactive data
  papers_data <- shiny::reactiveVal(NULL)
  factsheet_data <- shiny::reactiveVal(NULL)
  chat_messages <- shiny::reactiveVal(list())

  # Load data
  shiny::observe({
    data <- load_initial_data()
    papers_data(data$metadata)
    factsheet_data(data$factsheet)
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
    if (!is.null(search_term) && trimws(search_term) != "") {
      search_term <- tolower(trimws(search_term))
      data <- data |> dplyr::filter(
        grepl(search_term, tolower(title), fixed = TRUE) |
        grepl(search_term, tolower(authors), fixed = TRUE) |
        grepl(search_term, tolower(ifelse(is.na(abstract), "", abstract)), fixed = TRUE) |
        grepl(search_term, tolower(ifelse(is.na(summary), "", summary)), fixed = TRUE) |
        grepl(search_term, tolower(ifelse(is.na(key_results), "", key_results)), fixed = TRUE) |
        grepl(search_term, tolower(ifelse(is.na(chart_family), "", chart_family)), fixed = TRUE) |
        grepl(search_term, tolower(ifelse(is.na(application_domain), "", application_domain)), fixed = TRUE)
      )
    }

    if (!is.null(input$filter_chart_family) && input$filter_chart_family != "") {
      data <- data |> dplyr::filter(grepl(input$filter_chart_family, chart_family, fixed = TRUE))
    }
    if (!is.null(input$filter_statistic) && input$filter_statistic != "") {
      data <- data |> dplyr::filter(grepl(input$filter_statistic, chart_statistic, fixed = TRUE))
    }
    if (!is.null(input$filter_domain) && input$filter_domain != "") {
      data <- data |> dplyr::filter(grepl(input$filter_domain, application_domain, fixed = TRUE))
    }
    if (!is.null(input$filter_phase) && input$filter_phase != "") {
      data <- data |> dplyr::filter(phase == input$filter_phase)
    }
    data
  })

  # ===========================================================================
  # Overview Tab
  # ===========================================================================

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

  output$top_chart_type <- shiny::renderText({
    get_top_category(combined_data(), "chart_family", 15)
  })

  output$top_domain <- shiny::renderText({
    get_top_category(combined_data(), "application_domain", 15)
  })

  output$research_landscape <- shiny::renderUI({
    data <- combined_data()
    if (is.null(data)) return(shiny::p("Loading..."))

    # Filter by period
    period <- input$landscape_period
    if (period != "all") {
      months <- as.numeric(period)
      cutoff <- Sys.Date() - (months * 30)
      data <- data |> dplyr::filter(submitted_date >= cutoff)
    }

    n_papers <- nrow(data)
    if (n_papers == 0) return(shiny::p("No papers in selected period."))

    # Calculate stats
    families <- count_pipe_delimited(data, "chart_family", NULL, TRUE)
    univariate <- sum(families$count[grepl("Univariate", families$category, ignore.case = TRUE)])
    multivariate <- sum(families$count[grepl("Multivariate", families$category, ignore.case = TRUE)])

    phase_both <- sum(data$phase == "Both", na.rm = TRUE)
    phase_i <- sum(data$phase == "Phase I", na.rm = TRUE)
    phase_ii <- sum(data$phase == "Phase II", na.rm = TRUE)

    with_code <- sum(data$code_availability_source %in% c("GitHub", "Personal website", "Supplementary material"), na.rm = TRUE)
    nonparametric <- sum(data$assumes_normality == FALSE, na.rm = TRUE)

    period_label <- if (period == "all") "all time" else paste0("the last ", period, " months")

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
        shiny::tags$strong("Methods: "), nonparametric, " nonparamteric/distribution-free, ", with_code, " with available code."
      )
    )
  })

  output$mini_trend <- plotly::renderPlotly({
    data <- papers_data()
    if (is.null(data)) return(NULL)

    yearly <- data |> dplyr::count(year) |> dplyr::arrange(year)

    plotly::plot_ly(yearly, x = ~year, y = ~n, type = "bar", marker = list(color = "#C41230")) |>
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
        `PDF` = paste0('<a href="', link_pdf, '" target="_blank" class="pdf-btn"><i class="fa fa-file-pdf"></i> ', short_id, '</a>'),
        `AI Summary` = paste0('<button class="summary-btn" onclick="Shiny.setInputValue(\'overview_summary_id\', \'', id, '\', {priority: \'event\'})"><i class="fa fa-robot"></i> View</button>')
      ) |>
      dplyr::select(ID, `Submitted`, `Title`, `First Author`, `PDF`, `AI Summary`)

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

    data <- data |> dplyr::filter(
      submitted_date >= input$timeline_range[1],
      submitted_date <= input$timeline_range[2]
    )

    if (nrow(data) == 0) return(NULL)

    if (input$timeline_agg == "year") {
      plot_data <- data |> dplyr::count(year) |> dplyr::arrange(year)
      x_var <- ~year
      x_title <- "Year"
    } else {
      plot_data <- data |> dplyr::count(year_month) |> dplyr::arrange(year_month)
      x_var <- ~year_month
      x_title <- "Month"
    }

    if (input$show_cumulative) {
      plot_data$cumulative <- cumsum(plot_data$n)

      plotly::plot_ly(plot_data, x = x_var) |>
        plotly::add_bars(y = ~n, name = "Per Period", marker = list(color = "#C41230")) |>
        plotly::add_lines(y = ~cumulative, name = "Cumulative", yaxis = "y2",
          line = list(color = "#EFDB72", width = 3)) |>
        plotly::layout(
          xaxis = list(title = x_title, tickangle = -45),
          yaxis = list(title = "Papers per Period"),
          yaxis2 = list(title = "Cumulative", overlaying = "y", side = "right"),
          legend = list(x = 0.02, y = 0.98),
          hovermode = "x unified"
        ) |>
        plotly::config(displayModeBar = FALSE)
    } else {
      plotly::plot_ly(plot_data, x = x_var, y = ~n, type = "scatter", mode = "lines+markers",
        fill = "tozeroy", line = list(color = "#C41230", width = 2),
        marker = list(color = "#C41230", size = 8), fillcolor = "rgba(196, 18, 48, 0.2)") |>
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
    counts <- count_pipe_delimited(data, cat, 10, TRUE)
    if (is.null(counts)) return(NULL)

    choices <- counts$category
    shiny::selectizeInput("trend_topics", "Select topics (max 5):", choices = choices,
      selected = head(choices, 3), multiple = TRUE, options = list(maxItems = 5))
  })

  output$topic_trends <- plotly::renderPlotly({
    data <- combined_data()
    topics <- input$trend_topics
    if (is.null(data) || is.null(topics) || length(topics) == 0) return(NULL)

    cat <- input$trend_category

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

  # Update filter choices
  shiny::observe({
    data <- combined_data()
    if (is.null(data)) return()

    # Chart family
    fam <- count_pipe_delimited(data, "chart_family", NULL, TRUE)
    if (!is.null(fam)) {
      shiny::updateSelectInput(session, "filter_chart_family",
        choices = c("All" = "", setNames(fam$category, paste0(fam$category, " (", fam$count, ")"))))
    }

    # Statistic
    stat <- count_pipe_delimited(data, "chart_statistic", NULL, TRUE)
    if (!is.null(stat)) {
      shiny::updateSelectInput(session, "filter_statistic",
        choices = c("All" = "", setNames(stat$category, paste0(stat$category, " (", stat$count, ")"))))
    }

    # Domain
    dom <- count_pipe_delimited(data, "application_domain", NULL, TRUE)
    if (!is.null(dom)) {
      shiny::updateSelectInput(session, "filter_domain",
        choices = c("All" = "", setNames(dom$category, paste0(dom$category, " (", dom$count, ")"))))
    }

    # Phase
    phases <- unique(data$phase[!is.na(data$phase) & data$phase != ""])
    shiny::updateSelectInput(session, "filter_phase", choices = c("All" = "", phases))
  })

  # Clear filters
  shiny::observeEvent(input$clear_filters, {
    shiny::updateTextInput(session, "global_search", value = "")
    shiny::updateSelectInput(session, "filter_chart_family", selected = "")
    shiny::updateSelectInput(session, "filter_statistic", selected = "")
    shiny::updateSelectInput(session, "filter_domain", selected = "")
    shiny::updateSelectInput(session, "filter_phase", selected = "")
  })

  # Filter summary
  output$filter_summary <- shiny::renderText({
    total <- nrow(combined_data())
    filtered <- nrow(filtered_data())
    if (is.null(total) || is.null(filtered)) return("")

    filters <- c()
    search_term <- trimws(input$global_search)
    if (search_term != "") filters <- c(filters, paste0("Search: '", search_term, "'"))
    if (input$filter_chart_family != "") filters <- c(filters, input$filter_chart_family)
    if (input$filter_statistic != "") filters <- c(filters, input$filter_statistic)
    if (input$filter_domain != "") filters <- c(filters, input$filter_domain)
    if (input$filter_phase != "") filters <- c(filters, input$filter_phase)

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

  # Helper for bar charts
  make_bar_chart <- function(data, col_name, color) {
    counts <- count_pipe_delimited(data, col_name, 10, FALSE)
    if (is.null(counts)) return(NULL)

    plotly::plot_ly(counts, x = ~count, y = ~reorder(category, count), type = "bar",
      orientation = "h", marker = list(color = color),
      text = ~paste0(count, " papers"), hoverinfo = "text") |>
      plotly::layout(
        xaxis = list(title = "Papers"),
        yaxis = list(title = ""),
        margin = list(l = 150)
      ) |>
      plotly::config(displayModeBar = FALSE)
  }

  output$chart_family_plot <- plotly::renderPlotly({
    make_bar_chart(filtered_data(), "chart_family", CHART_COLORS[1])
  })

  output$statistic_plot <- plotly::renderPlotly({
    make_bar_chart(filtered_data(), "chart_statistic", CHART_COLORS[2])
  })

  output$domain_plot <- plotly::renderPlotly({
    make_bar_chart(filtered_data(), "application_domain", CHART_COLORS[3])
  })

  output$phase_plot <- plotly::renderPlotly({
    data <- filtered_data()
    if (is.null(data)) return(NULL)

    phase_counts <- data |>
      dplyr::filter(!is.na(phase) & phase != "") |>
      dplyr::count(phase) |>
      dplyr::arrange(desc(n))

    if (nrow(phase_counts) == 0) return(NULL)

    plotly::plot_ly(phase_counts, x = ~n, y = ~reorder(phase, n), type = "bar",
      orientation = "h", marker = list(color = CHART_COLORS[4]),
      text = ~paste0(n, " papers"), hoverinfo = "text") |>
      plotly::layout(
        xaxis = list(title = "Papers"),
        yaxis = list(title = ""),
        margin = list(l = 100)
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # Papers table
  output$filtered_papers_table <- DT::renderDataTable({
    data <- filtered_data()
    if (is.null(data)) return(NULL)

    display <- data |>
      dplyr::arrange(desc(submitted_date)) |>
      dplyr::mutate(
        ID = id,
        PDF = paste0('<a href="', link_pdf, '" target="_blank" class="pdf-btn"><i class="fa fa-file-pdf"></i></a>'),
        Summary = paste0('<button class="summary-btn" onclick="Shiny.setInputValue(\'topic_summary_id\', \'', id, '\', {priority: \'event\'})"><i class="fa fa-robot"></i></button>'),
        Title = title,
        Year = year,
        `Chart Type` = ifelse(is.na(chart_family), "N/A", substr(chart_family, 1, 30)),
        Method = ifelse(is.na(chart_statistic), "N/A", substr(chart_statistic, 1, 25)),
        Domain = ifelse(is.na(application_domain), "N/A", substr(application_domain, 1, 25))
      ) |>
      dplyr::select(ID, PDF, Summary, Title, Year, `Chart Type`, Method, Domain)

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

      # Metadata and PDF link
      content <- c(content, list(
        shiny::tags$div(
          style = "margin-top: 15px; padding: 15px; background: #F5F5F5; border-radius: 6px;",
          shiny::fluidRow(
            shiny::column(4, shiny::tags$strong("Chart Family: "), ifelse(is.na(paper$chart_family), "N/A", paper$chart_family)),
            shiny::column(4, shiny::tags$strong("Method: "), ifelse(is.na(paper$chart_statistic), "N/A", paper$chart_statistic)),
            shiny::column(4, shiny::tags$strong("Phase: "), ifelse(is.na(paper$phase), "N/A", paper$phase))
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

    top <- auth_data |> dplyr::count(author) |> dplyr::arrange(desc(n)) |> head(15)

    plotly::plot_ly(top, x = ~n, y = ~reorder(author, n), type = "bar",
      orientation = "h", marker = list(color = "#C41230")) |>
      plotly::layout(xaxis = list(title = "Papers"), yaxis = list(title = ""),
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
        credentials = openai_credentials
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
      content_div <- if (is_math) {
        shiny::tags$div(class = "math-content", style = paste0("padding: 15px; background: ", bg, "; border-radius: 6px;"),
          shiny::HTML(content))
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
            shiny::div(style = "display: inline-block; background: #EDECE2; padding: 10px 15px; border-radius: 15px 15px 15px 0; max-width: 75%;",
              shiny::HTML(m$content)))
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

    # Scroll to bottom
    shinyjs::runjs("document.getElementById('chat_box').scrollTop = document.getElementById('chat_box').scrollHeight;")
  })

}

# ============================================================================
# Run App
# ============================================================================

shiny::shinyApp(ui = ui, server = server)
