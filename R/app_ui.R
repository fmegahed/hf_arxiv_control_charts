# Page layout. Three modes share one page: the landing page, the track view
# (question box, chips, scope line and four tabs) and the paper page.

MATHJAX_CONFIG <- "
window.MathJax = {
  tex: {
    inlineMath: [['\\\\(', '\\\\)']],
    displayMath: [['\\\\[', '\\\\]']],
    processEscapes: false
  },
  options: { skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code'] }
};"

app_head <- function() {
  shiny::tags$head(
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    shiny::tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "miami-theme.css"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "qew.css"),
    shiny::tags$title("QE ArXiv Watch"),
    shiny::tags$script(shiny::HTML(MATHJAX_CONFIG)),
    shiny::tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js", async = NA),
    shiny::tags$script(src = "bookmark_ids.js"),
    shiny::tags$script(src = "personalization.js"),
    shiny::tags$script(src = "qew.js")
  )
}

ask_box <- function(input_id, button_id, label, placeholder) {
  shiny::div(
    class = "ask-box",
    shiny::tags$label(`for` = input_id, class = "ask-label", label),
    shiny::tags$input(id = input_id, type = "text", class = "form-control ask-input shiny-input-text",
                      placeholder = placeholder, maxlength = QUESTION_MAX_CHARS, autocomplete = "off",
                      `data-enter-clicks` = button_id),
    shiny::actionButton(button_id, "Go", class = "btn-primary ask-go"),
    help_button("ask", "How asking a question works"))
}

logo_row <- function(class) {
  shiny::div(
    class = class,
    shiny::tags$img(src = "miami-logo.png", alt = "Miami University"),
    shiny::tags$img(src = "university-of-dayton-vector-logo.png", alt = "University of Dayton"),
    shiny::tags$img(src = "uva-compacte-logo.png", alt = "University of Amsterdam"))
}

landing_ui <- function(deps) {
  settings <- deps$settings
  shiny::div(
    class = "landing-page",
    shiny::div(
      class = "landing-hero",
      shiny::tags$h1("QE ArXiv Watch"),
      shiny::tags$p(class = "landing-subtitle",
                    "arXiv papers in quality engineering, each with a structured factsheet written by a language model from the paper's PDF. Find papers, read one quickly, see the shape of a research area, find who works on it, and keep a reading list.")),
    shiny::div(class = "ask-panel landing-ask",
               ask_box("question_landing", "ask_landing", "Ask all tracks",
                       "For example: Bayesian methods with public code since 2023")),
    shiny::div(
      class = "track-selector-container",
      shiny::tags$h2("Or open a track"),
      shiny::uiOutput("landing_tracks")),
    shiny::div(
      class = "landing-footer",
      shiny::div(
        class = "landing-footer-content",
        logo_row("landing-logo-container"),
        shiny::div(class = "landing-authors",
                   shiny::tags$p(settings$authors),
                   shiny::tags$p(class = "landing-paper", paste0("Companion to the paper \"", settings$paper_title, "\". "),
                                 shiny::tags$a(href = settings$repo_url, target = "_blank", rel = "noopener noreferrer",
                                               "Source code"))),
        shiny::div(class = "landing-version",
                   paste0("Version ", settings$app_version, " | Data current as of ",
                          format(deps$data$data_date, "%d %B %Y"), " | "),
                   changes_link())))
  )
}

add_filter_ui <- function() {
  shiny::tags$details(
    class = "add-filter",
    shiny::tags$summary("+ add filter"),
    shiny::div(
      class = "add-filter-panel",
      shiny::selectizeInput("add_field", "Field", choices = NULL, width = "280px",
                            options = list(placeholder = "Search the fields")),
      shiny::selectizeInput("add_values", "Any of these values", choices = NULL, multiple = TRUE, width = "100%",
                            options = list(placeholder = "Choose values", plugins = list("remove_button"))),
      shiny::actionButton("add_apply", "Apply", class = "btn-primary btn-sm")))
}

track_view_ui <- function(deps) {
  shiny::tagList(
    shiny::uiOutput("header"),
    # The institution logos are on the landing page only, so the results start
    # higher on the track pages.
    shiny::div(
      class = "main-content",
      shiny::div(
        class = "ask-panel",
        ask_box("question", "ask_go", "Ask", "For example: nonparametric with public code, 2025"),
        shiny::uiOutput("note"),
        shiny::div(class = "chip-row", shiny::uiOutput("chips", inline = TRUE), add_filter_ui()),
        shiny::div(class = "scope-line", shiny::uiOutput("scope_line", inline = TRUE))),
      shiny::conditionalPanel("output.mode == 'paper'", paper_ui("paper")),
      shiny::conditionalPanel(
        "output.mode == 'tabs'",
        shiny::tabsetPanel(
          id = "main_tabs", type = "tabs",
          shiny::tabPanel("Explore", value = "explore", explore_ui("explore", deps)),
          shiny::tabPanel("Landscape", value = "landscape", landscape_ui("landscape", deps)),
          shiny::tabPanel("Authors", value = "authors", authors_ui("authors", deps)),
          shiny::tabPanel("Library", value = "library", library_ui("library", deps))))),
    shiny::div(
      class = "app-footer",
      shiny::p(deps$settings$authors, shiny::tags$br(),
               "Data from ", shiny::tags$a(href = "https://arxiv.org/", "arXiv"),
               " | Factsheets are written by a language model and can be wrong ",
               help_button("factsheet", "How factsheets are made and how good they are"),
               " | ", shiny::tags$a(href = deps$settings$repo_url, target = "_blank", rel = "noopener noreferrer",
                                    "Source code"),
               " | Version ", deps$settings$app_version, " | ", changes_link()))
  )
}

app_ui <- function(deps) {
  if (is.null(deps$data$papers)) {
    return(shiny::fluidPage(
      app_head(),
      shiny::div(class = "info-card", style = "margin: 40px auto; max-width: 700px;",
                 shiny::tags$h3("QE ArXiv Watch cannot start"),
                 shiny::tags$p("The data could not be loaded from '", deps$data$data_dir, "'."),
                 shiny::tags$ul(lapply(deps$data$problems, shiny::tags$li)),
                 shiny::tags$p("Set the environment variable QEW_DATA_DIR to a directory with factsheets in the version 2 layout."))))
  }
  shiny::fluidPage(
    app_head(),
    shiny::uiOutput("accent_style"),
    shiny::conditionalPanel("output.mode == 'landing'", landing_ui(deps)),
    shiny::conditionalPanel("output.mode && output.mode != 'landing'", track_view_ui(deps))
  )
}
