# QE ArXiv Watch
#
# Run from this directory:  shiny::runApp(".", port = 7860)
# The data directory is taken from the environment variable QEW_DATA_DIR
# (default "data"). Everything else lives in R/ (sourced by Shiny, or here
# when the file is run another way) and in config/.
#
#   R/app_data.R       loading and joining the data, once
#   R/app_filters.R    the filter engine and the chips
#   R/app_url.R        view <-> URL query string
#   R/app_counts.R     counting for charts      R/app_charts.R   drawing them
#   R/app_table.R      results table, CSV and BibTeX
#   R/app_paper_view.R the paper page           R/app_help.R     text behind every "?"
#   R/question.R       question -> filters      R/jev.R          relevance ranking
#   R/app_chat.R       chat prompts and limits
#   R/app_ui.R, R/app_server.R, R/app_mod_*.R   layout, server, tab modules

if (!exists("app_server", mode = "function")) {
  for (file in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(file)
}

load_dotenv()
deps <- app_deps()

shiny::shinyApp(ui = app_ui(deps), server = app_server(deps))
