# The paper page as a Shiny module: the factsheet (app_paper_view.R) and the
# chat with the paper. app$paper() is the open paper (one row) or NULL.

paper_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "tab-content-wrapper paper-page",
    shiny::actionButton(ns("back"), "Back to the list", class = "btn-info btn-sm back-button"),
    shiny::uiOutput(ns("view")))
}

paper_chat_panel <- function(ns, spec) {
  shiny::tagList(
    shiny::tags$p(class = "help-block",
                  "Your first message sends this paper's PDF to the model ", shiny::tags$code(spec$models$chat),
                  ". Answers are generated and can be wrong."),
    shiny::div(class = "chat-suggestions", lapply(PAPER_CHAT_SUGGESTIONS, function(text) {
      shiny::tags$button(type = "button", class = "btn btn-sm quick-question-btn",
                         `data-set-input` = ns("suggest"), `data-value` = text, text)
    })),
    shiny::div(class = "miami-chat-container",
               shinychat::chat_ui(ns("chat"), placeholder = "Ask about this paper", height = "380px", fill = FALSE)))
}

paper_server <- function(id, app, deps) {
  shiny::moduleServer(id, function(input, output, session) {
    spec <- deps$spec
    chat <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$back, app$close_paper())
    # A new paper starts a new conversation.
    shiny::observeEvent(app$paper(), chat(NULL), ignoreNULL = FALSE)

    output$view <- shiny::renderUI({
      paper <- app$paper()
      if (is.null(paper)) {
        return(shiny::div(class = "empty-state",
                          "This paper is not in the database. It may have been removed, or the link may be mistyped."))
      }
      paper_view(paper, spec, deps$settings,
                 chat = if (identical(paper$status, "ok")) paper_chat_panel(session$ns, spec),
                 also_in = paper_other_tracks(deps$data$papers, paper))
    })

    shiny::observeEvent(input$suggest, {
      shinychat::update_chat_user_input("chat", value = input$suggest, focus = TRUE)
    })

    shiny::observeEvent(input$chat_user_input, {
      paper <- app$paper()
      message <- input$chat_user_input
      if (is.null(paper) || is.null(message) || !nzchar(message)) return()
      # After a failure the PDF has not reached the model, so the next message starts over.
      fail <- function(e) {
        chat(NULL)
        shinychat::chat_append("chat", chat_error_text(conditionMessage(e)))
      }
      tryCatch({
        current <- chat()
        first <- is.null(current)
        if (first) current <- deps$chat_factory(paper_chat_system_prompt(paper))
        stream <- if (first) current$stream_async(message, ellmer::content_pdf_url(paper$link_pdf))
                  else current$stream_async(message)
        chat(current)
        promises::catch(shinychat::chat_append("chat", stream), fail)
      }, error = fail)
    })
  })
}
