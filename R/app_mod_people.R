# Authors tab (who works on the current selection) and Library tab
# (bookmarks, BibTeX export, chat with the collection, weekly feed).

# Compact table of papers with titles that open the paper view.
paper_list_widget <- function(table, page_length = 10L, empty = "No papers to show.") {
  names <- names(table)
  index <- function(name) match(name, names) - 1L
  DT::datatable(
    table, rownames = FALSE, escape = TRUE, selection = "none", class = "stripe hover results-table",
    options = list(pageLength = page_length, ordering = FALSE, dom = "tip", scrollX = TRUE, autoWidth = FALSE,
                   columnDefs = list(
                     list(targets = c(index("paper_id"), index("row_track"), index("status"), index("href")), visible = FALSE),
                     list(targets = index("Saved"), render = DT::JS("QEW.render.bookmark"), width = "34px", title = "",
                          className = "col-bookmark"),
                     list(targets = index("Title"), render = DT::JS("QEW.render.title"), className = "col-title"),
                     list(targets = index("Code"), render = DT::JS("QEW.render.code"))),
                   language = list(emptyTable = empty),
                   drawCallback = DT::JS("function() { QEW.afterDraw(this.api().table().container()); }")))
}

# ---- Authors -----------------------------------------------------------------

authors_ui <- function(id, deps) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "tab-content-wrapper",
    shiny::div(
      class = "chart-grid",
      shiny::div(class = "info-card chart-card",
                 shiny::tags$h4(class = "section-heading", shiny::textOutput(ns("top_title"), inline = TRUE),
                                help_button("authors", "About these counts")),
                 plotly::plotlyOutput(ns("top_authors"), height = "auto")),
      shiny::div(class = "info-card chart-card",
                 shiny::tags$h4(class = "section-heading", shiny::textOutput(ns("team_title"), inline = TRUE)),
                 plotly::plotlyOutput(ns("team_sizes"), height = "320px"))),
    shiny::div(
      class = "info-card",
      shiny::tags$h4(class = "section-heading", "Author lookup"),
      shiny::tags$p(class = "help-block", "Authors of the papers in the current selection. Click a bar above or search here."),
      shiny::selectizeInput(ns("author"), "Author", choices = NULL, width = "360px",
                            options = list(placeholder = "Type a name")),
      shiny::uiOutput(ns("author_info")),
      DT::DTOutput(ns("author_papers")))
  )
}

# Choices of the author box: "Name (papers)" -> name. Empty when no paper is
# selected, which a question can cause.
author_choices <- function(table) {
  if (nrow(table) == 0L) return(character(0))
  stats::setNames(table$author, sprintf("%s (%d)", table$author, table$n))
}

authors_server <- function(id, app, deps) {
  shiny::moduleServer(id, function(input, output, session) {
    spec <- deps$spec
    ns <- session$ns
    papers <- shiny::reactive(app$papers())
    track <- shiny::reactive(app$filters()$track)
    accent <- shiny::reactive(if (is.null(track())) ALL_TRACKS_ACCENT else spec_track(spec, track())$color)
    counts <- shiny::reactive(author_counts(papers()))
    need_papers <- function() shiny::validate(shiny::need(nrow(papers()) > 0L, EMPTY_CHART_TEXT))

    output$top_title <- shiny::renderText(paste0(
      "Authors with the most papers (", format(nrow(counts()), big.mark = ","), " authors of ",
      format(nrow(papers()), big.mark = ","), " papers in the current selection)"))
    output$top_authors <- plotly::renderPlotly({
      need_papers()
      top <- utils::head(counts(), TOP_AUTHORS_SHOWN)
      top$label <- top$value <- top$author
      chart_bars(top, accent(), source = ns("top_authors"), show_share = FALSE, max_bars = TOP_AUTHORS_SHOWN)
    })
    output$team_title <- shiny::renderText(paste0("Authors per paper (", format(nrow(papers()), big.mark = ","), " papers)"))
    output$team_sizes <- plotly::renderPlotly({
      need_papers()
      chart_columns(team_sizes(papers()), "size", accent(), "Authors on the paper")
    })

    shiny::observe({
      table <- counts()
      choices <- author_choices(table)
      current <- shiny::isolate(input$author)
      shiny::updateSelectizeInput(session, "author", choices = c("Type a name" = "", choices),
                                  selected = if (isTRUE(current %in% table$author)) current else "", server = TRUE)
    })
    author_click <- shiny::reactive(plotly_click_value(session, ns("top_authors")))
    shiny::observeEvent(author_click(), {
      shiny::updateSelectizeInput(session, "author", selected = author_click(), server = TRUE,
                                  choices = author_choices(counts()))
    })

    author_papers <- shiny::reactive({
      author <- input$author
      if (is.null(author) || !nzchar(author)) return(NULL)
      papers()[has_any_value(papers()$authors_merged %||% papers()$authors, author), , drop = FALSE]
    })
    output$author_info <- shiny::renderUI({
      found <- author_papers()
      if (is.null(found)) return(shiny::div(class = "empty-state", "Choose an author to list their papers in the current selection."))
      if (nrow(found) == 0L) return(shiny::div(class = "empty-state", "This author has no papers in the current selection."))
      years <- range(found$year, na.rm = TRUE)
      shiny::tags$p(shiny::tags$strong(input$author), ": ", nrow(found), if (nrow(found) == 1L) " paper" else " papers",
                    " in the current selection, ",
                    if (years[1] == years[2]) years[1] else paste0(years[1], " to ", years[2]), ".")
    })
    output$author_papers <- DT::renderDT({
      found <- author_papers()
      shiny::req(!is.null(found), nrow(found) > 0L)
      paper_list_widget(results_table(sort_papers(found), spec, track()))
    }, server = TRUE)
  })
}

# ---- Library -----------------------------------------------------------------

library_ui <- function(id, deps) {
  ns <- shiny::NS(id)
  track_names <- vapply(deps$spec$tracks, function(track) track$label, character(1))
  shiny::div(
    class = "tab-content-wrapper",
    shiny::div(
      class = "info-card",
      shiny::div(
        class = "library-header",
        shiny::tags$h4(class = "section-heading", shiny::textOutput(ns("heading"), inline = TRUE),
                       help_button("library", "About the library")),
        shiny::div(
          class = "library-actions",
          shiny::downloadButton(ns("bibtex"), "Export BibTeX", class = "btn-info btn-sm"),
          shiny::actionButton(ns("chat_open"), "Chat with collection", class = "btn-primary btn-sm"),
          help_button("chat", "About the chat"))),
      shiny::tags$p(class = "help-block",
                    "Bookmarks are stored in this browser. They cover all tracks and are not affected by the question or the filters."),
      shiny::uiOutput(ns("notice")),
      shiny::uiOutput(ns("body"))),
    shiny::div(
      class = "info-card rss-section",
      shiny::tags$h4(class = "section-heading", "Weekly digest feed"),
      shiny::tags$p("Each Monday a digest of the week's new papers in ", paste(track_names, collapse = ", "),
                    " is published as an RSS feed. The digest text is written by a language model from the factsheets. Paste the address below into a feed reader to subscribe."),
      shiny::div(class = "rss-feed-url",
                 shiny::tags$span(id = "weekly_feed_url", deps$settings$feed_url),
                 shiny::tags$button(type = "button", class = "btn btn-sm btn-info", `data-copy-from` = "weekly_feed_url",
                                    "Copy address")))
  )
}

library_server <- function(id, app, deps) {
  shiny::moduleServer(id, function(input, output, session) {
    spec <- deps$spec
    all_papers <- deps$data$papers
    ids <- shiny::reactive(unique(arxiv_base_id(as.character(unlist(app$bookmarks())))))
    saved <- shiny::reactive({
      found <- all_papers[all_papers$paper_id %in% ids() & all_papers$status != "failed", , drop = FALSE]
      sort_papers(found)
    })

    # A paper with a factsheet in two tracks is listed in both but exported once.
    saved_once <- shiny::reactive(saved()[!duplicated(saved()$paper_id), , drop = FALSE])
    output$heading <- shiny::renderText(paste0("Bookmarked papers (", nrow(saved_once()), ")"))
    output$notice <- shiny::renderUI({
      missing <- length(setdiff(ids(), saved()$paper_id))
      if (missing == 0L) return(NULL)
      shiny::tags$p(class = "ranking-warning", missing,
                    if (missing == 1L) " bookmarked paper is" else " bookmarked papers are",
                    " not in the current database and cannot be listed.")
    })
    output$body <- shiny::renderUI({
      if (nrow(saved()) == 0L) {
        return(shiny::div(class = "empty-state",
                          "No bookmarks yet. Use the star next to a paper, in any table or on a paper's page, to keep it here."))
      }
      DT::DTOutput(session$ns("table"))
    })
    output$table <- DT::renderDT({
      shiny::req(nrow(saved()) > 0L)
      paper_list_widget(results_table(saved(), spec, NULL))
    }, server = TRUE)

    output$bibtex <- shiny::downloadHandler(
      filename = function() "qe_arxiv_watch_bookmarks.bib",
      content = function(file) writeLines(bibtex_export(saved_once(), deps$settings), file, useBytes = TRUE))

    # ---- chat with the collection ----
    chat <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$chat_open, {
      if (nrow(saved()) == 0L) {
        shiny::showNotification("Bookmark at least one paper first.", type = "warning")
        return()
      }
      attached <- collection_chat_papers(saved_once())
      chat(list(papers = attached, session = NULL, first = TRUE))
      shiny::showModal(shiny::modalDialog(
        title = "Chat with your collection", size = "l", easyClose = FALSE, footer = shiny::modalButton("Close"),
        shiny::tags$p(
          class = "help-block",
          if (nrow(saved_once()) > nrow(attached)) {
            paste0("You have ", nrow(saved_once()), " bookmarks. The PDFs of the ", nrow(attached),
                   " most recent are sent to the model (the limit is ", COLLECTION_CHAT_MAX_PDFS, ").")
          } else {
            paste0("The PDFs of your ", nrow(attached), if (nrow(attached) == 1L) " bookmarked paper" else " bookmarked papers",
                   " are sent to the model with your first message (the limit is ", COLLECTION_CHAT_MAX_PDFS, ").")
          },
          " Model: ", shiny::tags$code(spec$models$chat), "."),
        shiny::div(class = "miami-chat-container collection-chat",
                   shinychat::chat_ui(session$ns("chat"), placeholder = "Ask about these papers", width = "100%", height = "420px",
                                      fill = FALSE))))
    })
    shiny::observeEvent(input$chat_user_input, {
      current <- chat()
      message <- input$chat_user_input
      if (is.null(current) || is.null(message) || !nzchar(message)) return()
      fail <- function(e) shinychat::chat_append("chat", chat_error_text(conditionMessage(e)))
      tryCatch({
        if (is.null(current$session)) current$session <- deps$chat_factory(collection_chat_system_prompt(spec))
        stream <- if (current$first) {
          do.call(current$session$stream_async,
                  c(list(message), lapply(current$papers$link_pdf, ellmer::content_pdf_url)))
        } else current$session$stream_async(message)
        current$first <- FALSE
        chat(current)
        promises::catch(shinychat::chat_append("chat", guard_math_stream(stream)), fail)
      }, error = fail)
    })
  })
}
