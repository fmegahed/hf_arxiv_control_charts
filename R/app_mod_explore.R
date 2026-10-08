# Explore tab: always-visible filters and the results table.
#
# `app` is the bundle of shared reactives built in app_server.R:
#   app$filters()        current filter state        app$set_filters(state)
#   app$papers()         papers selected by it       app$ranking()  relevance scores or NULL
# The inputs here are a second view of the same state as the chips.

EMPTY_RESULTS_TEXT <- "No papers match these filters. Remove a chip above, or use \"clear all\"."

explore_ui <- function(id, deps) {
  ns <- shiny::NS(id)
  years <- deps$data$year_range
  domain <- spec_field_any(deps$spec, deps$settings$domain_field)
  shiny::div(
    class = "tab-content-wrapper",
    shiny::div(
      class = "info-card filter-card",
      shiny::tags$h4(class = "section-heading", "Filters", help_button("explore", "How the filters work")),
      shiny::div(
        class = "filter-grid",
        shiny::sliderInput(ns("years"), "Year submitted", min = years[1], max = years[2], value = years,
                           step = 1, sep = "", ticks = FALSE, width = "100%"),
        shiny::conditionalPanel(
          "output.has_track", ns = ns, class = "filter-pair",
          shiny::selectizeInput(ns("topic"), "Topic", choices = NULL, multiple = TRUE, width = "100%",
                                options = list(placeholder = "Any", plugins = list("remove_button"))),
          shiny::selectizeInput(ns("method"), "Method", choices = NULL, multiple = TRUE, width = "100%",
                                options = list(placeholder = "Any", plugins = list("remove_button")))),
        shiny::selectizeInput(ns("domain"), domain$label, multiple = TRUE, width = "100%",
                              choices = stats::setNames(field_choices(domain), value_label(field_choices(domain))),
                              options = list(placeholder = "Any", plugins = list("remove_button")))),
      shiny::conditionalPanel(
        "!output.has_track", ns = ns,
        shiny::tags$p(class = "help-block", "Topic and method filters are specific to a track. They appear when one track is selected.")),
      shiny::div(
        class = "toggle-row",
        shiny::checkboxInput(ns("public_code"), "Public code", FALSE),
        shiny::checkboxInput(ns("real_data"), "Uses real data", FALSE),
        shiny::checkboxInput(ns("reviews_only"), "Reviews and tutorials only", FALSE),
        shiny::checkboxInput(ns("include_screened"), "Include screened-out papers", FALSE))),
    shiny::div(
      class = "info-card",
      shiny::div(
        class = "results-head",
        shiny::tags$h4(class = "section-heading", shiny::textOutput(ns("heading"), inline = TRUE)),
        shiny::div(
          class = "results-tools",
          shiny::selectInput(ns("sort"), "Sort by", choices = c("Newest first" = "newest", "Oldest first" = "oldest"),
                             width = "190px"),
          shiny::selectizeInput(ns("columns"), "More columns", choices = NULL, multiple = TRUE, width = "260px",
                                options = list(placeholder = "Add columns", plugins = list("remove_button"))),
          shiny::downloadButton(ns("download"), "Download CSV", class = "btn-info btn-sm"))),
      shiny::uiOutput(ns("ranking_note")),
      shiny::uiOutput(ns("main")),
      shiny::uiOutput(ns("less_likely")))
  )
}

# DT widget for a results table; all cells are plain text (see app_table.R).
results_widget <- function(table, page_length = RESULTS_PAGE_LENGTH) {
  names <- names(table)
  index <- function(name) match(name, names) - 1L
  defs <- list(
    list(targets = c(index("paper_id"), index("row_track"), index("status"), index("href")), visible = FALSE, searchable = FALSE),
    list(targets = index("Saved"), render = DT::JS("QEW.render.bookmark"), width = "34px", title = "",
         searchable = FALSE, className = "col-bookmark"),
    list(targets = index("Title"), render = DT::JS("QEW.render.title"), className = "col-title"),
    list(targets = index("Code"), render = DT::JS("QEW.render.code")))
  if ("Relevance" %in% names) {
    defs <- c(defs, list(list(targets = index("Relevance"), render = DT::JS("QEW.render.relevance"),
                              className = "col-relevance", searchable = FALSE)))
  }
  DT::datatable(
    table, rownames = FALSE, escape = TRUE, selection = "none", class = "stripe hover results-table",
    options = list(pageLength = page_length, lengthMenu = c(15, 30, 60), ordering = FALSE, dom = "ftlip",
                   scrollX = TRUE, autoWidth = FALSE, columnDefs = defs,
                   language = list(search = "Find in these results:", emptyTable = "No papers to show."),
                   drawCallback = DT::JS("function() { QEW.afterDraw(this.api().table().container()); }")))
}

explore_server <- function(id, app, deps) {
  shiny::moduleServer(id, function(input, output, session) {
    spec <- deps$spec
    settings <- deps$settings
    years <- as.integer(deps$data$year_range)

    track <- shiny::reactive(app$filters()$track)
    # Read by the conditional panels: "1" when one track is selected, else "".
    output$has_track <- shiny::renderText(if (is.null(track())) "" else "1")
    shiny::outputOptions(output, "has_track", suspendWhenHidden = FALSE)

    core_field <- function(which) {
      if (is.null(track())) return(NULL)
      spec_field(spec, track(), spec_track(spec, track())$core_filters[[which]])
    }

    # ---- state -> inputs ----
    same_set <- function(a, b) setequal(a %||% character(0), b %||% character(0))

    shiny::observeEvent(track(), {
      for (which in c("topic", "method")) {
        field <- core_field(which)
        if (is.null(field)) next
        choices <- field_choices(field)
        shiny::updateSelectizeInput(
          session, which, label = field$label, choices = stats::setNames(choices, value_label(choices)),
          selected = get_condition_values(shiny::isolate(app$filters()), field$name, track()))
      }
      shiny::updateSelectizeInput(session, "columns", choices = extra_column_choices(spec, track()),
                                  selected = character(0))
    }, ignoreNULL = FALSE)

    shiny::observe({
      state <- app$filters()
      wanted_years <- c(state$year_from %||% years[1], state$year_to %||% years[2])
      if (!identical(as.integer(shiny::isolate(input$years)), as.integer(wanted_years))) {
        shiny::updateSliderInput(session, "years", value = wanted_years)
      }
      for (which in c("topic", "method")) {
        field <- shiny::isolate(core_field(which))
        if (is.null(field)) next
        wanted <- get_condition_values(state, field$name, state$track)
        if (!same_set(shiny::isolate(input[[which]]), wanted)) {
          shiny::updateSelectizeInput(session, which, selected = wanted)
        }
      }
      wanted <- get_condition_values(state, settings$domain_field)
      if (!same_set(shiny::isolate(input$domain), wanted)) {
        shiny::updateSelectizeInput(session, "domain", selected = wanted)
      }
      for (flag in c("public_code", "real_data", "reviews_only", "include_screened")) {
        if (!identical(isTRUE(shiny::isolate(input[[flag]])), isTRUE(state[[flag]]))) {
          shiny::updateCheckboxInput(session, flag, value = isTRUE(state[[flag]]))
        }
      }
      can_rank <- nzchar(state$residual) || length(state$keywords) > 0L
      choices <- c("Newest first" = "newest", "Oldest first" = "oldest",
                   if (can_rank) c("Most relevant first" = "relevance"))
      shiny::updateSelectInput(session, "sort", choices = choices, selected = state$sort)
    })

    # ---- inputs -> state ----
    change <- function(update) {
      state <- shiny::isolate(app$filters())
      new <- normalize_state(update(state))
      if (!identical(new, normalize_state(state))) app$set_filters(new)
    }

    shiny::observeEvent(input$years, change(function(state) {
      picked <- as.integer(input$years)
      state$year_from <- if (picked[1] > years[1]) picked[1]
      state$year_to <- if (picked[2] < years[2]) picked[2]
      state
    }), ignoreInit = TRUE)

    for (which in c("topic", "method")) local({
      slot <- which
      shiny::observeEvent(input[[slot]], change(function(state) {
        field <- shiny::isolate(core_field(slot))
        if (is.null(field)) return(state)
        if (same_set(input[[slot]], get_condition_values(state, field$name, state$track))) return(state)
        set_condition(state, field$name, input[[slot]], state$track)
      }), ignoreNULL = FALSE, ignoreInit = TRUE)
    })

    shiny::observeEvent(input$domain, change(function(state) {
      if (same_set(input$domain, get_condition_values(state, settings$domain_field))) return(state)
      set_condition(state, settings$domain_field, input$domain)
    }), ignoreNULL = FALSE, ignoreInit = TRUE)

    for (flag in c("public_code", "real_data", "reviews_only", "include_screened")) local({
      slot <- flag
      shiny::observeEvent(input[[slot]], change(function(state) {
        state[[slot]] <- isTRUE(input[[slot]])
        state
      }), ignoreInit = TRUE)
    })

    shiny::observeEvent(input$sort, change(function(state) {
      if (isTRUE(input$sort %in% SORT_CHOICES)) state$sort <- input$sort
      state
    }), ignoreInit = TRUE)

    # ---- results ----
    grouped <- shiny::reactive({
      papers <- app$papers()
      ranking <- app$ranking()
      state <- app$filters()
      if (is.null(ranking)) {
        return(list(likely = sort_papers(papers, state$sort), less_likely = NULL, scores = NULL))
      }
      if (ranking$threshold <= 0) {
        # A keyword search: every listed paper matched, so there is one list.
        return(list(likely = sort_papers(papers, state$sort, ranking$scores), less_likely = NULL,
                    scores = ranking$scores))
      }
      groups <- group_by_relevance(papers, ranking$scores, ranking$threshold)
      if (!identical(state$sort, "relevance")) {
        groups <- lapply(groups, sort_papers, sort = state$sort)
      }
      c(groups, list(scores = ranking$scores))
    })

    output$heading <- shiny::renderText({
      groups <- grouped()
      n <- nrow(groups$likely)
      if (is.null(groups$less_likely)) paste0("Papers (", format(n, big.mark = ","), ")")
      else paste0("Likely matches (", format(n, big.mark = ","), ")")
    })

    output$ranking_note <- shiny::renderUI({
      ranking <- app$ranking()
      if (is.null(ranking)) return(NULL)
      what <- if (identical(ranking$source, "jev")) "Relevance to: " else "Keyword match with: "
      shiny::div(
        class = "ranking-note",
        shiny::tags$strong(what), ranking$label, help_button("relevance", "How relevance is computed"),
        if (isTRUE(ranking$capped)) shiny::tags$span(
          class = "ranking-cap",
          paste0(" Only the ", ranking$considered, " most recent of ", ranking$total,
                 " filtered papers were scored. Add filters to rank them all.")),
        if (nzchar(ranking$message %||% "")) shiny::tags$div(class = "ranking-warning", ranking$message))
    })

    table_for <- function(papers) {
      results_table(papers, spec, track(), extra = input$columns, scores = grouped()$scores)
    }

    output$main <- shiny::renderUI({
      if (nrow(grouped()$likely) == 0L) {
        text <- if (is.null(grouped()$less_likely) || nrow(grouped()$less_likely) == 0L) EMPTY_RESULTS_TEXT
                else "No paper reached the relevance threshold. The less likely matches are listed below."
        return(shiny::div(class = "empty-state", text))
      }
      DT::DTOutput(session$ns("table"))
    })
    output$table <- DT::renderDT(results_widget(table_for(grouped()$likely)), server = TRUE)

    output$less_likely <- shiny::renderUI({
      less <- grouped()$less_likely
      if (is.null(less) || nrow(less) == 0L) return(NULL)
      shiny::tags$details(
        class = "less-likely",
        shiny::tags$summary(paste0("Less likely matches (", format(nrow(less), big.mark = ","), ")")),
        shiny::tags$p(class = "help-block",
                      "These papers passed the filters but scored below the threshold, or were not scored. They are kept so nothing is hidden."),
        DT::DTOutput(session$ns("less_table")))
    })
    output$less_table <- DT::renderDT({
      less <- grouped()$less_likely
      shiny::req(!is.null(less), nrow(less) > 0L)
      results_widget(table_for(less))
    }, server = TRUE)

    output$download <- shiny::downloadHandler(
      filename = function() paste0("qe_arxiv_watch_", app$filters()$track %||% "all_tracks", "_results.csv"),
      content = function(file) {
        groups <- grouped()
        papers <- rbind(groups$likely, groups$less_likely)
        readr::write_csv(export_table(papers, spec, groups$scores), file, na = "")
      })
  })
}
