# Landscape tab: the shape of the current selection of papers. Every chart
# reads app$papers(), so it follows the question, the chips and the filters.

EMPTY_CHART_TEXT <- "No papers match the current filters, so there is nothing to draw. Remove a chip above."

chart_card <- function(title_output, help_output, ..., note = NULL) {
  shiny::div(class = "info-card chart-card",
             shiny::tags$h4(class = "section-heading", title_output, help_output),
             if (!is.null(note)) shiny::tags$p(class = "chart-subtitle", note),
             ...)
}

landscape_ui <- function(id, deps) {
  ns <- shiny::NS(id)
  plot <- function(name, height = "320px") plotly::plotlyOutput(ns(name), height = height)
  shiny::div(
    class = "tab-content-wrapper",
    chart_card(shiny::textOutput(ns("per_year_title"), inline = TRUE), help_button("per_year", "About this chart"),
               plot("per_year", "300px")),
    shiny::div(
      class = "chart-grid",
      chart_card(shiny::textOutput(ns("comp_a_title"), inline = TRUE), shiny::uiOutput(ns("comp_a_help"), inline = TRUE),
                 plot("comp_a", "auto")),
      chart_card(shiny::textOutput(ns("comp_b_title"), inline = TRUE), shiny::uiOutput(ns("comp_b_help"), inline = TRUE),
                 plot("comp_b", "auto"))),
    chart_card("Trends: papers per year carrying a label", shiny::uiOutput(ns("trend_help"), inline = TRUE),
               shiny::div(class = "chart-controls",
                          shiny::selectInput(ns("trend_field"), "Field", choices = NULL, width = "240px"),
                          shiny::selectizeInput(ns("trend_values"), paste0("Labels (up to ", TREND_MAX_SERIES, ")"),
                                                choices = NULL, multiple = TRUE, width = "100%",
                                                options = list(maxItems = TREND_MAX_SERIES, plugins = list("remove_button")))),
               plot("trends", "340px")),
    chart_card(shiny::textOutput(ns("gap_title"), inline = TRUE), shiny::uiOutput(ns("gap_help"), inline = TRUE),
               note = "Number of papers carrying both labels. Click a cell to see those papers.",
               shiny::div(class = "chart-controls",
                          shiny::selectInput(ns("gap_a"), "Rows", choices = NULL, width = "240px"),
                          shiny::selectInput(ns("gap_b"), "Columns", choices = NULL, width = "240px")),
               plot("gap", "auto")),
    chart_card(shiny::textOutput(ns("rising_title"), inline = TRUE), shiny::uiOutput(ns("rising_help"), inline = TRUE),
               note = shiny::textOutput(ns("rising_note"), inline = TRUE),
               shiny::div(class = "chart-controls",
                          shiny::selectInput(ns("rising_field"), "Field", choices = NULL, width = "240px")),
               plot("rising", "auto")),
    shiny::div(
      class = "chart-grid",
      chart_card(shiny::textOutput(ns("code_title"), inline = TRUE), shiny::uiOutput(ns("reuse_help"), inline = TRUE),
                 plot("code_share", "300px")),
      chart_card(shiny::textOutput(ns("platform_title"), inline = TRUE), help_button("reuse", "About this chart"),
                 plot("platforms", "auto"))),
    shiny::div(
      class = "chart-grid",
      chart_card(shiny::textOutput(ns("data_title"), inline = TRUE), shiny::uiOutput(ns("tested_help"), inline = TRUE),
                 plot("data_use", "auto")),
      chart_card(shiny::textOutput(ns("eval_title"), inline = TRUE), help_button("tested", "About this chart"),
                 plot("evaluation", "auto")))
  )
}

# Fields that can be charted in a scope: named vector, label -> field name.
chartable_fields <- function(spec, track = NULL) {
  fields <- if (is.null(track)) lapply(spec_shared_fields(spec), function(name) spec_field_any(spec, name))
            else spec_fields(spec, track)
  fields <- Filter(function(f) f$kind %in% c("single", "primary_additional", "multi") &&
                     isTRUE(f$filter %in% c("core", "more")), fields)
  stats::setNames(vapply(fields, function(f) f$name, character(1)), vapply(fields, function(f) f$label, character(1)))
}

# The two fields of the composition charts and the default gap map.
default_chart_fields <- function(spec, settings, track = NULL) {
  if (is.null(track)) return(c(settings$domain_field, settings$data_field))
  unname(unlist(spec_track(spec, track)$core_filters))
}

help_topic_with_fields <- function(topic, track, names) {
  paste(c(topic, paste0(track %||% "", ":", names)), collapse = ";")
}

landscape_server <- function(id, app, deps) {
  shiny::moduleServer(id, function(input, output, session) {
    spec <- deps$spec
    settings <- deps$settings
    ns <- session$ns
    track <- shiny::reactive(app$filters()$track)
    papers <- shiny::reactive(app$papers())
    accent <- shiny::reactive(if (is.null(track())) ALL_TRACKS_ACCENT else spec_track(spec, track())$color)
    n_text <- shiny::reactive(paste0(format(nrow(papers()), big.mark = ","), " papers in the current selection"))
    field_of <- function(name) spec_field_any(spec, name, condition_track(spec, name, NULL, track()))
    need_papers <- function() shiny::validate(shiny::need(nrow(papers()) > 0L, EMPTY_CHART_TEXT))
    null_values <- function(field) c(field_null_values(field))

    add_condition <- function(name, values, role = "any") {
      state <- shiny::isolate(app$filters())
      app$set_filters(set_condition(state, name, values, condition_track(spec, name, NULL, state$track), role))
    }
    clicked <- function(source) plotly_click_value(session, ns(source))

    # ---- papers per year ----
    output$per_year_title <- shiny::renderText(paste0("Papers per year (", n_text(), ")"))
    output$per_year <- plotly::renderPlotly({
      need_papers()
      chart_per_year(papers_per_year(papers()), spec, accent(), source = ns("per_year"))
    })
    shiny::observeEvent(clicked("per_year"), {
      year <- suppressWarnings(as.integer(clicked("per_year")))
      if (is.na(year)) return()
      state <- shiny::isolate(app$filters())
      state$year_from <- state$year_to <- year
      app$set_filters(normalize_state(state))
    })

    # ---- composition ----
    comp_fields <- shiny::reactive(default_chart_fields(spec, settings, track()))
    composition_chart <- function(slot, index) {
      field <- shiny::reactive(field_of(comp_fields()[index]))
      counts <- shiny::reactive(composition(papers(), primary_column(field()), exclude = null_values(field())))
      output[[paste0(slot, "_title")]] <- shiny::renderText({
        total <- if (nrow(counts()) > 0L) counts()$total[1] else 0L
        what <- if (identical(field()$kind, "multi")) "papers carrying each label" else "papers by primary label"
        paste0(field()$label, ": ", what, " (of ", format(total, big.mark = ","), " labelled papers)")
      })
      output[[paste0(slot, "_help")]] <- shiny::renderUI(
        help_button(help_topic_with_fields("composition", track(), field()$name), "About this chart"))
      output[[slot]] <- plotly::renderPlotly({
        need_papers()
        shiny::validate(shiny::need(nrow(counts()) > 0L, "This field is not recorded for the selected papers."))
        chart_bars(counts(), accent(), source = ns(slot))
      })
      shiny::observeEvent(clicked(slot), {
        add_condition(field()$name, clicked(slot),
                      if (identical(field()$kind, "primary_additional")) "primary" else "any")
      })
    }
    composition_chart("comp_a", 1L)
    composition_chart("comp_b", 2L)

    # ---- field selectors ----
    shiny::observeEvent(track(), {
      choices <- chartable_fields(spec, track())
      defaults <- default_chart_fields(spec, settings, track())
      shiny::updateSelectInput(session, "trend_field", choices = choices, selected = defaults[1])
      shiny::updateSelectInput(session, "gap_a", choices = choices, selected = defaults[1])
      shiny::updateSelectInput(session, "gap_b", choices = choices, selected = defaults[2])
      shiny::updateSelectInput(session, "rising_field", choices = choices, selected = defaults[1])
    }, ignoreNULL = FALSE)

    valid_field <- function(name) {
      if (is.null(name) || !nzchar(name) || !name %in% chartable_fields(spec, track())) return(NULL)
      field_of(name)
    }

    # ---- trends ----
    trend_field <- shiny::reactive(valid_field(input$trend_field))
    shiny::observeEvent(list(trend_field(), track()), {
      field <- trend_field()
      if (is.null(field)) return()
      counts <- composition(shiny::isolate(papers()), field$name, exclude = null_values(field))
      choices <- field_choices(field)
      top <- utils::head(intersect(counts$value, choices), 3L)
      shiny::updateSelectizeInput(session, "trend_values", choices = stats::setNames(choices, value_label(choices)),
                                  selected = top)
    })
    output$trend_help <- shiny::renderUI(
      help_button(help_topic_with_fields("trends", track(), input$trend_field %||% character(0)), "About this chart"))
    output$trends <- plotly::renderPlotly({
      need_papers()
      field <- trend_field()
      shiny::validate(shiny::need(!is.null(field) && length(input$trend_values) > 0L,
                                  "Choose a field and at least one label."))
      chart_trends(tag_trend(papers(), field$name, intersect(input$trend_values, field_choices(field))))
    })

    # ---- gap map ----
    gap_fields <- shiny::reactive(list(a = valid_field(input$gap_a), b = valid_field(input$gap_b)))
    gap <- shiny::reactive({
      fields <- gap_fields()
      if (is.null(fields$a) || is.null(fields$b)) return(NULL)
      gap_map(papers(), fields$a$name, fields$b$name,
              exclude = c(null_values(fields$a), null_values(fields$b)))
    })
    output$gap_title <- shiny::renderText({
      fields <- gap_fields()
      if (is.null(fields$a) || is.null(fields$b)) return("Gap map")
      paste0("Gap map: ", fields$a$label, " by ", fields$b$label, " (",
             format(gap()$papers %||% 0L, big.mark = ","), " papers with both recorded)")
    })
    output$gap_help <- shiny::renderUI(
      help_button(help_topic_with_fields("gap_map", track(), c(input$gap_a, input$gap_b)), "About this chart"))
    output$gap <- plotly::renderPlotly({
      need_papers()
      shiny::validate(
        shiny::need(!is.null(gap()), "Choose two fields."),
        shiny::need(!identical(input$gap_a, input$gap_b), "Choose two different fields."),
        shiny::need(nrow(gap()$cells) > 0L, "These two fields are not both recorded for any selected paper."))
      chart_gap_map(gap(), source = ns("gap"))
    })
    gap_click <- shiny::reactive(plotly_click_event(session, ns("gap")))
    shiny::observeEvent(gap_click(), {
      fields <- gap_fields()
      values <- if (!is.null(gap())) gap_map_click(gap_click(), gap())
      if (is.null(values) || is.null(fields$a) || is.null(fields$b)) return()
      state <- shiny::isolate(app$filters())
      state <- set_condition(state, fields$a$name, values[1], condition_track(spec, fields$a$name, NULL, state$track))
      state <- set_condition(state, fields$b$name, values[2], condition_track(spec, fields$b$name, NULL, state$track))
      app$set_filters(state)
    })

    # ---- what is rising ----
    latest_year <- as.integer(deps$data$year_range[2])
    # One field at a time: the labels of a field answer the same question, so
    # their rises and falls can be read against each other.
    rising_field <- shiny::reactive(valid_field(input$rising_field))
    rising <- shiny::reactive({
      field <- rising_field()
      if (is.null(field)) return(rising_tags(papers()[0, , drop = FALSE], character(0), latest_year))
      rising_tags(papers(), stats::setNames(field$name, field$label), latest_year, exclude = null_values(field))
    })
    output$rising_title <- shiny::renderText({
      field <- rising_field()
      paste0("What is rising", if (is.null(field)) "" else paste0(" in ", field$label),
             ": change in the share of papers carrying a label, ", latest_year - RISING_RECENT_YEARS + 1L,
             " to ", latest_year, " against earlier years")
    })
    output$rising_note <- shiny::renderText({
      rows <- rising()
      if (nrow(rows) == 0L) return("")
      paste0(rows$papers_recent[1], " papers in the recent years and ", rows$papers_earlier[1],
             " earlier. Each bar gives the counts behind the two shares. Labels on fewer than ",
             RISING_MIN_PAPERS, " papers are left out.")
    })
    output$rising_help <- shiny::renderUI(
      help_button(help_topic_with_fields("rising", track(), input$rising_field %||% character(0)), "About this chart"))
    output$rising <- plotly::renderPlotly({
      need_papers()
      shiny::validate(shiny::need(
        nrow(rising()) > 0L,
        paste0("A comparison needs papers both in the last ", RISING_RECENT_YEARS,
               " years and before them, and labels on at least ", RISING_MIN_PAPERS,
               " papers. The current selection does not have that.")))
      chart_rising(rising(), accent())
    })

    # ---- can I reuse it ----
    code_share <- shiny::reactive(code_share_by_year(papers()))
    output$code_title <- shiny::renderText(paste0(
      "Can I reuse it: share of each year's papers with public code (",
      format(sum(code_share()$papers), big.mark = ","), " papers with a recorded code status)"))
    output$reuse_help <- shiny::renderUI(
      help_button(help_topic_with_fields("reuse", NULL, c(settings$code_field, settings$platform_field)), "About this chart"))
    output$code_share <- plotly::renderPlotly({
      need_papers()
      shiny::validate(shiny::need(nrow(code_share()) > 0L, "Code status is not recorded for the selected papers."))
      chart_code_share(code_share(), accent())
    })
    platform_field <- spec_field_any(spec, settings$platform_field)
    platforms <- shiny::reactive(composition(papers(), platform_field$name, exclude = null_values(platform_field)))
    output$platform_title <- shiny::renderText(paste0(
      "Software named in the papers (", format(sum(!has_any_value(papers()[[platform_field$name]], platform_field$sentinel) &
                                                    !is.na(papers()[[platform_field$name]])), big.mark = ","),
      " of ", format(nrow(papers()), big.mark = ","), " papers name any)"))
    output$platforms <- plotly::renderPlotly({
      need_papers()
      shiny::validate(shiny::need(nrow(platforms()) > 0L, "No selected paper names its software."))
      chart_bars(platforms(), accent(), source = ns("platforms"), show_share = FALSE)
    })
    shiny::observeEvent(clicked("platforms"), add_condition(platform_field$name, clicked("platforms")))

    # ---- how is it tested ----
    output$data_title <- shiny::renderText(paste0("How is it tested: data analysed (", n_text(), ")"))
    output$tested_help <- shiny::renderUI(
      help_button(help_topic_with_fields("tested", NULL, settings$data_field), "About this chart"))
    output$data_use <- plotly::renderPlotly({
      need_papers()
      split <- data_use_split(papers(), settings, spec)
      split$label <- split$group
      split$value <- split$group
      chart_bars(split[split$n > 0L, , drop = FALSE], accent())
    })
    evaluation_field <- shiny::reactive({
      if (is.null(track())) NULL else spec_field(spec, track(), settings$evaluation_field)
    })
    evaluation <- shiny::reactive({
      field <- evaluation_field()
      if (is.null(field)) return(NULL)
      composition(papers(), field$name, exclude = null_values(field))
    })
    output$eval_title <- shiny::renderText({
      counts <- evaluation()
      total <- if (!is.null(counts) && nrow(counts) > 0L) counts$total[1] else 0L
      paste0("Kinds of evaluation used (of ", format(total, big.mark = ","), " papers with this recorded)")
    })
    output$evaluation <- plotly::renderPlotly({
      shiny::validate(shiny::need(!is.null(evaluation_field()),
                                  "Kinds of evaluation are defined per track. Select one track to see them."))
      need_papers()
      shiny::validate(shiny::need(nrow(evaluation()) > 0L, "Evaluation is not recorded for the selected papers."))
      chart_bars(evaluation(), accent(), source = ns("evaluation"))
    })
    shiny::observeEvent(clicked("evaluation"), add_condition(evaluation_field()$name, clicked("evaluation")))
  })
}
