# The server: one shared view (track, filters, tab, open paper) that the URL,
# the question box, the chips and every tab read and write.
#
# `deps` carries everything the server needs from outside, so tests can pass
# fakes: spec, settings, data, ctx (help context), question_model_fn,
# rank_fn and chat_factory.

app_deps <- function(spec = spec_load(), settings = app_settings_load(), data = load_app_data(spec),
                     reliability = reliability_load(), question_fn = NULL, rank_fn = rank_papers,
                     chat_factory = function(system_prompt) create_chat("openai", spec$models$chat, system_prompt)) {
  if (is.null(question_fn)) question_fn <- question_model_fn(spec)
  list(spec = spec, settings = settings, data = data,
       ctx = if (!is.null(data$papers)) help_context(spec, settings, data, reliability),
       question_model_fn = question_fn, rank_fn = rank_fn, chat_factory = chat_factory)
}

# Choices of the "add filter" list: fields grouped as in the specification.
# Values are "track:field" ("" for a field shared by all tracks).
add_filter_choices <- function(spec, track = NULL) {
  shared <- spec_shared_fields(spec)
  entries <- list()
  add <- function(group, label, value) {
    entries[[group]] <<- c(entries[[group]], stats::setNames(value, label))
  }
  tracks <- if (is.null(track)) names(spec$tracks) else track
  seen <- character(0)
  for (id in tracks) {
    for (filter in spec_filters(spec, id)) {
      is_shared <- filter$column %in% shared
      key <- paste0(if (is_shared) "" else id, ":", filter$column)
      if (key %in% seen) next
      seen <- c(seen, key)
      group <- if (is_shared || !is.null(track)) filter$group
               else paste0(spec$tracks[[id]]$short_label, " papers only: ", filter$group)
      add(group, filter$label, key)
    }
  }
  lapply(entries, as.list)
}

parse_field_key <- function(key) {
  parts <- strsplit(key %||% "", ":", fixed = TRUE)[[1]]
  if (length(parts) != 2L) return(NULL)
  list(track = if (nzchar(parts[1])) parts[1] else NULL, field = parts[2])
}

scope_category_text <- function(spec, category) {
  for (item in spec$scope_categories) if (identical(item[[1]], category)) return(item[[2]])
  "Reason not recorded."
}

app_server <- function(deps) {
  spec <- deps$spec
  settings <- deps$settings
  data <- deps$data

  function(input, output, session) {
    if (is.null(data$papers)) return(invisible(NULL))
    all_papers <- data$papers
    year_range <- data$year_range

    nav <- shiny::reactiveValues(view = "landing", tab = "explore", paper = NULL, paper_track = NULL)
    filters <- shiny::reactiveVal(new_filter_state())
    set_filters <- function(state) filters(normalize_state(state))
    note <- shiny::reactiveVal(NULL)
    asked <- shiny::reactiveVal(numeric(0))
    score_cache <- shiny::reactiveVal(NULL)
    ranking <- shiny::reactiveVal(NULL)

    current_view <- shiny::reactive(new_view(nav$view, nav$tab, nav$paper, filters(), nav$paper_track))
    apply_view <- function(view) {
      nav$view <- view$view
      nav$tab <- view$tab
      nav$paper <- view$paper
      nav$paper_track <- view$paper_track
      set_filters(view$filters)
    }

    filtered <- shiny::reactive(apply_filters(all_papers, filters(), spec, settings))
    counts <- shiny::reactive(scope_counts(all_papers, filters(), spec, settings))

    output$mode <- shiny::renderText({
      if (identical(nav$view, "landing")) "landing" else if (!is.null(nav$paper)) "paper" else "tabs"
    })
    shiny::outputOptions(output, "mode", suspendWhenHidden = FALSE)

    # ---- URL <-> view ----
    shiny::observeEvent(session$clientData$url_search, {
      view <- decode_view(session$clientData$url_search, spec, year_range)
      if (!identical(encode_view(view), encode_view(shiny::isolate(current_view())))) {
        note(NULL)
        apply_view(view)
      }
    }, priority = 100)

    query <- shiny::debounce(shiny::reactive(encode_view(current_view())), 250)
    shiny::observeEvent(query(), {
      in_browser <- encode_view(decode_view(shiny::isolate(session$clientData$url_search), spec, year_range))
      if (identical(query(), in_browser)) return()
      # The landing page has no parameters; a bare "?" is the empty query string.
      shiny::updateQueryString(if (nzchar(query())) query() else "?", mode = "push", session = session)
    }, ignoreInit = TRUE)

    # ---- tabs ----
    shiny::observeEvent(input$main_tabs, {
      if (isTRUE(input$main_tabs %in% APP_TABS)) nav$tab <- input$main_tabs
    }, ignoreInit = TRUE)
    shiny::observe({
      if (!identical(shiny::isolate(input$main_tabs), nav$tab)) {
        shiny::updateTabsetPanel(session, "main_tabs", selected = nav$tab)
      }
    })

    # ---- navigation events sent from the page (www/qew.js) ----
    shiny::observeEvent(input$qew_select_track, {
      track <- input$qew_select_track
      note(NULL)
      apply_view(new_view("browse", filters = new_filter_state(if (track %in% names(spec$tracks)) track)))
    })
    shiny::observeEvent(input$go_home, {
      note(NULL)
      apply_view(new_view())
    })
    shiny::observeEvent(input$qew_open_paper, {
      # The page sends the id, and the track of the row that was clicked.
      clicked <- input$qew_open_paper
      id <- if (is.list(clicked)) clicked$id else clicked
      track <- if (is.list(clicked)) clicked$track else NULL
      if (is.null(id) || !nzchar(id)) return()
      nav$paper <- arxiv_base_id(id)
      nav$paper_track <- if (isTRUE(track %in% names(spec$tracks)) && !identical(track, filters()$track)) track
      if (identical(nav$view, "landing")) nav$view <- "browse"
    })
    shiny::observeEvent(input$qew_tag_click, {
      click <- input$qew_tag_click
      checked <- sanitize_state(
        list(conditions = list(new_condition(click$field, click$value,
                                             if (nzchar(click$track %||% "")) click$track,
                                             click$role %||% "any"))),
        spec, year_range)$state
      if (length(checked$conditions) == 0L) return()
      cond <- checked$conditions[[1]]
      state <- filters()
      if (!is.null(cond$track) && !is.null(state$track) && !identical(cond$track, state$track)) {
        state <- new_filter_state(cond$track)
      }
      set_filters(set_condition(state, cond$field, cond$values, cond$track, cond$role))
      nav$paper <- NULL
      nav$tab <- "explore"
    })
    shiny::observeEvent(input$qew_chip_remove, {
      set_filters(remove_chip(filters(), input$qew_chip_remove))
    })
    shiny::observeEvent(input$clear_all, {
      note(NULL)
      set_filters(clear_filters(filters()))
    })

    # ---- help ----
    shiny::observeEvent(input$qew_help, {
      pieces <- strsplit(input$qew_help, ";", fixed = TRUE)[[1]]
      fields <- Filter(Negate(is.null), lapply(pieces[-1], function(piece) {
        key <- parse_field_key(piece)
        if (is.null(key)) return(NULL)
        tryCatch(spec_field_any(spec, key$field, key$track), error = function(e) NULL)
      }))
      content <- help_content(pieces[1], deps$ctx, track = filters()$track, fields = fields)
      shiny::showModal(shiny::modalDialog(title = content$title, shiny::div(class = "help-body", content$body),
                                          easyClose = TRUE, size = "l", footer = shiny::modalButton("Close")))
    })

    # ---- asking a question ----
    ask <- function(question, context_track) {
      problem <- check_question(question)
      if (is.null(problem) && !rate_limit_ok(asked())) {
        problem <- paste0("This session has reached the limit of ", QUESTION_RATE_LIMIT, " questions in ",
                          QUESTION_RATE_WINDOW_SEC / 60, " minutes. The filters still work.")
      }
      if (!is.null(problem)) {
        note(list(kind = "warning", text = problem))
        if (identical(nav$view, "landing")) shiny::showNotification(problem, type = "warning")
        return(invisible(NULL))
      }
      asked(c(asked(), as.numeric(Sys.time())))
      busy <- shiny::showNotification("Reading the question", duration = NULL, closeButton = FALSE)
      on.exit(shiny::removeNotification(busy), add = TRUE)
      result <- interpret_question(question, spec, year_range, context_track, model_fn = deps$question_model_fn)
      note(list(kind = result$mode, text = result$message, dropped = result$dropped, question = trimws(question)))
      apply_view(new_view("browse", filters = result$state))
    }
    shiny::observeEvent(input$ask_go, ask(input$question, filters()$track))
    shiny::observeEvent(input$ask_landing, {
      ask(input$question_landing, NULL)
      # Carry the question over to the box of the view that opens.
      shiny::updateTextInput(session, "question", value = input$question_landing)
    })

    # ---- relevance ranking ----
    ranking_input <- shiny::debounce(shiny::reactive({
      state <- filters()
      list(residual = state$residual, keywords = state$keywords, ids = filtered()$paper_id)
    }), 300)
    shiny::observeEvent(ranking_input(), {
      state <- filters()
      papers <- filtered()
      if (nzchar(state$residual)) {
        busy <- shiny::showNotification("Ranking papers by relevance", duration = NULL, closeButton = FALSE)
        on.exit(shiny::removeNotification(busy), add = TRUE)
        updated <- ranking_update(score_cache(), papers, state$residual, settings, rank_fn = deps$rank_fn)
        score_cache(updated$cache)
        ranking(updated$ranking)
      } else if (length(state$keywords) > 0L) {
        ranking(keyword_ranking(papers, state$keywords))
      } else {
        ranking(NULL)
      }
    })
    # Scores belong to one residual and one set of papers; until they are
    # recomputed for the current ones, the table shows no ranking.
    current_ranking <- shiny::reactive({
      found <- ranking()
      state <- filters()
      wanted <- if (nzchar(state$residual)) state$residual else paste(state$keywords, collapse = ", ")
      if (is.null(found) || !nzchar(wanted) || !identical(found$label, wanted)) return(NULL)
      found
    })

    # ---- header, chips, scope line ----
    output$accent_style <- shiny::renderUI({
      track <- filters()$track
      color <- if (is.null(track)) ALL_TRACKS_ACCENT else spec_track(spec, track)$color
      shiny::tags$style(shiny::HTML(paste0(":root { --track-color: ", color, "; --track-color-dark: ",
                                           darken(color), "; }")))
    })
    output$header <- shiny::renderUI({
      track <- filters()$track
      shiny::div(
        class = "app-header",
        shiny::div(
          class = "header-content",
          shiny::div(class = "header-left",
                     shiny::tags$h1(paste0("QE ArXiv Watch: ", if (is.null(track)) "All tracks" else spec_track(spec, track)$label)),
                     shiny::tags$p(class = "subtitle",
                                   if (is.null(track)) "Papers from every track. Only fields shared by all tracks filter all of them."
                                   else spec_track(spec, track)$description)),
          shiny::div(class = "header-right",
                     shiny::actionButton("go_home", "Home and tracks", class = "btn-header"),
                     shiny::tags$p(paste0("Data current as of ", format(data$data_date, "%d %B %Y"))))))
    })

    output$chips <- shiny::renderUI({
      chips <- state_chips(filters(), spec)
      shiny::tagList(
        shiny::tags$span(class = "read-as-label", if (length(chips) > 0L) "Read as:" else "No filters."),
        lapply(chips, function(chip) {
          shiny::tags$span(class = paste("filter-chip", if (identical(chip$id, "track")) "chip-track"),
                           chip$label,
                           shiny::tags$button(type = "button", class = "chip-remove", `data-chip-remove` = chip$id,
                                              `aria-label` = paste0("Remove filter: ", chip$label), "×"))
        }))
    })

    output$note <- shiny::renderUI({
      current <- note()
      if (is.null(current)) return(NULL)
      label <- switch(current$kind, model = "How the question was read: ", keyword = "Keyword search: ", "")
      shiny::div(class = paste("ask-note", paste0("ask-note-", current$kind)), role = "status",
                 shiny::tags$strong(label), current$text,
                 if (length(current$dropped) > 0L) shiny::tags$div(
                   class = "ask-dropped", "Ignored because it is not in the list of allowed values: ",
                   paste(current$dropped, collapse = "; "), "."))
    })

    output$scope_line <- shiny::renderUI({
      n <- counts()
      state <- filters()
      noun <- if (isTRUE(state$include_screened)) "papers (in scope and screened out)" else "in-scope papers"
      shiny::tagList(
        shiny::tags$span(class = "scope-count", "Showing ", shiny::tags$strong(format(n$shown, big.mark = ",")),
                         " of ", format(n$total, big.mark = ","), " ", noun),
        if (has_active_filters(state)) shiny::actionLink("clear_all", "clear all", class = "clear-all"),
        help_button("scope", "Which papers are counted"),
        shiny::tags$details(
          class = "scope-details",
          shiny::tags$summary(paste0(
            format(n$screened_out, big.mark = ","), " screened out",
            if (n$failed > 0L) paste0(", ", n$failed, " not processed") else "",
            if (isTRUE(state$include_screened)) " (screened-out papers are included)" else " (not counted)")),
          shiny::tags$p("Papers found by the arXiv search that turned out to be about something else:"),
          shiny::tags$ul(lapply(seq_len(nrow(n$reasons)), function(i) {
            shiny::tags$li(shiny::tags$strong(n$reasons$n[i]), ": ", scope_category_text(spec, n$reasons$category[i]))
          })),
          if (n$failed > 0L) shiny::tags$p(
            n$failed, if (n$failed == 1L) " paper has" else " papers have",
            " no factsheet because extraction failed. They are not listed anywhere."),
          shiny::tags$p("To list the screened-out papers, tick \"Include screened-out papers\" on the Explore tab.")))
    })

    # ---- add a filter ----
    shiny::observe({
      shiny::updateSelectizeInput(session, "add_field", choices = c(list("Choose a field" = ""),
                                                                    add_filter_choices(spec, filters()$track)),
                                  selected = "")
    })
    add_field <- shiny::reactive({
      key <- parse_field_key(input$add_field)
      if (is.null(key)) return(NULL)
      field <- tryCatch(spec_field_any(spec, key$field, key$track), error = function(e) NULL)
      if (is.null(field)) NULL else c(key, list(def = field))
    })
    shiny::observeEvent(add_field(), {
      choices <- field_choices(add_field()$def)
      shiny::updateSelectizeInput(
        session, "add_values", choices = stats::setNames(choices, value_label(choices)),
        selected = get_condition_values(filters(), add_field()$field, add_field()$track))
    })
    shiny::observeEvent(input$add_apply, {
      chosen <- add_field()
      if (is.null(chosen)) return()
      set_filters(set_condition(filters(), chosen$field, input$add_values, chosen$track))
      shiny::updateSelectizeInput(session, "add_field", selected = "")
      shiny::updateSelectizeInput(session, "add_values", choices = character(0))
    })

    # ---- landing ----
    output$landing_tracks <- shiny::renderUI({
      shiny::div(class = "track-cards", lapply(names(spec$tracks), function(id) {
        landing_track_card(id, spec$tracks[[id]], all_papers[all_papers$track == id, , drop = FALSE])
      }))
    })
    output$landing_scope <- shiny::renderUI(landing_scope_note(spec, all_papers))

    # ---- modules ----
    app <- list(
      filters = shiny::reactive(filters()),
      set_filters = set_filters,
      papers = filtered,
      ranking = current_ranking,
      bookmarks = shiny::reactive(input$personalization_bookmarks),
      paper = shiny::reactive(find_paper(all_papers[all_papers$status != "failed", , drop = FALSE], nav$paper,
                                         nav$paper_track %||% filters()$track)),
      close_paper = function() nav$paper <- NULL)
    explore_server("explore", app, deps)
    landscape_server("landscape", app, deps)
    authors_server("authors", app, deps)
    library_server("library", app, deps)
    paper_server("paper", app, deps)
  }
}
