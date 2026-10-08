# Drawing the charts. Each function takes a table from app_counts.R and
# returns a plotly object. Conventions: one accent colour per chart (the
# track's colour), bars sorted by size with their values written next to
# them, a single-hue light-to-dark scale for the heat map, and no second axis.

CHART_INK <- "#333333"
CHART_MUTED <- "#6b6b6b"
CHART_GRID <- "#e4e2d6"
CHART_NEUTRAL <- "#9a9a9a"
ALL_TRACKS_ACCENT <- "#C41230"
# Line colours for up to four trend series (ColorBrewer Dark2, distinct from the track colours).
TREND_COLORS <- c("#e7298a", "#66a61e", "#e6ab02", "#a6761d")
TREND_MAX_SERIES <- length(TREND_COLORS)
# Single-hue sequential scale for the heat map, light to dark.
HEAT_SCALE <- list(c(0, "#f2f0f7"), c(0.25, "#cbc9e2"), c(0.5, "#9e9ac8"), c(0.75, "#756bb1"), c(1, "#3f007d"))
GAP_MAP_MAX_VALUES <- 12L

wrap_label <- function(x, width = 26L) {
  vapply(x, function(label) paste(strwrap(label, width = width), collapse = "<br>"), character(1),
         USE.NAMES = FALSE)
}

percent <- function(x, digits = 0L) paste0(formatC(100 * x, format = "f", digits = digits), "%")

chart_base <- function(plot, source = NULL, margin = list(t = 10, r = 10, b = 40, l = 10)) {
  plot <- plotly::layout(
    plot,
    font = list(family = "Segoe UI, Tahoma, Geneva, Verdana, sans-serif", size = 12, color = CHART_INK),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    hoverlabel = list(bgcolor = "#ffffff", font = list(color = CHART_INK)),
    margin = margin)
  plot <- plotly::config(plot, displayModeBar = FALSE, responsive = TRUE)
  if (!is.null(source)) plot <- plotly::event_register(plot, "plotly_click")
  plot
}

# Vertical bars of papers per year; stacked by track when several are shown.
chart_per_year <- function(counts, spec, accent, source = NULL) {
  tracks <- intersect(names(spec$tracks), unique(counts$track))
  plot <- plotly::plot_ly(source = source)
  for (track in tracks) {
    rows <- counts[counts$track == track, , drop = FALSE]
    single <- length(tracks) == 1L
    plot <- plotly::add_bars(
      plot, x = rows$year, y = rows$n, name = spec$tracks[[track]]$short_label,
      customdata = rows$year,
      marker = list(color = if (single) accent else spec$tracks[[track]]$color,
                    line = list(color = "#ffffff", width = 1)),
      text = if (single) ifelse(rows$n > 0L, rows$n, "") else NULL,
      textposition = "outside", cliponaxis = FALSE, textfont = list(size = 10, color = CHART_MUTED),
      hovertemplate = paste0("%{x}: %{y} papers<extra>", spec$tracks[[track]]$short_label, "</extra>"))
  }
  plot <- plotly::layout(
    plot, barmode = "stack", showlegend = length(tracks) > 1L,
    legend = list(orientation = "h", x = 0, y = 1.12),
    xaxis = list(title = "Year first submitted to arXiv", dtick = if (length(unique(counts$year)) > 14L) 2 else 1,
                 fixedrange = TRUE),
    yaxis = list(title = "Papers", gridcolor = CHART_GRID, rangemode = "tozero", fixedrange = TRUE))
  chart_base(plot, source)
}

# Horizontal bars, largest on top, with "n (share)" written at the bar end.
chart_bars <- function(df, accent, source = NULL, show_share = TRUE, max_bars = COMPOSITION_BARS_SHOWN) {
  df <- utils::head(df[order(-df$n), , drop = FALSE], max_bars)
  df$wrapped <- wrap_label(df$label)
  text <- if (show_share && "share" %in% names(df)) paste0(df$n, " (", percent(df$share), ")") else as.character(df$n)
  plot <- plotly::plot_ly(
    df, x = ~n, y = ~wrapped, type = "bar", orientation = "h", source = source,
    height = max(160, 34 * nrow(df) + 40),
    customdata = df$value %||% df$label, marker = list(color = accent),
    text = text, textposition = "outside", cliponaxis = FALSE,
    textfont = list(size = 11, color = CHART_INK),
    hovertext = paste0(df$label, ": ", text), hoverinfo = "text")
  plot <- plotly::layout(
    plot,
    xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, fixedrange = TRUE,
                 range = c(0, max(df$n) * 1.3)),
    yaxis = list(title = "", categoryorder = "array", categoryarray = rev(df$wrapped), automargin = TRUE,
                 ticksuffix = " ", fixedrange = TRUE),
    showlegend = FALSE)
  chart_base(plot, source, margin = list(t = 5, r = 10, b = 10, l = 10))
}

# Vertical bars for an ordered numeric category (team size).
chart_columns <- function(df, x, accent, x_title, y_title = "Papers") {
  plot <- plotly::plot_ly(x = df[[x]], y = df$n, type = "bar", marker = list(color = accent),
                          text = df$n, textposition = "outside", cliponaxis = FALSE,
                          textfont = list(size = 10, color = CHART_MUTED),
                          hovertemplate = paste0(x_title, " %{x}: %{y} papers<extra></extra>"))
  plot <- plotly::layout(plot,
                         xaxis = list(title = x_title, dtick = 1, fixedrange = TRUE),
                         yaxis = list(title = y_title, gridcolor = CHART_GRID, fixedrange = TRUE))
  chart_base(plot)
}

# Yearly counts of up to four labels, each line named at its right end.
chart_trends <- function(trend) {
  values <- utils::head(unique(trend$value), TREND_MAX_SERIES)
  plot <- plotly::plot_ly()
  for (i in seq_along(values)) {
    rows <- trend[trend$value == values[i], , drop = FALSE]
    plot <- plotly::add_trace(
      plot, x = rows$year, y = rows$n, type = "scatter", mode = "lines+markers",
      name = tidy_value(values[i]), line = list(color = TREND_COLORS[i], width = 2),
      marker = list(color = TREND_COLORS[i], size = 7),
      hovertext = paste0(tidy_value(values[i]), ", ", rows$year, ": ", rows$n, " of ", rows$papers,
                         " papers (", percent(rows$share), ")"),
      hoverinfo = "text")
  }
  plot <- plotly::layout(
    plot, showlegend = TRUE, legend = list(orientation = "h", x = 0, y = 1.15),
    xaxis = list(title = "Year first submitted to arXiv", fixedrange = TRUE),
    yaxis = list(title = "Papers with the label", gridcolor = CHART_GRID, rangemode = "tozero",
                 fixedrange = TRUE))
  chart_base(plot)
}

# Heat map of two fields with the count written in every cell.
chart_gap_map <- function(gap, source = NULL, max_values = GAP_MAP_MAX_VALUES) {
  a_values <- utils::head(gap$a_values, max_values)
  b_values <- utils::head(gap$b_values, max_values)
  cells <- gap$cells[gap$cells$a %in% a_values & gap$cells$b %in% b_values, , drop = FALSE]
  z <- matrix(0L, nrow = length(a_values), ncol = length(b_values))
  z[cbind(match(cells$a, a_values), match(cells$b, b_values))] <- cells$n
  a_labels <- wrap_label(tidy_value(a_values), 30L)
  b_labels <- wrap_label(tidy_value(b_values), 16L)
  top <- max(z, 1L)
  hover <- outer(seq_along(a_values), seq_along(b_values), function(i, j) {
    paste0(tidy_value(a_values[i]), " and ", tidy_value(b_values[j]), ": ", z[cbind(i, j)], " papers")
  })
  custom <- outer(seq_along(a_values), seq_along(b_values), function(i, j) paste0(a_values[i], LIST_SEP, b_values[j]))
  plot <- plotly::plot_ly(
    x = b_labels, y = a_labels, z = z, type = "heatmap", source = source, customdata = custom,
    height = max(260, 34 * length(a_values) + 150),
    colorscale = HEAT_SCALE, zmin = 0, zmax = top, xgap = 2, ygap = 2,
    hovertext = hover, hoverinfo = "text",
    colorbar = list(title = "Papers", thickness = 10, len = 0.6))
  plot <- plotly::add_annotations(
    plot, x = rep(b_labels, each = length(a_labels)), y = rep(a_labels, times = length(b_labels)),
    text = ifelse(as.vector(z) == 0L, "", as.vector(z)), showarrow = FALSE,
    font = list(size = 11, color = ifelse(as.vector(z) > 0.55 * top, "#ffffff", CHART_INK)))
  plot <- plotly::layout(
    plot,
    xaxis = list(title = "", side = "top", tickangle = -40, fixedrange = TRUE, automargin = TRUE,
                 categoryorder = "array", categoryarray = b_labels),
    yaxis = list(title = "", autorange = "reversed", fixedrange = TRUE, automargin = TRUE,
                 categoryorder = "array", categoryarray = a_labels, ticksuffix = " "),
    showlegend = FALSE)
  chart_base(plot, source, margin = list(t = 110, r = 10, b = 10, l = 10))
}

rising_label <- function(rising) {
  paste0(percent(rising$share_recent), " recently (", rising$n_recent, " of ", rising$papers_recent,
         ") vs ", percent(rising$share_earlier), " earlier (", rising$n_earlier, " of ", rising$papers_earlier, ")")
}

# Change in share, in percentage points, largest rise on top.
chart_rising <- function(rising, accent, max_each = 8L) {
  up <- utils::head(rising[rising$change > 0, , drop = FALSE], max_each)
  down <- utils::tail(rising[rising$change < 0, , drop = FALSE], max_each)
  df <- rbind(up, down)
  df$label <- wrap_label(paste0(tidy_value(df$value), " [", df$field, "]"), 34L)
  df$points <- 100 * df$change
  plot <- plotly::plot_ly(
    df, x = ~points, y = ~label, type = "bar", orientation = "h", height = max(200, 34 * nrow(df) + 70),
    marker = list(color = ifelse(df$change > 0, accent, CHART_NEUTRAL)),
    text = paste0(ifelse(df$points > 0, "+", ""), formatC(df$points, format = "f", digits = 1), " pts: ",
                  df$n_recent, "/", df$papers_recent, " vs ", df$n_earlier, "/", df$papers_earlier),
    textposition = "outside", cliponaxis = FALSE, textfont = list(size = 10, color = CHART_INK),
    hovertext = paste0(tidy_value(df$value), ": ", rising_label(df)), hoverinfo = "text")
  span <- max(abs(df$points), 1)
  plot <- plotly::layout(
    plot,
    xaxis = list(title = "Change in share of papers (percentage points)", zeroline = TRUE,
                 zerolinecolor = CHART_MUTED, gridcolor = CHART_GRID, fixedrange = TRUE,
                 range = c(-span * 2.4, span * 2.4)),
    yaxis = list(title = "", categoryorder = "array", categoryarray = rev(df$label), automargin = TRUE,
                 ticksuffix = " ", fixedrange = TRUE))
  chart_base(plot)
}

# Share of papers with public code per year.
chart_code_share <- function(df, accent) {
  df <- df[df$papers > 0L, , drop = FALSE]
  plot <- plotly::plot_ly(
    df, x = ~year, y = ~I(100 * share), type = "scatter", mode = "lines+markers",
    line = list(color = accent, width = 2), marker = list(color = accent, size = 7),
    hovertext = paste0(df$year, ": ", df$public, " of ", df$papers, " papers (", percent(df$share), ")"),
    hoverinfo = "text")
  last <- df[nrow(df), , drop = FALSE]
  plot <- plotly::add_annotations(plot, x = last$year, y = 100 * last$share,
                                  text = paste0(percent(last$share), " (", last$public, " of ", last$papers, ")"),
                                  showarrow = FALSE, xanchor = "right", yshift = 14,
                                  font = list(size = 11, color = CHART_INK))
  plot <- plotly::layout(
    plot, showlegend = FALSE,
    xaxis = list(title = "Year first submitted to arXiv", fixedrange = TRUE),
    yaxis = list(title = "Papers with public code (%)", range = c(0, 105), gridcolor = CHART_GRID,
                 fixedrange = TRUE))
  chart_base(plot)
}

# A darker shade of a colour, for the header gradient.
darken <- function(color, factor = 0.75) {
  rgb <- grDevices::col2rgb(color)[, 1] * factor
  grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255)
}

# The value attached to the last click on a chart (its customdata). Reads the
# input that plotly sets, so nothing is needed before the chart is drawn.
plotly_click_value <- function(session, source) {
  raw <- session$rootScope()$input[[paste0("plotly_click-", source)]]
  if (is.null(raw)) return(NULL)
  event <- tryCatch(jsonlite::parse_json(raw, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(event) || is.null(event$customdata)) return(NULL)
  event$customdata[[1]]
}
