# The view a user is looking at, written to and read from the URL query
# string so that any view can be shared and the back button works.
#
# A view is list(view, tab, paper, filters):
#   view     "landing" or "browse"
#   tab      one of APP_TABS
#   paper    base arXiv id of the open paper, or NULL
#   paper_track  track whose factsheet of that paper is shown (a paper can
#            be in two tracks); NULL means the track in view, or the first
#   filters  a filter state (see app_filters.R)
#
# Query keys: track (a track id, or "all"), tab, paper, ptrack, years ("from-to"),
# f (repeated; "field.role.track:value|value"), code, real, reviews, screened,
# q (residual), kw ("term|term"), sort.

APP_TABS <- c("explore", "landscape", "authors", "library")
ALL_TRACKS_KEY <- "all"

new_view <- function(view = "landing", tab = "explore", paper = NULL, filters = new_filter_state(),
                     paper_track = NULL) {
  list(view = view, tab = tab, paper = paper, paper_track = if (!is.null(paper)) paper_track, filters = filters)
}

url_escape <- function(x) utils::URLencode(enc2utf8(x), reserved = TRUE)
url_unescape <- function(x) utils::URLdecode(gsub("+", " ", x, fixed = TRUE))

encode_condition <- function(cond) {
  paste0(cond$field, ".", cond$role, ".", cond$track %||% "", ":",
         paste(cond$values, collapse = LIST_SEP))
}

decode_condition <- function(text) {
  head <- sub(":.*$", "", text)
  values <- if (grepl(":", text, fixed = TRUE)) sub("^[^:]*:", "", text) else ""
  parts <- strsplit(head, ".", fixed = TRUE)[[1]]
  track <- if (length(parts) >= 3L && nzchar(parts[3])) parts[3] else NULL
  new_condition(parts[1], split_values(values), track,
                if (length(parts) >= 2L && nzchar(parts[2])) parts[2] else "any")
}

# View -> "?key=value&..." ("" for the landing page).
encode_view <- function(view) {
  if (!identical(view$view, "browse")) return("")
  state <- view$filters
  pairs <- list(c("track", state$track %||% ALL_TRACKS_KEY))
  add <- function(key, value) pairs[[length(pairs) + 1L]] <<- c(key, value)
  if (!identical(view$tab, "explore")) add("tab", view$tab)
  if (!is.null(view$paper)) add("paper", view$paper)
  if (!is.null(view$paper) && !is.null(view$paper_track) && !identical(view$paper_track, state$track)) {
    add("ptrack", view$paper_track)
  }
  if (!is.null(state$year_from) || !is.null(state$year_to)) {
    add("years", paste0(state$year_from %||% "", "-", state$year_to %||% ""))
  }
  for (cond in state$conditions) add("f", encode_condition(cond))
  if (isTRUE(state$public_code)) add("code", "1")
  if (isTRUE(state$real_data)) add("real", "1")
  if (isTRUE(state$reviews_only)) add("reviews", "1")
  if (isTRUE(state$include_screened)) add("screened", "1")
  if (nzchar(state$residual %||% "")) add("q", state$residual)
  if (length(state$keywords) > 0L) add("kw", paste(state$keywords, collapse = LIST_SEP))
  if (!identical(state$sort, "newest")) add("sort", state$sort)
  paste0("?", paste(vapply(pairs, function(p) paste0(p[1], "=", url_escape(p[2])), character(1)),
                    collapse = "&"))
}

parse_query <- function(query) {
  query <- sub("^\\?", "", query %||% "")
  if (!nzchar(query)) return(list())
  pieces <- strsplit(query, "&", fixed = TRUE)[[1]]
  pieces <- pieces[nzchar(pieces)]
  keys <- sub("=.*$", "", pieces)
  values <- ifelse(grepl("=", pieces, fixed = TRUE), sub("^[^=]*=", "", pieces), "")
  values <- vapply(values, function(v) tryCatch(url_unescape(v), error = function(e) ""), character(1),
                   USE.NAMES = FALSE)
  Encoding(values) <- "UTF-8"
  stats::setNames(as.list(values), keys)
}

# Query string -> view, checked against the spec. Anything unknown is ignored.
decode_view <- function(query, spec, year_range) {
  params <- parse_query(query)
  get <- function(key) params[[key]]
  if (is.null(get("track")) && is.null(get("paper"))) return(new_view())

  state <- new_filter_state()
  track <- get("track")
  if (!is.null(track) && !identical(track, ALL_TRACKS_KEY)) state$track <- track
  years <- get("years")
  if (!is.null(years) && grepl("^[0-9]*-[0-9]*$", years)) {
    ends <- strsplit(paste0(years, " "), "-", fixed = TRUE)[[1]]
    state$year_from <- if (nzchar(trimws(ends[1]))) trimws(ends[1])
    state$year_to <- if (nzchar(trimws(ends[2]))) trimws(ends[2])
  }
  state$conditions <- lapply(unname(params[names(params) == "f"]), decode_condition)
  state$public_code <- identical(get("code"), "1")
  state$real_data <- identical(get("real"), "1")
  state$reviews_only <- identical(get("reviews"), "1")
  state$include_screened <- identical(get("screened"), "1")
  state$residual <- get("q") %||% ""
  state$keywords <- split_values(get("kw") %||% "")
  state$sort <- get("sort") %||% "newest"

  clean <- sanitize_state(state, spec, year_range)$state
  tab <- get("tab") %||% "explore"
  paper <- get("paper")
  paper_track <- get("ptrack")
  new_view(view = "browse",
           tab = if (tab %in% APP_TABS) tab else "explore",
           paper = if (!is.null(paper) && grepl("^[A-Za-z0-9./-]+$", paper)) arxiv_base_id(paper) else NULL,
           paper_track = if (isTRUE(paper_track %in% names(spec$tracks)) &&
                             !identical(paper_track, clean$track)) paper_track,
           filters = clean)
}

# Link to a view of one paper, used in tables so a row can be opened in a new tab.
# `paper_track` (one per paper) is added when it differs from the track in view.
paper_href <- function(paper_id, track = NULL, paper_track = NULL) {
  href <- paste0("?track=", url_escape(track %||% ALL_TRACKS_KEY), "&paper=",
                 vapply(paper_id, url_escape, character(1), USE.NAMES = FALSE))
  if (is.null(paper_track)) return(href)
  ifelse(paper_track == (track %||% ""), href, paste0(href, "&ptrack=", paper_track))
}
