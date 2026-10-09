# One name per person.
#
# arXiv records an author's name as it was typed for each paper, so one person
# can appear as "Inez M. Zwetsloot", "Inez Maria Zwetsloot" and "Inez
# Zwetsloot". Counting authors needs one name per person. The rule:
#   - two names are the same person when they share the given name (written
#     in full) and the family name, and their middle names do not contradict
#     each other ("M." agrees with "Maria" and with no middle name);
#   - if the names that share a given and family name contradict each other
#     anywhere ("Wei X. Zhang" and "Wei Y. Zhang"), none of them is merged,
#     because "Wei Zhang" could then be either;
#   - the merged name is the form used on most papers (the longest on a tie);
#   - spellings the rule cannot catch are listed in config/author_aliases.json.
# The names shown on a paper's page stay as arXiv has them.

AUTHOR_ALIASES_FILE <- file.path("config", "author_aliases.json")

load_author_aliases <- function(path = AUTHOR_ALIASES_FILE) {
  if (!file.exists(path)) return(character(0))
  aliases <- jsonlite::fromJSON(path, simplifyVector = FALSE)$aliases
  if (length(aliases) == 0L) return(character(0))
  stats::setNames(vapply(aliases, as.character, character(1)), names(aliases))
}

# Lower-case words of a name without accents, dots and commas.
author_words <- function(name) {
  plain <- iconv(name, to = "ASCII//TRANSLIT", sub = "")
  if (is.na(plain)) plain <- name
  words <- strsplit(trimws(gsub("[.,]", " ", tolower(plain))), "[[:space:]]+")[[1]]
  words[nzchar(words)]
}

middle_names_agree <- function(a, b) {
  shared <- min(length(a), length(b))
  if (shared == 0L) return(TRUE)
  all(vapply(seq_len(shared), function(i) {
    x <- a[i]; y <- b[i]
    identical(x, y) || (nchar(x) == 1L && startsWith(y, x)) || (nchar(y) == 1L && startsWith(x, y))
  }, logical(1)))
}

# Named vector: every name that should be shown under another -> that name.
# `counts` is a named integer vector, name -> number of papers.
author_merge_map <- function(counts, aliases = character(0)) {
  names_all <- names(counts)
  map <- character(0)
  if (length(names_all) > 0L) {
    words <- lapply(names_all, author_words)
    key <- vapply(words, function(w) {
      if (length(w) < 2L || nchar(w[1]) < 2L) NA_character_ else paste(w[1], w[length(w)])
    }, character(1))
    for (group in unique(key[!is.na(key) & duplicated(key)])) {
      members <- which(key == group)
      middles <- lapply(words[members], function(w) w[-c(1L, length(w))])
      pairs <- utils::combn(seq_along(members), 2L)
      agree <- all(apply(pairs, 2L, function(pair) middle_names_agree(middles[[pair[1]]], middles[[pair[2]]])))
      if (!agree) next
      variants <- names_all[members]
      best <- variants[order(-counts[variants], -nchar(variants), variants)][1]
      others <- setdiff(variants, best)
      map[others] <- best
    }
  }
  # Listed spellings win, and may point at a name the rule has merged.
  for (alias in names(aliases)) {
    target <- aliases[[alias]]
    map[alias] <- if (target %in% names(map)) map[[target]] else target
  }
  map
}

# The authors column with one name per person.
merge_author_names <- function(authors, aliases = load_author_aliases()) {
  cells <- lapply(authors, split_values)
  counts <- table(unlist(cells, use.names = FALSE))
  map <- author_merge_map(stats::setNames(as.integer(counts), names(counts)), aliases)
  if (length(map) == 0L) return(authors)
  vapply(cells, function(cell) {
    if (length(cell) == 0L) return(NA_character_)
    hit <- cell %in% names(map)
    cell[hit] <- map[cell[hit]]
    paste(unique(cell), collapse = LIST_SEP)
  }, character(1))
}
