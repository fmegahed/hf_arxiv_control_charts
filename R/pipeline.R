# Running extraction over a track: resumable, checkpointed, with time and
# spend limits. All side effects (model calls, PDF downloads, the clock) are
# passed in, so the loop is testable with fakes.

# Errors that say the run cannot continue, as opposed to one paper failing.
# They are not recorded against the paper.
RUN_STOPPING_ERRORS <- c("no_credit")

# Deterministic split of papers across parallel workers.
in_shard <- function(paper_id, shard = 1L, n_shards = 1L) {
  if (n_shards <= 1L) return(rep(TRUE, length(paper_id)))
  bucket <- vapply(paper_id, function(id) sum(utf8ToInt(id)) %% n_shards, numeric(1)) + 1
  unname(bucket == shard)
}

append_raw <- function(path, record, pdf_truncated = FALSE) {
  entry <- list(
    paper_id = record$paper_id, id = record$id, track = record$track,
    model = record$llm_model, at = record$extracted_at, status = record$status,
    pdf_truncated = pdf_truncated,
    usage = record[c("input_tokens", "cached_input_tokens", "output_tokens", "cost_usd", "n_calls")],
    raw = attr(record, "raw")
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  cat(jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null", digits = NA), "\n",
      file = path, append = TRUE, sep = "")
}

read_raw <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lapply(lines[nzchar(lines)], jsonlite::fromJSON, simplifyVector = FALSE)
}

# Rebuild a factsheet from stored raw outputs; the last entry per paper wins.
rebuild_factsheet <- function(raw_entries, spec, track) {
  factsheet <- NULL
  for (entry in raw_entries) {
    row <- record_to_row(record_from_raw(entry, spec, track), spec, track)
    factsheet <- merge_records(factsheet, row)
  }
  factsheet
}

paper_from_metadata <- function(metadata, id) {
  row <- metadata[match(id, metadata$id), , drop = FALSE]
  field <- function(name) if (name %in% names(row)) as.character(row[[name]]) else NA_character_
  list(id = id, title = field("title"), abstract = field("abstract"), categories = field("categories"))
}

# Process the pending papers of one track.
# Returns list(factsheet, processed, stopped, spent_usd, remaining).
run_track <- function(track, spec, metadata, factsheet, llm, fetch_pdf, model,
                      factsheet_path, raw_path = NULL,
                      backfill = FALSE, retry_failed = FALSE,
                      max_papers = Inf, time_budget_min = Inf, max_spend_usd = Inf,
                      shard = 1L, n_shards = 1L, checkpoint_every = 5L,
                      now = Sys.time, log = message) {
  started <- now()
  plan <- plan_extraction(metadata, factsheet, spec$schema_version,
                          max_attempts = spec$limits$max_attempts,
                          backfill = backfill, retry_failed = retry_failed)
  pending <- pending_papers(plan)
  pending <- pending[in_shard(pending$paper_id, shard, n_shards), , drop = FALSE]
  total_pending <- nrow(pending)

  processed <- 0L
  spent <- 0
  stopped <- NA_character_
  since_checkpoint <- 0L

  for (i in seq_len(total_pending)) {
    if (processed >= max_papers) { stopped <- "max_papers"; break }
    if (as.numeric(difftime(now(), started, units = "mins")) >= time_budget_min) { stopped <- "time_budget"; break }
    if (spent >= max_spend_usd) { stopped <- "max_spend"; break }

    paper <- paper_from_metadata(metadata, pending$id[i])
    record <- extract_paper(paper, track, spec, llm, fetch_pdf, model, now())
    if (identical(record$status, "failed") && record$error_class %in% RUN_STOPPING_ERRORS) {
      stopped <- record$error_class
      break
    }
    spent <- spent + record$cost_usd
    factsheet <- merge_records(factsheet, record_to_row(record, spec, track))
    if (!is.null(raw_path)) {
      append_raw(raw_path, record, "pdf_truncated" %in% split_values(record$qa_flags))
    }
    processed <- processed + 1L
    since_checkpoint <- since_checkpoint + 1L
    log(sprintf("[%s] %d/%d %s %s%s", track, i, total_pending, record$id, record$status,
                if (identical(record$status, "failed")) paste0(" (", record$error_class, ")") else ""))
    if (since_checkpoint >= checkpoint_every) {
      write_factsheet_atomic(factsheet, factsheet_path)
      since_checkpoint <- 0L
    }
  }
  if (!is.null(factsheet) && processed > 0L) write_factsheet_atomic(factsheet, factsheet_path)

  list(factsheet = factsheet, processed = processed, stopped = stopped, spent_usd = spent,
       remaining = total_pending - processed)
}

# Parse "--name value" and "--flag" command-line arguments into a list.
parse_cli_args <- function(args, defaults = list()) {
  out <- defaults
  i <- 1L
  while (i <= length(args)) {
    key <- sub("^--", "", args[i])
    key <- gsub("-", "_", key, fixed = TRUE)
    if (i < length(args) && !startsWith(args[i + 1L], "--")) {
      out[[key]] <- args[i + 1L]
      i <- i + 2L
    } else {
      out[[key]] <- TRUE
      i <- i + 1L
    }
  }
  out
}
