# Deciding what to do with each paper on a run.
#
# Actions:
#   extract      no record yet
#   new_version  arXiv has a newer version than the one extracted
#   retry        the last attempt failed and attempts remain
#   reextract    the record was made with an older schema (backfill only)
#   skip         nothing to do

plan_extraction <- function(metadata, factsheet, schema_version, max_attempts = 5L,
                            backfill = FALSE, retry_failed = FALSE) {
  current <- keep_latest_version(metadata)
  plan <- data.frame(
    paper_id = arxiv_base_id(current$id),
    id = current$id,
    version = arxiv_version(current$id),
    action = "extract",
    stringsAsFactors = FALSE
  )
  if (is.null(factsheet) || nrow(factsheet) == 0L) return(plan)

  at <- match(plan$paper_id, factsheet$paper_id)
  has_record <- !is.na(at)
  status <- factsheet$status[at]
  attempts <- factsheet$attempts[at]
  attempts[is.na(attempts)] <- 0L
  done_version <- factsheet$arxiv_version[at]
  record_schema <- factsheet$schema_version[at]

  finished <- has_record & status %in% c("ok", "out_of_scope")
  failed <- has_record & status %in% "failed"
  newer <- finished & !is.na(plan$version) & !is.na(done_version) & plan$version > done_version
  old_schema <- has_record & !(record_schema %in% schema_version)

  plan$action[finished] <- "skip"
  plan$action[newer] <- "new_version"
  plan$action[failed] <- ifelse(attempts[failed] < max_attempts | retry_failed, "retry", "skip")
  plan$action[old_schema] <- if (backfill) "reextract" else "skip"
  plan
}

# Rows of the plan that need a model call, in a stable order.
pending_papers <- function(plan) {
  plan[plan$action != "skip", , drop = FALSE]
}
