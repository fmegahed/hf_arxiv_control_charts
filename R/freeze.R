# Freezing and verifying a snapshot of the published factsheets.
#
# A snapshot is a byte-for-byte copy of the data files plus a MANIFEST.json
# that records where the files came from and enough per-file facts
# (checksum, row counts) to detect any later change.

FROZEN_FILES <- c(
  "spc_arxiv_metadata.csv", "spc_factsheet.csv",
  "exp_design_arxiv_metadata.csv", "exp_design_factsheet.csv",
  "reliability_arxiv_metadata.csv", "reliability_factsheet.csv",
  "tracks.json"
)

file_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

# Per-file facts recorded in the manifest. Factsheet-only fields are absent for
# metadata files and for non-CSV files.
manifest_entry <- function(path) {
  entry <- list(
    file = basename(path),
    bytes = as.numeric(file.size(path)),
    sha256 = file_sha256(path)
  )
  if (!grepl("\\.csv$", path)) return(entry)

  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                        na.strings = c("NA", ""), encoding = "UTF-8")
  entry$rows <- nrow(df)
  entry$columns <- names(df)
  entry$distinct_ids <- length(unique(df$id))

  relevance <- grep("^is_.*_paper$", names(df), value = TRUE)
  if (length(relevance) == 1L) {
    flag <- as.logical(df[[relevance]])
    entry$relevance_field <- relevance
    entry$relevance_true <- sum(flag %in% TRUE)
    entry$relevance_false <- sum(flag %in% FALSE)
    entry$relevance_na <- sum(is.na(flag))
    entry$failed_rows <- sum(is.na(df$summary))
    entry$llm_models <- as.list(sort(unique(stats::na.omit(df$llm_model))))
  }
  entry
}

freeze_snapshot <- function(src_dir, dest_dir, source_commit, label,
                            files = FROZEN_FILES, frozen_at = Sys.time()) {
  missing <- files[!file.exists(file.path(src_dir, files))]
  if (length(missing) > 0L) {
    stop("Cannot freeze; missing files: ", paste(missing, collapse = ", "))
  }
  if (file.exists(file.path(dest_dir, "MANIFEST.json"))) {
    stop("A snapshot already exists in ", dest_dir, "; refusing to overwrite it.")
  }
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(file.path(src_dir, files), file.path(dest_dir, files),
                  overwrite = FALSE, copy.date = TRUE)
  if (!all(ok)) stop("Copy failed for: ", paste(files[!ok], collapse = ", "))

  manifest <- list(
    label = label,
    source_commit = source_commit,
    frozen_at = format(frozen_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    files = lapply(file.path(dest_dir, files), manifest_entry)
  )
  jsonlite::write_json(manifest, file.path(dest_dir, "MANIFEST.json"),
                       auto_unbox = TRUE, pretty = TRUE, digits = NA)
  invisible(manifest)
}

# Returns list(ok, problems). A snapshot verifies when every listed file exists
# with the recorded checksum.
verify_manifest <- function(dir) {
  manifest_path <- file.path(dir, "MANIFEST.json")
  if (!file.exists(manifest_path)) {
    return(list(ok = FALSE, problems = "MANIFEST.json not found"))
  }
  manifest <- jsonlite::read_json(manifest_path)
  problems <- character()
  for (entry in manifest$files) {
    path <- file.path(dir, entry$file)
    if (!file.exists(path)) {
      problems <- c(problems, paste0(entry$file, ": missing"))
    } else if (!identical(file_sha256(path), entry$sha256)) {
      problems <- c(problems, paste0(entry$file, ": checksum differs"))
    }
  }
  list(ok = length(problems) == 0L, problems = problems)
}
