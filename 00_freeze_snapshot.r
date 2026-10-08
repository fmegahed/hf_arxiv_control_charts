# Freeze the published factsheets as a named snapshot.
#
# Usage: Rscript 00_freeze_snapshot.r [label] [commit]
# The default label "v1" is the version that paper authors reviewed.
#
# Files are read from the git object store, not the working tree, so the
# snapshot holds the exact published bytes whatever the local line-ending
# settings are. data/frozen/ is marked -text in .gitattributes for the same
# reason.

source("R/freeze.R")

args <- commandArgs(trailingOnly = TRUE)
label <- if (length(args) >= 1L) args[[1]] else "v1"
commit <- if (length(args) >= 2L) args[[2]] else "HEAD"

source_commit <- system2("git", c("log", "-1", "--format=%H", commit, "--", "data"), stdout = TRUE)

export_dir <- tempfile("freeze_export_")
dir.create(export_dir)
for (f in FROZEN_FILES) {
  status <- system2("git", c("show", paste0(source_commit, ":data/", f)),
                    stdout = file.path(export_dir, f))
  if (status != 0L) stop("git show failed for data/", f)
}

dest <- file.path("data", "frozen", label)
manifest <- freeze_snapshot(export_dir, dest, source_commit = source_commit, label = label)
unlink(export_dir, recursive = TRUE)

check <- verify_manifest(dest)
if (!check$ok) stop("Snapshot failed verification: ", paste(check$problems, collapse = "; "))

for (entry in manifest$files) {
  cat(sprintf("%-34s %9.0f bytes  %s rows\n", entry$file, entry$bytes,
              if (is.null(entry$rows)) "-" else entry$rows))
}
cat("Frozen", label, "from commit", source_commit, "\n")
