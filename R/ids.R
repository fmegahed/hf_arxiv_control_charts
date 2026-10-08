# arXiv identifiers.
#
# arXiv ids carry a version suffix ("2403.01234v2", old style
# "math/0406049v1"). A paper is keyed by its base id; the version says which
# revision was read.

arxiv_base_id <- function(id) {
  sub("v[0-9]+$", "", id)
}

# Integer version, or NA when the id has no version suffix.
arxiv_version <- function(id) {
  has_version <- grepl("v[0-9]+$", id)
  out <- rep(NA_integer_, length(id))
  out[has_version] <- as.integer(sub("^.*v([0-9]+)$", "\\1", id[has_version]))
  out
}

# Keep one row per paper: the highest version. Rows without a version suffix
# rank below any versioned row of the same paper. Row order is otherwise kept.
keep_latest_version <- function(df, id_col = "id") {
  if (nrow(df) == 0L) return(df)
  base <- arxiv_base_id(df[[id_col]])
  version <- arxiv_version(df[[id_col]])
  version[is.na(version)] <- 0L
  latest <- stats::ave(version, base, FUN = max)
  keep <- version == latest & !duplicated(paste(base, version))
  df[keep, , drop = FALSE]
}
