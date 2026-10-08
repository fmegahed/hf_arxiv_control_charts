# Bake-off step 0: download the sample's PDFs once, so the model runs can
# share the cache and run side by side.
#
# Usage (from the app directory): Rscript analysis/bakeoff/00_fetch_pdfs.R

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

spec <- spec_load()
sample <- read_factsheet(file.path("analysis", "bakeoff", "local", "sample.csv"))
fetch_pdf <- make_fetch_pdf("pdf_cache", max_pages = spec$limits$max_pdf_pages)

results <- lapply(sample$id, function(id) fetch_pdf(list(id = id)))
ok <- vapply(results, function(r) r$ok, logical(1))
pages <- vapply(results, function(r) if (r$ok) as.numeric(r$pages) else NA_real_, numeric(1))
cat(sprintf("%d of %d PDFs available; %d over %d pages (cut); median %g pages, max %g\n",
            sum(ok), length(ok), sum(pages > spec$limits$max_pdf_pages, na.rm = TRUE),
            spec$limits$max_pdf_pages, stats::median(pages, na.rm = TRUE), max(pages, na.rm = TRUE)))
if (any(!ok)) cat("Unavailable:", paste(sample$id[!ok], collapse = ", "), "\n")
