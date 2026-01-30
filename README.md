---
sdk: docker
app_port: 7860
title: QE ArXiv Watch
emoji: ⚙️
colorFrom: red
colorTo: yellow
pinned: false
license: mit
short_description: Monitor quality engineering research from ArXiv
---

# QE ArXiv Watch

**Live app (Hugging Face Space):** https://huggingface.co/spaces/fmegahed/arxiv_control_charts
**GitHub repo (primary, source of truth):** https://github.com/fmegahed/hf_arxiv_control_charts

This repository powers (and mirrors) the Hugging Face Space above. The **primary development happens on GitHub**, and the Space is kept in sync by pushing the same `main` branch to Hugging Face.

## What this project does

QE ArXiv Watch is a multi-track literature monitoring dashboard for quality engineering research on ArXiv. It supports three research tracks:

| Track | Color | Query |
|-------|-------|-------|
| **Control Charts** | Teal (#1b9e77) | `(ti:"control chart" OR abs:"control chart")` |
| **Experimental Design** | Orange (#d95f02) | `(ti:"experimental design" OR ti:"designed experiment" OR ...)` |
| **Reliability Engineering** | Purple (#7570b3) | `((ti:reliability OR ti:degradation OR ...) AND (cat:stat.ME OR ...))` |

### 1) Daily discovery of new arXiv papers

A scheduled GitHub Actions workflow runs every day at **10:00 UTC**. It searches arXiv using track-specific queries.

The extraction pipeline is implemented in `01_extract_arxiv_papers.r`, which:

- pulls matching metadata from arXiv for each track
- downloads new PDFs (politely, with retries and rate limiting)
- caches outputs in CSVs so you do not repeatedly re-process the same papers

### 2) AI factsheets for each paper (structured extraction)

For each new PDF, the pipeline extracts track-relevant fields using **ellmer** and an OpenAI model configured as `gpt-5.2-2025-12-11`.

The extracted artifacts are written to track-specific files:

- `data/{track}_arxiv_metadata.csv`
- `data/{track}_factsheet.csv`

(These are the files the automation commits back to GitHub each day.)

### 3) A Shiny app that explores the literature and lets users chat with a paper

`app.R` is the deployed Shiny application.

It provides:
- **Landing page** with track selection cards (color-coded)
- **Overview** dashboard with key metrics and research landscape
- **Timeline** analysis with publication trends and topic evolution
- **Topic Analytics** with interactive filtering (excludes "Other"/"Not Applicable" categories)
- **Author Analytics** with top contributors and collaboration patterns
- **Paper Deep Dive** with AI summaries, key equations, and PDF-aware chat

The app uses track-specific colors throughout:
- Dynamic header gradient matches the selected track
- Active tab highlighting follows track color
- Charts and icons use track-appropriate colors

In the Deep Dive, the app initializes an ellmer chat session using `gpt-5-mini-2025-08-07` and supports PDF-aware chat with:
- Markdown rendering via `commonmark`
- MathJax equation support
- Thinking indicator during API calls
- Enter key to send messages

## Important files in this repo

- `app.R`
  The Shiny app deployed on the Hugging Face Space.

- `data/tracks.json`
  Configuration for all research tracks (labels, colors, queries, file paths).

- `01_extract_arxiv_papers.r`
  The daily ingestion + PDF download + structured AI extraction pipeline.

- `.github/workflows/daily_update.yml`
  GitHub Actions workflow that runs the pipeline daily, commits updated CSVs to GitHub, and mirrors the same commit to Hugging Face.

- `Dockerfile`
  Container build for the app (matches the Space configuration: `sdk: docker`, `app_port: 7860`).

- `www/miami-theme.css`
  Custom CSS with Miami University branding and track-specific color support.

## Required secrets

This repo expects two secrets to exist in **GitHub repo settings**:

- `OPENAI_API_KEY`
  Used by the extraction pipeline in `01_extract_arxiv_papers.r` and injected into the workflow run environment.

- `HF_TOKEN`
  A Hugging Face access token with write permission for the Space. The workflow uses it to authenticate and push to the Space remote.

Note: the secret name is case sensitive. The workflow references `HF_TOKEN` (all caps).

## How the daily automation works

At a high level, the workflow:

1. checks out the repo
2. installs pinned R + system dependencies
3. runs `Rscript 01_extract_arxiv_papers.r`
4. commits updated CSVs to GitHub
5. pushes the same HEAD commit to Hugging Face Space (`HEAD:main`)

## Deploying to both GitHub and Hugging Face

### Recommended approach (manual pushes)

Keep GitHub as your default remote (your `main` should track `origin/main`). Use this workflow for day-to-day updates:

```bash
git status
git pull
git add .
git commit -m "Your message"
git push

# optional: also update the Hugging Face Space
git push space main:main
```

### Optional: one command to push to both

If you ever want a single command, you can run:

```bash
git push origin main && git push space main:main
```

(You can also create a simple git alias for this in your global git config.)

## Running locally

### Option A: Run with R

```r
# From the repo root
shiny::runApp('.', host = '0.0.0.0', port = 7860)
```

### Option B: Run with Docker

```bash
docker build -t arxiv-control-charts .
docker run --rm -p 7860:7860 arxiv-control-charts
```

## Recent Updates (January 2026)

### Version 2.1.0 - UI Polish & Deployment Fixes

- **Dynamic theming**: Header, tabs, and UI elements change color based on selected track
- **Landing page**: Color-coded track selection cards with inline styles for cross-platform reliability
- **Chat improvements**: Markdown rendering, thinking indicator, Enter key support, MathJax equations
- **Topic Analytics**: Filters out "Other" and "Not Applicable" categories from charts
- **Timeline fixes**: Cumulative axis labels no longer overlap tick marks
- **Renamed**: "ArXiv Literature Monitor" → "QE ArXiv Watch"
- **Dockerfile**: Pinned R package versions for reproducible builds

### Version 2.0.0 - Multi-Track Support

- **Multi-track support**: Added Experimental Design and Reliability Engineering tracks

## Authors

Fadel M. Megahed, Ying-Ju (Tessa) Chen, Allison Jones-Farmer, Ibrahim Yousif, and Inez M. Zwetsloot.

A collaboration between Miami University, the University of Dayton, and the University of Amsterdam.
