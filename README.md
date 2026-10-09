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

[Live app](https://huggingface.co/spaces/fmegahed/arxiv_control_charts) | [Source](https://github.com/fmegahed/hf_arxiv_control_charts)

QE ArXiv Watch follows arXiv for quality engineering research in three areas and keeps a structured factsheet for each paper. It is the second application in the paper "What Quality Engineers Need to Know About Generative AI: Part 1".

| Track | What it covers |
|-------|----------------|
| Statistical Process Monitoring (SPM) | Control charts and other procedures that monitor a process over time |
| Experimental Design (DOE) | Choosing the runs of physical, computer and online experiments |
| Reliability Engineering | Failure, lifetime, degradation and maintenance of engineered systems |

## What you can do with it

- **Ask in plain words.** A question such as "nonparametric SPM papers from 2025 with public code" is turned into filters you can see and edit. Anything the filters cannot express is used to rank the results.
- **Explore.** Filter by year, topic, method, application domain and code availability, or add any other extracted field as a filter. Export the result as CSV or BibTeX.
- **Read one paper quickly.** The paper page shows the complete factsheet: labels, summary, key results, equations, limitations and future work, with the passage the model relied on for each main label.
- **See the shape of an area.** The Landscape tab shows papers per year, composition, trends, a two-field gap map, what is rising, code sharing over time and how methods are tested.
- **Find people and keep a library.** Authors for the current selection, bookmarks, and a chat over one paper or a collection.

Every view has a "?" that explains how it is produced. That text is built from the configuration and the data, so it names the models, queries and schema version actually in use.

## How a factsheet is made

1. **Search.** One arXiv query per track, built from what eight journals publish on the topic (see `analysis/query_design/REPORT.md`).
2. **Screen.** A language model reads the title and abstract and decides whether the paper belongs. Screened-out papers are kept and can be shown, but get no factsheet.
3. **First reader.** A language model reads the PDF and chooses every label from fixed lists, quoting its evidence.
4. **Second reader.** A different model answers the single-answer labels on its own from the paper's text.
5. **Tie-break.** Where the two disagree on a main label, a stronger model reads the PDF and decides.
6. **Narrative.** The first reader writes the summary, results, equations, limitations and future work using the final labels.

The paper page shows, for each checked label, whether the readers agreed. Agreement between models is not proof, and every factsheet has a link for reporting a problem.

The fields, their allowed values and definitions, the scope rules, the queries and the model names all live in `config/factsheet_spec.json`. The prompts, the schemas, the app's filters and the help text are generated from that one file.

## For developers

### Layout

```
app.R                     Shiny app entry point
R/                        All logic: spec, prompts, extraction, app modules
config/factsheet_spec.json  Single source of truth for fields, queries and models
config/app_settings.json  App settings
01_daily_update.r         Daily search and extraction of new papers
02_weekly_synthesis.r     Weekly digest (RSS and JSON)
03_reextract_all.r        Resumable re-extraction of every paper
00_freeze_snapshot.r      Byte-exact snapshot of the data with checksums
data/                     Metadata and factsheets, one pair of CSV files per track
data/frozen/v1/           Factsheets as they were when authors reviewed them
analysis/bakeoff/         Model comparison and pilot scripts
analysis/query_design/    Journal-based design of the arXiv queries
tests/                    Unit tests (testthat)
```

### Running locally

```r
shiny::runApp('.', host = '0.0.0.0', port = 7860)
```

```bash
docker build -t qe-arxiv-watch .
docker run --rm -p 7860:7860 -e OPENAI_API_KEY -e JEV_API_KEY qe-arxiv-watch
```

Without `OPENAI_API_KEY` the app still browses and filters; a question is treated as a keyword search and the chat reports that it is unavailable. Without `JEV_API_KEY` results are ranked by keyword instead, and the app says so.

### Tests

```bash
Rscript tests/testthat.R
```

No test calls a model service. A live regression test that encodes the author reviewers' corrections runs only when `RUN_LIVE_LLM=1` is set.

### Secrets

| Secret | Where | Purpose |
|--------|-------|---------|
| `OPENAI_API_KEY` | GitHub and the Space | Extraction, questions and chat |
| `JEV_API_KEY` | GitHub and the Space | Second reader and relevance ranking |
| `HF_TOKEN` | GitHub | Deployment to the Space |

### Automation

- `daily_update.yml`: searches arXiv and extracts new papers every day at 10:00 UTC.
- `weekly_synthesis.yml`: writes the weekly digest on Mondays.
- `reextract.yml`: manual, for re-extracting every paper on a branch.
- `tests.yml`: runs the unit tests on every push.

### Data versions

`data/frozen/v1/` holds the factsheets exactly as the author reviewers saw them, with a manifest of checksums. Later factsheets carry the model, schema version and extraction date in their own columns.

## Authors

Fadel M. Megahed, Ying-Ju Chen, Yamin Dahwich, Arthur Carvalho, L. Allison Jones-Farmer, Ibrahim Yousif, and Inez M. Zwetsloot.

A collaboration between Miami University, the University of Dayton, the University of Cambridge, and the University of Amsterdam.
