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

**Your AI-Powered Research Companion for Quality Engineering**

[![Live App](https://img.shields.io/badge/Live_App-Hugging_Face-yellow)](https://huggingface.co/spaces/fmegahed/arxiv_control_charts)
[![GitHub](https://img.shields.io/badge/Source-GitHub-blue)](https://github.com/fmegahed/hf_arxiv_control_charts)

Never miss a breakthrough. Get daily updates, AI summaries, and deep insights from the latest quality engineering research on arXiv.

---

## What is QE ArXiv Watch?

QE ArXiv Watch automatically monitors arXiv for new research papers in quality engineering, then uses AI to extract structured insights from each paper. Whether you're a researcher staying current in your field or a practitioner looking for the latest methods, this tool helps you:

- **Save time** - No more manual searching through arXiv
- **Understand faster** - AI-generated summaries highlight key findings
- **Discover trends** - Interactive analytics reveal publication patterns and emerging topics
- **Go deeper** - Chat with any paper to ask specific questions about methodology or results

---

## Key Features

| Feature | Description |
|---------|-------------|
| **AI Summaries** | Every paper gets an AI-generated factsheet with key contributions, methods, equations, and findings |
| **Chat with Papers** | Ask questions about any paper and get instant answers based on the PDF content |
| **Trend Analytics** | Interactive charts showing publication trends, topic evolution, and research landscape |
| **Author Analytics** | Discover top contributors, collaboration patterns, and author profiles |
| **Personal Library** | Bookmark papers, export BibTeX citations, and analyze your collection |
| **Weekly Digest** | Subscribe to an RSS feed for AI-synthesized weekly research summaries |

---

## Research Tracks

| Track | Focus | Papers Updated Daily |
|-------|-------|---------------------|
| **Control Charts** | Statistical process monitoring, SPC methods, Shewhart/CUSUM/EWMA charts | ✓ |
| **Experimental Design** | DOE, response surface methodology, optimal designs | ✓ |
| **Reliability Engineering** | Degradation modeling, maintenance optimization, failure analysis | ✓ |

---

## Quick Start

1. **Visit** the [live app](https://huggingface.co/spaces/fmegahed/arxiv_control_charts)
2. **Choose** a research track that matches your interests
3. **Explore** the Overview dashboard for key metrics and trends
4. **Dive deep** into any paper using the Paper Deep Dive tab
5. **Chat** with papers to ask specific questions about methods or results

---

## How It Works

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   Daily arXiv   │────>│  AI Extraction  │────>│  Interactive    │
│   Monitoring    │     │  (Summaries &   │     │  Dashboard      │
│                 │     │   Factsheets)   │     │                 │
└─────────────────┘     └─────────────────┘     └─────────────────┘
```

Every day at 10:00 UTC, automated workflows search arXiv for new papers, download PDFs, and use AI to extract structured information. The web app then lets you explore this data interactively.

---

<details>
<summary><strong>For Developers</strong></summary>

### Repository Structure

```
├── app.R                    # Shiny application
├── 01_extract_arxiv_papers.r # Daily ingestion pipeline
├── data/
│   ├── tracks.json          # Track configuration
│   ├── {track}_arxiv_metadata.csv
│   └── {track}_factsheet.csv
├── www/
│   └── miami-theme.css      # Custom styling
├── Dockerfile               # Container build
└── .github/workflows/
    └── daily_update.yml     # Automation
```

### Required Secrets (GitHub)

| Secret | Purpose |
|--------|---------|
| `OPENAI_API_KEY` | AI extraction pipeline |
| `HF_TOKEN` | Hugging Face Space deployment |

### Running Locally

**With R:**
```r
shiny::runApp('.', host = '0.0.0.0', port = 7860)
```

**With Docker:**
```bash
docker build -t qe-arxiv-watch .
docker run --rm -p 7860:7860 qe-arxiv-watch
```

### Deployment

The workflow pushes to both GitHub (primary) and Hugging Face Space (mirror):

```bash
# Standard push
git push origin main

# Also update Hugging Face
git push space main:main
```

### Recent Updates

**Version 3.3.0 (February 2026)**
- Enhanced landing page with feature showcase
- Video tutorial modal
- Stats section showing total papers
- Improved track card descriptions

**Version 3.2.0**
- Weekly research digest RSS feed
- AI-synthesized weekly summaries

**Version 2.1.0**
- Dynamic theming based on track selection
- Chat improvements with MathJax support

</details>

---

## Authors

**Fadel M. Megahed**, **Ying-Ju (Tessa) Chen**, **Allison Jones-Farmer**, **Ibrahim Yousif**, and **Inez M. Zwetsloot**

A collaboration between Miami University, the University of Dayton, and the University of Amsterdam.

---

**[Try It Now](https://huggingface.co/spaces/fmegahed/arxiv_control_charts)**
