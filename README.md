---
sdk: docker
app_port: 7860
title: ArXiv Control Chart Monitor
emoji: ⚙️
colorFrom: red
colorTo: yellow
pinned: false
license: mit
short_description: Monitor latest control chart research from ArXiv
---

# ArXiv Control Chart Literature Monitor

A Shiny application for tracking and analyzing Statistical Process Control (SPC) research papers from ArXiv, featuring **AI-generated summaries** and an interactive **Chat with Paper** capability.

## Key AI Features

### AI-Generated Summaries

Every paper in the database includes LLM-extracted insights:

- **Summary**: A concise overview of the paper's main contribution and methodology
- **Key Results**: The primary findings and performance metrics
- **Key Equations**: Important mathematical formulations with MathJax rendering

Access AI summaries from any paper table by clicking the robot icon, or explore full details in the Paper Deep Dive tab.

### Chat with Paper

The Paper Deep Dive tab provides an interactive chat interface powered by GPT:

- **Ask questions** about any paper in natural language
- **PDF-aware responses**: The chat includes the full paper PDF for accurate answers
- **Quick question buttons**: One-click access to common queries like "What is the main contribution?", "Explain the methodology", and "What are the limitations?"
- **Conversation history**: Follow-up questions maintain context from previous exchanges

## Application Tabs

### Overview Dashboard
- Key metrics: total papers, date coverage, most common chart types and application domains
- Research landscape summary with configurable time periods (12/24/36 months or all time)
- Publication trend mini-chart
- Full papers table with direct access to PDFs and AI summaries

### Timeline
- Interactive publication history with year or month aggregation
- Cumulative publication view option
- Topic trends over time for chart families, statistical methods, or application domains

### Topic Analytics
- Global free-text search across titles, authors, abstracts, summaries, and key results
- Filter by chart family, statistical method, application domain, and SPC phase
- Distribution charts for each category
- Filterable papers table with AI summary access

### Author Analytics
- Top 15 most published authors visualization
- Team size distribution analysis
- Author lookup with publication history

### Paper Deep Dive
- Filter papers by year and/or author
- Full paper details including AI-generated summary, key results, and key equations
- **Interactive Chat with Paper** for asking questions about the selected paper

## Data Source

Papers are collected from ArXiv using the search query:
```
ti:"control chart" OR abs:"control chart"
```

Each paper is processed using LLM extraction to identify SPC-relevant metadata including chart family, application domain, statistical method, phase, and other technical characteristics.

## Technology Stack

- **R Shiny**: Web application framework
- **ellmer**: LLM integration for Chat with Paper feature (GPT-powered)
- **plotly**: Interactive visualizations
- **DT**: Interactive data tables
- **MathJax**: Mathematical equation rendering

## Authors

Fadel M. Megahed, Ying-Ju (Tessa) Chen, Allison Jones-Farmer, Ibrahim Yousif, and Inez M. Zwetsloot

A collaboration between Miami University, the University of Dayton, and the University of Amsterdam.

## Related Work

Companion app to: "What Should Quality Engineers Know about Generative AI", submitted to [Quality Engineering](https://www.tandfonline.com/journals/lqen20).
