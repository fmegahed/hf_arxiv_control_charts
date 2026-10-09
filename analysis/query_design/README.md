# Query design analysis (Reliability, DOE, SPM tracks)

Evidence for choosing new arXiv API queries for the QE ArXiv Watch tracks. Nothing here modifies the app.
The findings are in `REPORT.md` (Part 1: follow-up on eight journals with Luna labels; Part 2: first round).

## Layout

- `01` to `24`: steps (Python unless the name ends in `.R`). `11` and `12` are the coordinator's SPM term scripts.
- `qd_*.py`, `00_common.R`, `00_rubric_v2.R`: shared code. `qd_terms.py` holds the terms tested, `qd_candidates.py`
  the first-round candidates, `qd_candidates_v2.py` the follow-up candidates, `00_rubric_v2.R` the Luna v2 rubric.
- `hand_labels/`: the analyst's labels and reviews (kept under version control).
- `cache/`: everything downloaded or derived (ignored by git). Raw API responses are cached.
- `report_followup.py`, `report_text.py`: prose; `10_build_report.py` merges it with the generated tables.

## Rerun (follow-up pipeline)

From this directory, in Git Bash. `R` stands for `"/c/Program Files/R/R-4.5.2/bin/Rscript.exe" --vanilla`.
Luna steps read `OPENAI_API_KEY` from the repository `.env`, never print it, are resumable, run with
`max_active = 4`, and stop (without retrying) when most of a chunk is refused.

```bash
python -I 01_fetch_openalex.py             # eight journals (nine OpenAlex sources), 2010 to 2026
python -I 03_match_journal_to_arxiv.py     # arXiv versions and missing abstracts (OpenAlex + Semantic Scholar)
R 02_label_journal_articles.R              # Luna v2 labels for every journal article
python -I 18_build_gold_v2.py              # coverage table; gold candidates; gold after hand_labels/gold_v2_review.json
python -I 13_term_fetch.py                 # what the API returns for every term (slow: 4 s per call, backs off on 429)
python -I 14_term_tables.py sample         # samples of new papers per term variant
R 15_label_arxiv_samples.R                 # Luna v2 labels for every sample file found
python -I 14_term_tables.py                # per-term tables, verdicts, assembled queries
python -I 14_term_tables.py strict         # SPM again, not counting theory-flagged papers
python -I 22_missed_gold_terms.py sample   # clauses aimed at missed gold: fetch and sample
R 15_label_arxiv_samples.R
python -I 22_missed_gold_terms.py          # clause tables and final queries
python -I 20_score_v2.py sample            # fetch the candidates, sample their new papers
R 15_label_arxiv_samples.R
python -I 20_score_v2.py                   # scoring tables, missed gold
python -I 16_clause_tests.py               # context and exclusion clauses against the silver flags
python -I 17_token_probes.py               # tokenisation and stemming probes
python -I 19_handcheck_v2.py               # agreement tables (journal_sample / arxiv_sample modes draw the blind samples)
python -I 21_url_length_probe.py           # URL length probes
R 23_httr_get_check.R                      # every candidate through httr GET, as aRxiv sends it
python -I 24_change_theory_table.py        # change-detection theory in the journals
python -I 05_vocabulary.py luna            # vocabulary tables with Luna labels
python -I 10_build_report.py --write       # writes REPORT.md and README.md (without --write: prints the report)
```

First-round steps (`04`, `05` without argument, `06`, `07`, `09`) still run but now read the eight-journal corpus,
so their tables differ from the ones quoted in Part 2 of the report.

## Changing a term or a threshold

Terms: `qd_terms.py` (single terms) and the `CLAUSES` dictionary in `22_missed_gold_terms.py`. Thresholds:
`SAMPLE_N`, `MAX_OFF`, `MIN_ADD` and the 60% bar in `14_term_tables.py` and `22_missed_gold_terms.py`.
After a change rerun from `13` (new terms) or `14` (thresholds) down to `20`, then `23` to confirm the
assembled query still fits the 4,094-byte request line. The prose in `report_followup.py` quotes numbers by hand.
