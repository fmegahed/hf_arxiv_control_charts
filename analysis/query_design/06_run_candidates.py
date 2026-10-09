"""Step 6. Run every candidate query (and the current one) against the arXiv API.

Raw Atom pages are cached under cache/arxiv_queries/. Output:
  cache/candidates/{track}__{name}.csv   retrieved papers (id, submitted, title, abstract, categories)
  cache/candidate_sizes.csv              totalResults reported by the API and rows parsed
  cache/arxiv_probes.csv                 small probes documenting stemming / phrase behaviour
Run:    python -I 06_run_candidates.py
"""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import CACHE, write_csv, load_corpus
from qd_candidates import CANDIDATES
import qd_arxiv

os.makedirs(os.path.join(CACHE, "candidates"), exist_ok=True)

probes = ['ti:reliability AND cat:stat.ME', 'ti:reliable AND cat:stat.ME',
          'ti:"reliability analysis" AND cat:stat.ME', 'ti:"reliable analysis" AND cat:stat.ME',
          'ti:"design of experiment"', 'ti:"design and experiments"', 'ti:"design experiment"',
          'ti:"experimental design" AND cat:cs.HC', 'ti:"experience design" AND cat:cs.HC',
          '(ti:"control chart" OR abs:"control chart")', 'all:"control chart"']
write_csv(os.path.join(CACHE, "arxiv_probes.csv"), [{"query": q, "total_results": qd_arxiv.count(q)} for q in probes])

sizes = []
for track, cands in CANDIDATES.items():
    corpus = load_corpus(track)
    for name, q in cands.items():
        total, entries = qd_arxiv.fetch(q)
        write_csv(os.path.join(CACHE, "candidates", f"{track}__{name}.csv"), entries,
                  ["id", "version_id", "submitted", "title", "abstract", "primary_category", "categories"])
        sizes.append({"track": track, "candidate": name, "api_total_results": total, "rows_parsed": len(entries),
                      "in_frozen_corpus": sum(1 for e in entries if e["id"] in corpus), "query_chars": len(q), "query": q})
        print(track, name, total, len(entries), flush=True)
write_csv(os.path.join(CACHE, "candidate_sizes.csv"), sizes)
