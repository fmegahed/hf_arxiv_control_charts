"""Step 13. Fetch, for every candidate term of a track, what the arXiv API returns for it.

Generalises the coordinator's 11_spm_term_marginals.py to all three tracks. For each term T:
  ti:T  and  abs:T  are counted; a result set of up to MAXN records is downloaded; when the unrestricted
  abstract query is larger than MAXN, the category-restricted query (abs:T AND CATS) is tried instead.
Category-restricted variants of the downloaded sets are derived locally (categories match exactly).

Output: cache/terms/{track}_sets.json    {variant key -> list of arXiv ids}, counts, flags
        cache/terms/{track}_papers.csv   metadata of every paper seen
Run:    python -I 13_term_fetch.py [track ...]
"""
import json, os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import CACHE, write_csv
from qd_terms import TRACK_TERMS
import qd_arxiv

MAXN = 4000
os.makedirs(os.path.join(CACHE, "terms"), exist_ok=True)

for track in (sys.argv[1:] or list(TRACK_TERMS)):
    cfg = TRACK_TERMS[track]
    cats = "(" + " OR ".join("cat:" + c for c in cfg["cats"]) + ")"
    papers, out = {}, {"base_query": cfg["base"], "cats": cfg["cats"], "terms": {}}
    _, base = qd_arxiv.fetch(cfg["base"])
    out["base"] = [e["id"] for e in base]
    papers.update({e["id"]: e for e in base})
    print(track, "base", len(base), flush=True)
    for term in cfg["terms"]:
        rec = {}
        for field in ("ti", "abs"):
            q = f"{field}:{term}"
            n = qd_arxiv.count(q)
            rec[f"{field}_count"] = n
            if n <= MAXN:
                _, es = qd_arxiv.fetch(q)
                rec[f"{field}_ids"] = [e["id"] for e in es]
                rec[f"{field}_complete"] = True
            else:
                qc = f"({q} AND {cats})"
                nc = qd_arxiv.count(qc)
                rec[f"{field}_cat_count"] = nc
                rec[f"{field}_complete"] = False
                if nc <= MAXN:
                    _, es = qd_arxiv.fetch(qc)
                    rec[f"{field}_cat_ids"] = [e["id"] for e in es]
                else:
                    es = []
            papers.update({e["id"]: e for e in es})
        out["terms"][term] = rec
        print(f"  {term}: ti {rec['ti_count']}, abs {rec['abs_count']}", flush=True)
    json.dump(out, open(os.path.join(CACHE, "terms", f"{track}_sets.json"), "w", encoding="utf-8"))
    write_csv(os.path.join(CACHE, "terms", f"{track}_papers.csv"), list(papers.values()),
              ["id", "submitted", "title", "abstract", "primary_category", "categories"])
