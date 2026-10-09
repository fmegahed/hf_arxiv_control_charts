"""Step 18. Eight-journal coverage table and the expanded gold sets.

Gold candidates for a track = journal articles (in the journals that count for that track) with an
accepted arXiv version whose Luna v2 label for the track is TRUE, plus the articles of the first-round
hand gold (hand_labels/gold_ids.json). The analyst then screens the candidates by title and abstract;
vetoes and additions are recorded in hand_labels/gold_v2_review.json:
    {"reliability": {"veto": [arxiv ids], "add": [arxiv ids]}, ...}
Gold = candidates - veto + add, written to hand_labels/gold_ids_v2.json.

Journals that count: reliability = all eight; DOE and SPM = all except IEEE Transactions on Reliability
(its DOE / SPM labelled articles are reported in the coverage table but kept out of those gold sets).

Output: cache/journal_coverage_v2.csv / .md, cache/gold_v2_candidates_{track}.csv, hand_labels/gold_ids_v2.json
Run:    python -I 18_build_gold_v2.py
"""
import json, os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import CACHE, HERE, TOPIC, read_csv, write_csv

works = read_csv(os.path.join(CACHE, "journal_works.csv"))
allw = read_csv(os.path.join(CACHE, "journal_works_all.csv"))
text = {r["openalex_id"]: r for r in read_csv(os.path.join(CACHE, "journal_text.csv"))}
luna = {r["key"]: r for r in read_csv(os.path.join(CACHE, "journal_labels_v2.csv"))}
match = {r["openalex_id"]: r for r in read_csv(os.path.join(CACHE, "journal_arxiv_matches.csv")) if r["accepted"] == "True"}
src = {r["journal"]: r for r in read_csv(os.path.join(CACHE, "journal_coverage.csv"))}
T = lambda w, t: luna.get(w["openalex_id"], {}).get(t, "").upper() == "TRUE"

order = ["Journal of Quality Technology", "Technometrics", "Quality Engineering",
         "Quality and Reliability Engineering International", "IEEE Transactions on Reliability", "IISE Transactions",
         "IIE Transactions", "Applied Stochastic Models in Business and Industry", "Computational Statistics & Data Analysis"]
rows = []
for j in order + ["all"]:
    ws = [w for w in works if j == "all" or w["journal"] == j]
    lab = [w for w in ws if w["openalex_id"] in luna]
    r = {"journal": j, "issn": src[j]["issn"] if j in src else "", "openalex_source": src[j]["openalex_source_id"] if j in src else "",
         "works_2010_2026": sum(1 for w in allw if j == "all" or w["journal"] == j), "research_items": len(ws),
         "abstract_openalex": sum(1 for w in ws if len(w["abstract"]) >= 100),
         "abstract_any_source": sum(1 for w in ws if text[w["openalex_id"]]["abstract_source"] != "none"),
         "luna_labelled": len(lab), "arxiv_version": sum(1 for w in ws if w["openalex_id"] in match)}
    for t in ("reliability", "doe", "spm", "change_theory"):
        r[t] = sum(1 for w in lab if T(w, t))
        r[t + "_with_arxiv"] = sum(1 for w in lab if T(w, t) and w["openalex_id"] in match)
    rows.append(r)
write_csv(os.path.join(CACHE, "journal_coverage_v2.csv"), rows)
pc = lambda a, b: f"{a} ({100 * a / b:.0f}%)" if b else "0"
md = ["| journal | ISSN | OpenAlex source | works 2010 to 2026 | research items | abstract in OpenAlex | abstract from any source | "
      "arXiv version | reliability (with arXiv) | DOE (with arXiv) | SPM (with arXiv) | change-detection theory flag (with arXiv) |",
      "|---|---|---|---|---|---|---|---|---|---|---|---|"]
for r in rows:
    md.append(f"| {r['journal']} | {r['issn']} | {r['openalex_source']} | {r['works_2010_2026']} | {r['research_items']} | "
              f"{pc(r['abstract_openalex'], r['research_items'])} | {pc(r['abstract_any_source'], r['research_items'])} | {r['arxiv_version']} | "
              f"{r['reliability']} ({r['reliability_with_arxiv']}) | {r['doe']} ({r['doe_with_arxiv']}) | {r['spm']} ({r['spm_with_arxiv']}) | "
              f"{r['change_theory']} ({r['change_theory_with_arxiv']}) |")
open(os.path.join(CACHE, "journal_coverage_v2.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
sys.stdout.reconfigure(encoding="utf-8")
print("\n".join(md))

old = json.load(open(os.path.join(HERE, "hand_labels", "gold_ids.json")))
rf = os.path.join(HERE, "hand_labels", "gold_v2_review.json")
review = json.load(open(rf)) if os.path.exists(rf) else {}
gold = {"_note": "Expanded gold sets (eight journals). Candidates = Luna v2 on-topic journal articles with an accepted arXiv "
                 "version plus the first-round hand gold; screened by the analyst (hand_labels/gold_v2_review.json)."}
for track, topic in TOPIC.items():
    cands = {}
    for w in works:
        m = match.get(w["openalex_id"])
        if not m:
            continue
        if track != "reliability" and w["journal"] == "IEEE Transactions on Reliability":
            continue
        in_old = m["arxiv_id"] in old[track]
        if T(w, topic) or in_old:
            cands[m["arxiv_id"]] = {"arxiv_id": m["arxiv_id"], "journal": w["journal"], "year": w["year"], "doi": w["doi"],
                                    "luna": int(T(w, topic)), "first_round_hand_gold": int(in_old),
                                    "journal_title": w["title"], "arxiv_title": m["arxiv_title"],
                                    "arxiv_categories": m["arxiv_categories"], "arxiv_abstract": m["arxiv_abstract"]}
    rv = review.get(track, {})
    for c in cands.values():
        c["veto"] = int(c["arxiv_id"] in rv.get("veto", []))
    write_csv(os.path.join(CACHE, f"gold_v2_candidates_{track}.csv"), sorted(cands.values(), key=lambda c: (c["journal"], c["arxiv_id"])))
    g = (set(cands) - set(rv.get("veto", []))) | set(rv.get("add", []))
    gold[track] = sorted(g)
    print(track, "candidates", len(cands), "vetoed", len(set(cands) & set(rv.get("veto", []))), "added", len(rv.get("add", [])), "gold", len(g))
json.dump(gold, open(os.path.join(HERE, "hand_labels", "gold_ids_v2.json"), "w"), indent=1)
