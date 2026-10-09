"""Step 4. Stand-in topic labels for the journal articles using the keyword rubric (qd_rubric.py),
and a random sample for hand checking.

Output: cache/journal_labelled_kw.csv
        cache/handcheck_journal_sample.csv  (20 per topic plus 20 'other', seed 20261008)
Run:    python -I 04_keyword_labels_journal.py
"""
import csv, os, random, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_rubric import classify

HERE = os.path.dirname(os.path.abspath(__file__)); CACHE = os.path.join(HERE, "cache")
works = list(csv.DictReader(open(os.path.join(CACHE, "journal_works.csv"), encoding="utf-8")))
for w in works:
    c = classify(w["title"], w["abstract"], w["keywords"].replace("|", "; "))
    w.update({"kw_reliability": c["reliability"], "kw_doe": c["doe"], "kw_spm": c["spm"], "kw_why": c["why"]})
with open(os.path.join(CACHE, "journal_labelled_kw.csv"), "w", newline="", encoding="utf-8") as f:
    wr = csv.DictWriter(f, fieldnames=list(works[0].keys())); wr.writeheader(); wr.writerows(works)

print(f"{'journal':52s} n   rel  doe  spm  none")
for j in sorted({w['journal'] for w in works}) + ["ALL"]:
    s = [w for w in works if j == "ALL" or w["journal"] == j]
    print(f"{j:52s} {len(s)} {sum(w['kw_reliability'] for w in s)} {sum(w['kw_doe'] for w in s)} "
          f"{sum(w['kw_spm'] for w in s)} {sum(not (w['kw_reliability'] or w['kw_doe'] or w['kw_spm']) for w in s)}")

random.seed(20261008)
sample = []
for stratum, test in [("reliability", lambda w: w["kw_reliability"]), ("doe", lambda w: w["kw_doe"]),
                      ("spm", lambda w: w["kw_spm"]),
                      ("other", lambda w: not (w["kw_reliability"] or w["kw_doe"] or w["kw_spm"]))]:
    pool = [w for w in works if test(w) and w["openalex_id"] not in {s["openalex_id"] for s in sample}]
    for w in random.sample(pool, 20):
        sample.append({"stratum": stratum, "openalex_id": w["openalex_id"], "journal": w["journal"], "year": w["year"],
                       "kw_reliability": w["kw_reliability"], "kw_doe": w["kw_doe"], "kw_spm": w["kw_spm"],
                       "title": w["title"], "abstract": w["abstract"][:900]})
with open(os.path.join(CACHE, "handcheck_journal_sample.csv"), "w", newline="", encoding="utf-8") as f:
    wr = csv.DictWriter(f, fieldnames=list(sample[0].keys())); wr.writeheader(); wr.writerows(sample)
