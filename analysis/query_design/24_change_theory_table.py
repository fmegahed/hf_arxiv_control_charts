"""Step 24. Do the eight journals publish sequential / quickest change-detection theory?

Combines three views:
  (a) the analyst's reading of every article found by a wording search or by the Luna CHANGE_THEORY flag
      (hand_labels/change_theory_review.csv: class, journal, year, doi, title)
  (b) counts of the Luna CHANGE_THEORY flag per journal (over-inclusive: it also fires on CUSUM / SPRT chart papers)
  (c) counts of articles whose title or abstract uses the wording itself
Output: cache/v2/change_theory.md
Run:    python -I 24_change_theory_table.py
"""
import os, re, sys
from collections import Counter
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import CACHE, HERE, read_csv

works = read_csv(os.path.join(CACHE, "journal_works.csv"))
text = {r["openalex_id"]: r for r in read_csv(os.path.join(CACHE, "journal_text.csv"))}
luna = {r["key"]: r for r in read_csv(os.path.join(CACHE, "journal_labels_v2.csv"))}
review = read_csv(os.path.join(HERE, "hand_labels", "change_theory_review.csv"))
merge = lambda j: "IISE Transactions (with IIE Transactions)" if j in ("IISE Transactions", "IIE Transactions") else j
journals = ["Journal of Quality Technology", "Technometrics", "Quality Engineering", "Quality and Reliability Engineering International",
            "IEEE Transactions on Reliability", "IISE Transactions (with IIE Transactions)",
            "Applied Stochastic Models in Business and Industry", "Computational Statistics & Data Analysis"]
WORD = {"quickest (change) detection": r"quickest (change[- ]?point |change )?detection",
        "sequential change(-point) detection": r"sequential(ly)? change[- ]?(point )?detection|sequential detection of (a )?change",
        "online change(-point) detection": r"on-?line change[- ]?(point )?detection",
        "Shiryaev, Lorden or Pollak named": r"shiryaev|\blorden\b|\bpollak\b",
        "e-detector, e-process, test martingale, conformal martingale, anytime-valid, confidence sequence":
            r"\be-detectors?\b|\be-process(es)?\b(?! variable)|test martingale|conformal (test )?martingale|anytime[- ]valid|confidence sequences?"}
n_all, n_abs, flag, word = Counter(), Counter(), Counter(), {k: Counter() for k in WORD}
for w in works:
    j = merge(w["journal"]); n_all[j] += 1
    a = text[w["openalex_id"]]["abstract_best"]
    n_abs[j] += bool(a)
    flag[j] += luna[w["openalex_id"]]["change_theory"] == "TRUE"
    t = (w["title"] + " " + a).lower()
    for k, p in WORD.items():
        # "e-process" also occurs inside "mixture-process"; require a word boundary before the e
        word[k][j] += bool(re.search(p, t)) and not (k.startswith("e-detector") and re.search(r"[a-z]-e-|[a-z]e-process|e-value", t) and not re.search(r"\be-detector|test martingale|conformal|anytime|confidence sequence", t))
cls = sorted({r["class"] for r in review})
byc = {c: Counter(merge(r["journal"]) for r in review if r["class"] == c) for c in cls}
md = ["| journal | research items | with an abstract | read as change-detection theory | read as monitoring-framed sequential change detection method | "
      "read as sequential acceptance / demonstration test | Luna CHANGE_THEORY flag | " + " | ".join(WORD) + " |",
      "|---|---|---|---|---|---|---|" + "---|" * len(WORD)]
order = ["theory", "monitoring-framed sequential change detection method", "sequential test for acceptance or reliability demonstration (not change detection)"]
for j in journals + ["all eight"]:
    g = lambda c: sum(c.values()) if j == "all eight" else c[j]
    md.append(f"| {j} | {g(n_all)} | {g(n_abs)} | " + " | ".join(str(g(byc[c])) for c in order) + f" | {g(flag)} | " + " | ".join(str(g(word[k])) for k in WORD) + " |")
md.append("")
for c in order:
    md.append(f"\n**Articles read as: {c}**\n")
    for r in sorted((r for r in review if r["class"] == c), key=lambda r: (r["journal"], r["year"])):
        md.append(f"- {r['journal']} ({r['year']}). {r['title']}. https://doi.org/{r['doi']}")
open(os.path.join(CACHE, "v2", "change_theory.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
sys.stdout.reconfigure(encoding="utf-8")
print("\n".join(md[:12]))
