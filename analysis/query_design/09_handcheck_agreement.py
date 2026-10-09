"""Step 9. Agreement between the analyst's hand labels and the automatic labels.

(a) Journal articles: keyword-rubric labels vs hand labels on the 80-article stratified random sample
    (hand_labels/journal_handcheck.csv; 20 per rubric-positive topic plus 20 rubric-negative).
(b) Journal articles: Luna labels vs hand labels on the same sample, when cache/journal_labels.csv covers it.
(c) Newly retrieved arXiv papers: keyword rubric vs hand labels (and Luna vs hand when available).
Output: cache/handcheck_agreement.csv and cache/handcheck_agreement.md
Run:    python -I 09_handcheck_agreement.py
"""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import CACHE, HERE, TOPIC, read_csv, write_csv

rows = []


def add(scope, source, topic, pairs):
    """pairs: list of (auto_label, hand_label) booleans"""
    n = len(pairs)
    if not n:
        return
    tp = sum(a and h for a, h in pairs); fp = sum(a and not h for a, h in pairs)
    fn = sum(h and not a for a, h in pairs); tn = n - tp - fp - fn
    rows.append({"scope": scope, "auto_source": source, "topic": topic, "n": n, "agree": tp + tn,
                 "agreement": round((tp + tn) / n, 3), "auto_yes_hand_yes": tp, "auto_yes_hand_no": fp,
                 "auto_no_hand_yes": fn, "auto_no_hand_no": tn,
                 "precision_of_auto_yes": round(tp / (tp + fp), 3) if tp + fp else ""})


hc = read_csv(os.path.join(HERE, "hand_labels", "journal_handcheck.csv"))
kw = {r["openalex_id"]: r for r in read_csv(os.path.join(CACHE, "journal_labelled_kw.csv"))}
lf = os.path.join(CACHE, "journal_labels.csv")
luna = {r["key"]: r for r in read_csv(lf)} if os.path.exists(lf) else {}
for t in ("reliability", "doe", "spm"):
    add("journal sample (80)", "keyword rubric", t, [(kw[r["openalex_id"]]["kw_" + t] == "True", r["hand_" + t] == "1") for r in hc])
    add("journal sample, rubric-positive stratum only (20)", "keyword rubric", t,
        [(kw[r["openalex_id"]]["kw_" + t] == "True", r["hand_" + t] == "1") for r in hc if r["stratum"] == t])
    add("journal sample (Luna-labelled subset)", "gpt-6-luna", t,
        [(luna[r["openalex_id"]][t].upper() == "TRUE", r["hand_" + t] == "1") for r in hc if r["openalex_id"] in luna])
    # the few Luna labels obtained before the API account ran out of credit, compared with the keyword rubric
    add("journal articles with a Luna label", "keyword rubric vs gpt-6-luna (Luna in the 'hand' columns)", t,
        [(kw[k]["kw_" + t] == "True", v[t].upper() == "TRUE") for k, v in luna.items() if k in kw])

nl = os.path.join(CACHE, "new_sample_luna_labels.csv")
nluna = {r["key"]: r for r in read_csv(nl)} if os.path.exists(nl) else {}
for track, t in TOPIC.items():
    hand = {r["id"]: r["on_topic"] == "1" for r in read_csv(os.path.join(HERE, "hand_labels", f"new_sample_hand_{track}.csv"))}
    samp = {r["id"]: r for r in read_csv(os.path.join(CACHE, f"new_sample_{track}.csv"))}
    add(f"new arXiv papers, {track}", "keyword rubric", t, [(samp[i]["kw_on_topic"] == "1", h) for i, h in hand.items() if i in samp])
    add(f"new arXiv papers, {track}", "gpt-6-luna", t,
        [(nluna[f"{track}:{i}"][t].upper() == "TRUE", h) for i, h in hand.items() if f"{track}:{i}" in nluna])

write_csv(os.path.join(CACHE, "handcheck_agreement.csv"), rows)
md = ["| scope | automatic label | topic | n | agreement | auto yes, hand yes | auto yes, hand no | auto no, hand yes | auto no, hand no |",
      "|---|---|---|---|---|---|---|---|---|"]
for r in rows:
    md.append(f"| {r['scope']} | {r['auto_source']} | {r['topic']} | {r['n']} | {r['agree']}/{r['n']} ({100 * r['agreement']:.0f}%) | "
              f"{r['auto_yes_hand_yes']} | {r['auto_yes_hand_no']} | {r['auto_no_hand_yes']} | {r['auto_no_hand_no']} |")
open(os.path.join(CACHE, "handcheck_agreement.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
print("\n".join(md))
