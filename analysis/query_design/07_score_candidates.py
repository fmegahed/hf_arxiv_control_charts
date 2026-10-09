"""Step 7. Score every candidate query.

Inputs : cache/candidates/*.csv, frozen corpus + silver flags, cache/journal_arxiv_matches.csv,
         hand_labels/gold_ids.json, hand_labels/new_sample_hand_{track}.csv (optional),
         cache/new_sample_luna_labels.csv (optional, produced by 08_label_new_samples.R)
Outputs: cache/score_{track}.csv           one row per candidate
         cache/lost_{track}__{cand}.csv    in-scope corpus papers the candidate drops
         cache/gained_{track}__{cand}.csv  papers the candidate adds (with stand-in / hand / Luna labels)
         cache/new_sample_{track}.csv      seeded random samples of newly retrieved papers
                                           (first HAND_N per candidate = hand sample, first LUNA_N = Luna sample)
         cache/gold_{track}.csv            gold articles and which candidates retrieve them
         cache/score_tables.md             markdown fragments for REPORT.md
Run:    python -I 07_score_candidates.py
"""
import json, os, random, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import CACHE, HERE, TRACKS, TOPIC, load_corpus, load_journal, read_csv, write_csv
from qd_candidates import CANDIDATES
from qd_rubric import classify

HAND_N, LUNA_N = 40, 150
gold_hand = json.load(open(os.path.join(HERE, "hand_labels", "gold_ids.json")))
matches = [m for m in read_csv(os.path.join(CACHE, "journal_arxiv_matches.csv")) if m["accepted"] == "True"]
journal = {r["openalex_id"]: r for r in load_journal("kw")}
luna_file = os.path.join(CACHE, "new_sample_luna_labels.csv")
luna = {r["key"]: r for r in read_csv(luna_file)} if os.path.exists(luna_file) else {}
md = []


def pct(a, b):
    return f"{a}/{b} ({100 * a / b:.0f}%)" if b else "0/0"


def wilson(k, n, z=1.96):
    """95% Wilson interval for a proportion, as a 'lo to hi' percent string."""
    p = k / n
    c = (p + z * z / (2 * n)) / (1 + z * z / n)
    h = z * ((p * (1 - p) / n + z * z / (4 * n * n)) ** 0.5) / (1 + z * z / n)
    return f"{100 * (c - h):.0f} to {100 * (c + h):.0f}%"


for track, cands in CANDIDATES.items():
    topic = TOPIC[track]
    corpus = load_corpus(track)
    IN = {i for i, p in corpus.items() if p["flag"] is True}
    OUT = {i for i, p in corpus.items() if p["flag"] is False}
    NA = set(corpus) - IN - OUT
    gold = set(gold_hand[track])
    gold_kw = {m["arxiv_id"] for m in matches if journal[m["openalex_id"]][topic]}
    hand_file = os.path.join(HERE, "hand_labels", f"new_sample_hand_{track}.csv")
    hand = {r["id"]: r["on_topic"] == "1" for r in read_csv(hand_file)} if os.path.exists(hand_file) else {}

    retrieved = {name: {e["id"]: e for e in read_csv(os.path.join(CACHE, "candidates", f"{track}__{name}.csv"))}
                 for name in cands}
    cur_name = list(cands)[0]
    rows, samples, gold_rows = [], {}, {g: {"arxiv_id": g} for g in sorted(gold)}
    mt = {m["arxiv_id"]: m for m in matches}
    for g in gold_rows:
        gold_rows[g].update({"journal": mt[g]["journal"], "arxiv_title": mt[g]["arxiv_title"],
                             "arxiv_categories": mt[g]["arxiv_categories"]})
    for name, q in cands.items():
        R = retrieved[name]
        ids = set(R)
        new = sorted(ids - set(corpus))
        kw = {i: classify(R[i]["title"], R[i]["abstract"])[topic] for i in new}
        order = new[:]
        random.Random(f"{track}|{name}|20261008").shuffle(order)
        hand_s, luna_s = order[:HAND_N], order[:LUNA_N]
        for rank, i in enumerate(luna_s):
            s = samples.setdefault(i, {"id": i, "title": R[i]["title"], "abstract": R[i]["abstract"],
                                       "categories": R[i]["categories"], "submitted": R[i]["submitted"],
                                       "kw_on_topic": int(kw[i]), "hand_sample_of": [], "luna_sample_of": []})
            s["luna_sample_of"].append(name)
            if rank < HAND_N:
                s["hand_sample_of"].append(name)
        hl = [hand[i] for i in hand_s if i in hand]
        ll = [luna[f"{track}:{i}"][topic].upper() == "TRUE" for i in luna_s if f"{track}:{i}" in luna]
        for g in gold_rows:
            gold_rows[g][name] = int(g in ids)
        # agreement between the keyword stand-in and the hand labels on this candidate's hand sample
        agree = sum(1 for i in hand_s if i in hand and hand[i] == kw[i])
        p_hand = sum(hl) / len(hl) if hl else None
        kept_in, kept_out = len(ids & IN), len(ids & OUT)
        row = {"candidate": name, "retrieved": len(ids),
               "in_scope_kept": kept_in, "in_scope_total": len(IN), "in_scope_lost": len(IN - ids),
               "out_scope_removed": len(OUT - ids), "out_scope_total": len(OUT), "out_scope_kept": kept_out,
               "unflagged_kept": len(ids & NA),
               "precision_on_corpus_part_silver": round(kept_in / (kept_in + kept_out), 3) if kept_in + kept_out else "",
               "new_papers": len(new),
               "gold_hand_retrieved": len(gold & ids), "gold_hand_total": len(gold),
               "gold_kw_retrieved": len(gold_kw & ids), "gold_kw_total": len(gold_kw),
               "new_kw_on_topic": sum(kw.values()), "new_kw_share": round(sum(kw.values()) / len(new), 3) if new else "",
               "hand_sample_n": len(hl), "hand_on_topic": sum(hl),
               "hand_precision_new": round(p_hand, 3) if hl else "",
               "kw_vs_hand_agree": agree,
               "luna_sample_n": len(ll), "luna_on_topic": sum(ll),
               "est_overall_precision": round((kept_in + p_hand * len(new)) / (kept_in + kept_out + len(new)), 3)
               if hl else (round(kept_in / (kept_in + kept_out), 3) if not new and kept_in + kept_out else ""),
               "est_relevant_papers": round(kept_in + p_hand * len(new)) if hl else (kept_in if not new else ""),
               "query": q}
        rows.append(row)
        lost = [{"id": i, "submitted": corpus[i]["submitted"], "categories": corpus[i]["categories"],
                 "title": corpus[i]["title"]} for i in sorted(IN - ids, reverse=True)]
        random.Random(f"lost|{track}|{name}").shuffle(lost)
        write_csv(os.path.join(CACHE, f"lost_{track}__{name}.csv"), lost, ["id", "submitted", "categories", "title"])
        gained = [{"id": i, "submitted": R[i]["submitted"], "categories": R[i]["categories"], "title": R[i]["title"],
                   "kw_on_topic": int(kw[i]), "hand_on_topic": ("" if i not in hand else int(hand[i])),
                   "luna_on_topic": (luna[f"{track}:{i}"][topic] if f"{track}:{i}" in luna else ""),
                   "sample_rank": order.index(i)} for i in new]
        gained.sort(key=lambda r: r["sample_rank"])
        write_csv(os.path.join(CACHE, f"gained_{track}__{name}.csv"), gained,
                  ["id", "submitted", "categories", "title", "kw_on_topic", "hand_on_topic", "luna_on_topic", "sample_rank"])
    write_csv(os.path.join(CACHE, f"score_{track}.csv"), rows)
    write_csv(os.path.join(CACHE, f"gold_{track}.csv"), list(gold_rows.values()))
    srows = sorted(samples.values(), key=lambda s: s["id"])
    for s in srows:
        s["in_hand_sample"] = int(bool(s["hand_sample_of"]))
        s["hand_sample_of"] = "|".join(s["hand_sample_of"]); s["luna_sample_of"] = "|".join(s["luna_sample_of"])
    write_csv(os.path.join(CACHE, f"new_sample_{track}.csv"), srows,
              ["id", "in_hand_sample", "hand_sample_of", "luna_sample_of", "kw_on_topic", "submitted", "categories", "title", "abstract"])

    md.append(f"\n### {track}\n")
    md.append(f"Frozen corpus at paper level: {len(corpus)} papers; silver in scope {len(IN)}, out of scope {len(OUT)}, "
              f"no flag {len(NA)}. Hand-labelled gold articles with an arXiv version: {len(gold)} "
              f"(keyword-rubric gold: {len(gold_kw)}).\n")
    md.append("| candidate | retrieved | silver in scope kept | silver out of scope removed | precision on corpus part (silver) | "
              "new papers | gold recall (hand gold) | new on-topic, hand sample | new on-topic, keyword stand-in (all new) | "
              "est. overall precision | est. relevant papers |\n|---|---|---|---|---|---|---|---|---|---|---|")
    for r in rows:
        hp = (pct(r["hand_on_topic"], r["hand_sample_n"]) + ", 95% CI " + wilson(r["hand_on_topic"], r["hand_sample_n"])) if r["hand_sample_n"] else ("n/a" if not r["new_papers"] else "not labelled")
        kwp = pct(r["new_kw_on_topic"], r["new_papers"]) if r["new_papers"] else "n/a"
        md.append(f"| {r['candidate']} | {r['retrieved']} | {pct(r['in_scope_kept'], r['in_scope_total'])} | "
                  f"{pct(r['out_scope_removed'], r['out_scope_total'])} | {r['precision_on_corpus_part_silver']} | {r['new_papers']} | "
                  f"{pct(r['gold_hand_retrieved'], r['gold_hand_total'])} | {hp} | {kwp} | {r['est_overall_precision']} | {r['est_relevant_papers']} |")
    for name in list(cands)[1:]:
        lost = read_csv(os.path.join(CACHE, f"lost_{track}__{name}.csv"))
        gained = read_csv(os.path.join(CACHE, f"gained_{track}__{name}.csv"))
        md.append(f"\n**{name}: in-scope papers lost** ({len(lost)} in total; random 15 shown)\n")
        md += [f"- {r['title']} ({r['id']}; {r['categories'].replace('|', ', ')})" for r in lost[:15]] or ["- none"]
        md.append(f"\n**{name}: new papers gained** ({len(gained)} in total; first 15 of the seeded random order; "
                  f"hand label in brackets where available)\n")
        lab = lambda r: {"1": "on topic", "0": "off topic", "": "not hand-labelled"}[r["hand_on_topic"]]
        md += [f"- [{lab(r)}] {r['title']} ({r['id']}; {r['categories'].replace('|', ', ')})" for r in gained[:15]] or ["- none"]
    md.append(f"\n**{track}: hand-labelled gold articles missed by the current query, and which candidates retrieve them**\n")
    names = list(cands)
    md.append("| arXiv id | title | categories | " + " | ".join(n.split("_")[0] for n in names) + " |\n|---|---|---|" + "---|" * len(names))
    for g in gold_rows.values():
        if not g[cur_name]:
            md.append(f"| {g['arxiv_id']} | {g['arxiv_title'][:90]} | {g['arxiv_categories'].replace('|', ', ')} | " +
                      " | ".join("yes" if g[n] else "." for n in names) + " |")

open(os.path.join(CACHE, "score_tables.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
sys.stdout.reconfigure(encoding="utf-8")
for track in CANDIDATES:
    print("==", track)
    for r in read_csv(os.path.join(CACHE, f"score_{track}.csv")):
        print(f"{r['candidate']:34s} n={r['retrieved']:>5} in_kept={r['in_scope_kept']}/{r['in_scope_total']} out_removed={r['out_scope_removed']}/{r['out_scope_total']} "
              f"new={r['new_papers']:>5} gold={r['gold_hand_retrieved']}/{r['gold_hand_total']} kwgold={r['gold_kw_retrieved']}/{r['gold_kw_total']} "
              f"kw_new={r['new_kw_share']} hand={r['hand_on_topic']}/{r['hand_sample_n']} est_prec={r['est_overall_precision']}")
