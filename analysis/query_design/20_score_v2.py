"""Step 20. Score the follow-up candidates against the expanded gold sets and the silver flags.

Per candidate: size, papers new versus the current query, gold recall (hand_labels/gold_ids_v2.json),
silver in-scope kept and out-of-scope removed, on-topic share of a seeded random sample of 40 newly
retrieved papers (Luna v2; cache/arxiv_luna_labels_v2.csv), URL length of the GET request, and the gold
articles still missed (with the journal title, DOI and arXiv categories).

Modes
  python -I 20_score_v2.py sample   fetches the candidates and writes cache/v2/{track}_sample.csv (to label, step 15)
  python -I 20_score_v2.py          writes cache/v2/score_{track}.csv, cache/v2/missed_{track}__{cand}.csv,
                                    cache/v2/score_v2.md
"""
import json, os, random, sys, urllib.parse
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import luna_on_topic, CACHE, HERE, TOPIC, load_corpus, read_csv, write_csv
from qd_candidates_v2 import CANDIDATES_V2
import qd_arxiv

MODE = sys.argv[1] if len(sys.argv) > 1 else "tables"
V2 = os.path.join(CACHE, "v2"); os.makedirs(V2, exist_ok=True)
N = 40
gold_all = json.load(open(os.path.join(HERE, "hand_labels", "gold_ids_v2.json")))
lf = os.path.join(CACHE, "arxiv_luna_labels_v2.csv")
luna = {r["key"]: r for r in read_csv(lf)} if os.path.exists(lf) else {}
md = []


def wilson(k, n, z=1.96):
    p = k / n
    c = (p + z * z / (2 * n)) / (1 + z * z / n)
    h = z * ((p * (1 - p) / n + z * z / (4 * n * n)) ** 0.5) / (1 + z * z / n)
    return f"{100 * (c - h):.0f} to {100 * (c + h):.0f}%"


for track, cands in CANDIDATES_V2.items():
    topic = TOPIC[track]
    corpus = load_corpus(track)
    IN = {i for i, p in corpus.items() if p["flag"] is True}
    OUT = {i for i, p in corpus.items() if p["flag"] is False}
    gold = set(gold_all[track])
    gmeta = {r["arxiv_id"]: r for r in read_csv(os.path.join(CACHE, f"gold_v2_candidates_{track}.csv"))}
    got = {}
    for name, q in cands.items():
        if q:
            got[name] = {e["id"]: e for e in qd_arxiv.fetch(q)[1]}
    cur = set(got[list(cands)[0]])
    rows, samples = [], {}
    for name, R in got.items():
        ids = set(R)
        new = sorted(ids - cur - set(corpus))
        order = new[:]
        random.Random(f"v2|{track}|{name}").shuffle(order)
        samp = order[:N]
        for i in samp:
            samples.setdefault(i, {"key": f"{track}:{i}", "id": i, "title": R[i]["title"], "abstract": R[i]["abstract"],
                                   "categories": R[i]["categories"], "of": []})["of"].append(name)
        lab = [luna_on_topic(luna[f"{track}:{i}"], track) for i in samp if f"{track}:{i}" in luna]
        theory = [luna_on_topic(luna[f"{track}:{i}"], track) and not luna_on_topic(luna[f"{track}:{i}"], track, True)
                  for i in samp if f"{track}:{i}" in luna]
        q = cands[name]
        url = qd_arxiv.API + "?" + urllib.parse.urlencode({"search_query": q, "start": 0, "max_results": 1000})
        # request line as httr (the HTTP client inside aRxiv) builds it: every reserved character percent-encoded
        httr_line = len("GET /api/query?search_query=" + urllib.parse.quote(q, safe="") + "&start=0&max_results=1000 HTTP/1.1")
        kin, kout = len(ids & IN), len(ids & OUT)
        p_new = sum(lab) / len(lab) if lab else None
        rows.append({"candidate": name, "retrieved": len(ids), "new_vs_current": len(new), "dropped_vs_current": len(cur - ids),
                     "gold_retrieved": len(gold & ids), "gold_total": len(gold),
                     "silver_in_kept": kin, "silver_in_total": len(IN), "silver_out_removed": len(OUT - ids),
                     "silver_out_total": len(OUT),
                     "sample_n": len(lab), "sample_on_topic": sum(lab), "sample_change_theory": sum(theory) if track == "spc" else "",
                     "est_precision": round((kin + p_new * len(new)) / (kin + kout + len(new)), 2) if lab and (kin + kout + len(new))
                     else (round(kin / (kin + kout), 2) if not new else ""),
                     "query_chars": len(q), "get_url_chars": len(url), "httr_request_line": httr_line,
                     "fits_arxiv_limit_4094": "yes" if httr_line <= 4094 else "NO", "query": q})
        missed = [{"arxiv_id": g, "journal": gmeta[g]["journal"], "year": gmeta[g]["year"], "doi": gmeta[g]["doi"],
                   "arxiv_categories": gmeta[g]["arxiv_categories"], "arxiv_title": gmeta[g]["arxiv_title"]}
                  for g in sorted(gold - ids)]
        write_csv(os.path.join(V2, f"missed_{track}__{name}.csv"), missed,
                  ["arxiv_id", "journal", "year", "doi", "arxiv_categories", "arxiv_title"])
    if MODE == "sample":
        srows = sorted(samples.values(), key=lambda s: s["id"])
        for s in srows:
            s["of"] = "|".join(s["of"])
        write_csv(os.path.join(V2, f"{track}_sample.csv"), srows, ["key", "id", "title", "abstract", "categories", "of"])
        print(track, {r["candidate"]: (r["retrieved"], r["new_vs_current"], f"gold {r['gold_retrieved']}/{r['gold_total']}") for r in rows},
              "to label:", len(srows))
        continue
    write_csv(os.path.join(V2, f"score_{track}.csv"), rows)
    md.append(f"\n### {track}\n")
    md.append("| candidate | retrieved | new versus current | dropped versus current | gold recall (expanded gold) | silver in scope kept | "
              "silver out of scope removed | on topic in sample of new papers (Luna v2) | of which change-detection theory flag | "
              "estimated overall precision | query characters | GET URL characters |\n|---|---|---|---|---|---|---|---|---|---|---|---|")
    for r in rows:
        samp = (f"{r['sample_on_topic']}/{r['sample_n']} ({100 * r['sample_on_topic'] / r['sample_n']:.0f}%, 95% CI "
                f"{wilson(r['sample_on_topic'], r['sample_n'])})") if r["sample_n"] else "n/a"
        md.append(f"| {r['candidate']} | {r['retrieved']} | {r['new_vs_current']} | {r['dropped_vs_current']} | "
                  f"{r['gold_retrieved']}/{r['gold_total']} ({100 * r['gold_retrieved'] / r['gold_total']:.0f}%) | "
                  f"{r['silver_in_kept']}/{r['silver_in_total']} | {r['silver_out_removed']}/{r['silver_out_total']} | {samp} | "
                  f"{r['sample_change_theory']} | {r['est_precision']} | {r['query_chars']} | {r['httr_request_line']} ({'fits' if r['fits_arxiv_limit_4094'] == 'yes' else 'TOO LONG'}) |")
    for r in rows:
        md.append(f"\n**{r['candidate']}** (verbatim)\n\n```\n{r['query']}\n```")
    for r in rows[2:]:  # every follow-up candidate: the gold articles it still misses
        missed = read_csv(os.path.join(V2, f"missed_{track}__{r['candidate']}.csv"))
        md.append(f"\n**Gold articles still missed by {r['candidate']}** ({len(missed)} of {len(gold)})\n")
        md.append("| arXiv id | journal | DOI | arXiv categories | arXiv title |\n|---|---|---|---|---|")
        for m in missed:
            md.append(f"| {m['arxiv_id']} | {m['journal']} | {m['doi']} | {m['arxiv_categories'].replace('|', ', ')} | {m['arxiv_title']} |")
if MODE != "sample":
    open(os.path.join(V2, "score_v2.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
    sys.stdout.reconfigure(encoding="utf-8")
    print("\n".join(l for l in md if l.startswith("| ") and "arXiv id" not in l and not l[2:6].isdigit())[:6000])
