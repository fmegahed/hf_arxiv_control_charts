"""Step 22. Clauses aimed at the gold articles that the kept-term query still misses.

The single terms that match the missed gold (for SPM: "change-point detection", "online monitoring",
"data streams", "real-time monitoring", "concept drift"; for DOE: "sequential design", "active learning",
"computer experiments"; for reliability: single words such as failure, lifetime, censored) were all
dropped in step 14 because alone they bring in too much off-topic work. Here they are tested again as
CONJUNCTIONS (a wording term AND a topic anchor), and a few further single terms are tested, in the same
per-term way: papers matched, papers new beyond the kept-term query, missed gold recovered, on-topic
share of a seeded random sample of the new papers (Luna v2), dominant primary categories, verdict.

Verdict rule: the same as step 14 (at least 60% on topic with n >= 8, or a small fully on-topic sample
or one that recovers gold; at most 50 estimated off-topic additions), and the clause must recover at
least one missed gold article or add at least 10 papers.

Modes
  python -I 22_missed_gold_terms.py sample   writes cache/v2/conj_{track}_sample.csv (to label; step 15)
  python -I 22_missed_gold_terms.py          writes cache/v2/conj_{track}.csv / .md and cache/v2/final_{track}.json
"""
import json, os, random, sys
from collections import Counter
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import luna_on_topic, CACHE, HERE, TOPIC, read_csv, write_csv
from qd_terms import TRACK_TERMS
import qd_arxiv

MODE = sys.argv[1] if len(sys.argv) > 1 else "tables"
V2 = os.path.join(CACHE, "v2")
N, MAX_OFF, MAXN = 25, 50, 4000
gold_all = json.load(open(os.path.join(HERE, "hand_labels", "gold_ids_v2.json")))
lf = os.path.join(CACHE, "arxiv_luna_labels_v2.csv")
luna = {r["key"]: r for r in read_csv(lf)} if os.path.exists(lf) else {}

CLAUSES = {
    "spc": [
        '(abs:"online monitoring" AND abs:"data streams")', '(ti:monitoring AND abs:"data streams")',
        '(ti:monitoring AND abs:"streaming data")', '(ti:monitoring AND abs:"change-point")',
        '(ti:monitoring AND abs:"false alarm")', '(ti:monitoring AND abs:"run length")',
        '(ti:monitoring AND abs:"quality control")', '(ti:monitoring AND abs:manufacturing)',
        '(ti:monitoring AND abs:"anomaly detection")', '(ti:monitoring AND abs:"high-dimensional")',
        '(abs:"change-point detection" AND abs:"online monitoring")', '(abs:"change-point detection" AND abs:"real-time monitoring")',
        '(abs:"online change-point detection" AND abs:"streaming data")', '(abs:"sequential change" AND abs:monitoring)',
        '(abs:"change detection" AND abs:"quality control")', '(abs:"change detection" AND abs:manufacturing)',
        '(ti:"concept drift" AND ti:monitoring)', '(abs:"fault detection" AND abs:"monitoring statistic")',
        '(abs:CUSUM AND abs:chart)', '(abs:CUSUM AND abs:monitoring)', '(abs:"average run length" AND abs:chart)',
        '(abs:"average run length" AND abs:monitoring)', '(abs:"run length" AND abs:monitoring)',
        '(ti:"control limits")', '(abs:"quickest change-point detection" AND abs:chart)',
        '(ti:monitoring AND abs:"Phase II")', '(ti:monitoring AND abs:"in-control")',
    ],
    "exp_design": [
        '(ti:"sequential design" AND abs:"computer experiments")', '(ti:"sequential design" AND abs:simulation)',
        '(abs:"sequential design" AND abs:"Gaussian process")', '(ti:"active learning" AND abs:"computer experiments")',
        '(ti:"active learning" AND abs:surrogate)', '(ti:"active learning" AND abs:"Gaussian process")',
        '(ti:"active learning" AND abs:simulation)', '(abs:"computer experiments" AND ti:design)',
        '(abs:"computer experiments" AND abs:"sequential design")', '(abs:"computer experiments" AND abs:"space-filling")',
        '(abs:"experimental design" AND abs:"Gaussian process")', '(abs:"experimental design" AND abs:simulator)',
        '(ti:design AND abs:"design of experiments")', '(ti:designs AND abs:"optimal design")', '(ti:designs AND abs:"optimal designs" AND abs:regression)',
        '(ti:"optimal designs")', '(ti:"online experimentation")', '(abs:"online experimentation")', '(ti:"adaptive design" AND abs:"computer experiments")',
        '(ti:"analysis of experiments")', '(abs:"unreplicated")', '(ti:"Bayesian optimization" AND abs:"computer experiments")',
        '(abs:"accelerated life test" AND abs:"optimal design")', '(ti:"experiment design")', '(abs:"multi-fidelity" AND abs:"sequential design")',
    ],
    "reliability": [
        '(abs:reliability AND abs:failure)', '(abs:reliability AND abs:lifetime)', '(abs:reliability AND abs:censored)',
        '(abs:reliability AND abs:weibull)', '(abs:reliability AND abs:degradation)', '(abs:reliability AND abs:maintenance)',
        '(abs:failure AND abs:weibull)', '(abs:failure AND abs:lifetime)', '(abs:"recurrent event" AND abs:reliability)',
        '(abs:prognostics AND abs:degradation)', '(ti:prognostics AND abs:"useful life")', '(abs:"time to failure" AND abs:reliability)',
        '(abs:lifetime AND abs:"accelerated")', '(ti:dependability)', '(abs:dependability)', '(ti:"fault injection")',
        '(abs:"soft errors" AND abs:reliability)', '(ti:availability AND abs:reliability)', '(ti:"load-sharing")',
        '(abs:"load-sharing" AND abs:reliability)', '(abs:"health indicator")', '(ti:"cascading failures")',
        '(abs:"damage model")', '(abs:"minimal path")', '(ti:maintenance AND abs:scheduling)', '(abs:"failure probability" AND abs:surrogate)',
        '(abs:"lifetime data" AND abs:reliability)', '(abs:"reliability analysis" AND abs:failure)', '(abs:"assurance test")',
    ],
}
BASE_NAME = {"spc": "S6_kept_terms", "exp_design": "D5_kept_terms", "reliability": "R6_kept_terms"}

from qd_candidates_v2 import CANDIDATES_V2
for track, clauses in CLAUSES.items():
    topic, cfg = TOPIC[track], TRACK_TERMS[track]
    cats, catq = set(cfg["cats"]), "(" + " OR ".join("cat:" + c for c in cfg["cats"]) + ")"
    base_q = CANDIDATES_V2[track][BASE_NAME[track]]
    base = {e["id"] for e in qd_arxiv.fetch(base_q)[1]}
    gold = set(gold_all[track]); missed = gold - base
    rows, samples, keep_forms = [], {}, []
    for cl in clauses:
        n_all = qd_arxiv.count(cl)
        n_cat = n_all if n_all <= MAXN else qd_arxiv.count(f"({cl} AND {catq})")
        if n_cat > MAXN:
            rows.append({"clause": cl, "variant": "in statistics categories", "matched": n_cat, "new": "", "missed_gold_recovered": "",
                         "gold_ids": "", "sample_n": 0, "sample_on_topic": 0, "sample_on_topic_strict": 0, "top_primary_new": "", "verdict": "drop",
                         "reason": f"too broad ({n_all} matches; {n_cat} inside the statistics categories)", "_ids": set()})
            continue
        got = {e["id"]: e for e in (qd_arxiv.fetch(cl)[1] if n_all <= MAXN else qd_arxiv.fetch(f"({cl} AND {catq})")[1])}
        for variant in (("unrestricted", "in statistics categories") if n_all <= MAXN else ("in statistics categories",)):
            ids = set(got) if variant == "unrestricted" else {i for i, e in got.items() if set(e["categories"].split("|")) & cats}
            new = sorted(ids - base)
            order = new[:]; random.Random(f"conj|{track}|{cl}|{variant}").shuffle(order)
            samp = order[:N]
            for i in samp:
                samples.setdefault(i, {"key": f"{track}:{i}", "id": i, "title": got[i]["title"], "abstract": got[i]["abstract"],
                                       "categories": got[i]["categories"], "of": []})["of"].append(cl)
            lab = [luna_on_topic(luna[f"{track}:{i}"], track) for i in samp if f"{track}:{i}" in luna]
            lab_strict = [luna_on_topic(luna[f"{track}:{i}"], track, True) for i in samp if f"{track}:{i}" in luna]
            g = sorted(missed & ids)
            prim = Counter(got[i]["primary_category"] for i in new).most_common(3)
            r = {"clause": cl, "variant": variant, "matched": n_all if variant == "unrestricted" else len(ids), "new": len(new),
                 "missed_gold_recovered": len(g), "gold_ids": " ".join(g), "sample_n": len(lab), "sample_on_topic": sum(lab), "sample_on_topic_strict": sum(lab_strict),
                 "top_primary_new": ", ".join(f"{c} {n}" for c, n in prim)}
            if not new:
                r.update({"verdict": "drop", "reason": "adds nothing"})
            elif not lab:
                r.update({"verdict": "", "reason": "sample not labelled yet"})
            else:
                n, k = len(lab), sum(lab); sh = k / n; off = round(len(new) * (1 - sh))
                ok = ((n >= 8 and sh >= 0.6) or (n < 8 and sh >= 0.6 and (g or k == n))) and off <= MAX_OFF and (g or len(new) >= 10)
                r.update({"verdict": "keep" if ok else "drop",
                          "reason": f"{k}/{n} on topic" + (f", recovers {len(g)} missed gold" if g else "") +
                                    ("" if ok else " (below 60%)" if sh < 0.6 else f" (about {off} off-topic additions)" if off > MAX_OFF else " (too little)")})
            r["_ids"] = ids
            rows.append(r)
    if MODE == "sample":
        srows = sorted(samples.values(), key=lambda s: s["id"])
        for s in srows:
            s["of"] = " ; ".join(s["of"])
        write_csv(os.path.join(V2, f"conj_{track}_sample.csv"), srows, ["key", "id", "title", "abstract", "categories", "of"])
        print(track, "missed gold:", len(missed), "papers to label:", len(srows))
        continue
    # greedy: prefer the unrestricted form of a clause when it is kept, otherwise the restricted one; most gold first
    best = {}
    for r in rows:
        if r["verdict"] == "keep" and (r["clause"] not in best or r["variant"] == "unrestricted"):
            best[r["clause"]] = r
    have, free, restr = set(base), [], []
    for r in sorted(best.values(), key=lambda r: (-r["missed_gold_recovered"], -r["sample_on_topic"] / max(1, r["sample_n"]))):
        add = r["_ids"] - have
        if len(gold & add) >= 1 or len(add) >= 10:
            have |= r["_ids"]; (free if r["variant"] == "unrestricted" else restr).append(r["clause"])
            r["in_final_query"] = "yes"
    parts = [base_q[1:-1] if False else base_q] + ([("(" + " OR ".join(free) + ")")] if free else []) + \
            ([f"(({' OR '.join(restr)}) AND {catq})"] if restr else [])
    final = "(" + " OR ".join(parts) + ")" if len(parts) > 1 else base_q
    json.dump({"query": final, "added_unrestricted": free, "added_category_restricted": restr},
              open(os.path.join(V2, f"final_{track}.json"), "w", encoding="utf-8"), indent=1)
    for r in rows:
        r.pop("_ids"); r.setdefault("in_final_query", "")
    write_csv(os.path.join(V2, f"conj_{track}.csv"), rows)
    md = [f"Baseline: `{BASE_NAME[track]}` ({len(base)} papers, gold {len(gold & base)} of {len(gold)}; {len(missed)} gold articles missed).", "",
          "| clause | variant | matched | new beyond the kept-term query | missed gold recovered | on topic in sample (Luna v2) | on topic without theory-flagged papers (SPM only) | dominant primary categories of new papers | verdict | reason | in final query |",
          "|---|---|---|---|---|---|---|---|---|---|---|"]
    for r in rows:
        md.append(f"| `{r['clause']}` | {r['variant']} | {r['matched']} | {r['new']} | {r['missed_gold_recovered']} | "
                  f"{r['sample_on_topic']}/{r['sample_n']} | {r['sample_on_topic_strict']}/{r['sample_n']} | {r['top_primary_new']} | {r['verdict']} | {r['reason']} | {r['in_final_query']} |")
    open(os.path.join(V2, f"conj_{track}.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
    print(track, "kept clauses:", free, restr, "| final query chars:", len(final))
