"""Step 14. Per-term marginal tables, keep/drop verdicts, and the query assembled from kept terms.

For each term and each of four variants (title or abstract, with or without the statistics-category
restriction) it reports: papers matched, papers NEW beyond the track's names-based base query, gold
(journal-matched) papers recovered that the base misses, the on-topic share of a seeded random sample
of the new papers under the Luna v2 rubric, the dominant primary categories of the new papers, and the
share of matches that contain the term literally (a low share flags a stemming / tokenisation trap).

Modes
  python -I 14_term_tables.py sample   writes cache/terms/{track}_sample.csv (papers to label; step 15)
  python -I 14_term_tables.py          writes cache/terms/{track}_terms.csv, cache/terms/{track}_terms.md,
                                       cache/terms/{track}_assembled.json (kept terms and assembled query)
Verdict rule (applied mechanically, stated in the report):
  keep  if the sample is at least 60% on topic (n >= 8), or n < 8 and at least 60% on topic and
        (gold recovered >= 1 or every sampled paper on topic); AND the variant would add no more than
        MAX_OFF (50) off-topic papers, estimated as new papers x (1 - on-topic share);
  drop  otherwise; "adds nothing" when the variant has no new papers; "too broad" when the API
        returns more than MAXN records for it, or when an unrestricted variant alone adds more than
        BROAD_NEW papers (those are not sampled).
"""
import json, os, random, re, sys
from collections import Counter
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import luna_on_topic, CACHE, HERE, TOPIC, load_corpus, read_csv, write_csv
from qd_terms import TRACK_TERMS

SAMPLE_N, MAXN, BROAD_NEW, MAX_OFF = 25, 4000, 1000, 50
MODE = sys.argv[1] if len(sys.argv) > 1 else "tables"
STRICT = MODE == "strict"   # SPM only: do not count papers with the change-detection-theory flag
SUF = "_strict" if STRICT else ""
TDIR = os.path.join(CACHE, "terms")
gold_all = json.load(open(os.path.join(HERE, "hand_labels", "gold_ids_v2.json"))) if os.path.exists(
    os.path.join(HERE, "hand_labels", "gold_ids_v2.json")) else json.load(open(os.path.join(HERE, "hand_labels", "gold_ids.json")))
lf = os.path.join(CACHE, "arxiv_luna_labels_v2.csv")
luna = {r["key"]: r for r in read_csv(lf)} if os.path.exists(lf) else {}


def literal_pattern(term):
    """Regex for a literal occurrence: words in order, hyphen or space between, optional plural s."""
    words = re.findall(r"[A-Za-z0-9]+", term)
    return re.compile(r"\b" + r"[\s\-/]+".join(re.escape(w) for w in words) + r"(s|es)?\b", re.I)


def variants(rec, papers, cats):
    """Return {variant: (ids or None, api_count or None)}; None ids = too broad to download."""
    out = {}
    incat = lambda ids: [i for i in ids if set(papers[i]["categories"].split("|")) & cats]
    for f in ("ti", "abs"):
        if rec[f + "_complete"]:
            ids = rec[f + "_ids"]
            out[f] = (ids, rec[f + "_count"])
            out[f + "+cat"] = (incat(ids), None)
        else:
            out[f] = (None, rec[f + "_count"])
            out[f + "+cat"] = ((rec.get(f + "_cat_ids"), rec.get(f + "_cat_count")) if f + "_cat_ids" in rec
                               else (None, rec.get(f + "_cat_count")))
    return out


for track, cfg in TRACK_TERMS.items():
    if STRICT and track != "spc":
        continue
    sf = os.path.join(TDIR, f"{track}_sets.json")
    if not os.path.exists(sf):
        continue
    topic = TOPIC[track]
    sets = json.load(open(sf, encoding="utf-8"))
    papers = {r["id"]: r for r in read_csv(os.path.join(TDIR, f"{track}_papers.csv"))}
    cats = set(cfg["cats"])
    base = set(sets["base"])
    gold = set(gold_all[track])
    corpus = load_corpus(track)
    IN = {i for i, p in corpus.items() if p["flag"] is True}
    OUT = {i for i, p in corpus.items() if p["flag"] is False}
    rows, samples, own_samples = [], {}, {}
    for term, rec in sets["terms"].items():
        pat = literal_pattern(term)
        vs = variants(rec, papers, cats)
        for vname in ("ti+cat", "ti", "abs+cat", "abs"):  # restricted first, so the unrestricted estimate can pool it
            ids, api_n = vs[vname]
            row = {"term": term, "variant": vname, "matched": api_n if ids is None else len(ids)}
            if ids is None:
                row.update({"verdict": "drop", "reason": f"too broad (more than {MAXN} records)"})
                rows.append(row); continue
            new = sorted(set(ids) - base)
            if "+cat" not in vname and len(new) > BROAD_NEW:
                prim = Counter(papers[i]["primary_category"] for i in new).most_common(3)
                row.update({"new": len(new), "gold_new": len(gold & set(new)), "gold_total_matched": len(gold & set(ids)),
                            "top_primary_new": ", ".join(f"{c} {n}" for c, n in prim), "verdict": "drop",
                            "reason": f"too broad without a category restriction (adds more than {BROAD_NEW} papers on its own)"})
                rows.append(row); continue
            field = "title" if vname.startswith("ti") else "abstract"
            lit = sum(1 for i in ids if pat.search(papers[i][field])) / len(ids) if ids else None
            order = new[:]
            random.Random(f"{track}|{term}|{vname}|20261008").shuffle(order)
            samp = order[:SAMPLE_N]
            for i in samp:
                samples.setdefault(i, {"key": f"{track}:{i}", "id": i, "title": papers[i]["title"],
                                       "abstract": papers[i]["abstract"], "categories": papers[i]["categories"], "of": []})
                samples[i]["of"].append(f"{term} [{vname}]")
            lab = [luna_on_topic(luna[f"{track}:{i}"], track, STRICT) for i in samp if f"{track}:{i}" in luna]
            lab_other = [luna_on_topic(luna[f"{track}:{i}"], track, not STRICT) for i in samp if f"{track}:{i}" in luna]
            strat = None
            if "+cat" not in vname and (term, vname + "+cat") in own_samples:
                # Stratified estimate for an unrestricted variant: papers inside the statistics categories are judged
                # from this sample pooled with the restricted variant's own sample; papers outside from this sample only.
                incat = lambda i: bool(set(papers[i]["categories"].split("|")) & cats)
                pool_in = {i for i in samp if incat(i)} | set(own_samples[(term, vname + "+cat")])
                pool_out = {i for i in samp if not incat(i)}
                L = lambda S: [luna_on_topic(luna[f"{track}:{i}"], track, STRICT) for i in S if f"{track}:{i}" in luna]
                li, lo = L(pool_in), L(pool_out)
                n_in = sum(1 for i in new if incat(i)); n_out = len(new) - n_in
                if (li or not n_in) and (lo or not n_out):
                    strat = ((n_in * (sum(li) / len(li)) if li else 0) + (n_out * (sum(lo) / len(lo)) if lo else 0)) / max(1, len(new))
            own_samples[(term, vname)] = samp
            prim = Counter(papers[i]["primary_category"] for i in new).most_common(3)
            row.update({"new": len(new), "gold_total_matched": len(gold & set(ids)), "gold_new": len(gold & set(new)),
                        "gold_new_ids": " ".join(sorted(gold & set(new))),
                        "silver_in_matched": len(IN & set(ids)), "silver_out_matched": len(OUT & set(ids)),
                        "sample_n": len(lab), "sample_on_topic": sum(lab), "sample_on_topic_other_rule": sum(lab_other),
                        "share": round(sum(lab) / len(lab), 2) if lab else "",
                        "top_primary_new": ", ".join(f"{c} {n}" for c, n in prim),
                        "literal_share": round(lit, 2) if lit is not None else ""})
            if not new:
                row.update({"verdict": "drop", "reason": "adds nothing beyond the base"})
            elif not lab:
                row.update({"verdict": "", "reason": "sample not labelled yet"})
            else:
                n, k, sh = len(lab), sum(lab), sum(lab) / len(lab)
                if strat is not None:
                    sh = strat
                    row["share"] = round(sh, 2)
                    row["share_note"] = "stratified by category, pooled with the restricted sample"
                off = round(len(new) * (1 - sh))  # estimated off-topic papers this variant would add
                row["est_off_topic_added"] = off
                keep = ((n >= 8 and sh >= 0.6) or (n < 8 and sh >= 0.6 and (row["gold_new"] >= 1 or k == n))) and off <= MAX_OFF
                row["verdict"] = "keep" if keep else "drop"
                row["reason"] = ((f"{k}/{n} sampled new papers on topic" if strat is None else
                                  f"{100 * sh:.0f}% on topic (stratified; own sample {k}/{n})") + (f", +{row['gold_new']} gold" if row["gold_new"] else "")
                                 + ("" if keep else " (below 60%)" if sh < 0.6 else
                                    f" (would add about {off} off-topic papers)" if off > MAX_OFF else " (too few to judge, no gold)"))
            rows.append(row)
    if MODE == "sample":
        srows = sorted(samples.values(), key=lambda s: s["id"])
        for s in srows:
            s["of"] = " ; ".join(s["of"])
        write_csv(os.path.join(TDIR, f"{track}_sample.csv"), srows, ["key", "id", "title", "abstract", "categories", "of"])
        print(track, "papers to label:", len(srows))
        continue
    fields = ["term", "variant", "matched", "new", "gold_new", "gold_total_matched", "silver_in_matched", "silver_out_matched",
              "sample_n", "sample_on_topic", "sample_on_topic_other_rule", "share", "est_off_topic_added", "top_primary_new", "literal_share", "verdict", "reason", "gold_new_ids"]
    write_csv(os.path.join(TDIR, f"{track}_terms{SUF}.csv"), rows, fields)

    # choose, per term, the broadest kept form; then a greedy pass keeps a form only if it still adds
    # something once the forms accepted before it are in the query (at least MIN_ADD papers or one gold article)
    MIN_ADD = 10  # raised from 3 so that the assembled queries stay within the 4,094-byte request line of the arXiv server
    by, idsets = {}, {}
    for r in rows:
        by.setdefault(r["term"], {})[r["variant"]] = r
    for term, rec in sets["terms"].items():
        for vname, (ids, _) in variants(rec, papers, cats).items():
            idsets[(term, vname)] = set(ids or [])
    forms = []
    for term, v in by.items():
        k = lambda name: v.get(name, {}).get("verdict") == "keep"
        for f in ("ti", "abs"):
            name = f if k(f) else (f + "+cat" if k(f + "+cat") else None)
            if name:
                r = v[name]
                forms.append({"term": term, "field": f, "restricted": name.endswith("+cat"), "ids": idsets[(term, name)],
                              "gold_new": int(r["gold_new"]), "share": float(r["share"] or 0), "sample": f"{r['sample_on_topic']}/{r['sample_n']}"})
    forms.sort(key=lambda f: (-f["gold_new"], -f["share"], -len(f["ids"])))
    have, kept_free, kept_cat, decisions = set(base), [], [], []
    for f in forms:
        add = f["ids"] - have
        g = len(gold & add)
        ok = len(add) >= MIN_ADD or g >= 1
        decisions.append({"form": f"{f['field']}:{f['term']}", "category_restricted": "yes" if f["restricted"] else "no",
                          "sample_on_topic": f["sample"], "adds_papers": len(add), "adds_gold": g,
                          "decision": "in query" if ok else "left out (redundant with terms already in the query)"})
        if ok:
            have |= f["ids"]
            (kept_cat if f["restricted"] else kept_free).append(f"{f['field']}:{f['term']}")
    write_csv(os.path.join(TDIR, f"{track}_assembly{SUF}.csv"), decisions)
    catq = "(" + " OR ".join("cat:" + c for c in cfg["cats"]) + ")"
    parts = [cfg["base"]]
    if kept_free:
        parts.append("(" + " OR ".join(kept_free) + ")")
    if kept_cat:
        parts.append("((" + " OR ".join(kept_cat) + f") AND {catq})")
    assembled = "(" + " OR ".join(parts) + ")"
    restricted = "(" + cfg["base"] + " OR ((" + " OR ".join(kept_free + kept_cat) + f") AND {catq}))" if kept_free + kept_cat else cfg["base"]
    json.dump({"query": assembled, "query_all_restricted": restricted, "kept_unrestricted": kept_free, "kept_category_restricted": kept_cat,
               "predicted_size": len(have), "predicted_gold": len(gold & have), "gold_total": len(gold)},
              open(os.path.join(TDIR, f"{track}_assembled{SUF}.json"), "w", encoding="utf-8"), indent=1)
    amd = ["| order | term form | only inside the statistics categories | on topic in its sample | papers it adds at that point | gold it adds | decision |",
           "|---|---|---|---|---|---|---|"]
    amd += [f"| {i + 1} | `{d['form']}` | {d['category_restricted']} | {d['sample_on_topic']} | {d['adds_papers']} | {d['adds_gold']} | {d['decision']} |"
            for i, d in enumerate(decisions)]
    open(os.path.join(TDIR, f"{track}_assembly{SUF}.md"), "w", encoding="utf-8").write(chr(10).join(amd) + chr(10))

    md = [f"Base query ({len(base)} papers, gold {len(gold & base)} of {len(gold)}): `{cfg['base']}`", "",
          f"Category restriction for the `+cat` variants: {', '.join(cfg['cats'])}. Sample size per variant: up to {SAMPLE_N} new papers.", "",
          "| term | variant | matched | new beyond base | gold recovered (beyond base) | on topic in sample (Luna v2) | dominant primary categories of new papers | literal share | verdict | reason |",
          "|---|---|---|---|---|---|---|---|---|---|"]
    for r in rows:
        samp = f"{r['sample_on_topic']}/{r['sample_n']}" if r.get("sample_n") else "n/a"
        md.append(f"| `{r['term']}` | {r['variant']} | {r['matched']} | {r.get('new', '')} | {r.get('gold_new', '')} | {samp} | "
                  f"{r.get('top_primary_new', '')} | {r.get('literal_share', '')} | {r['verdict']} | {r['reason']} |")
    open(os.path.join(TDIR, f"{track}_terms{SUF}.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
    print(track, "terms:", len(by), "forms kept by the sample rule:", len(forms), "in query: unrestricted", len(kept_free),
          "category-restricted", len(kept_cat), "| predicted size", len(have), "gold", len(gold & have), "/", len(gold),
          "| query chars:", len(assembled))
