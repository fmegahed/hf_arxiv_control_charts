"""Step 19. Blind hand-check samples for the Luna v2 labels, and the agreement tables.

  python -I 19_handcheck_v2.py journal_sample   seeded stratified sample of 80 journal articles (20 per Luna topic, 20 none),
                                                printed shuffled WITHOUT the Luna labels -> cache/v2/handcheck_journal_v2.csv
  python -I 19_handcheck_v2.py arxiv_sample     40 sampled arXiv papers per track (20 Luna on topic, 20 off), shuffled, no labels
                                                -> cache/v2/handcheck_arxiv_v2.csv
  python -I 19_handcheck_v2.py                  agreement tables from hand_labels/handcheck_v2_journal.csv and
                                                hand_labels/handcheck_v2_arxiv.csv -> cache/v2/agreement_v2.md
Also reports keyword rubric versus Luna v2 on all journal articles of the original four journals.
"""
import os, random, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import luna_on_topic, CACHE, HERE, TOPIC, read_csv, write_csv
sys.stdout.reconfigure(encoding="utf-8")
V2 = os.path.join(CACHE, "v2"); os.makedirs(V2, exist_ok=True)
mode = sys.argv[1] if len(sys.argv) > 1 else "tables"
TOP = ("reliability", "doe", "spm")

if mode == "journal_sample":
    luna = {r["key"]: r for r in read_csv(os.path.join(CACHE, "journal_labels_v2.csv"))}
    works = {w["openalex_id"]: w for w in read_csv(os.path.join(CACHE, "journal_works.csv"))}
    text = {r["openalex_id"]: r for r in read_csv(os.path.join(CACHE, "journal_text.csv"))}
    rng, pick = random.Random(20261009), []
    strata = [(t, [k for k, v in luna.items() if v[t] == "TRUE"]) for t in TOP]
    strata.append(("none", [k for k, v in luna.items() if all(v[t] == "FALSE" for t in TOP)]))
    for name, pool in strata:
        pool = sorted(set(pool) - {p["openalex_id"] for p in pick})
        for k in rng.sample(pool, 20):
            pick.append({"openalex_id": k, "stratum": name, "journal": works[k]["journal"], "title": works[k]["title"],
                         "abstract": text[k]["abstract_best"][:600]})
    rng.shuffle(pick)
    write_csv(os.path.join(V2, "handcheck_journal_v2.csv"), pick)
    for i, p in enumerate(pick):
        print(f"[{i}] {p['title'][:150]} || {p['abstract'][:300]}")
elif mode == "arxiv_sample":
    luna = {r["key"]: r for r in read_csv(os.path.join(CACHE, "arxiv_luna_labels_v2.csv"))}
    rng, pick = random.Random(20261009), []
    for track, t in TOPIC.items():
        samp = {r["key"]: r for r in read_csv(os.path.join(CACHE, "terms", f"{track}_sample.csv")) if r["key"] in luna}
        for val in ("TRUE", "FALSE"):
            pool = sorted(k for k in samp if luna[k][t] == val)
            for k in rng.sample(pool, min(20, len(pool))):
                pick.append({"key": k, "track": track, "title": samp[k]["title"], "abstract": samp[k]["abstract"][:700]})
    rng.shuffle(pick)
    write_csv(os.path.join(V2, "handcheck_arxiv_v2.csv"), pick)
    for i, p in enumerate(pick):
        print(f"[{i}] <{p['track']}> {p['title'][:140]} || {p['abstract'][:330]}")
else:
    rows = []

    def add(scope, a_name, b_name, topic, pairs):
        n = len(pairs)
        if not n:
            return
        tp = sum(a and b for a, b in pairs); fp = sum(a and not b for a, b in pairs); fn = sum(b and not a for a, b in pairs)
        rows.append({"scope": scope, "A": a_name, "B": b_name, "topic": topic, "n": n, "agree": n - fp - fn,
                     "A yes, B yes": tp, "A yes, B no": fp, "A no, B yes": fn, "A no, B no": n - tp - fp - fn})

    jl = {r["key"]: r for r in read_csv(os.path.join(CACHE, "journal_labels_v2.csv"))}
    kw = {r["openalex_id"]: r for r in read_csv(os.path.join(CACHE, "journal_labelled_kw.csv"))} if os.path.exists(
        os.path.join(CACHE, "journal_labelled_kw.csv")) else {}
    for t in TOP:
        add("all journal articles with both labels", "keyword rubric", "Luna v2", t,
            [(kw[k]["kw_" + t] == "True", jl[k][t] == "TRUE") for k in kw if k in jl])
    f = os.path.join(HERE, "hand_labels", "handcheck_v2_journal.csv")
    if os.path.exists(f):
        h = read_csv(f)
        for t in TOP:
            add("journal sample, 80 articles, eight journals (blind)", "Luna v2", "analyst hand label", t,
                [(jl[r["openalex_id"]][t] == "TRUE", r["hand_" + t] == "1") for r in h])
            if kw:
                add("journal sample (articles that have a keyword label)", "keyword rubric", "analyst hand label", t,
                    [(kw[r["openalex_id"]]["kw_" + t] == "True", r["hand_" + t] == "1") for r in h if r["openalex_id"] in kw])
    f = os.path.join(HERE, "hand_labels", "handcheck_v2_arxiv.csv")
    if os.path.exists(f):
        al = {r["key"]: r for r in read_csv(os.path.join(CACHE, "arxiv_luna_labels_v2.csv"))}
        h = read_csv(f)
        for track, t in TOPIC.items():
            add(f"arXiv papers from the term samples, {track} (blind)", "Luna v2", "analyst hand label", t,
                [(al[r["key"]][t] == "TRUE", r["hand"] == "1") for r in h if r["track"] == track])
            if track == "spc":
                add("arXiv papers from the term samples, spc (blind)", "Luna v2, SPM and not change-theory flag", "analyst hand label", t,
                    [(luna_on_topic(al[r["key"]], track, True), r["hand"] == "1") for r in h if r["track"] == track])
    write_csv(os.path.join(V2, "agreement_v2.csv"), rows)
    md = ["| scope | label A | label B | topic | n | agree | A yes, B yes | A yes, B no | A no, B yes | A no, B no |", "|---|---|---|---|---|---|---|---|---|---|"]
    for r in rows:
        md.append(f"| {r['scope']} | {r['A']} | {r['B']} | {r['topic']} | {r['n']} | {r['agree']} ({100 * r['agree'] / r['n']:.0f}%) | "
                  f"{r['A yes, B yes']} | {r['A yes, B no']} | {r['A no, B yes']} | {r['A no, B no']} |")
    open(os.path.join(V2, "agreement_v2.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
    print("\n".join(md))
