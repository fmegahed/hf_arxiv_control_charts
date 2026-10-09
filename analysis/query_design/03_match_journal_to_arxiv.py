"""Step 3. Find arXiv versions of the journal articles (the query-independent gold pool), and
fill in missing abstracts.

Two sources of arXiv ids, both keyed on the journal DOI and neither depending on any arXiv query:
  (a) OpenAlex locations/ids (already in cache/journal_works.csv, column openalex_arxiv_id)
  (b) Semantic Scholar Graph API batch lookup by DOI (externalIds.ArXiv); the same call returns
      Semantic Scholar's abstract, used where OpenAlex has none (mostly Elsevier journals)
Each id is then resolved against the arXiv API (id_list) and the arXiv title is compared with the
journal title (difflib ratio on normalised titles). Pairs below 0.60 are dropped as mismatches.

Output: cache/journal_arxiv_matches.csv
        cache/journal_text.csv   (openalex_id, abstract_best, abstract_source) best available abstract:
                                 OpenAlex, else Semantic Scholar, else the matched arXiv abstract
Run:    python -I 03_match_journal_to_arxiv.py
"""
import csv, difflib, hashlib, json, os, re, sys, time, urllib.request
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import qd_arxiv

HERE = os.path.dirname(os.path.abspath(__file__))
CACHE = os.path.join(HERE, "cache")
S2DIR = os.path.join(CACHE, "s2_raw_v2"); os.makedirs(S2DIR, exist_ok=True)
csv.field_size_limit(10**9)

works = list(csv.DictReader(open(os.path.join(CACHE, "journal_works.csv"), encoding="utf-8")))
dois = sorted({w["doi"] for w in works if w["doi"]})
print("journal works:", len(works), "with DOI:", len(dois))

# ---- (b) Semantic Scholar batch; cache file is keyed on the DOIs in the batch ----
s2, s2abs = {}, {}
for i in range(0, len(dois), 400):
    chunk = dois[i:i + 400]
    f = os.path.join(S2DIR, hashlib.sha1("|".join(chunk).encode()).hexdigest()[:16] + ".json")
    if not os.path.exists(f):
        body = json.dumps({"ids": ["DOI:" + d for d in chunk]}).encode()
        url = "https://api.semanticscholar.org/graph/v1/paper/batch?fields=externalIds,title,abstract"
        for attempt in range(10):
            try:
                req = urllib.request.Request(url, data=body, headers={"Content-Type": "application/json"})
                with urllib.request.urlopen(req, timeout=180) as r:
                    data = r.read().decode("utf-8")
                open(f, "w", encoding="utf-8").write(data)
                break
            except Exception as e:  # noqa
                print("S2 retry", i, attempt, e, file=sys.stderr)
                time.sleep(10 * (attempt + 1))
        else:
            print("S2 batch failed, skipping", i, file=sys.stderr)
            continue
        time.sleep(3)
    res = json.load(open(f, encoding="utf-8"))
    for d, r in zip(chunk, res):
        if not r:
            continue
        if (r.get("externalIds") or {}).get("ArXiv"):
            s2[d] = r["externalIds"]["ArXiv"]
        if r.get("abstract"):
            s2abs[d] = re.sub(r"\s+", " ", r["abstract"]).strip()
print("S2 arXiv ids:", len(s2), "S2 abstracts:", len(s2abs))

pairs = {}
for w in works:
    oa, ss = w["openalex_arxiv_id"], s2.get(w["doi"], "")
    if oa or ss:
        pairs[w["openalex_id"]] = {"openalex_id": w["openalex_id"], "doi": w["doi"], "journal": w["journal"],
                                   "year": w["year"], "journal_title": w["title"],
                                   "arxiv_id": ss or oa,
                                   "source": "+".join(s for s, v in (("openalex", oa), ("semantic_scholar", ss)) if v)}
print("articles with a candidate arXiv id:", len(pairs))

# ---- resolve against arXiv (cached per batch of ids by qd_arxiv) ----
meta = qd_arxiv.fetch_ids([p["arxiv_id"] for p in pairs.values()])
norm = lambda s: re.sub(r"[^a-z0-9 ]", " ", s.lower()).split()
out = []
for p in pairs.values():
    m = meta.get(p["arxiv_id"])
    if not m:
        continue
    sim = difflib.SequenceMatcher(None, " ".join(norm(p["journal_title"])), " ".join(norm(m["title"]))).ratio()
    out.append({**p, "arxiv_title": m["title"], "arxiv_abstract": m["abstract"], "arxiv_submitted": m["submitted"],
                "arxiv_primary_category": m["primary_category"], "arxiv_categories": m["categories"],
                "title_similarity": round(sim, 3), "accepted": sim >= 0.60})
out.sort(key=lambda r: r["title_similarity"])
with open(os.path.join(CACHE, "journal_arxiv_matches.csv"), "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=list(out[0].keys())); w.writeheader(); w.writerows(out)
acc = [r for r in out if r["accepted"]]
print("resolved on arXiv:", len(out), "accepted (similarity >= 0.60):", len(acc))
for j in sorted({r["journal"] for r in acc}):
    print(" ", j, sum(1 for r in acc if r["journal"] == j))

# ---- best available abstract per article ----
arx = {r["openalex_id"]: r["arxiv_abstract"] for r in acc}
rows, src = [], {}
for w in works:
    a, s = w["abstract"], "openalex"
    if len(a) < 100:
        if len(s2abs.get(w["doi"], "")) >= 100:
            a, s = s2abs[w["doi"]], "semantic_scholar"
        elif len(arx.get(w["openalex_id"], "")) >= 100:
            a, s = arx[w["openalex_id"]], "arxiv_version"
        else:
            a, s = "", "none"
    rows.append({"openalex_id": w["openalex_id"], "abstract_best": a, "abstract_source": s})
    src[(w["journal"], s)] = src.get((w["journal"], s), 0) + 1
with open(os.path.join(CACHE, "journal_text.csv"), "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0].keys())); w.writeheader(); w.writerows(rows)
cov = [{"journal": j, "abstract_source": s, "n": n} for (j, s), n in sorted(src.items())]
with open(os.path.join(CACHE, "journal_abstract_sources.csv"), "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=["journal", "abstract_source", "n"]); w.writeheader(); w.writerows(cov)
for c in cov:
    print(c)
