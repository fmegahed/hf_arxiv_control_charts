"""Step 1. Pull works for the reference journals (four original plus four added in the follow-up) from OpenAlex (2010 to 2026).

Raw API pages are cached under cache/openalex_raw/. Output:
  cache/journal_works.csv        one row per work
  cache/journal_coverage.csv     counts per journal (total, with abstract, with arXiv location)
Run:  python -I 01_fetch_openalex.py
"""
import csv, json, os, re, sys, time, urllib.parse, urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
CACHE = os.path.join(HERE, "cache")
RAW = os.path.join(CACHE, "openalex_raw")
os.makedirs(RAW, exist_ok=True)
MAILTO = "fmegahed@miamioh.edu"
JOURNALS = {  # ISSN -> expected name (verified against the source record)
    "0022-4065": "Journal of Quality Technology",
    "0040-1706": "Technometrics",
    "0898-2112": "Quality Engineering",
    "0748-8017": "Quality and Reliability Engineering International",
    # added in the follow-up analysis
    "0018-9529": "IEEE Transactions on Reliability",
    "2472-5854": "IISE Transactions",
    "0740-817X": "IIE Transactions",  # predecessor title of IISE Transactions
    "1524-1904": "Applied Stochastic Models in Business and Industry",
    "0167-9473": "Computational Statistics & Data Analysis",
}
SELECT = ",".join(["id", "doi", "title", "publication_year", "publication_date", "type",
                   "abstract_inverted_index", "keywords", "concepts", "topics",
                   "ids", "locations", "primary_location"])


def get(url, cache_file):
    if os.path.exists(cache_file):
        with open(cache_file, encoding="utf-8") as f:
            return json.load(f)
    for attempt in range(5):
        try:
            req = urllib.request.Request(url, headers={"User-Agent": f"qe-arxiv-watch (mailto:{MAILTO})"})
            with urllib.request.urlopen(req, timeout=60) as r:
                data = json.loads(r.read().decode("utf-8"))
            with open(cache_file, "w", encoding="utf-8") as f:
                json.dump(data, f)
            time.sleep(0.2)
            return data
        except Exception as e:  # noqa
            print("retry", attempt, e, file=sys.stderr)
            time.sleep(3 * (attempt + 1))
    raise RuntimeError("failed: " + url)


def abstract_from_index(inv):
    if not inv:
        return ""
    pos = []
    for w, ps in inv.items():
        for p in ps:
            pos.append((p, w))
    return " ".join(w for _, w in sorted(pos))


def arxiv_id_from(work):
    """Look for an arXiv id anywhere in locations / ids."""
    cands = []
    for loc in (work.get("locations") or []):
        for k in ("landing_page_url", "pdf_url"):
            u = loc.get(k) or ""
            if "arxiv.org" in u:
                cands.append(u)
    for v in (work.get("ids") or {}).values():
        if isinstance(v, str) and "arxiv" in v.lower():
            cands.append(v)
    for u in cands:
        m = re.search(r"arxiv\.org/(?:abs|pdf)/([a-z\-]+(?:\.[A-Z]{2})?/\d{7}|\d{4}\.\d{4,5})", u)
        if m:
            return m.group(1)
        m = re.search(r"arXiv\.(\d{4}\.\d{4,5})", u, flags=re.I)
        if m:
            return m.group(1)
    return ""


rows, cov = [], []
for issn, expected in JOURNALS.items():
    src = get(f"https://api.openalex.org/sources/issn:{issn}?mailto={MAILTO}",
              os.path.join(RAW, f"source_{issn}.json"))
    assert src["display_name"] == expected, (issn, src["display_name"])
    sid = src["id"].rsplit("/", 1)[1]
    cursor, page, n = "*", 0, 0
    while cursor:
        url = ("https://api.openalex.org/works?" + urllib.parse.urlencode({
            "filter": f"primary_location.source.id:{sid},publication_year:2010-2026",
            "select": SELECT, "per-page": 200, "cursor": cursor, "mailto": MAILTO}))
        data = get(url, os.path.join(RAW, f"works_{sid}_{page:03d}.json"))
        for w in data["results"]:
            n += 1
            kw = [k.get("display_name", "") for k in (w.get("keywords") or [])]
            con = [c.get("display_name", "") for c in (w.get("concepts") or []) if (c.get("score") or 0) >= 0.3]
            top = [t.get("display_name", "") for t in (w.get("topics") or [])]
            rows.append({
                "openalex_id": w["id"].rsplit("/", 1)[1],
                "journal": expected,
                "year": w.get("publication_year"),
                "type": w.get("type"),
                "doi": (w.get("doi") or "").replace("https://doi.org/", "").lower(),
                "title": re.sub(r"\s+", " ", w.get("title") or "").strip(),
                "abstract": re.sub(r"\s+", " ", abstract_from_index(w.get("abstract_inverted_index"))).strip(),
                "keywords": "|".join(kw),
                "concepts": "|".join(con),
                "topics": "|".join(top),
                "openalex_arxiv_id": arxiv_id_from(w),
            })
        cursor = data["meta"].get("next_cursor") if data["results"] else None
        page += 1
    print(expected, sid, "works fetched =", n)
    cov.append({"journal": expected, "issn": issn, "openalex_source_id": sid})

with open(os.path.join(CACHE, "journal_works_all.csv"), "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
    w.writeheader(); w.writerows(rows)

# Keep research items only: drop editorials, errata, book reviews, front matter.
DROP_TITLE = re.compile(
    r"^(editorial|erratum|errata|corrigendum|correction|book review|editor|from the editor|"
    r"issue information|cover|masthead|table of contents|referees|reviewers|acknowledg|announcement|"
    r"index|list of|in this issue|in memoriam|obituary|preface|foreword|letter to the editor|"
    r"comment|discussion|rejoinder|guest editorial|call for papers|contents|title page)\b", re.I)
keep = []
for r in rows:
    if r["type"] not in ("article", "review"):
        continue
    if not r["title"] or DROP_TITLE.search(r["title"]):
        continue
    keep.append(r)
with open(os.path.join(CACHE, "journal_works.csv"), "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
    w.writeheader(); w.writerows(keep)

for c in cov:
    a = [r for r in rows if r["journal"] == c["journal"]]
    k = [r for r in keep if r["journal"] == c["journal"]]
    c.update({
        "works_2010_2026": len(a),
        "kept_research_items": len(k),
        "kept_with_abstract": sum(1 for r in k if len(r["abstract"]) >= 100),
        "kept_with_keywords_or_topics": sum(1 for r in k if r["keywords"] or r["topics"]),
        "kept_with_openalex_arxiv_id": sum(1 for r in k if r["openalex_arxiv_id"]),
    })
with open(os.path.join(CACHE, "journal_coverage.csv"), "w", newline="", encoding="utf-8") as f:
    w = csv.DictWriter(f, fieldnames=list(cov[0].keys()))
    w.writeheader(); w.writerows(cov)
for c in cov:
    print(c)
