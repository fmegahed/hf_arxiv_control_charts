"""Step 21. Does a long query survive being sent as a GET request, the way the R package aRxiv sends it?

aRxiv 0.18 (the version in the app's renv.lock) calls httr::GET("http://export.arxiv.org/api/query",
query = list(search_query = ..., start = ..., max_results = ...)), so the whole query travels in the URL.
This probe pads a real query with OR clauses that match nothing until the URL reaches a target length,
sends it by GET to both the http and the https endpoint, and records the HTTP status and totalResults.
It also records the URL length of every candidate in qd_candidates_v2.py.

Output: cache/url_length_probe.csv, cache/url_length_probe.md
Run:    python -I 21_url_length_probe.py
"""
import os, sys, time, urllib.error, urllib.parse, urllib.request
import xml.etree.ElementTree as ET  # parses the arXiv API's own Atom feed
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import CACHE, write_csv
from qd_candidates_v2 import CANDIDATES_V2

OS_NS = "{http://a9.com/-/spec/opensearch/1.1/}totalResults"
SEED = '(ti:"control chart" OR abs:"control chart")'


def url_for(endpoint, q):
    return endpoint + "?" + urllib.parse.urlencode({"search_query": q, "start": 0, "max_results": 1})


def send(url):
    time.sleep(4)
    try:
        req = urllib.request.Request(url, headers={"User-Agent": "qe-arxiv-watch url length probe (mailto:fmegahed@miamioh.edu)"})
        with urllib.request.urlopen(req, timeout=120) as r:
            body = r.read()
            return r.status, int(ET.fromstring(body).find(OS_NS).text), r.geturl().split(":")[0]
    except urllib.error.HTTPError as e:
        return e.code, "", ""
    except Exception as e:  # noqa
        return type(e).__name__, "", ""


rows = []
for target in (1000, 2000, 3000, 4000, 4500, 5000, 5500, 6000, 8000, 12000, 16000):
    q, i = SEED, 0
    while len(url_for("https://export.arxiv.org/api/query", q)) < target:
        i += 1
        q = q[:-1] + f' OR ti:"zzqx{i:04d}nomatch")'
    for ep in ("https://export.arxiv.org/api/query", "http://export.arxiv.org/api/query"):
        u = url_for(ep, q)
        status, total, final = send(u)
        rows.append({"what": "padded probe", "endpoint": ep, "query_chars": len(q), "url_chars": len(u), "http_status": status,
                     "total_results": total, "final_scheme": final})
        print(rows[-1], flush=True)
for track, cands in CANDIDATES_V2.items():
    for name, q in cands.items():
        if not q:
            continue
        u = url_for("http://export.arxiv.org/api/query", q)
        status, total, final = send(u)
        rows.append({"what": f"{track} {name}", "endpoint": "http://export.arxiv.org/api/query", "query_chars": len(q),
                     "url_chars": len(u), "http_status": status, "total_results": total, "final_scheme": final})
        print(rows[-1], flush=True)
write_csv(os.path.join(CACHE, "url_length_probe.csv"), rows)
md = ["| request | endpoint | query characters | URL characters | HTTP status | totalResults | scheme after redirects |", "|---|---|---|---|---|---|---|"]
md += [f"| {r['what']} | {r['endpoint']} | {r['query_chars']} | {r['url_chars']} | {r['http_status']} | {r['total_results']} | {r['final_scheme']} |" for r in rows]
open(os.path.join(CACHE, "url_length_probe.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
