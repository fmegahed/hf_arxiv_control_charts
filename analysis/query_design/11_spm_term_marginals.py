"""Marginal contribution of each candidate SPM abstract term.

For every term, restricted to statistics categories, find the papers it would
add beyond a base query (control chart, SPC/SPM names, restricted "process
monitoring"), and how many journal-matched (gold) papers it recovers.
Writes cache/spm_term_new_papers.json for screening by 12_spm_term_screen.R.

Run from analysis/query_design:  python -I 11_spm_term_marginals.py
"""
import csv
import json
import random
import re
import time
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET

UA = {"User-Agent": "QE-ArXiv-Watch/2.0 (mailto:fmegahed@miamioh.edu)"}
NS = {"a": "http://www.w3.org/2005/Atom", "x": "http://arxiv.org/schemas/atom"}
BASE = ('((ti:"control chart" OR abs:"control chart") OR (ti:"statistical process monitoring" OR '
        'abs:"statistical process monitoring" OR ti:"statistical process control" OR '
        'abs:"statistical process control") OR ((ti:"process monitoring" OR abs:"process monitoring") '
        'AND (cat:stat.ME OR cat:stat.AP)))')
CATS = "(cat:stat.ME OR cat:stat.AP OR cat:stat.CO OR cat:stat.OT OR cat:stat.ML OR cat:math.ST)"
TERMS = ['"average run length"', '"run length"', '"control limits"', "EWMA", "CUSUM", "Shewhart",
         '"Hotelling"', '"profile monitoring"', '"Phase II monitoring"', '"in-control"',
         '"out-of-control"', '"statistical monitoring"', '"monitoring scheme"', '"self-starting"',
         '"false alarm rate"']
SAMPLE = 30
MAX_RESULTS = 3000  # a term matching more than this is too broad to use anyway


def run(query):
    papers, start = {}, 0
    while True:
        url = "https://export.arxiv.org/api/query?" + urllib.parse.urlencode(
            {"search_query": query, "start": start, "max_results": 500})
        xml = None
        for attempt in range(5):
            try:
                xml = urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=180).read()
                break
            except Exception as err:  # arXiv returns occasional 5xx; back off and retry
                print(f"  retry {attempt + 1} after {type(err).__name__}")
                time.sleep(10 * (attempt + 1))
        if xml is None:
            raise RuntimeError("arXiv API kept failing for: " + query)
        root = ET.fromstring(xml)
        entries = root.findall("a:entry", NS)
        for e in entries:
            pid = re.sub(r"v\d+$", "", e.find("a:id", NS).text.split("/abs/")[-1])
            papers[pid] = {
                "id": pid,
                "title": " ".join(e.find("a:title", NS).text.split()),
                "abstract": " ".join(e.find("a:summary", NS).text.split()),
                "primary": e.find("x:primary_category", NS).get("term"),
                "categories": "|".join(c.get("term") for c in e.findall("a:category", NS)),
                "submitted": e.find("a:published", NS).text[:10],
            }
        total = int(root.find("{http://a9.com/-/spec/opensearch/1.1/}totalResults").text)
        start += 500
        time.sleep(3)
        if start >= min(total, MAX_RESULTS) or not entries:
            return papers


def main():
    random.seed(20261008)
    gold = list(csv.DictReader(open("cache/gold_spc.csv", encoding="utf-8")))
    key = [k for k in gold[0] if "arxiv" in k.lower()][0]
    gold_ids = {re.sub(r"v\d+$", "", r[key]) for r in gold}
    base = run(BASE)
    out = {"base": {"n": len(base), "gold": len(set(base) & gold_ids), "gold_total": len(gold_ids)},
           "terms": {}, "papers": {}}
    print(f"base: {len(base)} papers, gold {out['base']['gold']}/{len(gold_ids)}")
    for term in TERMS:
        got = run(f"(abs:{term} AND {CATS})")
        new = {pid: p for pid, p in got.items() if pid not in base}
        sample = random.sample(sorted(new), min(SAMPLE, len(new)))
        for pid in sample:
            out["papers"][pid] = new[pid]
        primaries = {}
        for p in new.values():
            primaries[p["primary"]] = primaries.get(p["primary"], 0) + 1
        out["terms"][term] = {
            "matches": len(got), "new": len(new),
            "gold_new": sorted((set(new) & gold_ids)), "sample": sample,
            "top_primary": sorted(primaries.items(), key=lambda kv: -kv[1])[:4],
        }
        print(f"{term}: {len(got)} matches, {len(new)} new, gold +{len(set(new) & gold_ids)}, "
              f"primary {out['terms'][term]['top_primary']}")
    json.dump(out, open("cache/spm_term_new_papers.json", "w", encoding="utf-8"), indent=1)


if __name__ == "__main__":
    main()
