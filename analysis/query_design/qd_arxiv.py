"""arXiv API client with on-disk caching and the 3-second rate limit.

fetch(query)  -> (total_results, list of entry dicts); pages of 1000, cached as raw Atom XML under
                 cache/arxiv_queries/<sha1 of query>/
count(query)  -> totalResults only (max_results=1), cached.
CLI: python -I qd_arxiv.py count "<query>" ["<query>" ...]
"""
import hashlib, json, os, re, sys, time, urllib.parse, urllib.request
import xml.etree.ElementTree as ET  # input is the arXiv API's own Atom feed

HERE = os.path.dirname(os.path.abspath(__file__))
QDIR = os.path.join(HERE, "cache", "arxiv_queries")
os.makedirs(QDIR, exist_ok=True)
NS = {"a": "http://www.w3.org/2005/Atom", "x": "http://arxiv.org/schemas/atom",
      "o": "http://a9.com/-/spec/opensearch/1.1/"}
API = "https://export.arxiv.org/api/query"  # http now answers 301 to https
PAGE = 1000
_last = [0.0]


def _get(params, path, expect_entries=True):
    if os.path.exists(path):
        return open(path, "rb").read()
    url = API + "?" + urllib.parse.urlencode(params)
    for attempt in range(14):
        wait = 4.0 - (time.time() - _last[0])
        if wait > 0:
            time.sleep(wait)
        try:
            req = urllib.request.Request(url, headers={"User-Agent": "qe-arxiv-watch query design (mailto:fmegahed@miamioh.edu)"})
            with urllib.request.urlopen(req, timeout=180) as r:
                body = r.read()
            _last[0] = time.time()
            root = ET.fromstring(body)
            total = int(root.find("o:totalResults", NS).text)
            n = len(root.findall("a:entry", NS))
            # arXiv sometimes returns an empty page for a valid request; retry in that case
            if expect_entries and total > int(params.get("start", 0)) and n == 0:
                raise RuntimeError("empty page")
            open(path, "wb").write(body)
            return body
        except Exception as e:  # noqa
            _last[0] = time.time()
            print(f"  arXiv retry {attempt}: {e}", file=sys.stderr)
            # HTTP 429 means the API is throttling this address: back off for much longer
            time.sleep((45 if '429' in str(e) else 5) * (attempt + 1))
    raise RuntimeError("arXiv request failed: " + url)


def _dir(query):
    d = os.path.join(QDIR, hashlib.sha1(query.encode()).hexdigest()[:16])
    os.makedirs(d, exist_ok=True)
    qf = os.path.join(d, "query.txt")
    if not os.path.exists(qf):
        open(qf, "w", encoding="utf-8").write(query)
    return d


def count(query):
    body = _get({"search_query": query, "start": 0, "max_results": 1}, os.path.join(_dir(query), "count.xml"), False)
    return int(ET.fromstring(body).find("o:totalResults", NS).text)


def parse(body):
    out = []
    for e in ET.fromstring(body).findall("a:entry", NS):
        full = e.find("a:id", NS).text.rsplit("/abs/", 1)[1]
        pc = e.find("x:primary_category", NS)
        out.append({"id": re.sub(r"v\d+$", "", full), "version_id": full,
                    "submitted": (e.find("a:published", NS).text or "")[:10],
                    "title": re.sub(r"\s+", " ", e.find("a:title", NS).text or "").strip(),
                    "abstract": re.sub(r"\s+", " ", e.find("a:summary", NS).text or "").strip(),
                    "primary_category": pc.get("term") if pc is not None else "",
                    "categories": "|".join(c.get("term") for c in e.findall("a:category", NS))})
    return out


def fetch(query, limit=30000):
    d = _dir(query)
    total = count(query)
    entries, start = {}, 0
    while start < min(total, limit):
        body = _get({"search_query": query, "start": start, "max_results": PAGE,
                     "sortBy": "submittedDate", "sortOrder": "ascending"}, os.path.join(d, f"page_{start:06d}.xml"))
        for e in parse(body):
            entries[e["id"]] = e
        start += PAGE
    return total, list(entries.values())


def fetch_ids(ids):
    """Metadata for explicit arXiv ids (id_list), cached per batch of 100."""
    out = {}
    ids = sorted(set(ids))
    for i in range(0, len(ids), 100):
        chunk = ids[i:i + 100]
        d = _dir("id_list:" + ",".join(chunk))
        body = _get({"id_list": ",".join(chunk), "max_results": 100}, os.path.join(d, "page.xml"), False)
        for e in parse(body):
            out[e["id"]] = e
    return out


if __name__ == "__main__":
    if sys.argv[1] == "count":
        for q in sys.argv[2:]:
            print(count(q), "|", q)
