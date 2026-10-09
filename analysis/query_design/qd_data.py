"""Shared loaders for the frozen arXiv corpus and the labelled journal corpus."""
import csv, os, re

csv.field_size_limit(10**9)
HERE = os.path.dirname(os.path.abspath(__file__))
CACHE = os.path.join(HERE, "cache")
FROZEN = os.path.normpath(os.path.join(HERE, "..", "..", "data", "frozen", "v1"))
TRACKS = {"reliability": "is_reliability_paper", "exp_design": "is_exp_design_paper", "spc": "is_spc_paper"}
TOPIC = {"reliability": "reliability", "exp_design": "doe", "spc": "spm"}  # track -> rubric label
CURRENT_QUERY = {
    "spc": '(ti:"control chart" OR abs:"control chart")',
    "exp_design": '(ti:"experimental design" OR ti:"designed experiment" OR ti:"design of experiment" OR '
                  'ti:"response surface" OR ti:"supersaturated design")',
    "reliability": '((ti:reliability OR ti:degradation OR ti:maintenance OR ti:"remaining useful life" OR '
                   'ti:"failure analysis") AND (cat:stat.ME OR cat:stat.AP OR cat:stat.ML))',
}


def base_id(s):
    return re.sub(r"v\d+$", "", s.strip())


def version(s):
    m = re.search(r"v(\d+)$", s.strip())
    return int(m.group(1)) if m else 0


def read_csv(path):
    with open(path, encoding="utf-8") as f:
        return list(csv.DictReader(f))


def write_csv(path, rows, fields=None):
    if not rows and not fields:
        return
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields or list(rows[0].keys()), extrasaction="ignore")
        w.writeheader(); w.writerows(rows)


def load_corpus(track):
    """Frozen corpus at paper level (arXiv base id). The factsheet can hold several versions of a paper;
    the flag of the highest version is used. Papers whose flag is NA are returned with flag None."""
    flag_col = TRACKS[track]
    meta = {}
    for r in read_csv(os.path.join(FROZEN, f"{track}_arxiv_metadata.csv")):
        meta[base_id(r["id"])] = r
    best = {}
    for r in read_csv(os.path.join(FROZEN, f"{track}_factsheet.csv")):
        b = base_id(r["id"])
        if b not in best or version(r["id"]) > version(best[b]["id"]):
            best[b] = r
    out = {}
    for b, r in best.items():
        m = meta.get(b, {})
        out[b] = {"id": b, "flag": {"TRUE": True, "FALSE": False}.get(r[flag_col]),
                  "title": re.sub(r"\s+", " ", m.get("title", "")).strip(),
                  "abstract": re.sub(r"\s+", " ", m.get("abstract", "")).strip(),
                  "primary_category": m.get("primary_category", ""), "categories": m.get("categories", ""),
                  "submitted": m.get("submitted", "")[:10], "has_metadata": bool(m)}
    return out


def load_journal(label_source="kw"):
    """Journal articles with topic labels. label_source 'kw' = keyword rubric (cache/journal_labelled_kw.csv);
    'luna' = Luna labels (cache/journal_labelled.csv) when that file is complete."""
    if label_source == "luna":
        rows = read_csv(os.path.join(CACHE, "journal_labelled.csv"))
        for r in rows:
            for t in ("reliability", "doe", "spm"):
                r[t] = r[t].upper() == "TRUE"
    else:
        rows = read_csv(os.path.join(CACHE, "journal_labelled_kw.csv"))
        for r in rows:
            for t in ("reliability", "doe", "spm"):
                r[t] = r["kw_" + t] == "True"
    return rows


def luna_on_topic(row, track, strict=False):
    """On-topic decision from a Luna v2 label row for an arXiv paper.
    strict=True applies to SPM only: a paper that Luna also flags as change-detection theory is then NOT
    counted on topic. In the blind hand check Luna set both SPM and CHANGE_THEORY to TRUE for
    quickest-detection theory papers (8 of the 13 SPM disagreements), but it sets both flags for some
    CUSUM / Shiryaev-Roberts chart papers too, so the two counts bracket the truth:
    lenient = SPM label alone; strict = SPM label and no theory flag."""
    t = TOPIC[track]
    ok = row[t].upper() == "TRUE"
    if strict and track == "spc":
        ok = ok and row.get("change_theory", "").upper() != "TRUE"
    return ok
