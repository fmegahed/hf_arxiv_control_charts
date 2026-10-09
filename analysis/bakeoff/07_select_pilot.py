"""Bake-off step 7: choose the pilot papers for the queries in the spec.

Per track: 35 papers the new query brings in that the app has never held, and
15 papers the app already holds that the new query keeps. Papers in the
bake-off sample are left out, since the pipeline has already read them.
Uses the arXiv result cache of analysis/query_design (no model calls).

Run from the app directory:  python -I analysis/bakeoff/07_select_pilot.py
Output: analysis/bakeoff/local/pilot_sample.csv
"""
import csv
import json
import os
import random
import re
import sys

APP = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.join(APP, "analysis", "query_design"))
import qd_arxiv  # noqa: E402

N_NEW, N_KEPT, SEED = 35, 15, 20261009
spec = json.load(open(os.path.join(APP, "config", "factsheet_spec.json"), encoding="utf-8"))
local = os.path.join(APP, "analysis", "bakeoff", "local")
base = lambda x: re.sub(r"v\d+$", "", x)
sampled = {(r["track"], r["paper_id"]) for r in csv.DictReader(open(os.path.join(local, "sample.csv"), encoding="utf-8"))}

columns = ["id", "submitted", "updated", "title", "abstract", "primary_category", "categories", "pdf_url",
           "paper_id", "track", "pilot_group"]
rows = []
for track, info in spec["tracks"].items():
    held = {base(r["id"]) for r in csv.DictReader(open(os.path.join(APP, "data", info["metadata_csv"]), encoding="utf-8"))}
    total, entries = qd_arxiv.fetch(info["query"])
    entries = [e for e in entries if (track, e["id"]) not in sampled]
    new = sorted((e for e in entries if e["id"] not in held), key=lambda e: e["id"])
    kept = sorted((e for e in entries if e["id"] in held), key=lambda e: e["id"])
    rng = random.Random(f"{SEED}-{track}")
    picks = [(e, "new") for e in rng.sample(new, min(N_NEW, len(new)))] + \
            [(e, "kept") for e in rng.sample(kept, min(N_KEPT, len(kept)))]
    print(f"{track}: query returns {total}; new {len(new)}, kept {len(kept)}; picked {len(picks)}")
    for e, group in picks:
        rows.append({"id": e["version_id"], "submitted": e["submitted"], "updated": e["submitted"], "title": e["title"],
                     "abstract": e["abstract"], "primary_category": e["primary_category"], "categories": e["categories"],
                     "pdf_url": "https://arxiv.org/pdf/" + e["version_id"], "paper_id": e["id"], "track": track,
                     "pilot_group": group})

with open(os.path.join(local, "pilot_sample.csv"), "w", encoding="utf-8", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=columns)
    writer.writeheader()
    writer.writerows(rows)
print("wrote", len(rows), "rows")
