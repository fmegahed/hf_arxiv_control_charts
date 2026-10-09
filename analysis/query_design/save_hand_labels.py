"""Utility used to store the analyst's hand labels for the newly-retrieved-paper samples.
Usage: python -I save_hand_labels.py <track> <string of 0/1>
The string has one character per hand-sample row of cache/new_sample_<track>.csv that is NOT yet in
hand_labels/new_sample_hand_<track>.csv, in id order (the order printed by `list` mode).
       python -I save_hand_labels.py <track> list     prints the rows still to be labelled
Labels are keyed by arXiv id and appended, so earlier labels are never overwritten.
"""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import CACHE, HERE, read_csv, write_csv
sys.stdout.reconfigure(encoding="utf-8")
track, bits = sys.argv[1], sys.argv[2].replace(" ", "").replace(",", "")
path = os.path.join(HERE, "hand_labels", f"new_sample_hand_{track}.csv")
done = read_csv(path) if os.path.exists(path) else []
have = {r["id"] for r in done}
rows = [r for r in read_csv(os.path.join(CACHE, f"new_sample_{track}.csv")) if r["in_hand_sample"] == "1" and r["id"] not in have]
if bits == "list":
    for r in rows:
        print(f"{r['id']}|{r['categories'][:18]}|{r['title'][:100]} || {r['abstract'][:150]}")
    sys.exit()
assert len(rows) == len(bits), (len(rows), len(bits))
done += [{"id": r["id"], "on_topic": b, "title": r["title"]} for r, b in zip(rows, bits)]
write_csv(path, sorted(done, key=lambda r: r["id"]), ["id", "on_topic", "title"])
print(track, len(bits), "labels added;", len(done), "in total,", sum(r["on_topic"] == "1" for r in done), "on topic")
