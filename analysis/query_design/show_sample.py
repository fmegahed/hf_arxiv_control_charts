"""Utility: print a sample CSV compactly for hand checking.
Usage: python -I show_sample.py <csv> [abstract_chars] [start] [end]
"""
import csv, sys
sys.stdout.reconfigure(encoding="utf-8")
csv.field_size_limit(10**9)
path = sys.argv[1]
n = int(sys.argv[2]) if len(sys.argv) > 2 else 400
rows = list(csv.DictReader(open(path, encoding="utf-8")))
a = int(sys.argv[3]) if len(sys.argv) > 3 else 0
b = int(sys.argv[4]) if len(sys.argv) > 4 else len(rows)
for i, r in enumerate(rows[a:b], start=a):
    flags = " ".join(f"{k}={v}" for k, v in r.items() if k not in ("title", "abstract") and len(str(v)) < 40)
    print(f"[{i}] {flags}\n    T: {r.get('title', '')[:200]}\n    A: {r.get('abstract', '')[:n]}")
