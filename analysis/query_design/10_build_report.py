"""Step 10. Assemble the report from the prose in report_followup.py (Part 1) and report_text.py (Part 2) plus the tables the other steps wrote to cache/.

Markers in the prose:
  {{file:cache/NAME.md}}     inserts that generated markdown fragment
  {{queries:TRACK}}          inserts the candidate queries of a track verbatim, with API result counts
  {{blocks}}                 inserts the named building blocks of the queries
Run:    python -I 10_build_report.py            prints the report to stdout
        python -I 10_build_report.py --write    writes REPORT.md and README.md next to this script
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import CACHE, HERE, read_csv
from qd_candidates import CANDIDATES, BLOCKS
from report_text import REPORT
from report_followup import FOLLOWUP, README_V2 as README

sizes = {(r["track"], r["candidate"]): r for r in read_csv(os.path.join(CACHE, "candidate_sizes.csv"))}


def queries(track):
    out = []
    for name, q in CANDIDATES[track].items():
        s = sizes[(track, name)]
        out.append(f"**{name}** ({s['api_total_results']} results reported by the API, {s['rows_parsed']} parsed, "
                   f"{len(q)} characters)\n\n```\n{q}\n```\n")
    return "\n".join(out)


def blocks():
    return "\n".join(f"- `{k}` = `{v}`" for k, v in BLOCKS.items())


# Part 1 (follow-up) comes first; Part 2 is the first-round report with its headings moved one level down
first_round = re.sub(r"^(#+) ", lambda m: m.group(1) + "# ", REPORT, flags=re.M)
txt = re.sub(r"\{\{file:([^}]+)\}\}", lambda m: open(os.path.join(HERE, m.group(1)), encoding="utf-8").read().strip(), FOLLOWUP + first_round)
txt = re.sub(r"\{\{queries:([^}]+)\}\}", lambda m: queries(m.group(1)), txt)
txt = txt.replace("{{blocks}}", blocks())
if "--write" in sys.argv:
    open(os.path.join(HERE, "REPORT.md"), "w", encoding="utf-8").write(txt)
    open(os.path.join(HERE, "README.md"), "w", encoding="utf-8").write(README)
    print("REPORT.md and README.md written:", len(txt.splitlines()), "report lines; em dashes in report:", txt.count("—"))
else:
    sys.stdout.reconfigure(encoding="utf-8")
    print(txt)
