"""Step 16. Test the context-requirement and exclusion clauses by what they remove from the frozen corpus.

Every clause is judged against the silver flags: in-scope papers lost versus out-of-scope papers removed.
Membership is decided by the arXiv API (so stemming and phrase handling are the real ones), except for
category clauses, which match exactly and are computed locally.

Reliability
  (a) each current title word, alone and with the engineering-context requirement
  (b) each engineering-context term: papers it admits, and papers ONLY it admits
  (c) category clauses
DOE
  (a) each current title phrase
  (b) exclusion clauses (ANDNOT) by category and by abstract term
SPM
  (a) the process-mining exclusion on "process monitoring"
Output: cache/clauses_{track}.csv, cache/clause_tables.md
Run:    python -I 16_clause_tests.py
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import CACHE, CURRENT_QUERY, load_corpus, write_csv
import qd_arxiv
import qd_candidates as C

md = []


def ids(q):
    return {e["id"] for e in qd_arxiv.fetch(q)[1]}


def flags(track):
    c = load_corpus(track)
    return ({i for i, p in c.items() if p["flag"] is True}, {i for i, p in c.items() if p["flag"] is False}, c)


def table(title, note, rows, cols):
    md.append(f"\n**{title}**\n\n{note}\n")
    md.append("| " + " | ".join(cols) + " |\n|" + "---|" * len(cols))
    for r in rows:
        md.append("| " + " | ".join(str(r[c]) for c in cols) + " |")


# ------------------------------------------------------------------ reliability
IN, OUT, corp = flags("reliability")
cur = ids(CURRENT_QUERY["reliability"])
IN, OUT = IN & cur, OUT & cur
S3 = C.R_STAT3
ti_terms = ['ti:reliability', 'ti:degradation', 'ti:maintenance', 'ti:"remaining useful life"', 'ti:"failure analysis"']
rows = []
for t in ti_terms:
    a, b = ids(f"({t} AND {S3})"), ids(f"({t} AND {C.R_ENG} AND {S3})")
    rows.append({"clause": t, "in scope matched": len(a & IN), "out of scope matched": len(a & OUT),
                 "silver precision": round(len(a & IN) / max(1, len(a & (IN | OUT))), 2),
                 "in scope kept with context": len(b & IN), "out of scope kept with context": len(b & OUT),
                 "in scope lost by context": len((a - b) & IN), "out of scope removed by context": len((a - b) & OUT)})
table("Reliability (a): current title words, without and with the engineering-context requirement",
      f"Frozen corpus returned by the current query: {len(IN)} in scope, {len(OUT)} out of scope (silver flags).",
      rows, list(rows[0].keys()))
all_rows = [dict(r, group="title word") for r in rows]

eng_terms = re.findall(r'(?:abs|ti):(?:"[^"]+"|\S+?)(?= OR |\))', C.R_ENG)
adm = {e: ids(f"({C.R_TI} AND {e} AND {S3})") for e in eng_terms}
rows = []
for e in eng_terms:
    others = set().union(*[v for k, v in adm.items() if k != e])
    rows.append({"context term": e, "in scope admitted": len(adm[e] & IN), "out of scope admitted": len(adm[e] & OUT),
                 "admitted only by this term, in scope": len((adm[e] - others) & IN),
                 "admitted only by this term, out of scope": len((adm[e] - others) & OUT)})
rows.sort(key=lambda r: -r["in scope admitted"])
everything = set().union(*adm.values())
table("Reliability (b): each engineering-context term",
      f"All context terms together admit {len(everything & IN)} of {len(IN)} in-scope and {len(everything & OUT)} of "
      f"{len(OUT)} out-of-scope papers. 'Only' columns show what would change if that one term were deleted.",
      rows, list(rows[0].keys()))
all_rows += [dict(r, group="context term") for r in rows]

rows = []
combos = {}
for i in IN | OUT:
    s = "+".join(sorted(set(corp[i]["categories"].split("|")) & {"stat.ME", "stat.AP", "stat.ML"})) or "(none)"
    combos.setdefault(s, [0, 0, 0, 0])
    combos[s][0 if i in IN else 1] += 1
    if i in everything:
        combos[s][2 if i in IN else 3] += 1
for s, v in sorted(combos.items(), key=lambda kv: -(kv[1][0] + kv[1][1])):
    rows.append({"stat categories listed": s, "in scope": v[0], "out of scope": v[1],
                 "in scope with context": v[2], "out of scope with context": v[3]})
table("Reliability (c): category clause", "Which of the three required categories a paper lists, before and after the context requirement.",
      rows, list(rows[0].keys()))
write_csv(os.path.join(CACHE, "clauses_reliability.csv"), all_rows,
          sorted({k for r in all_rows for k in r}, key=lambda k: (k != "group", k)))

# ------------------------------------------------------------------ DOE
IN, OUT, corp = flags("exp_design")
cur = ids(CURRENT_QUERY["exp_design"])
IN, OUT = IN & cur, OUT & cur
phrases = ['ti:"experimental design"', 'ti:"designed experiment"', 'ti:"design of experiment"', 'ti:"response surface"',
           'ti:"supersaturated design"']
got = {p: ids(p) for p in phrases}
rows = []
for p in phrases:
    others = set().union(*[v for k, v in got.items() if k != p])
    rows.append({"clause": p, "in scope matched": len(got[p] & IN), "out of scope matched": len(got[p] & OUT),
                 "silver precision": round(len(got[p] & IN) / max(1, len(got[p] & (IN | OUT))), 2),
                 "only this phrase, in scope": len((got[p] - others) & IN),
                 "only this phrase, out of scope": len((got[p] - others) & OUT)})
table("DOE (a): current title phrases", f"Frozen corpus returned by the current query: {len(IN)} in scope, {len(OUT)} out of scope.",
      rows, list(rows[0].keys()))
all_rows = [dict(r, group="title phrase") for r in rows]

rows = []
catcount = {}
for i in IN | OUT:
    for c in set(corp[i]["categories"].split("|")):
        catcount.setdefault(c, [0, 0])[0 if i in IN else 1] += 1
for c, (a, b) in sorted(catcount.items(), key=lambda kv: -(kv[1][1] - 0.2 * kv[1][0])):
    if b >= 3:
        rows.append({"exclusion clause": f"ANDNOT cat:{c}", "in scope lost": a, "out of scope removed": b,
                     "removed per in-scope paper lost": round(b / a, 1) if a else "all gain"})
noise = {i for i in IN | OUT if set(corp[i]["categories"].split("|")) & {"cs.HC", "cs.RO", "physics.ed-ph"}}
rows.append({"exclusion clause": "ANDNOT (cat:cs.HC OR cat:cs.RO OR cat:physics.ed-ph)  [earlier D3]", "in scope lost": len(noise & IN),
             "out of scope removed": len(noise & OUT), "removed per in-scope paper lost": round(len(noise & OUT) / max(1, len(noise & IN)), 1)})
nostat = {i for i in IN | OUT if not any(c.startswith(("stat.", "math.ST", "math.OC", "math.NA", "cs.LG", "econ.EM", "cs.AI", "eess.SY", "q-bio.QM", "cs.CE", "cs.IT"))
                                         for c in corp[i]["categories"].split("|"))}
rows.append({"exclusion clause": "require any of stat.*, math.ST, math.OC, math.NA, cs.LG, cs.AI, econ.EM, eess.SY, q-bio.QM, cs.CE, cs.IT",
             "in scope lost": len(nostat & IN), "out of scope removed": len(nostat & OUT),
             "removed per in-scope paper lost": round(len(nostat & OUT) / max(1, len(nostat & IN)), 1)})
for t in ['abs:"user experience"', 'abs:prototype', 'abs:robot', 'abs:participants', 'abs:students', 'abs:hardware',
          'abs:"proof of concept"', 'abs:antenna', 'abs:detector', 'abs:telescope']:
    hit = ids(f"({CURRENT_QUERY['exp_design']} AND {t})")
    rows.append({"exclusion clause": f"ANDNOT {t}", "in scope lost": len(hit & IN), "out of scope removed": len(hit & OUT),
                 "removed per in-scope paper lost": round(len(hit & OUT) / len(hit & IN), 1) if hit & IN else "all gain"})
table("DOE (b): exclusion clauses", "Each clause applied alone to the current result set.", rows, list(rows[0].keys()))
all_rows += [dict(r, group="exclusion") for r in rows]
write_csv(os.path.join(CACHE, "clauses_exp_design.csv"), all_rows,
          sorted({k for r in all_rows for k in r}, key=lambda k: (k != "group", k)))

# ------------------------------------------------------------------ SPM
IN, OUT, corp = flags("spc")
pm = ids(C.S_PM)
bpm = ids(f"({C.S_PM} AND {C.S_BPM})")
pm_stat = ids(f"({C.S_PM} AND {C.S_STAT2})")
rows = [{"clause": '"process monitoring" in title or abstract, unrestricted', "papers": len(pm), "of which in frozen corpus, in scope": len(pm & IN),
         "in frozen corpus, out of scope": len(pm & OUT)},
        {"clause": "removed by the process-mining exclusion (S_BPM)", "papers": len(bpm), "of which in frozen corpus, in scope": len(bpm & IN),
         "in frozen corpus, out of scope": len(bpm & OUT)},
        {"clause": "kept when restricted to stat.ME or stat.AP", "papers": len(pm_stat), "of which in frozen corpus, in scope": len(pm_stat & IN),
         "in frozen corpus, out of scope": len(pm_stat & OUT)}]
table("SPM (a): the 'process monitoring' clause", "Most of these papers are outside the frozen corpus, so the silver flags say little; "
      "the on-topic shares are in the per-term table.", rows, list(rows[0].keys()))
write_csv(os.path.join(CACHE, "clauses_spc.csv"), rows)

open(os.path.join(CACHE, "clause_tables.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
sys.stdout.reconfigure(encoding="utf-8")
print("\n".join(md))
