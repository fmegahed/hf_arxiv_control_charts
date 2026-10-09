"""Step 17. Probe how the arXiv API tokenises and stems the candidate terms.

Each group compares the term as it would be written in a query with variants that differ only by a
hyphen, a stopword, a single letter, case, or a word ending. Equal counts mean the API does not
distinguish the two spellings (the result sets are the same size; for the pairs that matter the sets
were also compared in 14_term_tables.py through the literal-share column).

Output: cache/token_probes.csv, cache/token_probes.md
Run:    python -I 17_token_probes.py
"""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import CACHE, write_csv
import qd_arxiv

GROUPS = [
    # (what is tested, [queries]); the first query is the form used in a candidate query
    ("stemming: reliability / reliable", ['ti:reliability', 'ti:reliable', 'ti:reliably', 'ti:rely']),
    ("stemming: reliability phrases", ['ti:"reliability analysis"', 'ti:"reliable analysis"', 'ti:"reliability estimation"',
                                       'ti:"reliable estimation"', 'ti:"reliability assessment"', 'ti:"reliable assessment"',
                                       'ti:"reliability model"', 'ti:"reliable model"']),
    ("stemming: degradation", ['ti:degradation', 'ti:degrade', 'ti:degraded', 'ti:degrading']),
    ("stemming: maintenance", ['ti:maintenance', 'ti:maintain', 'ti:maintaining']),
    ("stemming: failure", ['ti:failure', 'ti:failures', 'ti:fail', 'ti:failing']),
    ("stemming: repairable", ['ti:repairable', 'ti:repair', 'ti:repairs', 'ti:"repairable system"', 'ti:"repair system"']),
    ("stemming: prognostics", ['ti:prognostics', 'ti:prognostic', 'ti:prognosis']),
    ("stemming: censored", ['ti:censored', 'ti:censor', 'ti:censoring', 'ti:censorship']),
    ("stemming: lifetime", ['ti:lifetime', 'ti:lifetimes', 'ti:"life time"']),
    ("stemming: warranty", ['ti:warranty', 'ti:warrant', 'ti:warranted']),
    ("stemming: life test", ['ti:"life test"', 'ti:"life testing"', 'ti:"life tests"', 'ti:"lifetime test"']),
    ("stemming: accelerated test", ['ti:"accelerated test"', 'ti:"accelerated testing"', 'ti:"acceleration test"', 'ti:"accelerating tests"']),
    ("stopword + stemming: burn-in", ['ti:"burn-in"', 'ti:"burn in"', 'ti:burn', 'ti:burning']),
    ("stopword: time to failure", ['ti:"time to failure"', 'ti:"time failure"', 'ti:"time of failure"']),
    ("stopwords and single letters: k-out-of-n", ['ti:"k-out-of-n"', 'ti:"k out of n"', 'ti:"k out n"', 'ti:"k n"']),
    ("hyphen: step-stress, stress-strength", ['ti:"step-stress"', 'ti:"step stress"', 'ti:"stress-strength"', 'ti:"stress strength"']),
    ("hyphen: reliability-based design", ['ti:"reliability-based design"', 'ti:"reliability based design"', 'ti:"reliable based design"']),
    ("stemming: experiment / experience", ['ti:"experimental design"', 'ti:"experiment design"', 'ti:"experience design"',
                                           'ti:"experiments design"', 'ti:experiment', 'ti:experience', 'ti:experimental']),
    ("stopword: design of experiment", ['ti:"design of experiment"', 'ti:"design of experiments"', 'ti:"design and experiments"',
                                        'ti:"design for experiments"', 'ti:"design experiment"', 'ti:"design experience"']),
    ("stemming: designed experiment", ['ti:"designed experiment"', 'ti:"design experiment"', 'ti:"designing experiments"']),
    ("plural: response surface, optimal design", ['ti:"response surface"', 'ti:"response surfaces"', 'ti:"optimal design"',
                                                  'ti:"optimal designs"', 'ti:"optimally designed"', 'ti:"optimization design"']),
    ("stemming: computer experiments", ['ti:"computer experiments"', 'ti:"computer experiment"', 'ti:"computational experiments"',
                                        'ti:"computing experience"', 'ti:"computed experiments"']),
    ("hyphen: split-plot, space-filling", ['ti:"split-plot"', 'ti:"split plot"', 'ti:splitplot', 'ti:"space-filling"',
                                           'ti:"space filling"', 'ti:"space filled"', 'ti:"space fill"']),
    ("stopword: order-of-addition", ['ti:"order-of-addition"', 'ti:"order of addition"', 'ti:"order addition"']),
    ("single letter: D-optimal, I-optimal, A-optimal", ['ti:"D-optimal"', 'ti:"D optimal"', 'ti:"I-optimal"', 'ti:"A-optimal"', 'ti:optimal']),
    ("slash and single letters: A/B testing", ['ti:"A/B testing"', 'ti:"A B testing"', 'ti:"AB testing"', 'ti:"B testing"',
                                               'ti:"A/B tests"', 'ti:"A/B test"', 'ti:testing']),
    ("hyphen: two-level, multi-fidelity", ['ti:"two-level"', 'ti:"two level"', 'ti:"two levels"', 'ti:"multi-fidelity"',
                                           'ti:"multi fidelity"', 'ti:multifidelity']),
    ("stemming: factorial, screening", ['ti:factorial', 'ti:factorials', 'ti:factor', 'ti:screening', 'ti:screen',
                                        'ti:"screening design"', 'ti:"screen design"', 'ti:"screened design"']),
    ("stemming: mixture experiment", ['ti:"mixture experiment"', 'ti:"mixture experiments"', 'ti:"mixed experiments"', 'ti:"mixture experience"']),
    ("stopword: in-control, out-of-control", ['abs:"in-control"', 'abs:"in control"', 'abs:control', 'abs:"out-of-control"',
                                              'abs:"out of control"', 'abs:"out control"']),
    ("hyphen: self-starting", ['abs:"self-starting"', 'abs:"self starting"', 'abs:"self start"', 'abs:starting']),
    ("roman numerals: Phase I, Phase II", ['abs:"Phase I"', 'abs:"Phase II"', 'abs:phase', 'abs:"Phase II monitoring"',
                                           'abs:"Phase monitoring"', 'abs:"Phase I monitoring"']),
    ("case: EWMA, CUSUM", ['abs:EWMA', 'abs:ewma', 'abs:CUSUM', 'abs:cusum', 'abs:CUSUMs']),
    ("stemming: monitoring phrases", ['abs:"monitoring scheme"', 'abs:"monitoring schemes"', 'abs:"monitor scheme"',
                                      'abs:"statistical monitoring"', 'abs:"statistically monitored"', 'abs:"statistical monitor"',
                                      'abs:"process monitoring"', 'abs:"process monitor"', 'abs:"processes monitored"']),
    ("plural: control chart, control limits", ['abs:"control chart"', 'abs:"control charts"', 'abs:"control charting"',
                                               'abs:"control limits"', 'abs:"control limit"', 'abs:"controlled limit"']),
    ("hyphen: change-point", ['abs:"change-point"', 'abs:"change point"', 'abs:changepoint', 'abs:"changing point"']),
    ("stemming: run length, profile monitoring", ['abs:"run length"', 'abs:"run lengths"', 'abs:"running length"',
                                                  'abs:"profile monitoring"', 'abs:"profiles monitored"', 'abs:"profiling monitor"']),
    ("stemming: dependability", ['abs:dependability', 'abs:dependable', 'abs:dependence', 'abs:depend']),
    ("field prefix: all versus ti/abs", ['all:"control chart"', '(ti:"control chart" OR abs:"control chart")']),
]

rows, md = [], ["| what is tested | query | results | same count as the first form |", "|---|---|---|---|"]
for what, qs in GROUPS:
    first = None
    for q in qs:
        n = qd_arxiv.count(q)
        first = n if first is None else first
        same = "(reference)" if q == qs[0] else ("yes" if n == first else "no")
        rows.append({"group": what, "query": q, "results": n, "same_as_first": same})
        md.append(f"| {what if q == qs[0] else ''} | `{q}` | {n} | {same} |")
    print(what, [r["results"] for r in rows if r["group"] == what], flush=True)
write_csv(os.path.join(CACHE, "token_probes.csv"), rows)
open(os.path.join(CACHE, "token_probes.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
