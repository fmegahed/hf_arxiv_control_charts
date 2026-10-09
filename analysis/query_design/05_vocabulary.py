"""Step 5. Distinguishing vocabulary and arXiv categories per track.

Groups per track:
  J = journal articles labelled on-topic (keyword rubric by default; pass 'luna' to use Luna labels)
  I = arXiv papers in the frozen corpus flagged in scope (silver label, gpt-5.2 on full PDF)
  O = arXiv papers in the frozen corpus flagged out of scope
Statistic: smoothed log-odds ratio of DOCUMENT frequency, (J+I) versus O, with a 0.5 pseudo-count,
divided by its standard error (z). Terms are 1 to 3 grams that do not start or end with a stopword.

Output (cache/): vocab_{track}_{field}.csv, categories_{track}.csv, phrase_coverage_{track}.csv
        vocab_tables.md  (markdown fragments used in REPORT.md)
Run:    python -I 05_vocabulary.py [kw|luna]
"""
import math, os, re, sys
from collections import Counter
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from qd_data import CACHE, TRACKS, TOPIC, load_corpus, load_journal, write_csv

SRC = sys.argv[1] if len(sys.argv) > 1 else "kw"
SUF = "_luna" if SRC == "luna" else ""   # Luna-labelled, eight-journal tables are written next to the first-round ones
STOP = set("""a an the of and or for in on to with by from at as is are was were be been being this that these those it its
we our their they them which who whose than then thus such can may might will would should could not no nor also both
between among into over under via using use used based new novel approach method methods paper propose proposed study
show shows shown results result problem problems when where while if however moreover furthermore therefore has have had
do does did more most some any each other one two three many much several often well due about against during within
without through up out only very i ii e g et al abstract article here there what how all but so because per vs versus
present presents consider considered considers provide provides given data model models analysis""".split())


def grams(text):
    toks = re.findall(r"[a-z0-9]+", text.lower().replace("-", " "))
    out = set()
    for n in (1, 2, 3):
        for i in range(len(toks) - n + 1):
            g = toks[i:i + n]
            if g[0] in STOP or g[-1] in STOP or any(len(t) < 2 for t in g) or all(t.isdigit() for t in g):
                continue
            out.add(" ".join(g))
    return out


def df(docs):
    c = Counter()
    for d in docs:
        c.update(grams(d))
    return c


def z_logodds(a, A, b, B):
    lo = math.log((a + .5) / (A - a + .5)) - math.log((b + .5) / (B - b + .5))
    se = math.sqrt(1 / (a + .5) + 1 / (A - a + .5) + 1 / (b + .5) + 1 / (B - b + .5))
    return lo, lo / se


PHRASES = {
    "reliability": ["reliability", "reliable", "degradation", "maintenance", "remaining useful life", "failure analysis",
                    "failure", "failures", "lifetime", "life test", "life testing", "accelerated life", "accelerated degradation",
                    "accelerated", "censored", "censoring", "weibull", "warranty", "repairable", "prognostics",
                    "predictive maintenance", "condition based maintenance", "reliability analysis", "system reliability",
                    "structural reliability", "software reliability", "reliability engineering", "failure time", "hazard",
                    "stress strength", "competing risks", "wiener process", "gamma process", "survival",
                    "neural", "deep", "learning", "language", "reinforcement learning", "conformal", "psychometric",
                    "inter rater", "large language"],
    "exp_design": ["experimental design", "experimental designs", "designed experiment", "design of experiment",
                   "design of experiments", "response surface", "supersaturated", "factorial", "fractional factorial",
                   "optimal design", "optimal designs", "optimal experimental design", "bayesian experimental design",
                   "bayesian optimal design", "space filling", "latin hypercube", "computer experiments", "orthogonal array",
                   "orthogonal arrays", "split plot", "screening", "screening design", "mixture experiment", "robust parameter design",
                   "sequential design", "active learning", "bayesian optimization", "a b testing", "a b tests", "minimum aberration",
                   "definitive screening", "blocking", "adaptive design", "sequential experimental design", "design criteria",
                   "optimality", "randomization", "causal"],
    "spc": ["control chart", "control charts", "process monitoring", "statistical process monitoring",
            "statistical process control", "process control", "cusum", "ewma", "run length", "average run length",
            "shewhart", "phase ii", "phase i", "profile monitoring", "change point", "change point detection",
            "changepoint", "sequential change", "anomaly detection", "fault detection", "surveillance", "monitoring",
            "online monitoring", "process capability", "in control", "out of control", "quickest detection", "spc"],
}

md = []
for track in TRACKS:
    topic = TOPIC[track]
    corp = load_corpus(track)
    J = [r for r in load_journal(SRC) if r[topic]]
    if SRC == "luna" and track != "reliability":  # IEEE Transactions on Reliability counts for the reliability track only
        J = [r for r in J if r["journal"] != "IEEE Transactions on Reliability"]
    I = [p for p in corp.values() if p["flag"] is True and p["has_metadata"]]
    O = [p for p in corp.values() if p["flag"] is False and p["has_metadata"]]
    md.append(f"\n### {track}: J = {len(J)} journal articles on-topic ({SRC} labels), I = {len(I)} arXiv in scope, O = {len(O)} arXiv out of scope\n")
    for field in ("title", "abstract"):
        dj, di, do = df(r[field] for r in J), df(p[field] for p in I), df(p[field] for p in O)
        nJ = len(J) if field == "title" else sum(1 for r in J if len(r["abstract"]) >= 100)
        rows = []
        for g in set(dj) | set(di) | set(do):
            a, b = dj[g] + di[g], do[g]
            if a + b < 5:
                continue
            lo, z = z_logodds(a, nJ + len(I), b, len(O))
            rows.append({"term": g, "n_words": len(g.split()), "df_journal": dj[g], "df_arxiv_in": di[g],
                         "df_arxiv_out": do[g], "log_odds": round(lo, 2), "z": round(z, 2)})
        rows.sort(key=lambda r: -r["z"])
        write_csv(os.path.join(CACHE, f"vocab_{track}_{field}{SUF}.csv"), rows)
        for side, sel in (("in scope side", rows), ("out of scope side", rows[::-1])):
            uni = [r for r in sel if r["n_words"] == 1][:20]
            multi = [r for r in sel if r["n_words"] > 1][:20]
            md.append(f"\n**{track}, {field}, {side}** (document counts out of J = {nJ}, I = {len(I)}, O = {len(O)})\n")
            md.append("| unigram | J | I | O | z | | phrase | J | I | O | z |\n|---|---|---|---|---|---|---|---|---|---|---|")
            for u, m in zip(uni, multi):
                md.append(f"| {u['term']} | {u['df_journal']} | {u['df_arxiv_in']} | {u['df_arxiv_out']} | {u['z']} | "
                          f"| {m['term']} | {m['df_journal']} | {m['df_arxiv_in']} | {m['df_arxiv_out']} | {m['z']} |")
    # phrase coverage: share of each group whose title / title-or-abstract contains the phrase
    norm = lambda s: " " + " ".join(re.findall(r"[a-z0-9]+", s.lower().replace("-", " "))) + " "
    Jt, It, Ot = [norm(r["title"]) for r in J], [norm(p["title"]) for p in I], [norm(p["title"]) for p in O]
    Ja, Ia, Oa = [norm(r["title"] + " " + r["abstract"]) for r in J], [norm(p["title"] + " " + p["abstract"]) for p in I], \
                 [norm(p["title"] + " " + p["abstract"]) for p in O]
    cov = []
    for ph in PHRASES[track]:
        k = " " + ph + " "
        cnt = lambda docs: sum(1 for d in docs if k in d)
        cov.append({"phrase": ph, "journal_title": cnt(Jt), "journal_title_or_abs": cnt(Ja), "n_journal": len(J),
                    "arxiv_in_title": cnt(It), "arxiv_in_title_or_abs": cnt(Ia), "n_in": len(I),
                    "arxiv_out_title": cnt(Ot), "arxiv_out_title_or_abs": cnt(Oa), "n_out": len(O)})
    write_csv(os.path.join(CACHE, f"phrase_coverage_{track}{SUF}.csv"), cov)
    md.append(f"\n**{track}, exact phrase coverage** (whole-word match after lower-casing and replacing hyphens by spaces; "
              f"J = {len(J)}, I = {len(I)}, O = {len(O)})\n")
    md.append("| phrase | J title | J title or abstract | I title | I title or abstract | O title | O title or abstract |\n|---|---|---|---|---|---|---|")
    for c in cov:
        md.append(f"| {c['phrase']} | {c['journal_title']} | {c['journal_title_or_abs']} | {c['arxiv_in_title']} | "
                  f"{c['arxiv_in_title_or_abs']} | {c['arxiv_out_title']} | {c['arxiv_out_title_or_abs']} |")
    # categories
    cats = []
    pc_i, pc_o = Counter(p["primary_category"] for p in I), Counter(p["primary_category"] for p in O)
    ac_i, ac_o = Counter(), Counter()
    for p in I: ac_i.update(set(p["categories"].split("|")))
    for p in O: ac_o.update(set(p["categories"].split("|")))
    for c in set(ac_i) | set(ac_o):
        cats.append({"category": c, "primary_in": pc_i[c], "primary_out": pc_o[c], "any_in": ac_i[c], "any_out": ac_o[c],
                     "n_in": len(I), "n_out": len(O),
                     "share_in_scope_any": round(ac_i[c] / (ac_i[c] + ac_o[c]), 2)})
    cats.sort(key=lambda r: -(r["any_in"] + r["any_out"]))
    write_csv(os.path.join(CACHE, f"categories_{track}{SUF}.csv"), cats)
    md.append(f"\n**{track}, arXiv categories** (I = {len(I)}, O = {len(O)}; 'any' counts a paper once per listed category)\n")
    md.append("| category | primary, in scope | primary, out of scope | any, in scope | any, out of scope | in-scope share (any) |\n|---|---|---|---|---|---|")
    for c in cats[:18]:
        md.append(f"| {c['category']} | {c['primary_in']} | {c['primary_out']} | {c['any_in']} | {c['any_out']} | {c['share_in_scope_any']} |")
    if track == "reliability":
        # stat-category combinations among the three categories the current query requires
        combo_i, combo_o = Counter(), Counter()
        for grp, cc in ((I, combo_i), (O, combo_o)):
            for p in grp:
                s = set(p["categories"].split("|")) & {"stat.ME", "stat.AP", "stat.ML"}
                cc["+".join(sorted(s)) or "(none)"] += 1
        md.append("\n**reliability, combination of the three required stat categories**\n")
        md.append("| stat categories listed | in scope | out of scope |\n|---|---|---|")
        for k in sorted(set(combo_i) | set(combo_o), key=lambda k: -(combo_i[k] + combo_o[k])):
            md.append(f"| {k} | {combo_i[k]} | {combo_o[k]} |")
open(os.path.join(CACHE, f"vocab_tables{SUF}.md"), "w", encoding="utf-8").write("\n".join(md) + "\n")
sys.stdout.reconfigure(encoding="utf-8")
print("\n".join(md))
