"""Transparent keyword rubric used as a stand-in for the Luna labels (which could not be
obtained in this run, see REPORT.md). Same three topics, same inclusion/exclusion intent as the
Luna rubric in 00_common.R, expressed as regular expressions.

Rule per topic: label TRUE when
    (a) the TITLE or the author/OpenAlex KEYWORDS contain at least one topic term, or
    (b) the ABSTRACT contains at least two DISTINCT topic terms,
and no exclusion pattern for that topic matches the title.
"""
import csv, re

csv.field_size_limit(10**9)

TERMS = {
    "reliability": [
        r"reliabilit", r"\bfailure", r"\blifetime", r"\blife[- ]test", r"\blife data", r"degradation",
        r"maintenance", r"warrant", r"remaining useful life", r"\bprognos", r"accelerated (life|degradation|test|stress)",
        r"\brepairable", r"\bcensor", r"weibull", r"hazard rate", r"fault tree", r"\bfmea\b", r"failure mode",
        r"burn[- ]in", r"stress[- ]strength", r"\bavailability\b", r"k[- ]out[- ]of[- ]n", r"\bshock model",
        r"mean time (to|between) failure", r"\bmttf\b", r"\bmtbf\b", r"replacement polic", r"competing risk",
        r"\bwear\b", r"condition[- ]based", r"reliability growth", r"survival signature",
    ],
    "doe": [
        r"design(s)? of experiment", r"experimental design", r"designed experiment", r"\bfactorial", r"response surface",
        r"screening (design|experiment)", r"split[- ]plot", r"supersaturated", r"optimal design", r"\b[adgi][- ]optimal",
        r"space[- ]filling", r"latin hypercube", r"computer experiment", r"orthogonal array", r"mixture (experiment|design)",
        r"robust parameter design", r"taguchi", r"definitive screening", r"central composite", r"minimum aberration",
        r"\balias", r"sequential design", r"\bblocked design|\bblocking\b", r"box[- ]behnken", r"plackett[- ]burman",
        r"\bdesign construction|construction of .*design", r"uniform design", r"maximin", r"design criteri",
        r"bayesian (experimental|optimal) design", r"experiments? with mixtures", r"a/b test", r"order[- ]of[- ]addition",
    ],
    "spm": [
        r"control chart", r"\bcusum\b", r"\bewma\b", r"statistical process (control|monitoring)", r"process monitoring",
        r"run length", r"\bphase i\b", r"\bphase ii\b", r"shewhart", r"profile monitoring", r"process capabilit",
        r"change[- ]?point detection", r"surveillance", r"hotelling", r"\bspc\b", r"\bspm\b", r"control limit",
        r"out[- ]of[- ]control", r"in[- ]control", r"monitoring scheme", r"charting", r"\bmonitoring (of )?(the )?(process|profile|mean|variance|dispersion|covariance)",
        r"fault detection", r"\bx-?bar chart|\bchart(s)? (for|with|based)", r"sequential (change|detection)",
    ],
}
# Title-level exclusions for the "other senses" of the keywords (mostly matter on arXiv, rarely in the journals).
EXCLUDE_TITLE = {
    "reliability": [r"inter[- ]?rater", r"test[- ]retest", r"cronbach", r"psychometric", r"questionnaire",
                    r"image degradation", r"reliab\w+ (of|in) (large )?language model", r"\bllm", r"neural network",
                    r"deep learning", r"reliable (prediction|inference|learning|estimation|uncertaint)",
                    r"book review", r"gau?ge r", r"measurement system"],
    "doe": [r"book review", r"potential energy surface"],
    "spm": [r"book review"],
}
COMPILED = {t: [re.compile(p, re.I) for p in ps] for t, ps in TERMS.items()}
COMPILED_EX = {t: [re.compile(p, re.I) for p in ps] for t, ps in EXCLUDE_TITLE.items()}


def classify(title, abstract="", keywords=""):
    """Return dict topic -> bool, plus the matched terms (for auditing)."""
    out, why = {}, []
    tk = f"{title} || {keywords}"
    for t in TERMS:
        strong = [p.pattern for p in COMPILED[t] if p.search(tk)]
        weak = [p.pattern for p in COMPILED[t] if p.search(abstract or "")]
        excl = any(p.search(title) for p in COMPILED_EX[t])
        out[t] = (bool(strong) or len(weak) >= 2) and not excl
        if out[t]:
            why.append(f"{t}:" + ",".join((strong or weak)[:3]))
    out["why"] = "; ".join(why)
    return out
