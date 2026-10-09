"""Candidate arXiv API queries, built from named blocks so the report can print them verbatim.

Note on arXiv search behaviour (verified with 06_run_candidates.py probes, see REPORT.md):
terms are stemmed (ti:reliability returns the same set as ti:reliable), and quoted phrases ignore
stopwords and punctuation (ti:"design of experiment" returns the same set as ti:"design and experiments").
"""
from qd_data import CURRENT_QUERY


def OR(*xs):
    return "(" + " OR ".join(xs) + ")"


# ---------------- reliability ----------------
R_TI = OR('ti:reliability', 'ti:degradation', 'ti:maintenance', 'ti:"remaining useful life"', 'ti:"failure analysis"')
R_STAT2 = OR('cat:stat.ME', 'cat:stat.AP')
R_STAT3 = OR('cat:stat.ME', 'cat:stat.AP', 'cat:stat.ML')
R_STAT5 = OR('cat:stat.ME', 'cat:stat.AP', 'cat:stat.ML', 'cat:stat.CO', 'cat:stat.OT')
R_STAT7 = OR('cat:stat.ME', 'cat:stat.AP', 'cat:stat.ML', 'cat:stat.CO', 'cat:stat.OT', 'cat:math.ST', 'cat:eess.SY')
# engineering-context terms required in the abstract (plus the same phrases when they appear in the title)
R_ENG = OR('abs:failure', 'ti:failure', 'abs:lifetime', 'abs:maintenance', 'abs:"remaining useful life"',
           'abs:"reliability analysis"', 'abs:"system reliability"', 'abs:"structural reliability"',
           'abs:"reliability engineering"', 'abs:"reliability assessment"', 'abs:"reliability growth"',
           'abs:"reliability-based design"', 'abs:weibull', 'abs:prognostics', 'abs:repairable', 'abs:"life test"',
           'abs:hazard', 'abs:"stress-strength"', 'abs:"rare event"', 'abs:"coherent system"', 'abs:"series system"',
           'abs:"degradation model"', 'abs:"degradation data"', 'abs:"degradation process"', 'abs:"degradation test"',
           'ti:"reliability analysis"', 'ti:"system reliability"', 'ti:"structural reliability"',
           'ti:"reliability assessment"', 'ti:"reliability engineering"', 'ti:"degradation model"',
           'ti:"degradation data"', 'ti:"degradation test"', 'ti:"stress-strength"')
# journal vocabulary: title words and title-or-abstract phrases
R_TP = OR('ti:weibull', 'ti:"life test"', 'ti:"accelerated life"', 'ti:"accelerated degradation"', 'ti:warranty',
          'ti:"repairable system"', 'ti:"failure time"', 'ti:"failure data"', 'ti:"step-stress"', 'ti:"stress-strength"',
          'ti:"lifetime data"', 'ti:"lifetime distribution"', 'ti:"lifetime prediction"',
          'ti:"prognostics and health management"')
R_AP = OR('abs:"reliability analysis"', 'abs:"system reliability"', 'abs:"structural reliability"',
          'abs:"software reliability"', 'abs:"reliability engineering"', 'abs:"remaining useful life"',
          'abs:"accelerated life"', 'abs:"accelerated degradation"', 'abs:"predictive maintenance"',
          'abs:"condition-based maintenance"', 'abs:"repairable system"', 'abs:"lifetime data"',
          'abs:"degradation data"', 'abs:"life test"', 'abs:"stress-strength"')

# the subset of the journal vocabulary that was most specific in the hand-checked sample
R_SPECIFIC = OR('abs:"life test"', 'abs:"accelerated life"', 'abs:"accelerated degradation"', 'ti:"step-stress"',
                'abs:"remaining useful life"', 'abs:"degradation data"', 'ti:"failure data"', 'ti:"lifetime prediction"',
                'abs:"repairable system"', 'abs:"stress-strength"', 'ti:warranty', 'abs:"condition-based maintenance"',
                'abs:"reliability analysis"', 'abs:"software reliability"', 'abs:"reliability growth"')

RELIABILITY = {
    "R0_current": CURRENT_QUERY["reliability"],
    "R1_drop_statML": f"({R_TI} AND {R_STAT2})",
    "R2_statML_needs_context": f"({R_TI} AND ({R_STAT2} OR (cat:stat.ML AND {R_ENG})))",
    "R3_context_required": f"({R_TI} AND {R_ENG} AND {R_STAT3})",
    "R4_context_plus_specific_phrases": f"(({R_TI} AND {R_ENG} AND {R_STAT3}) OR ({R_SPECIFIC} AND {R_STAT5}))",
    "R5_broad": f"(({R_TI} AND ({R_STAT2} OR (cat:stat.ML AND {R_ENG}))) OR (({R_TP} OR {R_AP}) AND {R_STAT7}))",
}

# ---------------- DOE ----------------
D_CUR = CURRENT_QUERY["exp_design"]
D_STAT = OR('cat:stat.ME', 'cat:stat.AP', 'cat:stat.CO', 'cat:stat.ML', 'cat:math.ST')
D_STAT_CORE = OR('cat:stat.ME', 'cat:stat.AP', 'cat:stat.CO', 'cat:math.ST')
# unambiguous design vocabulary from the journals (title, no category restriction)
D_TP = OR('ti:"factorial design"', 'ti:"fractional factorial"', 'ti:"split-plot"', 'ti:"space-filling design"',
          'ti:"latin hypercube"', 'ti:"screening design"', 'ti:"screening experiment"', 'ti:"definitive screening"',
          'ti:"robust parameter design"', 'ti:"mixture experiment"', 'ti:"order-of-addition"')
# ambiguous title vocabulary, only inside statistics categories
D_TA = OR('ti:"optimal design"', 'ti:"computer experiments"', 'ti:"orthogonal array"', 'ti:"space-filling"',
          'ti:"online experiments"', 'ti:"controlled experiments"', 'ti:"A/B testing"')
# broader title vocabulary (brings clinical-trial designs), used only in the broad candidate
D_TB = OR('ti:"sequential design"', 'ti:"adaptive design"', 'ti:"Bayesian design"')
D_AP = OR('abs:"experimental design"', 'abs:"design of experiments"', 'abs:"designed experiment"', 'abs:"optimal design"',
          'abs:"response surface"', 'abs:"computer experiments"', 'abs:"factorial design"', 'abs:"space-filling"')
D_NOISE = OR('cat:cs.HC', 'cat:cs.RO', 'cat:physics.ed-ph')

DOE = {
    "D0_current": D_CUR,
    "D1_add_title_vocab": f"({D_CUR} OR {D_TP})",
    "D2_add_stat_title_vocab": f"({D_CUR} OR {D_TP} OR ({D_TA} AND {D_STAT}))",
    "D3_D2_minus_noisy_cats": f"(({D_CUR} OR {D_TP} OR ({D_TA} AND {D_STAT})) ANDNOT {D_NOISE})",
    "D4_add_stat_abstracts": f"({D_CUR} OR {D_TP} OR (({D_TA} OR {D_TB}) AND {D_STAT}) OR ({D_AP} AND {D_STAT_CORE}))",
}

# ---------------- SPM ----------------
S_CUR = CURRENT_QUERY["spc"]
S_STAT = OR('cat:stat.ME', 'cat:stat.AP', 'cat:stat.CO', 'cat:stat.OT', 'cat:stat.ML', 'cat:math.ST')
S_NAMES = OR('ti:"statistical process monitoring"', 'abs:"statistical process monitoring"',
             'ti:"statistical process control"', 'abs:"statistical process control"')
S_PM = OR('ti:"process monitoring"', 'abs:"process monitoring"')
S_STAT2 = OR('cat:stat.ME', 'cat:stat.AP')
# monitoring vocabulary that was specific in the hand-checked sample
S_TOOLS = OR('abs:"average run length"', 'abs:"profile monitoring"', 'abs:"control limits"', 'abs:EWMA',
             'abs:"Phase II monitoring"')
S_CPD = OR('abs:"sequential change-point detection"', 'abs:"online change-point detection"',
           'abs:"sequential change detection"', 'abs:"quickest change detection"', 'abs:"quickest detection"')
# "process monitoring" is also the name of a process-mining task (predictive / prescriptive business process monitoring)
S_BPM = OR('abs:"business process"', 'abs:"process mining"', 'abs:"event log"', 'ti:"predictive process monitoring"',
           'ti:"prescriptive process monitoring"')

SPM = {
    "S0_current": S_CUR,
    "S1_add_SPC_SPM_names": f"({S_CUR} OR {S_NAMES})",
    "S2_add_process_monitoring": f"({S_CUR} OR {S_NAMES} OR {S_PM})",
    "S2x_process_monitoring_minus_process_mining": f"({S_CUR} OR {S_NAMES} OR ({S_PM} ANDNOT {S_BPM}))",
    "S3_names_plus_stat_monitoring_terms": f"({S_CUR} OR {S_NAMES} OR ({S_PM} AND {S_STAT2}) OR ({S_TOOLS} AND {S_STAT}))",
    "S4_plus_sequential_change_detection": f"({S_CUR} OR {S_NAMES} OR ({S_PM} AND {S_STAT2}) OR (({S_TOOLS} OR {S_CPD}) AND {S_STAT}))",
}

CANDIDATES = {"reliability": RELIABILITY, "exp_design": DOE, "spc": SPM}
BLOCKS = {"R_TI": R_TI, "R_ENG": R_ENG, "R_TP": R_TP, "R_AP": R_AP, "R_SPECIFIC": R_SPECIFIC, "D_TP": D_TP, "D_TA": D_TA, "D_TB": D_TB, "D_AP": D_AP,
          "S_NAMES": S_NAMES, "S_PM": S_PM, "S_TOOLS": S_TOOLS, "S_CPD": S_CPD, "S_BPM": S_BPM}
