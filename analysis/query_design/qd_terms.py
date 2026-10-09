"""Term lists and base queries for the term-by-term marginal analysis (steps 13 and 14).

For each track: BASE is a names-based query; CATS is the statistics-category restriction used for the
restricted variants; TERMS are the candidate words or phrases, each tested in the title and in the
abstract, with and without CATS.
"""
from qd_candidates import R_TI, R_ENG, R_STAT3, D_CUR


def OR(*xs):
    return "(" + " OR ".join(xs) + ")"


TRACK_TERMS = {
    "reliability": {
        # names-based base: the current title words, an engineering-context term, the current categories
        "base": f"({R_TI} AND {R_ENG} AND {R_STAT3})",
        "cats": ["stat.ME", "stat.AP", "stat.ML", "stat.CO", "stat.OT"],
        "terms": [
            # phrases of the earlier R4 recommendation (R_SPECIFIC)
            '"life test"', '"accelerated life"', '"accelerated degradation"', '"step-stress"', '"remaining useful life"',
            '"degradation data"', '"failure data"', '"lifetime prediction"', '"repairable system"', '"stress-strength"',
            'warranty', '"condition-based maintenance"', '"reliability analysis"', '"software reliability"',
            '"reliability growth"',
            # the rest of the journal vocabulary tested earlier (R_TP, R_AP) and context terms
            'weibull', '"failure time"', '"lifetime data"', '"lifetime distribution"', '"prognostics and health management"',
            '"system reliability"', '"structural reliability"', '"reliability engineering"', '"predictive maintenance"',
            '"reliability assessment"', '"degradation model"', '"degradation process"', '"degradation test"',
            'lifetime', 'repairable', 'prognostics', 'failure', 'hazard', '"rare event"', '"coherent system"',
            '"series system"', '"reliability-based design"',
            # further vocabulary surfaced by the journal tables
            '"accelerated test"', '"failure rate"', '"hazard rate"', '"time to failure"', '"failure probability"',
            '"failure mode"', '"competing risks"', '"progressive censoring"', 'censored', '"one-shot device"',
            '"k-out-of-n"', '"minimal repair"', '"preventive maintenance"', '"maintenance policy"',
            '"imperfect maintenance"', '"wiener process"', '"gamma process"', '"burn-in"', '"fault tree"',
            '"reliability estimation"', '"reliability model"', '"reliability demonstration"', '"reliability test"',
            '"life prediction"', '"useful life"', '"health management"', '"shock model"', '"survival signature"',
            'fatigue', '"reliability optimization"', '"redundancy allocation"', '"condition monitoring"',
        ],
    },
    "exp_design": {
        "base": D_CUR,
        "cats": ["stat.ME", "stat.AP", "stat.CO", "stat.ML", "math.ST"],
        "terms": [
            # terms of the earlier D2 recommendation
            '"factorial design"', '"fractional factorial"', '"split-plot"', '"space-filling design"', '"latin hypercube"',
            '"screening design"', '"screening experiment"', '"definitive screening"', '"robust parameter design"',
            '"mixture experiment"', '"order-of-addition"', '"optimal design"', '"computer experiments"',
            '"orthogonal array"', '"space-filling"', '"online experiments"', '"controlled experiments"', '"A/B testing"',
            # tested and dropped earlier, and abstract-level forms of the base phrases
            '"sequential design"', '"adaptive design"', '"Bayesian design"', '"active learning"',
            '"experimental design"', '"design of experiments"', '"designed experiment"', '"response surface"',
            '"supersaturated design"',
            # further vocabulary surfaced by the journal tables
            'factorial', 'supersaturated', '"D-optimal"', '"I-optimal"', '"A-optimal"', '"Bayesian optimal design"',
            '"minimum aberration"', '"block design"', '"blocked design"', '"central composite"', '"Box-Behnken"',
            '"Plackett-Burman"', 'Taguchi', '"design criterion"', '"main effects"', '"two-level"', 'foldover',
            '"uniform design"', 'maximin', '"choice experiments"', '"response surface methodology"',
            '"Bayesian optimization"', '"A/B tests"', 'rerandomization', '"optimal allocation"', '"design construction"',
            '"design region"', '"mixture design"', '"sequential experimentation"', '"strong orthogonal array"',
            '"sliced latin hypercube"', '"minimum energy design"', '"multi-fidelity"', '"emulator"',
        ],
    },
    "spc": {
        "base": ('((ti:"control chart" OR abs:"control chart") OR (ti:"statistical process monitoring" OR '
                 'abs:"statistical process monitoring" OR ti:"statistical process control" OR '
                 'abs:"statistical process control") OR ((ti:"process monitoring" OR abs:"process monitoring") '
                 'AND (cat:stat.ME OR cat:stat.AP)))'),
        "cats": ["stat.ME", "stat.AP", "stat.CO", "stat.OT", "stat.ML", "math.ST"],
        "terms": [
            # the coordinator's list (11_spm_term_marginals.py)
            '"average run length"', '"run length"', '"control limits"', 'EWMA', 'CUSUM', 'Shewhart', 'Hotelling',
            '"profile monitoring"', '"Phase II monitoring"', '"in-control"', '"out-of-control"',
            '"statistical monitoring"', '"monitoring scheme"', '"self-starting"', '"false alarm rate"',
            # added: wording used by gold articles that the revised query still misses, and journal vocabulary
            '"process monitoring"', '"online monitoring"', '"real-time monitoring"', '"change-point detection"',
            '"sequential change-point detection"', '"online change-point detection"', '"change detection"',
            '"data streams"', '"streaming data"', '"concept drift"', '"anomaly detection"', '"fault detection"',
            'surveillance', '"process capability"', '"Phase I"', '"quality control"', '"statistical quality control"',
            '"run rules"', '"time between events"', '"monitoring statistic"', '"monitoring procedure"',
            '"monitoring method"', '"process control"', '"multivariate monitoring"', '"network monitoring"',
            '"dynamic networks"', '"quickest detection"', '"quickest change detection"', '"control charts"',
            '"charting"', '"sequential monitoring"', '"change-point monitoring"', '"statistical surveillance"',
            '"process surveillance"', '"nonconforming"', '"point cloud"', '"functional data monitoring"',
            '"monitoring of"',
        ],
    },
}
