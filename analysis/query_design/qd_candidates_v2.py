"""Candidates compared in the follow-up scoring (step 20).

Per track: the current query, the first-round recommendation, and two queries assembled from the kept
terms (kept terms unrestricted where they passed unrestricted; and every kept term inside the statistics categories) of the per-term analysis (cache/terms/{track}_assembled.json, written by 14_term_tables.py).
For SPM the coordinator's revised query is included verbatim.
"""
import json, os
from qd_data import CACHE, CURRENT_QUERY
from qd_candidates import RELIABILITY, DOE, SPM

COORD_SPM = ('(((ti:"control chart" OR abs:"control chart") OR (ti:"statistical process monitoring" OR '
             'abs:"statistical process monitoring" OR ti:"statistical process control" OR abs:"statistical process control") OR '
             '((ti:"process monitoring" OR abs:"process monitoring") AND (cat:stat.ME OR cat:stat.AP))) OR ((abs:"control limits" OR '
             'abs:EWMA OR abs:Shewhart OR abs:"profile monitoring" OR abs:"Phase II monitoring" OR abs:"out-of-control" OR '
             'abs:"statistical monitoring" OR abs:"monitoring scheme" OR abs:"self-starting") AND (cat:stat.ME OR cat:stat.AP OR '
             'cat:stat.CO OR cat:stat.OT OR cat:stat.ML OR cat:math.ST)))')


def assembled(track, key="query"):
    f = os.path.join(CACHE, "terms", f"{track}_assembled.json")
    return json.load(open(f, encoding="utf-8"))[key] if os.path.exists(f) else None


def assembled_strict(track):
    """SPM only: the query assembled when papers flagged as change-detection theory are not counted on topic."""
    f = os.path.join(CACHE, "terms", f"{track}_assembled_strict.json")
    return json.load(open(f, encoding="utf-8"))["query"] if os.path.exists(f) else None


CANDIDATES_V2 = {
    "reliability": {"R0_current": CURRENT_QUERY["reliability"],
                    "R4_first_round": RELIABILITY["R4_context_plus_specific_phrases"],
                    "R6_kept_terms": assembled("reliability"),
                    "R7_kept_terms_stat_only": assembled("reliability", "query_all_restricted")},
    "exp_design": {"D0_current": CURRENT_QUERY["exp_design"],
                   "D2_first_round": DOE["D2_add_stat_title_vocab"],
                   "D5_kept_terms": assembled("exp_design"),
                   "D6_kept_terms_stat_only": assembled("exp_design", "query_all_restricted")},
    "spc": {"S0_current": CURRENT_QUERY["spc"],
            "S3_first_round": SPM["S3_names_plus_stat_monitoring_terms"],
            "S5_coordinator_revised": COORD_SPM,
            "S6_kept_terms": assembled("spc"),
            "S7_kept_terms_strict": assembled_strict("spc")},
}


def _final(track):
    f = os.path.join(CACHE, "v2", f"final_{track}.json")
    return json.load(open(f, encoding="utf-8"))["query"] if os.path.exists(f) else None


# kept terms plus the clauses of step 22 that recover missed gold (only when step 22 kept at least one)
for _t, _base, _name in (("reliability", "R6_kept_terms", "R8_kept_terms_plus_clauses"),
                         ("exp_design", "D5_kept_terms", "D7_kept_terms_plus_clauses"),
                         ("spc", "S6_kept_terms", "S8_kept_terms_plus_clauses")):
    _q = _final(_t)
    if _q and _q != CANDIDATES_V2[_t][_base]:
        CANDIDATES_V2[_t][_name] = _q
