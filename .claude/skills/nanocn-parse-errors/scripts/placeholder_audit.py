#!/usr/bin/env python3
"""Report PLACEHOLDER coverage in parser.messages.

Usage:
    scripts/placeholder_audit.py
"""

import re
import sys
from collections import Counter
from pathlib import Path

MESSAGES = Path("lib/parser.messages")
PLACEHOLDER = "<YOUR SYNTAX ERROR MESSAGE HERE>"


def classify_form(items_text):
    """Classify a state's grammar items into a form name."""
    checks = [
        (r"decl -> FUN ", "fun-decl"),
        (r"decl -> SORT ", "sort-decl"),
        (r"decl -> TYPE ", "type-decl"),
        (r"prog_eof -> list\(decl\) MAIN", "main-entry"),
        (r"branch -> pat", "fun-branch"),
        (r"case_branch -> pat", "case-branch"),
        (r"ctor_decl -> LABEL", "sort-ctor-decl"),
        (r"type_ctor_decl -> LABEL", "type-ctor-decl"),
        (r"seq_expr -> LET ", "let"),
        (r"seq_expr -> TAKE ", "take"),
        (r"seq_expr -> IF ", "if"),
        (r"seq_expr -> CASE ", "case"),
        (r"seq_expr -> ITER ", "iter"),
        (r"seq_expr -> RETURN ", "return"),
        (r"seq_expr -> FAIL", "fail"),
        (r"app_expr -> state_prim ", "state-prim"),
        (r"app_expr -> NOT_KW", "not"),
        (r"simple_expr -> LPAREN", "paren-expr"),
        (r"(add_expr|mul_expr|cmp_expr|and_expr|or_expr|eq_expr)", "expr-precedence"),
        (r"rprog_eof -> list\(rdecl\) MAIN", "rmain-entry"),
        (r"rdecl -> RFUN", "rfun-decl"),
        (r"pf_domain_entry", "pf-domain-entry"),
        (r"pf_entry", "pf-entry"),
        (r"crt_seq_expr -> LET CORE", "let-core"),
        (r"crt_seq_expr -> LET LOG", "let-log"),
        (r"crt_seq_expr -> LET RES", "let-res"),
        (r"crt_seq_expr -> LET ", "let-crt"),
        (r"rpat_res", "rpat-res"),
        (r"rpat_elem", "rpat-elem"),
        (r"lpat_inner", "lpat-inner"),
        (r"cpat_inner", "cpat-inner"),
        (r"rpf_expr|rpf_atom_expr", "rpf-expr"),
        (r"lpf_expr|lpf_atom_expr", "lpf-expr"),
        (r"spine_arg", "spine-arg"),
        (r"MAKE_TAKE", "make-take"),
        (r"MAKE_RET", "make-ret"),
        (r"UNFOLD", "unfold"),
        (r"OPEN_RET", "open-ret"),
    ]
    for pattern, form in checks:
        if re.search(pattern, items_text):
            return form
    return "other"


def main():
    if not MESSAGES.exists():
        print(f"Error: {MESSAGES} not found (run from repo root)", file=sys.stderr)
        sys.exit(1)

    text = MESSAGES.read_text()

    # Parse entries
    parts = re.split(r"\n(?=[a-z_]+:)", text)
    total = 0
    ph_count = 0
    form_total = Counter()
    form_ph = Counter()

    for part in parts:
        state_m = re.search(r"Ends in an error in state: (\d+)", part)
        if not state_m:
            continue
        total += 1

        items = [l.strip("# ").strip()
                 for l in part.split("\n")
                 if "->" in l and l.strip().startswith("##")]
        items_text = " ".join(items)
        form = classify_form(items_text)
        form_total[form] += 1

        is_ph = PLACEHOLDER in part
        if is_ph:
            ph_count += 1
            form_ph[form] += 1

    real = total - ph_count
    pct = 100.0 * real / total if total > 0 else 0.0

    print(f"=== PLACEHOLDER audit for {MESSAGES} ===")
    print(f"Total states:    {total}")
    print(f"Hand-crafted:    {real}")
    print(f"PLACEHOLDER:     {ph_count}")
    print(f"Coverage:        {pct:.1f}%")
    print()
    print(f"{'form':<25} {'total':>6} {'PH':>6}")
    print("-" * 39)
    for form in sorted(form_total):
        t = form_total[form]
        p = form_ph.get(form, 0)
        if p > 0 or True:  # show all
            print(f"{form:<25} {t:>6} {p:>6}")

    if ph_count > 0:
        print()
        print("=== PLACEHOLDER states ===")
        for part in parts:
            state_m = re.search(r"Ends in an error in state: (\d+)", part)
            if state_m and PLACEHOLDER in part:
                items = [l.strip("# ").strip()
                         for l in part.split("\n")
                         if "->" in l and l.strip().startswith("##")]
                items_text = " ".join(items)
                form = classify_form(items_text)
                print(f"  State {state_m.group(1)} ({form})")


if __name__ == "__main__":
    main()
