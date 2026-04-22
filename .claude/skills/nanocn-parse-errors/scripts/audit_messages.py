#!/usr/bin/env python3
"""Comprehensive audit of parser error messages.

Reads every entry in parser.messages and verifies that:
- The prose message matches the grammar items and known suffix
- No stale syntax references remain (DepRes pair, old let core[a], etc.)
- Form-specific messages mention the right constructs

Usage:
    scripts/audit_messages.py [--fix-stale] [--verbose]

Options:
    --fix-stale   Automatically replace known stale patterns
    --verbose     Print OK entries too
"""

import argparse
import re
import sys
from pathlib import Path

MESSAGES = Path("lib/parser.messages")
PLACEHOLDER = "<YOUR SYNTAX ERROR MESSAGE HERE>"

# Stale patterns to detect, with their replacements
STALE_PATTERNS = [
    ("DepRes pair", None),
    ("pair form", None),
    ("(<log-binder>", None),
    ("(<res-binder>", None),
    ("sort-parameter name", None),
]

# Stale patterns with automatic replacements
STALE_FIXES = {
    "let core[a]": "let core[<lpat>]",
}


def parse_entries(text):
    """Parse parser.messages into structured entries."""
    entries = []
    parts = re.split(r"\n(?=[a-z_]+:)", text)
    for part in parts:
        state_m = re.search(r"Ends in an error in state: (\d+)", part)
        if not state_m:
            continue

        state = state_m.group(1)

        # Grammar items (## lines with ->)
        items = [l.strip("# ").strip()
                 for l in part.split("\n")
                 if "->" in l and l.strip().startswith("##")]

        # Known suffix
        suffix_m = re.search(r"known suffix.*?\n## (.*)", part)
        suffix = suffix_m.group(1).strip() if suffix_m else ""

        # Message (text after the ## block)
        msg_m = re.search(r"\n##\n\n(.+?)(?:\n\n[a-z_]|\Z)", part, re.DOTALL)
        msg = msg_m.group(1).strip() if msg_m else ""

        # Sentence
        sent_m = re.match(r"([a-z_]+:.+)", part)
        sentence = sent_m.group(1) if sent_m else ""

        entries.append({
            "state": state,
            "items": items,
            "suffix": suffix,
            "msg": msg,
            "sentence": sentence,
            "raw": part,
        })

    return entries


def check_entry(entry):
    """Check a single entry for issues. Returns list of problem strings."""
    msg = entry["msg"]
    items = entry["items"]
    suffix = entry["suffix"]
    problems = []

    if PLACEHOLDER in msg:
        return []  # Skip placeholders

    # Check stale patterns
    for pattern, _ in STALE_PATTERNS:
        if pattern in msg:
            problems.append(f"stale reference: '{pattern}'")

    for old_pat in STALE_FIXES:
        # Only flag if the pattern appears outside of backtick-quoted examples
        # e.g. "let core[a]" in prose is stale, but "`let core[a] x = ...`" is a valid example
        stripped = re.sub(r"`[^`]*`", "", msg)  # remove backtick-quoted text
        if old_pat in stripped:
            problems.append(f"stale syntax: '{old_pat}' (can auto-fix)")

    # Form-specific checks
    items_text = " ".join(items)

    if "rpat_res" in items_text:
        if not any(kw in msg.lower() for kw in [
            "resource pattern", "return", "take", "fail", "let", "case",
            "iftrue", "iffalse", "unfold", "annot", "after"
        ]):
            problems.append("rpat_res state but message doesn't mention resource patterns")

    if "cpat_inner" in items_text:
        if not any(kw in msg.lower() for kw in [
            "core pattern", "tuple", "variable", "identifier", "binder",
            "pattern", "after"
        ]):
            problems.append("cpat_inner state but message doesn't mention pattern/variable")

    if "lpat_inner" in items_text:
        if not any(kw in msg.lower() for kw in [
            "logical", "auto", "variable", "lpat", "after", "equality"
        ]):
            problems.append("lpat_inner state but message doesn't mention logical/auto")

    # Suffix consistency checks
    suffix_tokens = suffix.split()
    if "LET" in suffix_tokens and "RES" in suffix_tokens:
        if "let res" not in msg.lower() and "resource" not in msg.lower() and "after" not in msg.lower():
            problems.append("suffix has LET RES but message doesn't mention it")

    if "LET" in suffix_tokens and "CORE" in suffix_tokens:
        if "let core" not in msg.lower() and "core" not in msg.lower() and "after" not in msg.lower():
            problems.append("suffix has LET CORE but message doesn't mention it")

    return problems


def fix_stale(text):
    """Apply automatic stale-pattern fixes."""
    for old, new in STALE_FIXES.items():
        text = text.replace(old, new)
    return text


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--fix-stale", action="store_true",
                        help="Automatically replace known stale patterns")
    parser.add_argument("--verbose", action="store_true",
                        help="Print OK entries too")
    args = parser.parse_args()

    if not MESSAGES.exists():
        print(f"Error: {MESSAGES} not found (run from repo root)", file=sys.stderr)
        sys.exit(1)

    text = MESSAGES.read_text()
    entries = parse_entries(text)

    total = len(entries)
    placeholders = sum(1 for e in entries if PLACEHOLDER in e["msg"])
    issues = []

    for entry in entries:
        problems = check_entry(entry)
        if problems:
            issues.append((entry, problems))
        elif args.verbose and PLACEHOLDER not in entry["msg"]:
            print(f"  OK  State {entry['state']} ({entry['suffix']})")

    print(f"Total entries: {total}")
    print(f"PLACEHOLDERs:  {placeholders}")
    print(f"Real messages: {total - placeholders}")
    print(f"Issues:        {len(issues)}")

    if issues:
        print()
        for entry, problems in issues:
            print(f"State {entry['state']} (suffix: {entry['suffix']})")
            print(f"  Message: {entry['msg'][:100]}...")
            for p in problems:
                print(f"  ISSUE: {p}")
            print()

    if args.fix_stale and any("can auto-fix" in p for _, probs in issues for p in probs):
        fixed = fix_stale(text)
        MESSAGES.write_text(fixed)
        fixed_count = sum(1 for _, probs in issues
                         for p in probs if "can auto-fix" in p)
        print(f"Auto-fixed {fixed_count} stale patterns.")

    sys.exit(1 if issues else 0)


if __name__ == "__main__":
    main()
