#!/usr/bin/env python3
"""Compare parser.messages state set against a fresh menhir --list-errors.

Usage:
    scripts/diff_states.py
"""

import re
import subprocess
import sys
import tempfile
from pathlib import Path

MESSAGES = Path("lib/parser.messages")
GRAMMAR = Path("lib/parser.mly")


def extract_states(text):
    return sorted(set(re.findall(r"Ends in an error in state: (\d+)", text)), key=int)


def main():
    if not MESSAGES.exists() or not GRAMMAR.exists():
        print("Error: run from repo root", file=sys.stderr)
        sys.exit(1)

    msg_text = MESSAGES.read_text()
    r = subprocess.run(["menhir", "--list-errors", str(GRAMMAR)],
                       capture_output=True, text=True)
    if r.returncode != 0:
        print(f"menhir --list-errors failed:\n{r.stderr}", file=sys.stderr)
        sys.exit(1)
    fresh_text = r.stdout

    msg_states = set(extract_states(msg_text))
    fresh_states = set(extract_states(fresh_text))

    removed = sorted(msg_states - fresh_states, key=int)
    new = sorted(fresh_states - msg_states, key=int)

    print(f"Current {MESSAGES}: {len(msg_states)} states")
    print(f"Fresh  {GRAMMAR}    : {len(fresh_states)} states")
    print()

    if removed:
        print(f"=== REMOVED states (in {MESSAGES} but not in grammar) ===")
        for i in range(0, len(removed), 10):
            print(" ".join(removed[i:i+10]))
        print()

    if new:
        print(f"=== NEW states (in grammar but not in {MESSAGES}) ===")
        for i in range(0, len(new), 10):
            print(" ".join(new[i:i+10]))
        print()

    if not removed and not new:
        print("State sets match — no grammar drift detected.")


if __name__ == "__main__":
    main()
