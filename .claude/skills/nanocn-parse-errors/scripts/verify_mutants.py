#!/usr/bin/env python3
"""Run each mutant through the parser and report what happened.

Usage:
    scripts/verify_mutants.py [--placeholders-only] [--parses-only] [files...]
"""

import argparse
import glob
import subprocess
import sys

PLACEHOLDER = "<YOUR SYNTAX ERROR MESSAGE HERE>"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--placeholders-only", action="store_true",
                        help="Only report [PH] entries")
    parser.add_argument("--parses-only", action="store_true",
                        help="Only report [OK] (stale mutant) entries")
    parser.add_argument("files", nargs="*",
                        help="Specific files (default: all in examples/errors/parsing/)")
    args = parser.parse_args()

    files = args.files
    if not files:
        files = sorted(glob.glob("examples/errors/parsing/*.cn") +
                       glob.glob("examples/errors/parsing/*.rcn"))

    for f in files:
        cmd = "check-refined" if f.endswith(".rcn") else "check"
        r = subprocess.run(["dune", "exec", "--no-build", "bin/main.exe", "--", cmd, f],
                          capture_output=True, text=True)
        out = r.stdout + r.stderr

        if "parse error" in out.lower() or "Parse error" in out:
            lines = out.strip().split("\n")
            msg_line = ""
            pos = "-"
            for i, line in enumerate(lines):
                if "Parse error" in line or "parse error" in line:
                    pos = line.split(":")[-2] if ":" in line else "-"
                if i > 0 and not line.startswith(" ") and "error" not in line.lower():
                    msg_line = line[:80]
                    break
            if not msg_line:
                msg_line = lines[-1][:80] if lines else "(empty)"

            flag = "PH" if PLACEHOLDER in out else "ERR"
        else:
            flag = "OK"
            pos = "-"
            msg_line = "(parses successfully)"

        if args.placeholders_only and flag != "PH":
            continue
        if args.parses_only and flag != "OK":
            continue

        print(f"[{flag}] {f:<50} pos={pos:<12} msg={msg_line}")


if __name__ == "__main__":
    main()
