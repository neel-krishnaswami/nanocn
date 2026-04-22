#!/usr/bin/env python3
"""Verify all example files parse cleanly with tree-sitter.

Detects when grammar.js is out of sync with parser.mly — any file that
the Menhir parser accepts but tree-sitter rejects indicates a grammar.js
update is needed.

Usage:
    scripts/verify_treesitter.py [files...]

Runs from the repo root. Requires npx and tree-sitter-cli.
"""

import argparse
import glob
import os
import subprocess
import sys

TS_DIR = os.path.join("scripts", "editor", "tree-sitter-nanocn")


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("files", nargs="*",
                        help="Files to check (default: all examples/*.cn and examples/*.rcn)")
    args = parser.parse_args()

    files = args.files
    if not files:
        files = sorted(glob.glob("examples/*.cn") + glob.glob("examples/*.rcn"))

    if not files:
        print("No files to check.", file=sys.stderr)
        sys.exit(1)

    # Ensure tree-sitter grammar is generated
    r = subprocess.run(["npx", "tree-sitter", "generate"],
                       capture_output=True, text=True, cwd=TS_DIR)
    if r.returncode != 0:
        print(f"tree-sitter generate failed:\n{r.stderr}", file=sys.stderr)
        sys.exit(1)

    errors = 0
    for f in files:
        abspath = os.path.abspath(f)
        r = subprocess.run(["npx", "tree-sitter", "parse", abspath],
                           capture_output=True, text=True, cwd=TS_DIR)
        output = r.stdout + r.stderr
        error_count = output.count("ERROR") + output.count("MISSING")

        if error_count > 0:
            print(f"FAIL: {f} ({error_count} error nodes)")
            # Show the error lines
            for line in output.split("\n"):
                if "ERROR" in line or "MISSING" in line:
                    print(f"  {line.strip()}")
            errors += 1
        else:
            print(f"  OK: {f}")

    print()
    if errors == 0:
        print(f"All {len(files)} files parse cleanly with tree-sitter.")
    else:
        print(f"{errors} file(s) have tree-sitter parse errors.")
        print("This likely means grammar.js needs updating to match parser.mly.")

    sys.exit(1 if errors > 0 else 0)


if __name__ == "__main__":
    main()
