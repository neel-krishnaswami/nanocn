#!/usr/bin/env python3
"""Verify all baseline .cn/.rcn files in examples/ still parse and typecheck.

Usage:
    scripts/verify_baselines.py [--typecheck]
"""

import argparse
import glob
import subprocess
import sys


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--typecheck", action="store_true",
                        help="Also check for type errors (not just parse errors)")
    args = parser.parse_args()

    files = sorted(glob.glob("examples/*.cn") + glob.glob("examples/*.rcn"))
    regressions = 0

    for f in files:
        cmd = "check-refined" if f.endswith(".rcn") else "check"
        r = subprocess.run(["dune", "exec", "--no-build", "bin/main.exe", "--", cmd, f],
                          capture_output=True, text=True)
        out = r.stdout + r.stderr

        if "parse error" in out.lower() or "Parse error" in out:
            print(f"PARSE-REGRESSION: {f}")
            for line in out.split("\n")[:3]:
                print(f"  {line}")
            regressions += 1
        elif args.typecheck and ("Type error" in out or "Error" in out):
            print(f"TYPECHECK-REGRESSION: {f}")
            for line in out.split("\n")[:3]:
                print(f"  {line}")
            regressions += 1

    if regressions == 0:
        what = " and typecheck" if args.typecheck else ""
        print(f"OK: all {len(files)} baselines parse{what}.")
    else:
        print(f"\n{regressions} regression(s) found.")

    sys.exit(1 if regressions > 0 else 0)


if __name__ == "__main__":
    main()
