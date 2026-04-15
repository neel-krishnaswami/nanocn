#!/bin/bash
# Run each mutant through the nanocn parser and report what happened.
#
# Usage:
#   scripts/verify_mutants.sh                      # all mutants
#   scripts/verify_mutants.sh file1.cn file2.cn    # specific files
#   scripts/verify_mutants.sh --placeholders-only  # only report [PH]
#   scripts/verify_mutants.sh --parses-only        # only report [OK] (stale mutants)
#
# Output per file (one line):
#   [PH|OK|ERR] <filename>  pos=<line:col>  msg=<first 80 chars of message>
#
# [PH] — parser reports a parse error whose message still contains "PLACEHOLDER"
# [OK] — the mutant now parses successfully (likely stale after a grammar change)
# [ERR] — a non-PLACEHOLDER parse error (the good case)
#
# Run from the repo root. Assumes `dune build` has succeeded.

set -e

PHONLY=0
OKONLY=0
FILES=()

while [ $# -gt 0 ]; do
  case "$1" in
    --placeholders-only) PHONLY=1; shift ;;
    --parses-only) OKONLY=1; shift ;;
    --help|-h)
      sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
      exit 0 ;;
    --*) echo "unknown flag: $1"; exit 1 ;;
    *) FILES+=("$1"); shift ;;
  esac
done

if [ ${#FILES[@]} -eq 0 ]; then
  for f in examples/errors/parsing/*.cn examples/errors/parsing/*.rcn; do
    [ -f "$f" ] && FILES+=("$f")
  done
fi

for f in "${FILES[@]}"; do
  base=$(basename "$f")
  if [[ "$f" == *.rcn ]]; then cmd="check-refined"; else cmd="check"; fi
  out=$(dune exec --no-build nanocn -- "$cmd" "$f" 2>&1 || true)
  if echo "$out" | grep -q "parse error"; then
    msg=$(echo "$out" | grep -A1 "parse error" | head -2 | tail -1 | cut -c1-80)
    pos=$(echo "$out" | grep "parse error" | head -1 | sed 's|.*/||;s/: parse error.*//;s/^[^:]*://')
    if echo "$msg" | grep -q "^PLACEHOLDER"; then
      flag="PH"
    else
      flag="ERR"
    fi
  else
    flag="OK"
    pos="-"
    msg="(parses successfully)"
  fi
  if [ "$PHONLY" = "1" ] && [ "$flag" != "PH" ]; then continue; fi
  if [ "$OKONLY" = "1" ] && [ "$flag" != "OK" ]; then continue; fi
  printf "[%s] %-50s pos=%-12s msg=%s\n" "$flag" "$base" "$pos" "$msg"
done
