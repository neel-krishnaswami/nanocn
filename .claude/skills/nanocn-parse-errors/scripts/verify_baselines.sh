#!/bin/bash
# Sweep all `.cn` and `.rcn` files in `examples/` (but not `examples/errors/`)
# and flag any that don't parse or typecheck cleanly.
#
# Usage:
#   scripts/verify_baselines.sh              # just parse, no typecheck
#   scripts/verify_baselines.sh --typecheck  # full typecheck
#
# Output: one line per regression. Clean tree produces no output (exit 0).

set -e

TYPECHECK=0
while [ $# -gt 0 ]; do
  case "$1" in
    --typecheck) TYPECHECK=1; shift ;;
    --help|-h)
      sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown: $1"; exit 1 ;;
  esac
done

REGRESSIONS=0
for f in examples/*.cn examples/*.rcn; do
  [ -f "$f" ] || continue
  if [[ "$f" == *.rcn ]]; then cmd="check-refined"; else cmd="check"; fi
  out=$(dune exec --no-build nanocn -- "$cmd" "$f" 2>&1 || true)
  if echo "$out" | grep -q "parse error"; then
    echo "PARSE-REGRESSION: $(basename $f)"
    echo "$out" | head -3 | sed 's/^/  /'
    REGRESSIONS=$((REGRESSIONS+1))
  elif [ "$TYPECHECK" = "1" ] && echo "$out" | grep -qE "Type error|^Error"; then
    echo "TYPECHECK-REGRESSION: $(basename $f)"
    echo "$out" | head -3 | sed 's/^/  /'
    REGRESSIONS=$((REGRESSIONS+1))
  fi
done
[ "$REGRESSIONS" -eq 0 ] && echo "OK: all baselines parse$([ "$TYPECHECK" = "1" ] && echo " and typecheck")."
exit 0
