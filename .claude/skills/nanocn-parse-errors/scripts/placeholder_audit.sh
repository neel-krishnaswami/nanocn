#!/bin/bash
# Report PLACEHOLDER coverage: total count, surface vs refined,
# per-form breakdown.
#
# Usage:
#   scripts/placeholder_audit.sh
#
# Runs from the repo root.

set -e
SKILL_DIR=$(dirname "$(dirname "$(readlink -f "$0")")")
CLASSIFY="$SKILL_DIR/scripts/classify_states.sh"
MESSAGES="${MESSAGES_FILE:-lib/parser.messages}"

TOTAL=$(grep -c "^PLACEHOLDER" "$MESSAGES" || echo 0)
ALL_STATES=$(awk '/^## Ends in an error in state:/' "$MESSAGES" | wc -l)
HANDCRAFTED=$((ALL_STATES - TOTAL))

echo "=== PLACEHOLDER audit for $MESSAGES ==="
echo "Total states:         $ALL_STATES"
echo "Hand-crafted:         $HANDCRAFTED"
echo "PLACEHOLDER:          $TOTAL"
if [ "$ALL_STATES" -gt 0 ]; then
  PCT=$(awk -v d="$HANDCRAFTED" -v t="$ALL_STATES" 'BEGIN { printf "%.1f", 100.0 * d / t }')
  echo "Coverage:             $PCT%"
fi
echo
echo "=== By form (PH = PLACEHOLDER, includes 0 rows) ==="
bash "$CLASSIFY" --summary
echo
echo "=== Remaining PLACEHOLDER states by form ==="
bash "$CLASSIFY" --placeholders-only | awk -F'\t' '{print $3}' | sort | uniq -c | sort -rn
