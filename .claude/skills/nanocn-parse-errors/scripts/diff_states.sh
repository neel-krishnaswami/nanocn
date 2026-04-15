#!/bin/bash
# Compare the current parser.messages state set against a fresh
# `menhir --list-errors` output for parser.mly.
#
# Usage:
#   scripts/diff_states.sh
#
# Output:
#   in_messages_only: state numbers present in parser.messages but not in
#     menhir --list-errors (these have been removed by a grammar edit —
#     Menhir's --compare-errors will complain).
#   in_grammar_only: state numbers fresh from the grammar that are not in
#     parser.messages (new error states waiting for messages).
#   Plus a summary of how many of each.
#
# Runs `menhir --list-errors` in the background; needs menhir on PATH and
# parser.mly at lib/parser.mly.

set -e

MESSAGES="${MESSAGES_FILE:-lib/parser.messages}"
GRAMMAR="${GRAMMAR_FILE:-lib/parser.mly}"
[ -f "$MESSAGES" ] || { echo "missing $MESSAGES"; exit 1; }
[ -f "$GRAMMAR" ] || { echo "missing $GRAMMAR"; exit 1; }

TMP=$(mktemp -d)
trap "rm -rf $TMP" EXIT

menhir --list-errors "$GRAMMAR" > "$TMP/fresh.messages" 2>/dev/null

extract_states() {
  awk '/^## Ends in an error in state:/ {
    s=$0; sub(/.*state: /,"",s); sub(/\..*/,"",s); print s
  }' "$1" | sort -n -u
}

extract_states "$MESSAGES" > "$TMP/msg_states"
extract_states "$TMP/fresh.messages" > "$TMP/fresh_states"

MSG_ONLY=$(comm -23 "$TMP/msg_states" "$TMP/fresh_states")
GRAMMAR_ONLY=$(comm -13 "$TMP/msg_states" "$TMP/fresh_states")
MSG_COUNT=$(wc -l < "$TMP/msg_states")
FRESH_COUNT=$(wc -l < "$TMP/fresh_states")

echo "Current $MESSAGES: $MSG_COUNT states"
echo "Fresh  $GRAMMAR    : $FRESH_COUNT states"
echo
if [ -n "$MSG_ONLY" ]; then
  echo "=== REMOVED states (in $MESSAGES but not in grammar) ==="
  echo "$MSG_ONLY" | tr '\n' ' ' | fold -w 80 -s
  echo
  echo
fi
if [ -n "$GRAMMAR_ONLY" ]; then
  echo "=== NEW states (in grammar but not in $MESSAGES) ==="
  echo "$GRAMMAR_ONLY" | tr '\n' ' ' | fold -w 80 -s
  echo
fi
if [ -z "$MSG_ONLY" ] && [ -z "$GRAMMAR_ONLY" ]; then
  echo "State sets match — no grammar drift detected."
fi
