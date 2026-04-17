#!/usr/bin/env bash
# check-grammar.sh — CI gate for the tree-sitter grammar.
#
# Parses every .cn and .rcn example file and fails if any parse
# tree contains ERROR or MISSING nodes. Run from the project root:
#
#   bash scripts/editor/tree-sitter-nanocn/check-grammar.sh
#
# Requires `tree-sitter` on PATH and the grammar already generated
# (run `tree-sitter generate` in scripts/editor/tree-sitter-nanocn/).
#
# Also runs `tree-sitter test` for the corpus tests.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
cd "$SCRIPT_DIR"

# Ensure the grammar is generated.
if [ ! -f src/parser.c ]; then
  echo "ERROR: src/parser.c not found. Run 'tree-sitter generate' first." >&2
  exit 1
fi

# Run corpus tests.
echo "=== Corpus tests ==="
tree-sitter test
echo ""

# Parse every positive example.
echo "=== Parsing examples ==="
fail=0
for f in "$REPO_ROOT"/examples/*.cn "$REPO_ROOT"/examples/*.rcn; do
  [ -f "$f" ] || continue
  output=$(tree-sitter parse "$f" 2>&1)
  if echo "$output" | grep -qE '(ERROR|MISSING)'; then
    echo "FAIL: $f"
    echo "$output" | grep -E 'ERROR|MISSING' | head -5
    fail=1
  else
    echo "  ok: $(basename "$f")"
  fi
done

if [ "$fail" -ne 0 ]; then
  echo ""
  echo "FAILED: some examples produced ERROR/MISSING nodes." >&2
  exit 1
fi

echo ""
echo "All examples parse cleanly."
