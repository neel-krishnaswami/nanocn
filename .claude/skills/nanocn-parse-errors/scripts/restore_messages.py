#!/usr/bin/env python3
"""Restore parser error messages after a grammar change.

Extracts old messages from a git ref, renumbers them to match the current
grammar via `menhir --update-errors`, deduplicates, merges with the current
parser.messages (preserving any hand-written new messages), and installs
the result.

Usage:
    scripts/restore_messages.py [--from REF] [--dry-run]

Options:
    --from REF   Git ref to extract old messages from (default: HEAD~1)
    --dry-run    Print statistics but don't install
"""

import argparse
import re
import subprocess
import sys
import tempfile
from pathlib import Path

MESSAGES = Path("lib/parser.messages")
GRAMMAR = Path("lib/parser.mly")
PLACEHOLDER = "<YOUR SYNTAX ERROR MESSAGE HERE>"


def run(cmd, **kwargs):
    r = subprocess.run(cmd, capture_output=True, text=True, **kwargs)
    return r


def extract_states(text):
    """Return set of state numbers found in a messages file."""
    return set(re.findall(r"Ends in an error in state: (\d+)", text))


def split_entries(text):
    """Split a messages file into (preamble, entries) where each entry
    starts with a sentence line (lowercase_name: TOKENS...)."""
    parts = re.split(r"\n(?=[a-z_]+:)", text)
    return parts


def entry_state(entry):
    """Extract the state number from an entry, or None."""
    m = re.search(r"Ends in an error in state: (\d+)", entry)
    return m.group(1) if m else None


def entry_is_placeholder(entry):
    """Check if an entry has a PLACEHOLDER message."""
    return PLACEHOLDER in entry


def dedup_entries(entries):
    """Remove duplicate entries for the same state, keeping the first."""
    seen = set()
    kept = []
    for entry in entries:
        state = entry_state(entry)
        if state is not None:
            if state in seen:
                continue
            seen.add(state)
        kept.append(entry)
    return kept


def non_placeholder_states(text):
    """Return set of states that have real (non-PLACEHOLDER) messages."""
    states = set()
    for entry in split_entries(text):
        state = entry_state(entry)
        if state is not None and not entry_is_placeholder(entry):
            states.add(state)
    return states


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--from", dest="git_ref", default="HEAD~1",
                        help="Git ref to extract old messages from (default: HEAD~1)")
    parser.add_argument("--dry-run", action="store_true",
                        help="Print statistics but don't install")
    args = parser.parse_args()

    if not MESSAGES.exists():
        print(f"Error: {MESSAGES} not found (run from repo root)", file=sys.stderr)
        sys.exit(1)
    if not GRAMMAR.exists():
        print(f"Error: {GRAMMAR} not found", file=sys.stderr)
        sys.exit(1)

    # Step 1: Extract old messages from git
    r = run(["git", "show", f"{args.git_ref}:{MESSAGES}"])
    if r.returncode != 0:
        print(f"Error: git show {args.git_ref}:{MESSAGES} failed:\n{r.stderr}", file=sys.stderr)
        sys.exit(1)
    old_text = r.stdout
    old_states = extract_states(old_text)
    print(f"Old messages ({args.git_ref}): {len(old_states)} states, "
          f"0 PLACEHOLDERs" if PLACEHOLDER not in old_text else "")

    # Step 2: Renumber via update-errors
    with tempfile.NamedTemporaryFile(mode="w", suffix=".messages", delete=False) as f:
        f.write(old_text)
        old_path = f.name

    r = run(["menhir", "--update-errors", old_path, str(GRAMMAR)])
    if r.returncode != 0:
        print(f"Error: menhir --update-errors failed:\n{r.stderr}", file=sys.stderr)
        sys.exit(1)
    updated_text = r.stdout
    updated_states = extract_states(updated_text)
    print(f"After update-errors: {len(updated_states)} states")

    # Step 3: Read current messages, find states with real messages
    current_text = MESSAGES.read_text()
    current_real = non_placeholder_states(current_text)
    current_ph = extract_states(current_text) - current_real
    print(f"Current messages: {len(extract_states(current_text))} states, "
          f"{len(current_real)} real, {len(current_ph)} PLACEHOLDERs")

    # Step 4: Deduplicate updated old, removing states already covered by current
    entries = split_entries(updated_text)
    seen = set()
    kept = []
    removed_dup = 0
    removed_covered = 0
    for entry in entries:
        state = entry_state(entry)
        if state is not None:
            if state in current_real:
                removed_covered += 1
                continue
            if state in seen:
                removed_dup += 1
                continue
            seen.add(state)
        kept.append(entry)
    deduped_text = "\n".join(kept)
    print(f"After dedup: removed {removed_dup} duplicates, "
          f"{removed_covered} already-covered states")

    # Step 5: Write deduped to temp file, merge with current
    with tempfile.NamedTemporaryFile(mode="w", suffix=".messages", delete=False) as f:
        f.write(deduped_text)
        deduped_path = f.name

    r = run(["menhir", "--merge-errors", deduped_path,
             "--merge-errors", str(MESSAGES), str(GRAMMAR)])
    if r.returncode != 0:
        print(f"Error: menhir --merge-errors failed:\n{r.stderr}", file=sys.stderr)
        sys.exit(1)
    merged_text = r.stdout

    # Step 6: Final dedup pass
    final_entries = dedup_entries(split_entries(merged_text))
    final_text = "\n".join(final_entries)

    # Step 7: Verify
    with tempfile.NamedTemporaryFile(mode="w", suffix=".messages", delete=False) as f:
        f.write(final_text)
        final_path = f.name

    r = run(["menhir", "--compile-errors", final_path, str(GRAMMAR)])
    if r.returncode != 0:
        print(f"Error: final messages don't compile:\n{r.stderr}", file=sys.stderr)
        sys.exit(1)

    final_states = extract_states(final_text)
    final_ph = final_text.count(PLACEHOLDER)
    final_real = len(final_states) - final_ph
    restored = final_real - len(current_real)

    print(f"\nResult: {len(final_states)} states, {final_real} real, {final_ph} PLACEHOLDERs")
    print(f"Restored {restored} messages from {args.git_ref}")

    if args.dry_run:
        print("(dry run — not installing)")
    else:
        MESSAGES.write_text(final_text)
        print(f"Installed to {MESSAGES}")


if __name__ == "__main__":
    main()
