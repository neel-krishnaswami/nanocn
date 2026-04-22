#!/usr/bin/env python3
"""Bulk-insert error messages into parser.messages from a mapping file.

The mapping file has one entry per line:
    STATE: message text here

Usage:
    scripts/splice_messages.py messages.map [--dry-run]
"""

import argparse
import re
import sys
from pathlib import Path

MESSAGES = Path("lib/parser.messages")
PLACEHOLDER = "<YOUR SYNTAX ERROR MESSAGE HERE>"


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("mapfile", help="File with STATE: message lines")
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()

    # Parse mapping file
    mapping = {}
    with open(args.mapfile) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            m = re.match(r"(\d+):\s*(.*)", line)
            if m:
                mapping[m.group(1)] = m.group(2)
            else:
                print(f"Warning: skipping unparseable line: {line}", file=sys.stderr)

    if not mapping:
        print("No mappings found.", file=sys.stderr)
        sys.exit(1)

    print(f"Loaded {len(mapping)} message mappings")

    text = MESSAGES.read_text()
    replaced = 0

    for state, msg in mapping.items():
        pattern = rf"(## Ends in an error in state: {state}\..*?\n##\n\n){re.escape(PLACEHOLDER)}"
        new_text, n = re.subn(pattern, rf"\g<1>{msg}", text, flags=re.DOTALL)
        if n > 0:
            text = new_text
            replaced += 1
        else:
            print(f"  State {state}: not found or already has a message")

    print(f"Replaced {replaced} PLACEHOLDER messages")

    if args.dry_run:
        print("(dry run — not writing)")
    else:
        MESSAGES.write_text(text)
        print(f"Written to {MESSAGES}")


if __name__ == "__main__":
    main()
