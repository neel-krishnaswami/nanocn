---
name: nanocn-parse-errors
description: Maintain nanoCN's Menhir parser error messages. Use this skill whenever the user mentions PLACEHOLDER messages, `parser.messages`, parse-error quality, Menhir error states, or the `examples/errors/parsing/` corpus; whenever they've edited `lib/parser.mly` and need the messages/baselines/mutants updated; or when a test user reports a confusing parse error. The skill assumes the four-location workflow (grammar → baselines → mutants → messages) and provides bundled scripts for state classification, mutant verification, and drift detection. Invoke proactively after any grammar change, even if the user doesn't explicitly ask.
---

# nanoCN parse-error messages

Maintain the correspondence between four artefacts:

| Artefact | Location | Role |
|---|---|---|
| Grammar | `lib/parser.mly` | Source of truth for states |
| Messages | `lib/parser.messages` | One message per LR error state |
| Baselines | `examples/*.cn`, `examples/*.rcn` | Programs that parse + typecheck, exercise each syntactic form |
| Mutants | `examples/errors/parsing/*.cn` | Erroneous programs, one per LR state we want to cover |

Menhir's `--compare-errors` is the drift detector: when the grammar changes, states renumber, and some messages may become orphaned while new ones come in as PLACEHOLDER. The corpus of mutants is the second drift detector: after a grammar change, some mutants may now parse, and others may hit a different state than intended.

## When to use this skill

Use it proactively whenever any of these apply:

1. The user edits `lib/parser.mly` (any grammar or lexer change).
2. The user mentions a PLACEHOLDER message, confusing parse error, or wants to "improve parse errors".
3. The user asks about a specific state number or a specific syntactic form's error messages.
4. The user wants to audit coverage of error states or check drift between grammar and messages.

Even if the user says something vague like "these parse errors are unhelpful", run a quick coverage audit (`placeholder_audit.sh`) and offer concrete next steps.

## The two flows

### Flow A: grammar-change impact assessment

Triggered when the user says "I changed the grammar" or after you observe an edit to `lib/parser.mly`, `lib/lexer.ml`, or any syntax-tree module that the parser consumes. Run this sequence:

1. **Baseline check.** Run `scripts/verify_baselines.sh`. Report any regressions — these need the *programs* updated, not the messages.
2. **State diff.** Run `scripts/diff_states.sh`. Categorises states as new, removed, or renumbered.
3. **Mutant sweep.** Run `scripts/verify_mutants.sh`. Reports which mutants now parse OK (mutation is stale), which hit an unexpected state (target drifted), and which are still correct.
4. **Triage**:
   - New states → identify the form they belong to (see `references/form-index.md`), then apply Flow B.
   - Removed states → delete the corresponding entries from `parser.messages` and the stale mutants from `examples/errors/parsing/`.
   - Renumbered states → the message may still be correct in prose; verify, but expect Menhir's `--compare-errors` CI gate to force consistency.
   - Stale mutants → rewrite or delete; if the mutation targets a form the user removed, delete.
5. **Rebuild**: `dune build`. Must succeed — Menhir's `--compare-errors` step in `lib/dune` will fail if `parser.messages` doesn't cover every error state.

### Flow B: tackle one form or one state

Triggered when the user asks to handle a specific form ("the let-res states", "the refined iter messages") or a specific state ("state 142 is a PLACEHOLDER").

**For a form:**

1. Find the form's canonical baseline in `examples/` (e.g., `let_plain.cn`, `fibonacci.cn`, `spec_take.cn`). See `references/form-index.md`. If none exists, write one: a minimal program that parses + typechecks and exercises the feature.
2. Enumerate the form's states via `scripts/classify_states.sh <form>`.
3. For each state with a PLACEHOLDER, write one mutant in `examples/errors/parsing/` that introduces a plausible single-point mistake at that state's position.
4. Run `scripts/verify_mutants.sh <mutant-file>` on each. The script reports which state actually fired. If it hit the wrong state (via spurious reduction — see `references/spurious-reductions.md`), adjust the mutant, typically by swapping the trailing token to a comma or semicolon which forces full reduction.
5. For each state you successfully hit, craft a message (see `references/message-patterns.md`) and splice it into `parser.messages`, anchored on the unique state-item block.
6. Rebuild. Verify each mutant's error message is the new prose via `scripts/verify_mutants.sh`.

**For a single state:**

1. Look up the state's grammar item via `scripts/classify_states.sh --state <N>`.
2. Skip to step 3 of the per-form workflow.

## The core methodology

### Principle 1 — What you had to look up is what the message should have said

Every time you open `lib/parser.mly`, `lib/surfExpr.ml`, `lib/refinedExpr.ml`, or any other file while writing a mutant or message, ask: *what could the error message have told the user to make this unnecessary?* The answer is almost always the next sentence of the message.

For example: the user wrote `Done r : Step(Int, Int) [impure]` — an expression-level sort annotation with a trailing `[eff]` suffix. The grammar doesn't allow `[eff]` on expression annotations (only on signatures). Before I'd seen this, I would have had to check `parser.mly:183` to confirm. That's the signal: the state's message should say **"a sort annotation on an expression takes a sort only — no `[<eff>]` suffix — since an expression's effect is inferred from context."** That one caveat saves every future user the same lookup.

### Principle 2 — A mutant that doesn't hit the target state is diagnostic

If you expect state 184 but get state 148 ("Expression complete"), the reason is almost always LR(1) spurious reduction: the expression chain reduces through `mul_expr` before lookahead can force full reduction to `expr`. The fix is not to rewrite the state's message; it's to pick a lookahead token that forces reduction — `,`, `;`, `|`, or `)` in an inner context. See `references/spurious-reductions.md` for the full pattern.

### Principle 3 — Not every state needs a message

Expression-precedence scaffolding states (`add_expr +`, `mul_expr *`, `cmp_expr <`, ...) are essentially unreachable through normal user mistakes because the parser errors at state 148 (or equivalent) first, and state 148 has a good generic "Expression complete" message. Leaving these with PLACEHOLDER is fine — they're sinks, not user-facing. `references/spurious-reductions.md` lists the categories that are OK to leave.

### Principle 4 — Messages are prose, not telegrams

A good message has three parts:
- **Shape**: what was just parsed, what's expected next. Lead with this.
- **Example**: a minimal concrete program fragment showing the correct form.
- **Caveat** (optional, but high-value): a common confusion, a cross-reference to a related form, or a rule that sounds like it should apply but doesn't.

Examples in `references/message-patterns.md`. Bad messages mention tokens from the Menhir grammar ("after `LABEL COLON`, expected `sort`"); good ones mention user-visible constructs ("after a constructor name in a `type { ... }` body, expected `:` followed by the payload sort; use `: ()` for a nullary constructor").

### Principle 5 — If a mistake recurs, fix the grammar, not the message

When I was writing the `take` messages, I discovered that `take x : Int = incP n` was annotating the RHS with `Int` when the correct annotation should be the payload sort. The grammar was wrong. Fixing it in `parser.mly` (wrap the annotation in `Pred`) was the right move — the message then could say "the annotation is the payload type" without needing a workaround. Don't write messages that paper over a grammar bug; fix the grammar.

## Recovery: restoring messages after a grammar change

When a grammar change causes `parser.messages` to become stale (e.g., you installed fresh all-PLACEHOLDER messages to get the build working), use the restore script to recover old messages:

```
python3 scripts/restore_messages.py --from <git-ref-with-good-messages>
```

This extracts old messages, renumbers states via `menhir --update-errors`, deduplicates, merges with the current file (preserving any new hand-written messages), and installs the result. Use `--dry-run` to preview.

After restoring, run the audit to check for stale references:

```
python3 scripts/audit_messages.py
```

## Workflow checklist for Flow B

1. `dune build` — ensure the tree is clean and Menhir regenerates.
2. `python3 scripts/placeholder_audit.py` — identify PLACEHOLDER states.
3. For each PLACEHOLDER state:
   - Write baseline if missing (`examples/<form>.cn`).
   - Write mutant (`examples/errors/parsing/<form>-<defect>.cn`).
   - `python3 scripts/verify_mutants.py <file>` — did it hit the right state?
   - Craft message. For bulk insertion, write a mapping file and use `python3 scripts/splice_messages.py messages.map`.
4. `dune build` — Menhir regenerates, `--compare-errors` succeeds.
5. `python3 scripts/verify_baselines.py` — no baseline regressed.
6. `python3 scripts/verify_mutants.py` — every mutant produces a non-PLACEHOLDER message.
7. `python3 scripts/audit_messages.py` — comprehensive prose audit, 0 issues.

## Bundled scripts

All scripts are Python (in `scripts/`) and run from the repo root.

- `restore_messages.py [--from REF]` — recover old messages after a grammar change. Extracts from git, renumbers, deduplicates, merges, installs.
- `audit_messages.py [--fix-stale]` — comprehensive audit: checks every message against its grammar items for stale references and form mismatches.
- `splice_messages.py <mapfile>` — bulk-insert messages from a `STATE: message` mapping file.
- `placeholder_audit.py` — PLACEHOLDER count, per-form breakdown.
- `diff_states.py` — compare current messages vs fresh grammar; report new/removed states.
- `verify_baselines.py [--typecheck]` — verify all example files still parse.
- `verify_mutants.py [files...]` — run mutants, report [PH]/[OK]/[ERR] status.

## References

- `references/methodology.md` — deeper discussion of the five principles.
- `references/spurious-reductions.md` — the mutant-miss diagnostic, including a table of which reduction-lookahead tokens force which state to fire.
- `references/message-patterns.md` — canonical good/bad message examples drawn from the existing corpus.
- `references/form-index.md` — one-page map of nanoCN's syntactic forms, their canonical baseline files, and rough state counts.
