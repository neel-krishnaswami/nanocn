# Plan: Use `typed_ce` Throughout Refinement Typing

## Context

After the previous refactoring, `Elaborate` produces `typed_ce` (core expressions
annotated with `ctx`, `sort`, `eff`, `loc`). But the refinement modules immediately
strip the extra info via `strip_info`, discarding sort/ctx/eff, then sometimes
call `Typecheck.synth` to re-derive the sort. This is wasteful and loses information.

**Goal**: Make the entire refinement pipeline (`rCheck`, `RSig`, `RCtx`, `Constraint`,
`ProofSort`) use `typed_ce` instead of `CoreExpr.ce`.

**Benefit**: Eliminates ~5 calls to `Typecheck.synth` that exist only to recover
sort info. Provides richer output for downstream consumers.

## Steps

0. Define `typed_info`, `typed_ce`, `subst_gen` in CoreExpr
1. Change RCtx to use `typed_ce`
2. Change ProofSort to use `typed_ce`
3. Change Constraint to add `typed_ct`
4. Change RSig to use `typed_ce`
5. Update rCheck.ml: remove strip_info, typed constructors, eliminate Typecheck.synth
6. Update callers (main.ml, test_main.ml)

## Files modified

CoreExpr, RCtx, ProofSort, Constraint, RSig, rCheck, main, test_main
