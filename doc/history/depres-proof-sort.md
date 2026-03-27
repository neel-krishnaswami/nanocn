# Plan: Add predicate output binding (DepRes) to proof sorts

## Context

The Ott spec (`doc/syntax.ott`) includes `DepRes` rules for a dependent resource
form `x : (y:τ).ce [res]` where the value is existentially bound. This plan
brought the implementation up to match.

## Changes made

### ProofSort (`lib/proofSort.mli`, `lib/proofSort.ml`)
- Added `DepRes of { var; bound_var; bound_sort; pred }` variant
- Updated all pattern matches: `map_entry`, `pf_types`, `bind`, `pf_to_ctx`, `subst`, `print`

### RPat (`lib/rPat.mli`, `lib/rPat.ml`)
- Changed from `Var.t list` to `pat_elem list` with `Single of Var.t | Pair of Var.t * Var.t`
- Pair elements are used for DepRes pattern matching

### ElabM (`lib/elabM.mli`, `lib/elabM.ml`)
- Added `lift : ('a, string) result -> 'a t` to promote plain results into the monad

### Parser (`lib/parser.mly`)
- Added DepRes pf_entry production: `[res] x : (take y : τ = ce)`
- Updated rpat to use `rpat_elem` with `Single`/`Pair` variants
- Pair syntax: `(x, y)` within outer rpat parens, e.g. `let ((x', r')) = ...`

### rCheck (`lib/rCheck.mli`, `lib/rCheck.ml`)
- Converted from `Result.bind` to `ElabM` monad throughout all typing judgements
- Replaced all `Var.of_string` hacks with `fresh` for hygiene
- Added DepRes cases in: `elab_pf_entry`, `elab_pf`, `pf_eq`, `rpat_match`,
  `check_spine_inner`, `_check_tuple`
- Simplified `.mli` to expose only `check_rprog` (all other judgements are internal)
- `check_rprog` runs the ElabM monad at the boundary

### Example (`examples/incr.rcn`)
- Updated to use DepRes codomain: `[res] r' : (take x' : int = Own[int](p))`
- Pattern uses Pair: `let ((x', r')) = incr(p, 0, res r)`

## Concrete syntax

- Proof sort entry: `[res] x : (take y : τ = ce)`
- Refined pattern pair: `(x, y)` inside rpat parentheses
