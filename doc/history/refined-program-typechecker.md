# Refined Program Parser and Typechecker

## Summary

Implemented the refined type system from the Ott spec (lines 258–1672) as an OCaml
parser and typechecker layered on top of nanocn's existing core type system.

## Architecture

**Pipeline:** Parse `.rcn` file → `RProg.parsed` → `RCheck.check_rprog` → `(RSig.t, Constraint.t)`

**Key design decisions:**

1. **Parameterized ASTs.** `RefinedExpr`, `ProofSort`, `RFunType`, and `RProg` are all
   parameterized by `'e` (the embedded expression type). At parse time, `'e = SurfExpr.se`;
   after elaboration, `'e = CoreExpr.ce`. This avoids duplicating types.

2. **Explicit Delta/Ct threading.** No monad — pass `RCtx.t` in, return `RCtx.t` out,
   matching the Ott exactly. Constraints returned as explicit `Constraint.t` values.

3. **Two-phase processing.** Parsing produces `RProg.parsed` with surface expressions.
   `RCheck` first elaborates surface expressions to core via `elab_crt`/`elab_spine`/etc.,
   then typechecks the elaborated terms.

4. **Delegate to core.** Embedded core expressions are checked via `Typecheck.synth/check`
   using `RSig.comp` (erasure to core sig) and `RCtx.erase` (erasure to core context).

## Modules added (13 new files)

| Module | Role |
|--------|------|
| `Usage` | Usage flags: Used, Avail, Opt with meet/affinize |
| `Constraint` | SMT constraint trees with smart constructors |
| `RCtx` | Refined contexts with comp/log/res entries |
| `ProofSort` | Proof sorts (parameterized by expression type) |
| `RFunType` | Refined function types Pf₁ ⊸ Pf₂ [eff] |
| `RPat` | Refined patterns (flat variable tuples) |
| `RefinedExpr` | Mutually recursive ASTs: crt, lpf, rpf, spine |
| `RSig` | Refined signatures with erasure to core |
| `RProg` | Refined program structure |
| `RCheck` | All refined typing judgements + elaboration |

## Modified files

| File | Change |
|------|--------|
| `coreExpr.mli/ml` | Added `subst : Var.t -> ce -> ce -> ce` |
| `lexer.ml` | New keywords: exfalso, auto, unfold, log, res, forall; hyphenated: open-ret, open-take, make-ret, make-take; new tokens: `@`, `~>` |
| `parser.mly` | New entry point `rprog_eof`; new nonterminals for refined programs, proof sorts, crt, lpf, rpf, spines |
| `parse.mli/ml` | Added `parse_rprog` |
| `main.ml` | Added `check-refined` command |
| `test/test_main.ml` | Added new module tests to test suite |

## Concrete syntax

```
// Refined function declaration
fun f (x : int [pure], h : p [log]) ~> y : int [pure] [pure] =
  body_crt

// Refined main
main : x : int [pure] [pure] =
  body_crt

// Spine arguments with keyword disambiguation
f(42, log auto, res r)

// Proof annotations
42 : x : int [pure]
```

## Known limitations / TODOs

- `iter` not yet implemented in refined checker
- `make-take` checking is incomplete (sort inference from pred)
- `open-take` sort inference is placeholder
- Case branch type substitution not applied for parameterized types
- Parser has shift/reduce conflicts (benign, resolved correctly by Menhir)
- FORALL token defined but unused (reserved for future constraint syntax)
