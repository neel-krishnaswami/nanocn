# Plan: Make Elaborate Produce `typed_ce`

## Summary

Refactored the elaborator (`elaborate.ml`) to directly produce `typed_ce`
(core expressions annotated with `ctx`, `sort`, `eff`, `loc` at every node),
eliminating the redundant typecheck pass that previously re-walked core
expressions to add the same information.

## Key Changes

### 1. Replaced EvalCtx with let_binding list

The `EvalCtx` module used binding info `b` as both the binding info for
`(x, b)` and the node info for `CoreExpr.mk b`, which doesn't work for
`typed_ce` where these differ. Replaced with a `let_binding` record:

```ocaml
type let_binding = {
  var : Var.t;   rhs : Var.t;   sort : Sort.sort;
  eff : Effect.t;   loc : SourcePos.t;
}
```

At `Cov_done`, `wrap_lets` constructs the typed let-chain with correct
info at each level.

### 2. Elaborate produces typed_ce directly

`synth` returns `(typed_ce * Sort.sort) ElabM.t` and `check` returns
`typed_ce ElabM.t`. Each case constructs nodes with `mk_typed ctx pos sort eff0`.

### 3. Scrutinee context threading

Callers extend `ctx` with scrutinee variables before calling `coverage_check`.
Similarly, `build_*_con_branches` extends `ctx` with fresh constructor
argument variables before recursive calls.

### 4. Removed redundant typecheck passes

- `typecheck.ml`: `elaborate_fun` and `check_prog` no longer call
  `Typecheck.check`/`synth` on the elaborated result.
- `rCheck.ml`: `elab_se`, `elab_se_check`, `elab_fundecl_body` strip
  typed info directly from elaborate output.
- `bin/main.ml`: `handle_let` no longer calls `Typecheck.synth`.

### 5. Deleted EvalCtx

`evalCtx.ml` and `evalCtx.mli` removed as dead code.

## Files Modified

- `lib/elaborate.ml` — Core change: produces typed_ce, uses let_binding list
- `lib/elaborate.mli` — Updated types and signatures
- `lib/typecheck.ml` — Removed redundant re-typecheck in elaborate_fun/check_prog
- `lib/rCheck.ml` — Simplified elab_se/elab_se_check/elab_fundecl_body
- `bin/main.ml` — Simplified handle_let
- `test/test_main.ml` — Updated elab_synth/elab_check helpers

## Files Deleted

- `lib/evalCtx.ml`
- `lib/evalCtx.mli`
