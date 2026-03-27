# Plan: Merge elaboration into refined typechecking

## Context

The refined typechecker (`rCheck.ml`) currently has a two-phase architecture:

1. **Elaboration pre-pass** (`elab_crt`, `elab_spine`, `elab_lpf`, `elab_rpf`): walks the
   parsed AST (`parsed_crt` with `SurfExpr.se`), converting every embedded surface expression
   to a core expression (`located_crt` with `CoreExpr.ce`).
2. **Refined typechecking** (`synth_crt`, `check_crt`, etc.): walks the elaborated AST,
   delegating core expression checking to `Typecheck.synth/check`.

The problem: phase 1 needs sort/effect information that's only available during phase 2.
For example, `elab_spine` needs each argument's entry effect (pure vs spec) to elaborate
correctly, which requires knowing the function's domain proof sort. We've been patching
this by looking up the RF during elaboration, but it's fragile:

- `elab_spine` must look up the function's domain to determine per-argument effects
- For tuples, no domain is available at elaboration time, requiring a permissive fallback
- `elab_crt` for `CLet` must synthesize `e1` during elaboration to extend the context for `e2`
- Each patch couples elaboration more tightly to typechecking, defeating the purpose of separation

**Goal**: Merge the two phases so that each embedded `SurfExpr.se` is elaborated inline
during refined typechecking, when context and proof sort information are available.

---

## Design

### Core idea

Make `synth_crt`, `check_crt`, `synth_lpf`, etc. accept `parsed_crt` (with `SurfExpr.se`)
directly, instead of `located_crt` (with `CoreExpr.ce`). Each typing judgement elaborates
the surface expressions it encounters using the context available at that point.

### Type changes

```
(* Before *)
val synth_crt : RSig.t -> RCtx.t -> Effect.t -> located_crt -> ...
val check_crt : RSig.t -> RCtx.t -> Effect.t -> located_crt -> ...

(* After *)
val synth_crt : RSig.t -> RCtx.t -> Effect.t -> parsed_crt -> ...
val check_crt : RSig.t -> RCtx.t -> Effect.t -> parsed_crt -> ...
```

Similarly for `synth_lpf`, `check_lpf`, `synth_rpf`, `check_rpf`, `check_spine_inner`,
`_check_tuple`.

### Where elaboration moves

Each call to `tc_check`/`tc_synth` (which typechecks an already-elaborated `CoreExpr.ce`)
becomes a call to `elab_se`/`elab_se_check` (which elaborates a `SurfExpr.se` *and*
typechecks it in one step). The context and effect are already available at each call site.

Concretely:

| Current (two-phase) | After (merged) |
|---------------------|----------------|
| `elab_spine` converts `se` to `ce`, `check_spine_inner` calls `tc_check rs gamma ce sort eff` | `check_spine_inner` calls `elab_se_check rs gamma se sort eff` directly |
| `elab_crt` for `CIf` converts condition `se` to `ce`, `check_crt` calls `tc_check rs gamma ce bool_sort eff'` | `check_crt` calls `elab_se_check rs gamma se bool_sort eff'` directly |
| `elab_crt` for `CCase` converts scrutinee `se` to `ce`, `check_crt` calls `tc_synth rs gamma eff' ce` | `check_crt` calls `elab_se rs gamma eff' se` directly, getting `(ce, sort)` |
| `elab_pf_entry` converts log/res expressions | `elab_pf_entry` stays the same (already inline) |

### What gets deleted

The entire elaboration pass: `elab_crt`, `elab_spine`, `elab_spine_permissive`,
`elab_spine_no_domain`, `elab_lpf`, `elab_rpf`, `elab_case_branches`. These are ~120 lines
of code that duplicate the recursive structure of the typing judgements.

### What stays

- `elab_se` / `elab_se_check` / `elab_and_synth` — these are the leaf functions that convert
  a single `SurfExpr.se` to `CoreExpr.ce`. They stay as helpers.
- `elab_pf` / `elab_pf_entry` — proof sort elaboration stays (proof sorts in parsed form
  use `SurfExpr.se` and need to be elaborated to `CoreExpr.ce ProofSort.t`).
- `elab_fundecl_body` — stays for core function declarations.
- `strip_info` — stays.

### check_rprog changes

```ocaml
(* Before *)
let* main_body = elab_crt rs RCtx.empty prog.main_eff prog.main_body in
let* (_delta, ct) = check_crt rs RCtx.empty prog.main_eff main_body main_pf in

(* After — no separate elab pass *)
let* (_delta, ct) = check_crt rs RCtx.empty prog.main_eff prog.main_body main_pf in
```

Similarly for RFunDecl processing.

### The CLet elaboration problem goes away

Currently `elab_crt` for `CLet` must synthesize `e1` to extend the context for `e2`.
After merging, `check_crt` for `CLet` naturally synthesizes `e1` first, extends the
context, then checks `e2` — no special elaboration logic needed.

### The spine effect problem goes away

Currently `elab_spine` needs the domain proof sort to determine per-argument effects.
After merging, `check_spine_inner` already has the domain and dispatches pure/spec
correctly — it just calls `elab_se_check` instead of `tc_check`.

---

## Files to modify

| File | Change |
|------|--------|
| `lib/rCheck.mli` | Change `located_*` to `parsed_*` in all signatures |
| `lib/rCheck.ml` | Delete elab_crt/elab_spine/etc; change typing fns to accept parsed types; replace tc_check/tc_synth with elab_se/elab_se_check |

No other files change — the parser, AST types, and external interface (`check_rprog`
already takes `RProg.parsed`) are unaffected.

---

## Implementation steps

1. In `rCheck.mli`: change all `located_*` types to `parsed_*` types in the public signatures.

2. In `rCheck.ml` typing judgements (`synth_crt`, `check_crt`, `synth_lpf`, `check_lpf`,
   `synth_rpf`, `check_rpf`, `check_spine_inner`, `_check_tuple`, `check_case_branches`,
   `rpat_match`, `pf_eq`):
   - Change type annotations from `located_*` to `parsed_*`
   - Replace `tc_check rs gamma ce sort eff` with `let* ce = elab_se_check rs gamma se sort eff in`
     (where `se` is the `SurfExpr.se` from the parsed shape)
   - Replace `tc_synth rs gamma eff ce` with `let* (ce, sort) = elab_se rs gamma eff se in`
   - For `check_spine_inner` Spine_pure/Spine_spec: change `ce` to `se` in the shape match,
     call `elab_se_check` at the appropriate effect
   - For `pf_eq`: no embedded expressions to change (works on `CoreExpr.ce ProofSort.t`, not parsed)
   - For `rpat_match`: similarly works on elaborated proof sorts, no change needed

3. In `check_rprog` and `check_rprog_decl`: remove `elab_crt` calls, pass parsed bodies
   directly to `check_crt`/`synth_crt`.

4. Delete `elab_crt`, `elab_spine`, `elab_spine_permissive`, `elab_spine_no_domain`,
   `elab_lpf`, `elab_rpf`, `elab_case_branches`.

5. Delete `tc_synth` and `tc_check` (no longer used — replaced by `elab_se`/`elab_se_check`).

6. Clean up: `_elab_and_check` can probably be deleted too. Check for any remaining dead code.

---

## Verification

1. `dune build` compiles with no errors
2. `dune test` — all 140 tests pass
3. All 8 example `.rcn` files pass `check-refined`
4. The `refined_fun.rcn` example (with spec args) now works without the spine effect hack
