# Plan: Implement iter in the refined typechecker

## Context

The `iter` construct is the loop primitive for nanocn's refined type system. It iterates
over a step-indexed predicate `P : pred D(A, B)` where `D` is a datatype with constructors
`Next : A` and `Done : B`. The initializer `crt1` produces the first loop state, and the
body `crt2` processes each step, consuming the previous resource and producing either a
`Next` (continue) or `Done` (finish).

## The Rule (synthesis, from syntax.ott)

```
CS = comp RS    type D(x, y) = { Next : a, Done : b } in CS
G = erase Delta    CS; G |- [spec] ce ==> pred D(A, B)
RS; Delta |- [pure] crt1 r<== x:A [pure], y:ce @ Next x [res], pfnil -| Delta' ~~> Ct
RS; Delta', x:A [pure], y:ce @ Next x [res(avail)]
  |- [impure] crt2 r<== z:D(A,B) [pure], y:ce @ z [res], pfnil
  -| Delta', x:A [pure], y:ce @ Next x [res(used)] ~~> Ct'
--------------------------------------------------------------------
RS; Delta |- [impure] iter[ce] (q = crt1) { crt2 }
  r==> z:B [pure], y:ce @ Done z [res], pfnil -| Delta' ~~> Ct /\ forall x:A. Ct'
```

## Implementation (in lib/rCheck.ml)

### Location: `synth_crt_impl`

The CIter case was moved from `check_crt_impl` (where it was a stub returning error)
to `synth_crt_impl` (where it is a proper synthesis rule).

### Logic

1. **Effect check**: Require `Impure <= eff`
2. **Elaborate predicate**: `elab_and_synth rs delta Spec se_pred` -> `(ce_pred, pred D(A,B))`
3. **Extract step type**: Match `pred_sort` as `Pred(App(D, args))`
4. **Look up constructors**: `CtorLookup.lookup` for Next -> `a_sort`, Done -> `b_sort`
5. **Build init proof sort**: `x:A [pure], y:ce_pred @ Next(x) [res]`
6. **Check init**: `check_crt rs delta Pure crt1 init_pf` -> `(delta', ct)`
7. **Build body context**: `delta' + rpat_match(gamma, pat, init_pf)`
8. **Build body proof sort**: `z:D(A,B) [pure], y2:ce_pred @ z [res]`
9. **Check body**: `check_crt rs delta_body Impure crt2 body_pf` -> `(delta_out, ct')`
10. **Validate output**: Split delta_out, verify pattern resources consumed (`RCtx.zero`)
11. **Build result**: `zr:B [pure], yr:ce_pred @ Done(zr) [res]`
12. **Constraint**: `Ct /\ forall x_pat:A. Ct'` where `x_pat` is the first pattern variable

### Key design choices

- The forall quantifies over the **pattern** variable (not the fresh proof sort variable),
  since `rpat_match` substitutes proof sort vars to pattern vars, and `ct'` references the
  pattern var.
- Resource consumption check uses `RCtx.zero` (same pattern as CLet) rather than explicit
  usage comparison.
- Checking mode falls through via the synth-then-pf_eq pattern in `check_crt_impl`.

## Test

A QCheck test exercises iter with a heap-allocated step cell:
- Allocates `New[step(int, ())](Next 0)`
- Init binds the resource via pattern
- Body does Get/Set to transition to `Done ()`
- After iter, deletes the heap cell
