# Fix: Align Effect Computation with Ott Specification

## Problem

The typechecker (`typecheck.ml`) and elaborator (`elaborate.ml`) use a top-down
ambient effect but don't compute result effects bottom-up via joins as the Ott
spec requires. Additionally, several rules use the wrong effect for purification
and binding.

## Key Architectural Change

The spec's checking judgement `CS; G |- [eff0] ce <== tau [eff]` has two distinct
effect parameters:

- `eff0`: the **ambient** effect (top-down, for subsumption checks and purification)
- `eff`: the **result** effect (bottom-up, computed via joins)

The implementation conflates these into a single `eff` parameter. The fix separates
them by having `check` return the computed result effect alongside the typed expression.

## Specific Fixes

### 1. Bottom-up effect computation
- `check` returns `(typed_ce * Effect.t)` — the second component is the computed effect
- Each rule joins sub-expression effects per the spec
- `check_list` and `check_case_branches` also return effects

### 2. Syn_app / Syn_call: argument at `purify eff0`
- Spec: `eff'' = purify eff0; check arg at eff''`
- Was: `check arg at eff`

### 3. Chk_if: condition at `purify eff0`
- Spec: `eff' = purify eff0; check cond at eff'`
- Was: `check cond at eff`

### 4. Chk_case: scrutinee at `purify eff0`
- Spec: `eff' = purify eff0; synth scrut at eff'`
- Was: `synth scrut at eff`

### 5. Chk_case: branch bindings at scrutinee effect
- Spec: `xi : tau [eff'']` where `eff''` is scrutinee effect
- Was: `xi : tau [purify eff]`

### 6. Chk_let/take/lettuple: binding at `purify eff'` (RHS effect)
- Spec: `eff_b = purify eff'` where `eff'` is synthesized RHS effect
- Was: `eff_b = purify eff` (ambient)

### 7. Chk_iter: initial expression at `pure`
- Spec: `synth ce1 at [pure]`
- Was: `synth ce1 at eff`

### 8. Syn_eq: join operand effects
- Spec: `eff = eff' join eff''`
- Was: `eff = eff` (ambient)

### 9. TWfG_App: wf not guarded for type args
- Spec: `CS; Phi |- Ai wf`
- Was: `type_guarded_list` on args
