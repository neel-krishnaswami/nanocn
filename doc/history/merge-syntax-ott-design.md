# Design: Merging syntax.ott Steps 3–7

## Overview

Steps 1–2 of the merging plan are done (three-level effects, remove `stau`).
Steps 3–7 merge the expression syntax, signatures, typing rules, elaboration,
and program declarations into unified judgements.

## Step 3: Merge expression syntax

### Core expressions

Merge `ce` (computation) and `ct` (assertion) into one `ce` grammar.

**Shared forms** (already in both):
- Var, IntLit, BoolLit, Let, Tuple, LetTuple, Inject/Con, Case, If, Call, App/Prim

**Comp-only forms** (keep in merged `ce`):
- `iter (x = ce1) { ce2 }` — iteration

**Assertion-only forms** (add to merged `ce`):
- `ce1 == ce2` — equality (spec effect)
- `ce1 && ce2` — conjunction (spec effect)
- `not ce` — negation (spec effect)
- `Own [tau]` — ownership predicate (spec effect)
- `take x = ce1 ; ce2` — resource take (spec effect)
- `return ce` — return (spec effect)
- `f` — spec constant (spec effect)

**Annotation unification**: `ce : A [eff]` and `ct : tau` become `ce : tau [eff]`.

**Case syntax**: Unify to `case ce of { ... }` (with `of`).

### Surface expressions

Merge `se` (computation) and `st` (assertion) into one `se` grammar.
Same structure as core, but with patterns in Let, Case, Iter, Take.
`Own` is core-only (not in surface syntax).

### Remove

- `ct` (core assertion) — merged into `ce`
- `st` (surface assertion) — merged into `se`
- `chi` (sort schemes) — no longer needed
- `cbinds` — merged into `binds`
- `CEC` — merged into `E`
- `cmbranches` — merged into `mbranches`

### Pattern matching infrastructure

Unify into one set:
- `binds` uses `tau` (already does)
- `E` (eval context): `hole | let x = ce ; E` (using merged `ce`)
- `mbranches`: `binds --> [G] [E] se` (using merged `se` and `E`)

## Step 4: Merge signatures

Current:
```
S , f : F                              -- comp function (F = A -> B [eff])
S , spec f : tau -> tau'               -- spec function
S , spec f : tau                       -- spec constant
```

After merge:
```
S , f : F                              -- ALL functions (F = tau -> tau' [eff])
S , spec f : tau                       -- spec constants only
```

Spec functions become `f : tau -> tau' [spec]`, i.e., normal `f : F` entries.
The `F` grammar changes from `A -> B [eff]` to `tau -> tau' [eff]`.

## Step 5: Merge core typing rules

### Key design decision: unified variable binding

Contexts use ONE binding form: `G, x : tau [eff]` where eff is pure or spec.

```
G ::=
  | empty
  | G , x : tau [ eff ]
  | G , G'
```

**Invariant**: `eff` is never `impure`. Variables are VALUES — once computed,
accessing them is pure (comp) or spec. The `impure` effect is about computations,
not about the values they produce.

A `purify` helper judgement computes the binding effect from the checking effect:
- `purify(pure) = pure`
- `purify(impure) = pure`
- `purify(spec) = spec`

This eliminates the need for comp/spec variants of binding rules (Let, LetTuple,
Case, Take, strip_var, etc.). ONE rule handles both modes.

### Synth rules (unified judgement `S ; G |- ce ==> tau [eff]`)

| Form | Effect | Notes |
|------|--------|-------|
| IntLit | pure | |
| BoolLit | pure | |
| Var | eff | `x : tau [eff] in G`, eff is pure or spec |
| Annot | eff | from annotation |
| App (prim) | eff | from prim's effect |
| Call (comp) | eff | arg checked at pure |
| Call (spec) | spec | arg checked at spec |
| PureCall | spec | pure function called from spec context |
| Eq (==) | spec | spec-only form |
| And (&&) | spec | spec-only form |
| Not | spec | spec-only form |
| Own | spec | synthesizes `pred tau` |
| Const | spec | spec constant lookup |
| SpecApp | spec | pure prim used in spec context |

### Check rules (unified judgement `S ; G |- ce <== tau [eff]`)

All rules are single (no comp/spec variants needed):
- **Let**: binds `x : tau [purify(eff)]`
- **LetTuple**: binds vars at `purify(eff)`
- **Case**: unified ctor lookup, binds vars at `purify(eff)`
- **Tuple**: uniform
- **Inject**: uniform (unified constructor lookup)
- **Iter**: always impure, binds at `purify(impure) = pure`
- **If**: uniform
- **Return**: spec-only (`return ce <== pred tau [eff]`)
- **Take**: binds at `purify(eff)`, spec-only via `pred` type
- **Sub**: `ce ==> tau [eff'], eff' <= eff`

### PureCall / SpecApp pattern

A pure comp function (or prim) can be called from spec context:
- The arg is checked at spec (not pure), so spec variables can be passed
- The result has spec effect (not pure), since the context is spec

Three rules for function calls:
1. **call**: `f : F in S`, arg at pure → result at eff_f
2. **speccall**: `f : tau -> tau' [spec]`, arg at spec → result at spec
3. **purecall**: `f : A -> B [pure]`, arg at spec → result at spec

Similarly for prims:
1. **app**: `prim : A -> B [eff]`, arg at pure → result at eff
2. **specapp**: `prim : A -> B [pure]`, arg at spec → result at spec

### Constructor lookup unification

Unify `ctor_lookup` and `type_ctor_lookup` into one judgement
`L : tau in D(tau1,...,taui) in S` with two rules:
- Sort case: looks up `sort D(...)`, uses sort substitution
- Type case: looks up `type D(...)`, uses type substitution (A <:: tau)

Similarly, unify full declaration lookup into
`D(a1,...,ak) = { L1:tau1 | ... | Ln:taun } decl_in S`.

### Remove

- `spec_prim_type` judgement — subsumed by `prim_type` + subeffecting
- `core_synth` / `core_check` — merged into `synth` / `check`
- `core_branch` — folded into case rules

## Step 6: Merge elaboration rules

One elaboration judgement:
```
S ; G |- se ==> tau [eff] ~~> ce     (synth)
S ; G |- se <== tau [eff] ~~> ce     (check)
```

Rules mirror the merged core typing, adding `~~> ce` translation.
Single rules for all forms (no comp/spec variants).

## Step 7: Merge program declarations

### Surface programs (`prog`)

All functions become clausal:
```
fun f : tau -> tau' [eff] = { pat1 -> se1 | .. | patn -> sen } prog
```

Spec definitions stay: `spec f : tau = se prog`

Sort/type declarations unchanged. Main becomes `main : tau [eff] = se`.

### Program typing rules

Three function declaration rules:
1. **purefun**: non-recursive, types must be `A`
2. **impurefun**: recursive, types must be `A`
3. **specfun**: recursive, types can be `tau` (sort)

Spec definitions: check body at spec effect.
Sort/type decls: unchanged.
Main: elaborate at checking effect.

## Potential issues

1. **Unified ctor lookup and subrules**: The type case of the unified ctor lookup
   produces `A` (type) where `tau` (sort) is expected. This works via `A <:: tau`.
   Ott should handle this implicitly. **Risk**: Ott might not handle the subrule
   embedding in judgement conclusions. If so, we keep separate lookups.

2. **No impure bindings invariant**: Enforced by `purify`. Every variable-binding
   rule uses `purify(eff)` to compute the binding effect. Since `purify` never
   returns `impure`, the invariant holds by construction.

3. **Spec-only forms**: Forms like `return`, `take`, `Own`, `Eq`, etc. are restricted
   to spec contexts by their types (`pred tau`) or by synthesizing `spec` effect.
   No additional side conditions needed — the effect system enforces it.

4. **Pattern matching with effects**: `strip_var`, `spec_con`, `expand_tup` take
   an `eff_b` parameter (the purified binding effect). The `coverage` rule computes
   `eff_b = purify(eff)` and threads it through.
