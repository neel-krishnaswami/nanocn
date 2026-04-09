# Plan: Unified kind checking and explicit substitutions in syntax.ott

## Stage 1: Unify type and sort well-formedness via kinds

### Current state

There are three separate well-formedness judgements:

- `CS ; Φ ⊢ A wf` (type_wf, TWf_) — types only, uses `type D(...) in CS`
- `CS ; Φ ⊢ τ wf` (sort_wf, SWf_) — sorts only, uses `sort D(...) in CS`, adds `Pred` rule
- `CS ; Φ ; D' ⊢ A guarded` (type_guarded, TWfG_) — guardedness for type decls

The type variable context `Φ` is a flat list of names with no kind annotations,
and sorts and types are separate grammars.

### Changes

#### 1.1. Add kind grammar

```
kind ::= type | sort
```

With subkinding `type ≤ sort`, `type ≤ type`, `sort ≤ sort`.

#### 1.2. Extend the type variable context with kinds

Replace:

```
Φ ::= · | Φ, a
```

With:

```
Φ ::= · | Φ, a : kind
```

#### 1.3. Replace type_wf and sort_wf with a single judgement

Replace both `CS ; Φ ⊢ A wf` and `CS ; Φ ⊢ τ wf` with:

```
CS ; Φ ⊢ τ : kind
```

Rules (from explicit-substitutions.md):

```
a:kind' ∈ Φ    kind' ≤ kind
————————————————————————————
CS ; Φ ⊢ a : kind

CS ; Φ ⊢ τi : kind  for i ∈ [1..n]
————————————————————————————————————
CS ; Φ ⊢ (τ1, ..., τn) : kind

————————————————————
CS ; Φ ⊢ Int : kind

————————————————————
CS ; Φ ⊢ Bool : kind

CS ; Φ ⊢ τ : sort
————————————————————
CS ; Φ ⊢ Pred τ : sort

CS ; Φ ⊢ τ : type
————————————————————
CS ; Φ ⊢ Ptr τ : kind

type D(a1, ..., ak) = {...} ∈ CS    n = k
CS ; Φ ⊢ τi : type  for i ∈ [1..n]
——————————————————————————————————————
CS ; Φ ⊢ D(τ1, ..., τn) : type

sort D(a1, ..., ak) = {...} ∈ CS    n = k
CS ; Φ ⊢ τi : sort  for i ∈ [1..n]
——————————————————————————————————————
CS ; Φ ⊢ D(τ1, ..., τn) : sort
```

Key points:
- `TVar` uses subkinding: a variable at kind `type` can be used at kind `sort`
- `Ptr` requires its argument to be a type (not a sort — no `Ptr (Pred τ)`)
- `App` dispatches on whether `D` was declared as `sort` or `type`
- `Int`, `Bool`, tuples are polymorphic in kind

#### 1.4. Update guardedness to use kinds

Change the judgement signature to carry the full applied form:

```
CS ; Φ ; D'(a1, ..., an) ⊢ τ guarded
```

This makes the parameters available for the `Ptr` rule. Operating on the
unified sort grammar `τ` instead of the type grammar `A`. Rules are the same
structurally, but:

- `Ptr` rule: premise changes to `CS, type D'(a1,...,an) = {} ; Φ ⊢ τ : type`
  (bug fix from explicit-substitutions.md — adds D' back under Ptr so
  recursive occurrences are permitted there)
- `App` rule: checks `D ≠ D'` and `CS ; Φ ⊢ τi : type`

#### 1.5. Update sort/type declaration well-formedness

Sort declarations check at kind `sort`:

```
CS, sort D(a1,...,an) = {}; a1:sort, ..., an:sort ⊢ τi : sort  for i ∈ [1..n]
——————————————————————————————————————————————————————————————————————————————
CS ⊢ sort D(a1,...,an) = {L1 : τ1 | ... | Ln : τn} ok
```

Type declarations use guardedness (which now checks at kind `type`):

```
CS; a1:type, ..., an:type; D(a1,...,an) ⊢ τi guarded  for i ∈ [1..n]
——————————————————————————————————————————————————————————————————————
CS ⊢ type D(a1,...,an) = {L1 : τ1 | ... | Ln : τn} ok
```

Note: type declaration omits `D` from `CS` (bug fix from explicit-substitutions.md).

#### 1.6. Update all rules that invoke type/sort well-formedness

Search for all uses of `TWf_` and `SWf_` in premises and update them to use
the new `CS ; Φ ⊢ τ : kind` form with the appropriate kind.

#### 1.7. Remove the separate type grammar

Since types are now just sorts at kind `type`, the separate grammar `A, B, C`
can be eliminated. All rules that formerly used `A` now use `τ` with a kind `type`
annotation where needed. The signature entries for `type` declarations change from
`L : A` to `L : τ`, with the well-kindedness guaranteed by the declaration rule.


## Stage 2: Add explicit substitution rules

### 2.1. Add substitution grammar

```
γ ::= · | γ, (ce/x) | γ, (τ/a)
```

#### 2.2. Add substitution well-formedness judgement

```
CS ; Γ ⊢ γ : Γ'
```

Rules:

```
————————————
CS ; Γ ⊢ · : ·

CS ; Γ ⊢ γ : Γ'    CS ; Γ ⊢[eff] e ⟹ τ'    τ' = τ
——————————————————————————————————————————————————————
CS ; Γ ⊢ (γ, e/x) : (Γ', x:τ[eff])

CS ; Γ ⊢ γ : Γ'    CS ; Γ ⊢ τ : kind
——————————————————————————————————————
CS ; Γ ⊢ (γ, τ/a) : (Γ', a:kind)
```

### 2.3. Add substitution application formulae

Define application `[γ]τ` and `[γ]ce` as formula-level operations (not new
judgements), following the homomorphic definitions in explicit-substitutions.md:

- `[γ]a = γ(a)`
- `[γ]Int = Int`, `[γ]Bool = Bool`
- `[γ](τ1, ..., τn) = ([γ]τ1, ..., [γ]τn)`
- `[γ](Pred τ) = Pred([γ]τ)`, `[γ](Ptr τ) = Ptr([γ]τ)`
- `[γ]D(τ1, ..., τn) = D([γ]τ1, ..., [γ]τn)`
- `[γ]x = γ(x)`, `[γ]n = n`, `[γ]b = b`
- Binders extend: `[γ](let x = ce1; ce2) = let x = [γ]ce1; [γ, x/x]ce2`
- etc. for case, iter, take, return

### 2.4. Add identity substitution

```
id(·) = ·
id(Γ, x:τ[eff]) = id(Γ), x/x
id(Γ, a:kind) = id(Γ), a/a
```

### 2.5. Add composition

```
γ0; (·) = ·
γ0; (γ1, e/x) = (γ0;γ1, [γ0]e/x)
γ0; (γ1, τ/a) = (γ0;γ1, [γ0]τ/a)
```

### 2.6. Redefine single-variable substitution

The existing `[ce/x]` notation in proof sorts and the `subst` formula become
shorthand for explicit substitution application:

- `[ce/x]Pf` means `[id(Γ), ce/x]Pf`
- `subst τ1/a1, ..., τn/an in τ'` means `[·, τ1/a1, ..., τn/an]τ'`

### 2.7. State well-formedness properties

These are the key metatheoretic properties to state (not prove in Ott, but
document as expected invariants):

- `CS ; Γ ⊢ id(Γ) : Γ`
- `[id(Γ)]τ = τ` and `[id(Γ)]e = e`
- If `CS ; Γ ⊢ γ : Γ'` and `CS ; Γ' ⊢ τ : kind` then `CS ; Γ ⊢ [γ]τ : kind`
- If `CS ; Γ ⊢ γ : Γ'` and `CS ; Γ' ⊢[eff] ce ⟸ τ` then `CS ; Γ ⊢[eff] [γ]ce ⟸ [γ]τ`
- Composition is associative: `(γ0; γ1); γ2 = γ0; (γ1; γ2)`
- Composition is functorial: `[γ0; γ1]τ = [γ1]([γ0]τ)`
