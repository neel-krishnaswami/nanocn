# Simplified application rules via refined effect checking

## The Problem

Right now, the core types function and primitive calls with SYN_APP, SYN_CALL, SYN_SPECCALL, SYN_PURECALL, SYN_SPECAPP. All of these look like they can be unified. 

The main issue is a difference between function calls in computational and spec mode.

Suppose we have a function f : A → B [pure]. 

Within a computation (i.e, within a term which has effect pure or impure), the argument
e in a call f(e) must be pure – i.e., e <== A [pure]. 

Within a spec, (i.e., a term which has effect spec), the call f(e) is valid *both* if
e <== A [pure] and e <== A [spec]. Furthermore, if e <== A [pure], then f(e) ==> B [pure],
and if e <== A [spec], then f(e) ==> B [spec]. 

So to typecheck a call f(e) properly, we need to know *both* what the maximum allowed
effect is, *and* what the minimum effect of the call is. 

I was afraid that the typechecker would have a rat's nest of backtracking, but it's actually
worse: it simply doesn't implement the typing rules correctly at all! There's just one case,
and it does something plausible looking but wrong.

## The approach 

What we will do is to change the typing judgements so that they are of the form

* [Synthesis]   Σ_c; Γ ⊢_eff1 ce ==> A [eff2]
* [Checking]    Σ_c; Γ ⊢_eff1 ce <== A [eff2]

The moding is as follows: 

* For synthesis, 
  - Inputs: Σ_c, Γ, eff1, ce
  - Outputs: A, eff2  

* For checking, Σ_c, Γ, eff1, A, ce are *inputs*,  and eff2 are *outputs*. 
  - Inputs: Σ_c, Γ, eff1, ce, A
  - Outputs: eff2  

This means that the ML code should have a type approximately like: 
   
   synth : Sig.t -> Context.t -> Effect.t -> CoreExpr.t -> (Sort.t * Effect.t, error) result 
   check : Sig.t -> Context.t -> Effect.t -> CoreExpr.t -> Sort.t -> (Effect.t, error) result 

So *both* synthesis and checking take an upper bound on the effect as an argument, and infer a 
lower bound on the effect. 

## Rules

### Function calls

We now have a *single* typing rule for function calls that looks like this: 

1. f : A → B [eff1]   
2. eff1 ≤ eff0
3. eff2 = ⌊eff0⌋   
4. Σ_c; Γ ⊢_eff2 ce <== A [eff3]
5. eff4 = eff1 ∨ eff3 
————————————————————————————————————————————
Σ_c; Σ_c; Γ ⊢_eff0 ce ==> B [eff4]

What this does is as follows. We are checking a program with ambient effect eff0. Then, 

* In premise 1, we see that f has effect eff1 
* In premise 2, we check that eff1 is permitted at eff0. That is, if the ambient effect is pure, 
  then we can only call pure functions. In impure effect, we can call pure and impure functions.
  In spec effect, we can call pure and spec functions. 
* In premise 3, we compute the maximum allowed effect of the argument ce. If the ambient effect is
  pure or impure, then the argument must be pure. If the ambient effect is a spec, then the argument
  can be a spec or lower. 
* In premise 4, we check that the argument ce is bounded above by effect eff2, but has a lower
  bound effect of eff3. 
* In premise 5, we decide that the actual effect of the call eff4, is bounded below by the join of
  the spec of the function and its argument. So if eff1 is pure, and eff3 is spec, then eff4 is 
  spec. If eff1 is pure and eff3 is pure, then eff4 is pure. 

### Primitives

These should now be typed identically to function calls. 

### Literals

Σ_c; Γ ⊢_eff0 lit ==> int [pure]

Σ_c; Γ ⊢_eff0 blit ==> bool [pure]


### Variables 

x:_eff1 A ∈ Γ   eff1 ≤ eff0
——————————————————————————————————
Σ_c; Σ_c; Γ ⊢_eff0 x ==> τ [eff1]

Note how we check that that the variable's effect is permitted in the embient effect. 

### Annotations 

Σ_c; Γ ⊢_eff0 ce <== τ [eff1]
——————————————————————————————————
Σ_c; Γ ⊢_eff0 ce <== τ [eff1]

We remove specs from the syntax of type annotations. 

### Let-bindings 

Σ_c; Γ ⊢_eff0 ce1 ==> τ' [eff1]
eff2 = ⌊eff1⌋
Σ_c; Γ, x:_eff2 τ' ⊢_eff0 ce2 <== τ [eff3]
eff4 = eff1 ∨ eff3
————————————————————————————————————————————
Σ_c; Γ ⊢_eff0 let x = ce1; ce2 <== τ [eff4]

## Injection

This case just threads the effects through the judgement. 

### Case expressions

1. eff' = ⌊eff0⌋
2. Σ_c; Γ ⊢_eff' ce ==> D(τ1, ..., τn) [eff'']
3. D(a...) = {L:τ, ...}
4. L1 : τ1'' in D(τ1, ..., τn) in Σ ... Ln : τn'' in D(τ1, ..., τn) in Σ
5. Σ_c; Γ, x1:_eff'' τ1'' [eff1] ⊢_eff0 ce1 <== τ ... Σ_c; Γ, xn:_eff'' τn'' ⊢_eff0 cen <== τ [effn]
6. eff = eff'' ∨ eff1 ... ∨ effn 
—————————————————————————————————————————————————————————————————————————————————————
Σ_c; Γ ⊢_eff0 case ce of {L1 x1 -> ce1, ...} <== τ [eff]

### if-then-else

1. eff' = ⌊eff0⌋
2. Σ_c; Γ ⊢_eff' ce1 <== bool [eff1]
3. Σ_c; Γ ⊢_eff0 ce2 <== τ [eff2]
4. Σ_c; Γ ⊢_eff0 ce3 <== τ [eff3]
5. eff = eff1 ∨ eff2 ∨ eff3 
——————————————————————————————————————————————————————
Σ_c; Γ ⊢_eff0 if ce1 then ce2 else ce3 <== τ [eff2]


### Tuples

Σ; Γ ⊢_eff0 ce1 <== τ1 [eff1] ... Σ; Γ ⊢_eff0 cen <== τn [effn]
eff' = eff1 ∨ ... ∨ effn 
——————————————————————————————————————————————————————————————————
Σ; Γ ⊢_eff0 (ce1, ..., cen) <== (τ1 * ... * τn) [eff']

### Let-tuple

Let-tuple treats effects similarly to the let-binding rule.

### Iter 

1. impure ≤ eff0 
2. type D(a, b) = {Next:a | Done: b} ∈ Σ_c 
3. Σ_c; Γ ⊢_pure ce1 ==> A [pure] 
4. Σ_c; Γ ⊢_impure ce2 <== D(A,B) [eff1] 
——————————————————————————————————————————————————–
Σ_c; Γ ⊢_eff0 iter(x = ce1){ ce2 } <== B [impure]

## Return

Like injection, return just threads the effects through the judgement.  

### Take 

Take should treat effects similarly to the let-binding rule. 


### Chk-Syn switching


Σ_c; Γ ⊢_eff0 ce → τ [eff1]
——————————————————————————————
Σ_c; Γ ⊢_eff0 ce ⇐ τ [eff1]

We no longer do any subeffecting. 



## Programs 

We retain effect declarations on programs, since the declaration gives
us the upper bound we pass to the typechecking judgement. Typechecking computes a lower 
bound, but we ignore it. 


### Surface program elaboration

The judgements are now

* [Synthesis]   Σ_c; Γ ⊢_eff1 se ==> A [eff2] ↝ ce
* [Checking]    Σ_c; Γ ⊢_eff1 se <== A [eff2] ↝ ce

Each surface rule checking and synthesis rule should handle effects identically to the 
core rule. 

### Pattern matching. 

This is the biggest change. The main coverage judgement is now: 

Σ_c; Γ ⊢_eff y1, ..., yn; M ⇐_eff'' τ [eff_body] ↝ ce 

Where eff'' is the effect of the variabless, eff is the upper bound on 
the effects of the branches, and eff_body is the synthesized lower bound on the effects
of the bodies. 

The case rule will now be: 

1. eff' = ⌊eff⌋
2. Σ_c; Γ ⊢_eff' se ↝ τ' [eff''] ↝ ce
3. Σ_c; Γ ⊢_eff 
4. M = pat1:τ' → [][]se1 | ... | pat1:τ' → [][]sen
5. Σ_c; Γ ⊢_eff y; M ⇐_eff'' τ [eff_body] ↝ ce'
6. eff_final = eff'' ∨ eff_body
————————————————————————————————————————————————————————————————————————————————————————
Σ_c; Γ ⊢_eff case se of {pat1 → se1 ... patn → sen} ⇐ τ [eff_final] ↝ let y = ce; ce'


The let- and take- rules will invoke the coverage judgement similarly. 


The rules for coverage Σ_c; Γ ⊢_eff y1, ..., yn; M ⇐_eff'' τ [eff_body] ↝ ce 
invoke the M/* ↝^y_eff M' judgements. The eff passed to them should always be eff''. 





