# Adding refinement types to nanocn 

Next, we are going to add support refinement types to nanoCN. 

## Extending the signature. 

The core signature must contain the full definition of all spec functions. (We have already
done this.) 


## Updating the context

The context should now be extended with logical assumptions (terms of type bool), and 
resource aassumptions (pairs of terms of sort τ and Pred τ). Because resources are substructural,
resource assumptions are marked with a 0/1 usage flag indicating whether they can be used (1)
or have already been used (0). 

TODO: Update resource to hndle affine constraints introduced by exfalso. 

u ::= 0 | 1 | ?

κ ::= eff | log | res(u) 
X ::= τ | ϕ | ct@ct 

Δ ::= · | Δ, x:X [κ]

Within Δ, eff can only be in {pure, spec}. 


For usage, we can think of ? as 0 ∨ 1 – it can be either used or ignored. 

We can define a partial meet operation on it: 

u ⊓ u = u 
1 ⊓ ? = 1
? ⊓ 1 = 1
0 ⊓ ? = 0
? ⊓ 1 = 0


### Context Well-formedness

* Σ ⊢ Δ wf 

  - Inputs: Σ, Δ
  - Outputs: none

—————————
Σ ⊢ · wf 


Σ ⊢ Δ wf  eff ∈ {pure, spec}
——————————————————————————————
Σ ⊢ Δ, x:A [eff] wf


Σ ⊢ Δ wf    Σ; Δ ⊢ ct ⇐ bool
———————————————————————————————
Σ ⊢ Δ, x:ct [log] wf 


Σ ⊢ Δ wf   Σ; Δ ⊢ ct ⇒ pred τ   Σ; Δ ⊢ ct' ⇐ τ    
—————————————————————————————————————————————————
Σ ⊢ Δ, x:(ct @ ct') [res(u)]


### Resource usage in contexts

#### affinize: make resource usage optional 

aff(0) = 0 
aff(1) = ?
aff(?) = ?

affinize(·) = ·
affinize(Δ, x:τ[eff]) = affinize(Δ), x:τ[eff]
affinize(Δ, x:ct[log]) = affinize(Δ), x:ct[log]
affinize(Δ, x:ct@ct' [res(u)]) = affinize(Δ), x:ct@ct' [res(aff(u))]


#### zero: Checking that resources are used.

zero(Δ) holds when all of the resources are ignorable, and fails when some resources
are going to leak. 

———————
zero(·)


zero(Δ)
—————————————————
zero(Δ, x:τ[eff])


zero(Δ)
—————————————————
zero(Δ, x:ct[log])


zero(Δ)        u ∈ {0, ?}
——————————————————————————
zero(Δ, x:ct@ct' [res(u)])


### Context erasure 

There is an erasure judgement  |Δ| = Γ

|·|                 = ·
|Δ, res x:ᵘ(ct@ct)| = |Δ|
|Δ, log x:ϕ|        = |Δ|
|Δ, x:_eff τ]|      = |Δ|, x:_eff τ

We use this to typecheck any core terms occuring inside refined terms, using our existing
judgemental machinery. 

## SMT Constraints 

The grammar of SMT constraint problems is as follows: 

C ::= ⊤ 
   |  C ∧ C 
   | ∀x:τ. C 
   | ϕ → C 
   | ϕ 

They are checked for well-formedness relative to a signature Σ and erased context Γ: 

* Σ; Γ ⊢ C wf 

  - Inputs: Σ, Γ, C 
  - Outputs: none 

—————————————
Σ; Γ ⊢ T wf 


Σ; Γ ⊢ C1 wf   Σ; Γ ⊢ C1 wf
————————————————————————————
Σ; Γ ⊢ C1 ∧ C2 wf


Σ, Γ, x:τ[spec] ⊢ C wf
———————————————————————
Σ; Γ ⊢ ∀x:τ.C wf


Σ; Γ ⊢[spec] ϕ <== Bool [eff']    Σ; Γ ⊢ C wf
———————————————————————————————————————————————
Σ; Γ ⊢ ϕ → C wf


Σ; Γ ⊢[spec] ϕ <== Bool [eff'] 
———————————————————————————————
Σ; Γ ⊢ ϕ wf



The auxilliary function Δ ⇒ C = C' is defined as follows:
·              ⇒ C = C
(Δ, x:τ [eff]) ⇒ C = (Δ ⇒ ∀x:τ.C)
(Δ, log x:ϕ)   ⇒ C = (Δ ⇒ ϕ → C)
(Δ, x:ct@ct')  ⇒ C = (Δ ⇒ C)


The invariants we maintain about it is that if Σ ⊢ Δ wf and Σ; |Δ| ⊢ C wf then Σ; · ⊢ Δ ⇒ C wf. 
(This should be part of our implementation test suite!) 


## Refined terms 

Refined terms are terms, augmented with additional clauses for manipulating the proof state.

### Proof sorts/terms

I give the grammar of proof sorts as follows: 

    Pf ::= (x:ϕ [log]), Pf | (x : ct@ct [res]), Pf | (x:τ [eff]), Pf | · 

(This is not the same as Δ, since (1) it associates the other way around, and (2) resources
don't have a flag.)

So a proof sort represents a list of logical facts, resources, and logical and computational values.
The computational erasure of this is

    Comp(Pf) = Prod {Pf}
    
    Prod [A1; ...;An] = (A1, ..., An)
    
    {ϕ ⊗ Pf}       = {Pf}
    {ct@ct' ⊗ Pf}  = {Pf}
    {∃:τ. Pf}       = {Pf}
    {Σx:A. Pf}      = A :: {Pf}
    {I}             = []

So Comp(Pf) = A means that A is the computational content of the proof sort. 

We can also extract the binding content – the logical and computational bindings: 

    Bind(Γ; ·) = Γ
    Bind(Γ; x:τ[eff], Pf) = Bind(Γ; x:τ[eff], Pf)   
    Bind(Γ; x:ct[log], Pf) = Bind(Γ; Pf)
    Bind(Γ; x:ct@ct'[res]), Pf) = Bind(Γ; Pf)

Bind(Γ, Pf) adds the computational and spec variables bound in Pf to Γ. 

A refined function type has the shape: 

    RF ::= Pf1 ⊸ Pf2 [eff]

The variables in Pf1 will be in scope in Pf2, like in a dependent function space. Because
it mention terms, it also need a well-formedness check. 

* Σ; Γ ⊢[eff] Pf wf

  - Inputs: Σ, Γ, Pf 
  - Outputs: none

—————————————————
Σ; Γ ⊢[eff] · wf 


Σ; Γ, x:τ[eff] ⊢ Pf wf  eff ∈ {spec, ⌊eff0⌋}
————————————————————————————————————————————
Σ; Γ ⊢[eff0] x:τ [eff], Pf wf


Σ; Γ ⊢[spec] ct ⇐ bool [eff]    Σ; Γ ⊢ Pf wf    
———————————————————————————————————————————————————
Σ; Γ ⊢ x:ct [log], Pf wf


Σ; Γ ⊢[spec] ct' ⇒ τ [eff']  Σ; Γ ⊢[spec] ct ⇐ pred τ [eff]   Σ; Γ ⊢[eff0] Pf wf   
———————————————————————————————————————————————————————————————————————————————————
Σ; Γ ⊢ x:(ct @ ct') [res], Pf


Σ; · ⊢[pure] Pf1 wf    Σ; Bind(·; Pf1) ⊢[eff] Pf2 wf   
—————————————————————————————————————————————————————
Σ ⊢ Pf1 ⊸ Pf2 [eff] wf 


These rules ensure that if a refined function has sort spec, it can only returns logical
and resource results – it cannot return a computational (i.e., pure) value. However, it is
allowed to require computational values as arguments (for whatever reason). A refined
computational (pure or impure) function can receive and return any mix of sorts.

### Grammar of core terms 

The grammar of core refined patterns is given as follows: 

q ::= (x1, ..., xn)

We use these to avoid introducing variables of arbitrary proof sort
in the context.

We use core refined terms will be as follows: 

crt ::=
    | lit 
    | blit 
    | x
    | let q = crt1; crt2 
    | crt : Pf 
    | prim spine
    | f spine 
    | (spine)
    | iter(q = crt1) { crt2 } 
    | if[a] ce then crt1 else crt2
    | case[a] ce of {L1 x1 -> crt1 | ... } 
    | pcmd crt 

spine ::= ct, spine | crt, spine | ·

There are seven typechecking judgements, organized as follows: 

* Type/term equality: Σ; Δ ⊢ Pf1 = Pf2 ↝ C 

  - Inputs: Σ, Δ, Pf1, Pf2
  - Outputs: C 

* Context merge : Σ ⊢ Δ0 ⊓ Δ1 = Δ

  - Inputs: Σ, Δ0, Δ1
  - Outputs: Δ  (compute the usage bounds)

* Variable lookup: Σ; Δ ⊢[eff] x ==> Pf ⊣ Δ'

  - Inputs: Σ, Δ, eff0, crt1 
  - Outputs: Pf, eff1, Δ', C

* Pattern matching: Σ; Γ ⊢ q : Pf ⊣ Δ

  - Inputs: Σ, Γ, q, P
  - Outputs: Δ  (types for the new q-bindings to add to the context) 

* Synthesis: Σ; Δ ⊢[eff] crt1 ==> Pf  ⊣ Δ' / C

  - Inputs: Σ, Δ, eff, crt1 
  - Outputs: Pf, Δ', C 

* Checking: Σ; Δ ⊢[eff] crt2 <== Pf ⊣ Δ' / C 

  - Inputs: Σ, Δ, eff, crt1, Pf 
  - Outputs: Δ', C 

* Spine: Σ; Δ ⊢[eff] spine : F ⊸ Pf  ⊣ Δ' / C 

  - Inputs: Σ, Δ, eff, spine, F
  - Outputs: Pf, Δ', C 

* Tuple checking Σ; Δ ⊢[eff] spine : Pf ⊣ Δ' / C 

  - Inputs: Σ, Δ, eff, spine, F
  - Outputs: Δ', C 



1. The decision to emit constraints rather than solve them on the fly is 
   basically so tha we can statically check the rough well-formedness
   of a program *before* we try solving any constraints. 

   In reality, we probably want to first check well-formedness, and
   then solve constraints as we go. We probably also want an option to
   turn certain constraints problems ininvolve non-decidable theories
   (like multiplication). Possibly we also want the option to generate
   proofs from CVC5/Z3, and then cache them for later reuse.

2. Why is there no output effect? The reason we don't have it is that
   that Pf already annotates each component with whether it is a value or 
   a spec sort, and refined terms can have spec parts appear freely within
   them.

   We still need an input effect, to prevent specs from calling impure functions, 
   so we can pass spec arguments to pure functions. In pure/impure code, 
   a spec effect means all its bindings must be demoted to spec.

3. Look at the Call rule in the unrefined type system. Possibly the output effect
   is simply not needed! 

#### Type equality 

——————————————————
Σ; Δ ⊢ · = · ↝ ⊤


τ = τ'    Σ; Δ, x:τ[eff] ⊢ Pf1 = [y/x]Pf2 ↝ C
————————————————————————————————————————————————————
Σ; Δ ⊢ (x:τ[eff], Pf1) = (y:τ'[eff], Pf2) ↝ ∀x:τ.C 


Σ; Δ ⊢ Pf1 = Pf2 ↝ C 
———————————————————————————————————————————————————————————————————
Σ; Δ ⊢ (x:ct1 [log], Pf1) = (y:ct2 [log], Pf2) ↝ (ct1 = ct2) ∧ C


Σ; Δ ⊢ Pf1 = Pf2 ↝ C 
——————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢ (x:(ct1@ct2) [res], Pf1) = (y:(ct1'@ct2') [rest], Pf2) 
↝ (ct1 = ct1' ∧ ct2 = ct2') ∧ C


#### Context merge

 

———————————————————
Σ; · ⊓ · = ·


Σ ⊢ Δ0 ⊓ Δ1 = Δ
——————————————————————————————————————————————————————
Σ; (Δ0, x:τ[eff]) ⊓ (Δ1, x:τ[eff]) = (Δ, x:τ[eff]) 


Σ ⊢ Δ0 ⊓ Δ1 = Δ
———————————————————————————————————————————————————————
Σ; (Δ0, x:ct[log]) ⊓ (Δ1, x:ct[log])


Σ ⊢ Δ0 ⊓ Δ1 = Δ
———————————————————————————————————————————————————————
Σ; (Δ0, x:ct[log]) ⊓ (Δ1, x:ct[log])


Σ ⊢ Δ0 ⊓ Δ1 = Δ    u = u0 ⊓ u1
————————————————————————————————————————————————————————————————————————————————
Σ; (Δ0, x:ct1@ct2[res(u0)]) ⊓ (Δ1, x:ct1@ct2[res(u1)]) = (Δ, x:ct1@ct2[res(u)])



#### Variable Lookup 

x ≠ y   Σ; Δ ⊢[eff1] x : X [κ] ⊣ Δ'
————————————————————————————————————————————
Σ; Δ, y:X[κ] ⊢[eff1] x : X [κ] ⊣ Δ', y:X[κ]


————————————————————————————————————————————————————————————
Σ; Δ, x : ϕ [log] ⊢[eff1] x : ϕ [log] ⊣ Δ, x : ϕ [log]


eff0 ≤ eff1 
————————————————————————————————————————————————————————
Σ; Δ, x : τ [eff0] ⊢[eff1] x : τ [eff0] ⊣ Δ, x : τ [eff0]


u = 1
———————————————————————————————————————————————————————————————————————————————————————
Σ; Δ, x : (ct@ct') [res(u)] ⊢[eff1] x : (ct@ct') [res] ⊣ Δ, x : (ct@ct') [res(0)]


These are the basic lookup rules, except that resource lookup only succeeds if the usage
is 1, and accessing it sets the usage to 0. (IDEA: support fractions by permitting
partial ownership of variables.) 

#### Pattern matching


————————————————————————
Σ; Γ ⊢ () : · ⊣ ·


Σ; Γ, x:τ[eff]] ⊢ q : [x/y]Pf ⊣ Δ
—————————————————————————————————————————————
Σ; Γ ⊢ (x,q) : (y:τ[eff], Pf) ⊣ x:τ[eff], Δ


Σ; Γ ⊢ q : [x/y]Pf ⊣ Δ
————————————————————————————————————————————————————
Σ; Γ ⊢ (x,q) : (y:ct[log], [y/x]Pf) ⊣ x:ct[log], Δ


Σ; Γ ⊢ q : [x/y]Pf ⊣ Δ
————————————————————————————————————————————————————————————————
Σ; Γ ⊢ (x,q) : (y:ct@ct'[res], [y/x]Pf) ⊣ x:(ct@ct')[res(1)], Δ



#### Typechecking


————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] lit ==> (x:int[pure], u: x = lit) ⊣ Δ ↝ ⊤


—————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] blit ==> (x:bool[pure], u: x = blit) ⊣ Δ ↝ ⊤


Σ; Δ ⊢[eff] x : X [κ] ⊣ Δ'  
———————————————————————————————— 
Σ; Δ ⊢[eff] x ==> (x:X[κ]) ⊣ Δ'


Something slightly wonky here is that we're packing variables into single-element sequences,
because that's what all Pf types morally are. (Note to self: think about this!)

Σ; Δ0 ⊢[eff] crt ==> Pf' ⊣ Δ1
Σ; Δ1 ⊢ q : Pf' ⊣ Δ' 
Σ; Δ1, Δ' ⊢[eff] crt2 <== Pf ⊣ Δ2, Δ''
zero(Δ'')
————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff0] let q = crt1; crt2 <== Pf [eff4] ⊣ Δ2



Σ; Δ ⊢[eff] crt <== Pf ⊣ Δ' ↝ C 
————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (crt : Pf) ==> Pf ⊣ Δ' ↝ C'


Σ ⊢ f : Pf1 ⊸ Pf2 [eff']   eff' ∈ {⌊eff⌋, spec}    Σ; Δ ⊢[eff] crt : (Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ' ↝ C 
———————————————————————————————————————————————————————————————————————————————–————————————————
Σ; Δ ⊢[eff] f crt ==> Pf3 ⊣ Δ' ↝ C ∧ C'


prim : Pf1 ⊸ Pf2 [eff']   eff' ∈ {⌊eff⌋, spec}    Σ; Δ ⊢[eff] crt : (Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ' ↝ C 
———————————————————————————————————————————————————————————————————————————————–————————————————
Σ; Δ ⊢[eff] prim crt ==> Pf3 ⊣ Δ' ↝ C ∧ C'



Σ; |Δ| ⊢[spec] P ==> Pred (Step(A, B)) 
Σ; Δ' ⊢[pure] crt1 <== (a:A, u: P @ Next a) ⊣ Δ' ↝ C 
Σ; Δ', x : A [pure], u : P@(Next x) [res] ⊢[impure] crt2 <== (y:A+B. P@y) ⊣ Δ' ↝ C'
——————————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[impure] iter[P] ((x,u) = crt1) { crt2 } ==> (b:B[pure[, u : P @ Done b) ⊣ Δ' ↝ C ∧ ∀x:A.C'


eff' = ⌊eff⌋
Σ; |Δ| ⊢[eff'] ct ⇐ bool 
Σ; Δ, x : ct = true [log] ⊢[eff'] crt1 <== Pf ⊣ Δ1, x : ct = true ↝ C1
Σ; Δ, x : ct = false [log] ⊢[eff'] crt2 <== Pf ⊣ Δ2, x : ct = false  ↝ C2
Σ; |Δ| ⊢ Δ1 = Δ2
——————————————————————————————————————————————————————
Σ; Δ ⊢[eff] if[x] ct then crt1 else crt2 <== Pf ⊣ Δ1 
↝ (ct = true → C1) ∧
   (ct = false → C2) ∧
   C3


eff' = ⌊eff⌋
Σ; Δ ⊢[eff'] ct ⇒ D(σ1, ..., σk) 
Σ ⊢ L1:τ1 in D(σ1, ..., σk) ... Σ ⊢ Ln:τn in D(σ1, ..., σk) 
Σ, Δ, x:τ1, y = L1(x) ⊢ crt1 <== Pf ⊣ Δ1, x:τ1, y = L1(x) ↝ C1 ... 
   Σ, Δ, x:τn, y = L1(x) ⊢ crtn <== Pf ⊣ Δn, x:τn, y = Ln(x) ↝ Cn
Σ; |Δ| ⊢ Δ1 = Δ2 ... Σ; |Δ| ⊢ Δ(n-1) = Δn 
———————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] case[y] ct of {L1 x → crt1 | ... | Ln x → crtn} <== Pf ⊣ Δ1
↝ (∀x:τ1. ct = L1 x → C1) ∧
   ...
   (∀x:τn. ct = Ln x → Cn) 
   

Σ; Δ ⊢ crt ==> Pf1 ⊣ Δ' ↝ C1    Σ; |Δ| ⊢ Pf1 = Pf2 ↝ C2
————————————————————————————————————————————————————————————
Σ; Δ ⊢ (crt : Pf2) ==> Pf2 ⊣ Δ' ↝ C1 ∧ C2


#### Spine 


———————————————————————————————————————————————
Σ; Δ ⊢ · : (· ⊸ Pf) >> Pf ⊣ Δ ↝ ⊤


Σ; |Δ| ⊢[eff] ct <== τ 
Σ; Δ ⊢ spine : ([ct/x]Pf1 ⊸ [ct/x]Pf2) >> Pf3 ⊣ Δ' ↝ C
—————————————————————————————————————————————————————————————
Σ; Δ ⊢ (ct, spine) : (x:τ[eff], Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ' ↝ C 


Σ; Δ ⊢ spine : (Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ' ↝ C'
——————————————————————————————————————————————————————————————————————
Σ; Δ ⊢ (auto, spine) : (x:ct[log], Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ' ↝ ct ∧ C'


Σ; Δ ⊢ crt ⇐ (ct1@ct2 [res]) ⊣ Δ' ↝ C
Σ; Δ' ⊢ spine : (Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ'' ↝ C'
—————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢ (crt, spine) : (x:ct1@ct2[res], Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ' ↝ C ∧ C'



#### Tuple 

Σ; Δ ⊢[eff] spine : Pf ⊣ Δ' / C 


———————————————————————————————————————————————
Σ; Δ ⊢[eff] · : · ⊣ Δ / ⊤


Σ; |Δ| ⊢[eff] ct <== τ    Σ; Δ ⊢ spine : [ct/x]Pf ⊣ Δ' ↝ C 
—————————————————————————————————————————————————————————
Σ; Δ ⊢ (ct, spine) : (x:τ[eff], Pf) ⊣ Δ' ↝ C 


Σ; Δ ⊢[eff] crt ⇐ (x:ct[log]) ⊣ Δ' ↝ C 
Σ; Δ' ⊢ spine : Pf ⊣ Δ'' ↝ C 
—————————————————————————————————————————————————————————
Σ; Δ' ⊢ (crt, spine) : (x:ct[log], Pf) ⊣ Δ'' ↝ C ∧ C'


Σ; Δ ⊢[eff] crt ⇐ (x:ct@ct'[res]) ⊣ Δ' ↝ C 
Σ; Δ' ⊢[eff] spine : Pf ⊣ Δ'' ↝ C'
———————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (crt, spine) : (x:ct@ct'[res], Pf) ⊣ Δ' ↝ C ∧ C'


#### Proof commands


pcmd ::= auto | open-ret | make-ret | open-take | make-take | unfold[f,i]

The commands `pcmd` all range over a number of substructural proof term
lemmas, with the following schemas for manipulating the proof states. The
pcmd are intended to behave like: 

open-ret     : (u : return ct1@ct2)          ⊸ (x:τ, u : ct1 = ct2)   [spec]
make-ret     : (u : ct1 = ct2)               ⊸ (u : (return ct1)@ct2) [spec]
open-take    : (u : (take x = ct1; ct2)@ct3) ⊸ (x:τ, u : ct1@x, ct2@ct3) [spec]
make-take    : (x:τ, ct1@x, ct2@ct3)         ⊸ (u : (take x = ct1; ct2)@ct3) [spec]

These are all schematic, and so cannot be primitives/functions unless we add
type quantification.

unfold[f, i] : Pf                      ⊸ Pf  // unfold the i-th occurence of f in Pf 


Δ' = affinize(Δ)
—————————————————————————————————————
Σ; Δ ⊢[eff] exfalso <== Pf ↝ Δ' / ⊥  

(If we are in dead code, there are no constraints on resource usage.)



Σ; Δ ⊢[eff] crt ==> (x:return ct1@ct2 [res]) ⊣ Δ' ↝ C
—————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] open-ret crt ==> (x:τ[spec], u : ct1 = ct2 [log]) ⊣ Δ' ↝ C 


Σ; Δ ⊢[eff] crt ==> (x:return ct1@ct2 [res]) ⊣ Δ' ↝ C
——————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] close-ret crt ==> (u : ct1 = ct2 [log]) ⊣ Δ' ↝ C 


Σ; Δ ⊢[eff] crt ==> (u : (take x = ct1; ct2)@ct3 [res]) ⊣ Δ' ↝ C
—————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] open-take crt ==> (x:τ[spec], y : ct1@x [res], z : ct2@ct3 [res]) ⊣ Δ' ↝ C 


Σ; Δ ⊢[eff] crt <== (x:τ[spec], y : ct1@x [res], z : ct2@ct3 [res]) ⊣ Δ' ↝ C
—————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] close-take crt <== (u : (take x = ct1; ct2)@ct3 [res]) ⊣ Δ' ↝ C 



# OLD STUFF – IGNORE FOR NOW

The grammar of refined terms rt is as follows

rt ::= x | (rt, ..., rt) | (ct, rt) | pcmd rt | auto | exfalso | rt : Pf 


pcmd ::= auto | open-ret | make-ret | open-take | make-take | unfold[f,i]

The commands `pcmd` all range over a number of substructural proof term
lemmas, with the following schemas for manipulating the proof states. The
pcmd 

open-ret     : (u : return ct1@ct2)          ⊸ (x:τ, u : ct1 = ct2)   [spec]
make-ret     : (u : ct1 = ct2)               ⊸ (u : (return ct1)@ct2) [spec]
open-take    : (u : (take x = ct1; ct2)@ct3) ⊸ (x:τ, u : ct1@x, ct2@ct3) [spec]
make-take    : (x:τ, ct1@x, ct2@ct3)         ⊸ (u : (take x = ct1; ct2)@ct3) [spec]

These are all schematic, and so cannot be primitives/functions unless we add
type quantification.

unfold[f, i] : Pf                      ⊸ Pf  // unfold the i-th occurence of f in Pf 


The typing rules will be as follows: 

log x : ϕ ∈ Δ
——————————————————
Σ; Δ ⊢ x ==> ϕ ⊣ Δ ↝ ⊤ 


Δ = Δ0, res x :¹ (ct@ct'), Δ1
————————————————————————————————————————
Σ; Δ ⊢ x ==> ϕ ⊣ Δ0, res x:⁰(ct@ct'), Δ1


Σ; Δ1 ⊢ pf1 <== Pf1 ⊣ Δ1 ... Σ; Δn ⊣ pfn <== Pfn ⊣ Δ_(n+1)
————————————————————————————————————————————————————————
Σ; Δ ⊢ (pf1, ..., pfn) ⇐ Pf1 ⊗ ... ⊗ Pfn ⊣ Δ_(n+1)


Σ; Δ ⊢ ct ⇐ τ    Σ; Δ ⊢ pf : [ct/x]Pf
———————————————————————————————————————
Σ; Δ ⊢ pack(ct, pf) ⇐ ∃x:τ. Pf


Σ; Δ ⊢ crt ==> Pf1    Σ; Δ ⊢ pcmd : Pf1 ⊸ Pf2    Σ; Δ ⊢ 
——————————————————————————————————————————————
Σ; Δ ⊢ pcmd crt ==> Pf2 



Σ; Δ ⊢ open-ret : (return ct1@ct2) ⊸ (x:ct1 = ct2 [log]) 

Σ; Δ ⊢ make-ret : (u : ct1 = ct2 [log]) ⊸ (u : (return ct1)@ct2 [res])

Σ; Δ ⊢ open-take : (u : (take x = ct1; ct2)@ct3 [res]) ⊸ (x:τ[spec], u : ct1@x ⊗ ct2@ct3 [res])

Σ; Δ ⊢ make-take : (x:τ[spec], y : ct1@x [res], z: ct2@ct3 [res])  ⊸ (u : (take x = ct1; ct2)@ct3 [res])



————————————————————————
Σ; Δ ⊢ exfalso ⇐ P ↝ ⊥ 


—————————————————————
Σ; Δ ⊢ auto ⇐ ϕ ↝ ϕ


def(f(x) = e ∈ Σ)     Σ; Δ ⊢ unfold f @ i in Pf1 ↝ Pf2 
————————————————————————————————————————————————————————
Σ; Δ ⊢ unfold[f,i] pf : Pf1 ⊸ Pf2 


Σ; Δ ⊢ unfold f(x) = e @ i in Pf1 ↝ Pf2 is a judgement finding the i-th occurence of
f(t) within  (ordering the subterms left-to-right, depth-first order), and using the
definition f(x) = t' in Σ to rewrite f(t) to [t/x]t'. 






















### Refined judgements 




## Solver representation

I think the entire CN assertion language can fit inside SMT-LIB 2.7, *including* resource assertions. 
This looks like it can make it easier to simplify many things, because many manual simplifications
now come for free with the SMT equality machinery.  

### The Pred monad. 

We will declare a new sort for predicates. 

(declare-sort Pred 1)

(declare-sort-parameter A)
(declare-sort-parameter B)

(declare-fun return (A) (Pred A))
(declare-fun bind ((Pred A) (-> A (Pred B))) (Pred B))


;; We will need triggers for these, but I'm confident that they can be added in a non-decidability-breaking way. 

(assert (forall ((m (M A)))
  (= (bind m return) m)))

(assert (forall ((x A) (f (-> A (M B))))
  (= (bind (return x) f) (@ f x))))

(assert (forall ((m (M A)) (f (-> A (M B))) (g (-> B (M C))))
  (= (bind (bind m f) g)
     (bind m (lambda ((x A)) (bind (@ f x) g))))))



