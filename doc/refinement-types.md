# Adding refinement types to nanocn 

Next, we are going to add support refinement types to nanoCN. 

## Extending the signature. 

The core signature must contain the full definition of all spec functions. (We have already
done this.) 


## Updating the context

The context should now be extended with logical assumptions (terms of type bool), and 
resource aassumptions (pairs of terms of sort τ and Pred τ). Because resources are substructural,
resource assumptions are marked with a 0/1 usage flag indicating whether they can be used (1),
have already been used (0), or can optionally be used (?). 

u ::= 0 | 1 | ?

κ ::= eff | log | res(u) 
X ::= τ 
   | ce          (ce : Bool)
   | ce@ce'      (ce : Pred τ, ce' : τ)


Δ ::= · | Δ, x:X [κ]

Within Δ, eff can only be in {pure, spec}. 

For usage, we can think of ? as 0 ∨ 1 – it can be either used or ignored. 

We can define a partial meet operation on usages: 

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


Σ ⊢ Δ wf    Σ; |Δ| ⊢ ce ⇐ bool
———————————————————————————————
Σ ⊢ Δ, x:ce [log] wf 


Σ ⊢ Δ wf   Σ; |Δ| ⊢ ce ⇒ pred τ   Σ; |Δ| ⊢ ce' ⇐ τ    
———————————————————————————————————————————————————
Σ ⊢ Δ, x:(ce @ ce') [res(u)]


### Resource usage in contexts

#### affinize: make resource usage optional
 
aff(0) = 0 
aff(1) = ?
aff(?) = ?

affinize(·) = ·
affinize(Δ, x:τ[eff]) = affinize(Δ), x:τ[eff]
affinize(Δ, x:ce[log]) = affinize(Δ), x:ce[log]
affinize(Δ, x:ce@ce' [res(u)]) = affinize(Δ), x:ce@ce' [res(aff(u))]


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
zero(Δ, x:ce[log])


zero(Δ)        u ∈ {0, ?}
——————————————————————————
zero(Δ, x:ce@ce' [res(u)])


### Context erasure 

There is an erasure judgement  |Δ| = Γ

|·|                 = ·
|Δ, res x:ᵘ(ce@ce)| = |Δ|
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

·              	       ⇒ C = C
(Δ, x:τ [eff]) 	       ⇒ C = (Δ ⇒ ∀x:τ.C)
(Δ, x:ϕ [log])	       ⇒ C = (Δ ⇒ ϕ → C)
(Δ, x:ce@ce' [res(u)]) ⇒ C = (Δ ⇒ C)


The invariants we maintain about it is that if Σ ⊢ Δ wf and Σ; |Δ| ⊢ C wf then Σ; · ⊢ Δ ⇒ C wf. 
(This should be part of our implementation test suite!) 


## Refined terms 

Refined terms are terms, augmented with additional clauses for manipulating the proof state.

### Proof sorts/terms

I give the grammar of proof sorts as follows: 

    Pf ::= (x:ϕ [log]), Pf | (x : rs [res]), Pf | (x:τ [eff]), Pf | · 
    rs ::= ce@ce'
        |  (x:τ).ce     
 

(This is not the same as Δ, since (1) it associates the other way around, and (2) resources
don't have a flag.)

So a proof sort represents a list of logical faces, resources, and logical and computational values.
The computational erasure of this is

    Comp(Pf) = Prod {Pf}
    
    Prod [A1; ...;An] = (A1, ..., An)
    
    {x:ϕ[log], Pf}    = {Pf}
    {rs[res], Pf}     = {Pf}
    {x:τ[spec], Pf}   = {Pf}
    {x:A[pure], Pf}   = A :: {Pf}
    {·}               = []

So Comp(Pf) = A means that A is the computational content of the proof sort. 

We can also extract the binding content – the logical and computational bindings: 

    Bind(Γ; ·)                   = Γ
    Bind(Γ; x:τ[eff], Pf)        = Bind(Γ; x:τ[eff], Pf)   
    Bind(Γ; x:ce[log], Pf)       = Bind(Γ; Pf)
    Bind(Γ; x:ce@ce'[res]), Pf)  = Bind(Γ; Pf)
    Bind(Γ; x:(y:τ).ce[res], Pf) = Bind(Γ, y:τ; Pf)
    
Bind(Γ, Pf) adds the computational and spec variables bound in Pf to Γ. 

Finally, we can append Pf sorts to refined contexts Δ as follows:

PfToCtx(Δ; ·)                   = Δ
PfToCtx(Δ; x:τ[eff], Pf)        = PfToCtx(Δ, x:τ[eff]; Pf)
PfToCtx(Δ; x:ce[log], Pf)       = PfToCtx(Δ, x:ce[log]; Pf)
PfToCtx(Δ; x:ce@ce'[res], Pf)   = PfToCtx(Δ, x:ce@ce'[res(1)]; Pf)
PfToCtx(Δ; x:(y:τ).ce[res], Pf) = PfToCtx(Δ, y:τ, x:ce@y[res(1)]; Pf)

The invariant of this is that if Σ ⊢ Δ wf and Σ; |Δ| ⊢ Pf wf, then Σ ⊢ PfToCtx(Δ; Pf) wf.

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
Σ; Γ ⊢[eff] x:τ [eff], Pf wf


Σ; Γ ⊢[spec] ce ⇐ bool [eff]    Σ; Γ ⊢ Pf wf    
———————————————————————————————————————————————————
Σ; Γ ⊢ x:ce [log], Pf wf


Σ; Γ ⊢[spec] ce' ⇒ τ [eff']  Σ; Γ ⊢[spec] ce ⇐ pred τ   Σ; Γ ⊢[eff] Pf wf   
——————————————————————————————————————————————————————————————————————————————
Σ; Γ ⊢[eff] x:(ce @ ce') [res], Pf


Σ; Γ ⊢[spec] ce ⇐ pred τ [eff]   Σ; Γ, y:τ, x:ce@y ⊢[eff] Pf wf   
——————————————————————————————————————————————————————————————————————————————
Σ; Γ ⊢[eff] x:(y:τ).ce [res], Pf


Σ; · ⊢[eff] Pf1 wf    Σ; Bind(·; Pf1) ⊢[eff] Pf2 wf   
—————————————————————————————————————————————————————
Σ ⊢ Pf1 ⊸ Pf2 [eff] wf 


These rules ensure that if a refined function has sort spec, it can only operates on spec
and resource arguments/returns values – it cannot return any computational (i.e., pure) values.
A refined computational (pure or impure) function can receive and return both pure and spec 
arguments. Return types can can also depend upon the bindings of their inputs. 

### Grammar of core terms 

The grammar of core refined patterns is given as follows: 

qbase ::= x | (x, y)

q ::= (qbase1, ..., qbasen)

We use these to avoid introducing variables of arbitrary proof sort
in the context.

We use core refined terms will be as follows: 

crt ::=
    | let q = crt1; crt2 
    | crt : Pf 
    | prim spine
    | f spine 
    | (spine)
    | iter(q = crt1) { crt2 } 
    | if[a] ce then crt1 else crt2
    | case[a] ce of {L1 x1 -> crt1 | ... } 
    | exfalso | open-take rpf 

spine ::= ce, spine | rpf, spine | lpf, spine | · 

lpf ::= x | auto | unfold f(ce) | open-ret rpf | lpf : ϕ

rpf ::= x | make-ret lpf | make-take crt | rpf : ce@ce'


There are 12 typechecking judgements, organized as follows: 

* Synthesizing logical facts: Σ; Δ ⊢ lpf ==> ϕ ⊣ Δ' ↝ C 

  - Inputs: Σ, Δ, lpf
  - Outputs: ϕ, Δ', C 

* Checking logical facts: Σ; Δ ⊢ lpf <== ϕ ⊣ Δ' ↝ C 

  - Inputs: Σ, Δ, lpf, ϕ
  - Outputs: C, Δ' 

* Resource variable lookup: Σ; Δ ⊢ x : ce@ce' ⊣ Δ'

  - Inputs: Σ, Δ, x
  - Outputs: ce, ce', Δ'

* Synthesizing resource usage: Σ; Δ ⊢ rpf ==> ce@ce' ̣⊣ Δ' ↝ C 

  - Inputs: Σ, Δ, rpf, ce, ce' 
  - Outputs: ce@ce', Δ', C

* Checking resource usage:  Σ; Δ ⊢ rpf <== ce@ce' ⊣ Δ' ↝ C 

  - Inputs: Σ, Δ, ce, ce' 
  - Outputs: Δ', C 

* Type/term equality: Σ; Δ ⊢ Pf1 = Pf2 ↝ C 

  - Inputs: Σ, Δ, Pf1, Pf2
  - Outputs: C 

* Context merge : Σ ⊢ Δ0 ⊓ Δ1 = Δ

  - Inputs: Σ, Δ0, Δ1
  - Outputs: Δ (checks that the types are the same, and that the usage bounds are compatible)

* Pattern matching: Σ; Γ ⊢ q : Pf ⊣ Δ

  - Inputs: Σ, Γ, q, Pf
  - Outputs: Δ  (types for the new q-bindings to add to the context) 

* Synthesis: Σ; Δ ⊢[eff] crt ==> Pf ⊣ Δ' ↝ C

  - Inputs: Σ, Δ, eff, crt
  - Outputs: Pf, Δ', C 

* Checking: Σ; Δ ⊢[eff] crt <== Pf ⊣ Δ' ↝ C 

  - Inputs: Σ, Δ, eff, crt, Pf 
  - Outputs: Δ', C 

* Spine: Σ; Δ ⊢[eff] spine : F ⊸ Pf  ⊣ Δ' ↝ C 

  - Inputs: Σ, Δ, eff, spine, F
  - Outputs: Pf, Δ', C 

* Tuple checking Σ; Δ ⊢[eff] spine : Pf ⊣ Δ' ↝ C 

  - Inputs: Σ, Δ, eff, spine, F
  - Outputs: Δ', C 



1. The decision to emit constraints rather than solve them on the fly is 
   basically so tha we can statically check the rough well-formedness
   of a program *before* we try solving any constraints. 

   In reality, we probably want to first check well-formedness, and
   then solve constraints as we go. We probably also want an option to
   turn certain constraints problems involving non-decidable theories
   (like multiplication) and generate proof obligations. Possibly we also
   want the option to generate proofs from CVC5/Z3, and then cache them
   for later reuse.

#### Checking/synthesizing logical facts: 

———————————————————————
Σ; Δ ⊢ auto <== ϕ ⊣ Δ ↝ ϕ 


x:ce [log] ∈ Δ
——————————————————————————————
Σ; Δ ⊢ x ==> ce ⊣ Δ ↝ ⊤


Σ ⊢ f (x : τ) → τ' [eff] = ce_body
eff ≤ spec 
Σ; |Δ| ⊢[spec] ce <== τ 
——————————————————————————————————————————————————————
Σ; Δ ⊢ unfold f(ce) ==> (f(ce) = [ce/x]ce_body) ↝ ⊤


Σ; Δ ⊢ lpf ==> ϕ ⊣ Δ' ↝ C 
———————————————————————————————————
Σ; Δ ⊢ lpf <== ϕ' ⊣ Δ' ↝ C ∧ (ϕ → ϕ')


Σ; Δ ⊢ lpf <== ϕ ⊣ Δ' ↝ C
——————————————————————————————
Σ; Δ ⊢ (lpf : ϕ) ==> ϕ ⊣ Δ' ↝ C 


Σ; Δ ⊢ rpf ==> (return ce)@ce' ⊣ Δ' ↝ C
——————————————————————————————————————————
Σ; Δ ⊢ open-ret rpf ==> (ce = ce') ⊣ Δ' ↝ C 


#### Resource variable Lookup 

x ≠ y   Σ; Δ ⊢ x : ce@ce' ⊣ Δ'
————————————————————————————————————————————
Σ; Δ, y:X[κ] ⊢ x : ce@ce' ⊣ Δ', y:X[κ]


u ∈ {1, ?}
——————————————————————————————————————————————————————————————————————
Σ; Δ, x : (ce@ce') [res(u)] ⊢ x : (ce@ce') ⊣ Δ, x : (ce@ce') [res(0)]


These are the basic lookup rules, except that resource lookup only
succeeds if the usage is 1 or ?, and accessing it sets the usage to 0.

#### Checking/synthesizing resource facts: 

Σ; Δ ⊢[spec] x : ce@ce' [res] ⊣ Δ' 
————————————————————————————————————
Σ; Δ ⊢ x ==> ce@ce' ⊣ Δ' ↝ ⊤ 


Σ; Δ ⊢ lpf <== (ce1 = ce2) ⊣ Δ' ↝ C 
——————————————————————————————————————————————————————
Σ; Δ ⊢ make-ret lpf <== (return ce1)@ce2 ⊣ Δ' ↝ C 


Σ; Δ ⊢ crt <== (x:τ, y: ce1@x [res]; z: ce2@ce3 [res]) ⊣ Δ' ↝ C 
——————————————————————————————————————————————————————————————————
Σ; Δ ⊢ make-take crt <== (take x = ce1; ce2)@ce3 ⊣ Δ' ↝ C 


Σ; Δ ⊢ rpf <== ce@ce' ⊣ Δ' ↝ C
————————————————————————————————————————————
Σ; Δ ⊢ (rpf : ce@ce') ==> ce@ce' ⊣ Δ' ↝ C


Σ; Δ ⊢ rpf ==> ce1@ce1' ⊣ Δ' ↝ C
—————————————————————————————————————————————————————————————
Σ; Δ ⊢ rpf <== ce2@ce2' ⊣ Δ' ↝ C ∧ (ce1,ce1') = (ce2,ce2')




#### Type equality 

——————————————————
Σ; Δ ⊢ · = · ↝ ⊤


τ = τ'    Σ; Δ, x:τ[eff] ⊢ Pf1 = [y/x]Pf2 ↝ C
————————————————————————————————————————————————————
Σ; Δ ⊢ (x:τ[eff], Pf1) = (y:τ'[eff], Pf2) ↝ ∀x:τ.C 


Σ; Δ ⊢ Pf1 = Pf2 ↝ C 
———————————————————————————————————————————————————————————————————
Σ; Δ ⊢ (x:ce1 [log], Pf1) = (y:ce2 [log], Pf2) ↝ (ce1 = ce2) ∧ C


Σ; Δ ⊢ Pf1 = Pf2 ↝ C 
——————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢ (x:(ce1@ce1') [res], Pf1) = (y:(ce2@ce2') [res], Pf2) 
↝ (ce1 = ce2 ∧ ce1' = ce2') ∧ C


Σ; Δ, c:τ ⊢ [c/a]Pf1 = [c/b]Pf2 ↝ C 
——————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢ (x:(a:τ).ce [res], Pf1) = (y:(b:τ).ce' [res], Pf2) ↝ (ce = ce') ∧ ∀c:τ.C



#### Context merge

———————————————————
Σ; · ⊓ · = ·


Σ ⊢ Δ0 ⊓ Δ1 = Δ
——————————————————————————————————————————————————————
Σ; (Δ0, x:τ[eff]) ⊓ (Δ1, x:τ[eff]) = (Δ, x:τ[eff]) 


Σ ⊢ Δ0 ⊓ Δ1 = Δ
———————————————————————————————————————————————————————
Σ; (Δ0, x:ce[log]) ⊓ (Δ1, x:ce[log]) = Δ, x:ce[log]


Σ ⊢ Δ0 ⊓ Δ1 = Δ    u = u0 ⊓ u1
————————————————————————————————————————————————————————————————————————————————
Σ; (Δ0, x:ce1@ce2[res(u0)]) ⊓ (Δ1, x:ce1@ce2[res(u1)]) = (Δ, x:ce1@ce2[res(u)])


#### n-ary context merge 

Δs ::= · | Δ; Δs


Σ ⊢ Δ1 ⊓ Δ2 = Δ'    Σ ⊢ ⊓ (Δ'; Δs) = Δ
—————————————————————————————————————————
Σ ⊢ ⊓ (Δ1; Δ2; Δs) = Δ

———————————————
Σ ⊢ ⊓ (Δ) = Δ



#### Pattern matching

————————————————————————
Σ; Γ ⊢ () : · ⊣ ·


Σ; Γ, x:τ[eff]] ⊢ q : [x/y]Pf ⊣ Δ
—————————————————————————————————————————————
Σ; Γ ⊢ (x,q) : (y:τ[eff], Pf) ⊣ x:τ[eff], Δ


Σ; Γ ⊢ q : [x/y]Pf ⊣ Δ
————————————————————————————————————————————————————
Σ; Γ ⊢ (x,q) : (y:ce[log], Pf) ⊣ x:ce[log], Δ


Σ; Γ ⊢ q : [x/y]Pf ⊣ Δ
————————————————————————————————————————————————————————————————
Σ; Γ ⊢ (x,q) : (y:ce@ce'[res], Pf) ⊣ x:(ce@ce')[res(1)], Δ


Σ; Γ, x:τ ⊢ q : [x/z]Pf ⊣ Δ
—————————————————————————————————————————————————————————————————————
Σ; Γ ⊢ ((x,a),q) : (y:(z:τ).ce[res], Pf) ⊣ x:τ, a:(ce@x)[res(1)], Δ



#### Typechecking



Σ; Δ0 ⊢[eff] crt ==> Pf' ⊣ Δ1 ↝ C
Σ; Δ1 ⊢ q : Pf' ⊣ Δ' 
Σ; Δ1, Δ' ⊢[eff] crt2 <== Pf ⊣ Δ2, Δ'' ↝ C'
length(Δ1) = length(Δ2) 
length(Δ') = length(Δ'')
zero(Δ'')
————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff0] let q = crt1; crt2 <== Pf [eff4] ⊣ Δ2 ↝ C ∧ C'



Σ ⊢ f : Pf1 ⊸ Pf2 [eff']   
eff' ≤ eff 
eff'' = ⌊eff⌋
Σ; Δ ⊢[eff''] crt : (Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ' ↝ C 
———————————————————————————————————————————————————————————————————————————————–————————————————
Σ; Δ ⊢[eff] f crt ==> Pf3 ⊣ Δ' ↝ C


prim : Pf1 ⊸ Pf2 [eff']
eff' ≤ eff
eff'' = ⌊eff⌋
Σ; Δ ⊢[eff''] crt : (Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ' ↝ C 
———————————————————————————————————————————————————————————————————————————————–————————————————
Σ; Δ ⊢[eff] prim crt ==> Pf3 ⊣ Δ' ↝ C



Σ; |Δ| ⊢[spec] P ==> Pred (Step(A, B)) 
Σ; Δ' ⊢[pure] crt1 <== (a:A, u: P @ Next a) ⊣ Δ' ↝ C 
Σ; Δ', x : A [pure], u : P@(Next x) [res(1)] ⊢[impure] crt2 <== (y:A+B. P@y) ⊣ Δ' ↝ C'
——————————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[impure] iter[P] ((x,u) = crt1) { crt2 } ==> (b:B[pure[, u : P @ Done b) ⊣ Δ' ↝ C ∧ ∀x:A.C'


eff' = ⌊eff⌋
Σ; |Δ| ⊢[eff'] ce ⇐ bool 
Σ; Δ, x : ce = true [log] ⊢[eff'] crt1 <== Pf ⊣ Δ1, x : ce = true ↝ C1
Σ; Δ, x : ce = false [log] ⊢[eff'] crt2 <== Pf ⊣ Δ2, x : ce = false  ↝ C2
Σ ⊢ Δ1 ⊓ Δ2 = Δ'
——————————————————————————————————————————————————————
Σ; Δ ⊢[eff] if[x] ce then crt1 else crt2 <== Pf ⊣ Δ'
↝ (ce = true → C1) ∧ (ce = false → C2)


eff' = ⌊eff⌋
Σ; |Δ| ⊢[eff'] ce ⇒ D(σ1, ..., σk) 
Σ ⊢ L1:τ1 in D(σ1, ..., σk) ... Σ ⊢ Ln:τn in D(σ1, ..., σk) 
Σ, Δ, x:τ1, y = L1(x) ⊢ crt1 <== Pf ⊣ Δ1, x:τ1, y = L1(x) ↝ C1 ... 
   Σ, Δ, x:τn, y = L1(x) ⊢ crtn <== Pf ⊣ Δn, x:τn, y = Ln(x) ↝ Cn
Σ ⊢ ⊓ (Δ1; ... Δn) = Δ'
———————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] case[y] ce of {L1 x → crt1 | ... | Ln x → crtn} <== Pf ⊣ Δ'
↝ (∀x:τ1. ce = L1 x → C1) ∧
   ...
   (∀x:τn. ce = Ln x → Cn) 
   

Σ; |Δ| ⊢ Pf wf     Σ; Δ ⊢ crt <== Pf ⊣ Δ' ↝ C  
———————————————————–————————————————————————————
Σ; Δ ⊢ (crt : Pf) ==> Pf ⊣ Δ' ↝ C


#### Spine 


———————————————————————————————————————————————
Σ; Δ ⊢[eff] · : (· ⊸ Pf) >> Pf ⊣ Δ ↝ ⊤


Σ; |Δ| ⊢[eff] ce <== τ 
Σ; Δ ⊢[eff] spine : ([ce/x]Pf1 ⊸ [ce/x]Pf2) >> Pf3 ⊣ Δ' ↝ C
—————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (ce, spine) : (x:τ[eff], Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ' ↝ C 


Σ; Δ ⊢ lpf <== ce ⊣ Δ' ↝ C 
Σ; Δ' ⊢[eff] spine : (Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ'' ↝ C'
——————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (lpf, spine) : (x:ce[log], Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ'' ↝ C ∧ C'


Σ; Δ ⊢ rpf <== (ce1@ce2) ⊣ Δ' ↝ C
Σ; Δ' ⊢[eff] spine : (Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ'' ↝ C'
—————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (rpf, spine) : (x:ce1@ce2[res], Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ'' ↝ C ∧ C'


Σ; Δ ⊢ rpf ==> (ce@ce') ⊣ Δ' ↝ C
Σ; Δ' ⊢[eff] spine : ([ce'/y]Pf1 ⊸ [ce'/y]Pf2) >> Pf3 ⊣ Δ'' ↝ C'
—————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (rpf, spine) : (x:(y:τ).ce[res], Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ'' ↝ C ∧ C'


#### Tuple (Σ; Δ ⊢[eff] spine : Pf ⊣ Δ' ↝ C) 

———————————————————————————————————————————————
Σ; Δ ⊢[eff] · : · ⊣ Δ ↝ ⊤


Σ; |Δ| ⊢[eff] ce <== τ    Σ; Δ ⊢ spine : [ce/x]Pf ⊣ Δ' ↝ C 
—————————————————————————————————————————————————————————
Σ; Δ ⊢ (ce, spine) : (x:τ[pure], Pf) ⊣ Δ' ↝ C 

Σ; |Δ| ⊢[spec] ce <== τ    Σ; Δ ⊢ spine : [ce/x]Pf ⊣ Δ' ↝ C 
—————————————————————————————————————————————————————————
Σ; Δ ⊢ (ce, spine) : (x:τ[spec], Pf) ⊣ Δ' ↝ C 


Σ; Δ ⊢ lpf <== ce ⊣ Δ' ↝ C 
Σ; Δ' ⊢ spine : Pf ⊣ Δ'' ↝ C'
—————————————————————————————————————————————————————————
Σ; Δ ⊢ (lpf, spine) : (x:ce[log], Pf) ⊣ Δ'' ↝ C ∧ C'


Σ; Δ ⊢[eff] rpf <== ce@ce' ⊣ Δ' ↝ C 
Σ; Δ' ⊢[eff] spine : Pf ⊣ Δ'' ↝ C'
———————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (rpf, spine) : (x:ce@ce'[res], Pf) ⊣ Δ'' ↝ C ∧ C'


Σ; Δ ⊢[eff] rpf ==> ce@ce' ⊣ Δ' ↝ C 
Σ; Δ' ⊢[eff] spine : [ce'/y]Pf ⊣ Δ'' ↝ C'
———————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (rpf, spine) : (x:(y:τ).ce[res], Pf) ⊣ Δ'' ↝ C ∧ C'



#### Proof commands

Δ' = affinize(Δ)
—————————————————————————————————————
Σ; Δ ⊢[eff] exfalso <== Pf ↝ Δ' / ⊥  

(If we are in dead code, there are no more constraints on resource usage.)


Σ; Δ ⊢ rpf ==> (take x = ce1; ce2)@ce3) ⊣ Δ' ↝ C
—————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] open-take rpf ==> (x:τ[spec], y : ce1@x [res], z : ce2@ce3 [res]) ⊣ Δ' ↝ C 




* Commentary
   
   The proof commands range over a number of substructural proof term
   lemmas, with the following schemas for manipulating the proof states. The
   pcmd are intended to behave like: 
   
       open-ret     : (return ce1@ce2)              ⊸ (x:τ, u : ce1 = ce2)  
       make-ret     : (ce1 = ce2)                   ⊸ (return ce1)@ce2      
       open-take    : (take x = ce1; ce2)@ce3       ⊸ (x:τ, u : ce1@x, ce2@ce3) 
       make-take    : (x:τ, ce1@x, ce2@ce3)         ⊸ take x = ce1; ce2)@ce3
       unfold(f,τ)  : ()                            ⊸ (f(ce) = ce') 
   
   These are all schematic, and so cannot be primitives/functions unless
   we add type quantification. Furthermore, they live in different sorts
   – the unfold(f, t) command produces a logical fact, make-ret takes a
   logical fact and produces a resource, make-take takes a proof and
   produces a resource, and and open-ret and open-take consume resources,
   but produce full proofs. So they all live in different judgements! 


#### Core refined signatures: 

Σr ::= · | Σr, f:F | Σr, fun f(x:τ) → τ'[eff] = ce | Σr, f : RF 

First, we do lookup in a signature: 

f:RF ∈ Σr
——————————————————
Σr ⊢ f : RF 


f:τ → τ' [eff] ∈ Σr
———————————————————————————————
Σr ⊢ f :  (x:τ) ⊸ (y:τ') [eff]


fun f (x:τ) → τ' [eff] = ce ∈ Σr
—————————————————————————————————
Σr ⊢ f :  (x:τ) ⊸ (y:τ') [eff]

Next, we show how to erase refinements 

Comp(·)                              = ·
Comp(Σr, f:F)                        = Comp(Σr), f:F
Comp(Σr, fun f(x:τ) → τ'[eff] = ce) = Comp(Σr), fun f(x:τ) → τ'[eff] = ce
Comp(Σr, f : Pf1 ⊸ Pf2[eff])        = Comp(Σr), f : Comp(Pf1) → Comp(Pf2) [eff] 

Next, we give signature well-formedness ⊢ Σr wf 

———————
⊢ · wf 


⊢ Σr wf
——————————————————————————
⊢ Σr, f : τ → τ' [eff] wf 


⊢ Σr wf   Comp(Σr); x:τ ⊢[eff] ce <== τ'
—————————————————————————————————————————
⊢ Σr, fun f (x : τ) → τ' [eff] = ce wf 


⊢ Σr wf    Σ; · ⊢ RF wf 
——————————————————————————
⊢ Σr, f : RF wf 


#### Refined Programs: 

A refined program consists 

rprog ::= data D(a1, ..., ak) = {L1 : τ1 | ... | τn: τn} rprog
       |  type D(a1, ..., ak) = {L1 : A1 | ... | τn: An} rprog
       |  fun f(x : τ) → τ' [eff] = { e } rprog
       |  fun f(Pf1) → Pf2 [eff] = { crt } rprog
       |  main : Pf [eff] = crt 


We typecheck these programs with the Σr ⊢ rprog ⊣ Σr' ↝ C judgement


Σr; · ⊢[eff] crt <== Pf ↝ C
—————————————————————————————————
Σr ⊢ main : Pf [eff] = crt ⊣ Σr ↝ C


Σc = Comp(Σr)
Σc ⊢ type D(a1, ..., ak) = {L1 : A1 | ... | τn: An} ok
Σr, type D(a1, ..., ak) = {L1 : A1 | ... | τn: An} ⊢ rprog ⊣ Σr' ↝ C
———————————————————————————————————————————————————————————————————————
Σr ⊢ type D(a1, ..., ak) = {L1 : A1 | ... | τn: An} rprog ⊣ Σr' ↝ C


Σc = Comp(Σr)
Σc ⊢ sort D(a1, ..., ak) = {L1 : τ1 | ... | τn: τn} ok
Σr, sort D(a1, ..., ak) = {L1 : τ1 | ... | τn: τn} ⊢ rprog ⊣ Σr' ↝ C
———————————————————————————————————————————————————————————————————————
Σr ⊢ type D(a1, ..., ak) = {L1 : τ1 | ... | τn: τn} rprog ⊣ Σr' ↝ C


Σc = Comp(Σr)
Σc; x:A ⊢[pure] ce <== B
Σr, fun f(x:A) → B [pure] = ce ⊢ rprog ⊣ Σr' ↝ C
————————————————————————————————————————————————————————
Σr ⊢ fun f(x : A) → B [pure] = { ce } rprog ⊣ Σr' ↝ C


Σc = Comp(Σr)
Σc; x:A ⊢[impure] ce <== B
Σr, f : A → B [impure] ⊢ rprog ⊣ Σr' ↝ C
————————————————————————————————————————————————————————
Σr ⊢ fun f(x : A) → B [impure] = { ce } rprog ⊣ Σr' ↝ C


Σc = Comp(Σr)
Σc, f : τ → τ' [spec]; x:τ ⊢[spec] ce <== τ'
Σr, fun f(x:A) → B [pure] = ce ⊢ rprog ⊣ Σr' ↝ C
————————————————————————————————————————————————————————
Σr ⊢ fun f(x : τ) → τ' [spec] = { ce } rprog ⊣ Σr' ↝ C


Σr; · ⊢ Pf1 ⊸ Pf2 [spec] wf 
PfToCtx(·; Pf1) = Δ
Σr, f : Pf1 ⊸ Pf2 [spec]; Δ ⊢[spec] crt <== Pf2 ↝ C
Σr, f:Pf1 ⊸ Pf2 [spec] ⊢ rprog ⊣ Σr' ↝ C'
——————————————————————————————————————————————————————————————
Σr ⊢ fun f(Pf1) → Pf2 [spec] = { crt } rprog ⊣ Σr' ↝ C ∧ C'


Σr; · ⊢ Pf1 ⊸ Pf2 [impure] wf 
PfToCtx(·; Pf1) = Δ
Σr, f : Pf1 ⊸ Pf2 [impure]; Δ ⊢[impure] crt <== Pf2 ↝ C
Σr, f:Pf1 ⊸ Pf2 [impure] ⊢ rprog ⊣ Σr' ↝ C'
——————————————————————————————————————————————————————————————
Σr ⊢ fun f(Pf1) → Pf2 [impure] = { crt } rprog ⊣ Σr' ↝ C ∧ C'


Σr; · ⊢ Pf1 ⊸ Pf2 [pure] wf 
PfToCtx(·; Pf1) = Δ
Σr; Δ ⊢[pure] crt <== Pf2 ↝ C
Σr, f:Pf1 ⊸ Pf2 [pure] ⊢ rprog ⊣ Σr' ↝ C'
——————————————————————————————————————————————————————————————
Σr ⊢ fun f(Pf1) → Pf2 [pure] = { crt } rprog ⊣ Σr' ↝ C ∧ C'

## Refined primitives

Here are refined types for all of the primitives: 

Add : (x:int[pure], y:int[pure]) ⊸ (z:int[pure], prop:z = x + y) [pure]
Mul : (x:int[pure], y:int[pure]) ⊸ (z:int[pure], prop:z = x * y) [pure]
Sub : (x:int[pure], y:int[pure]) ⊸ (z:int[pure], prop:z = x - y) [pure]
Div : (x:int[pure], y:int[pure], pre:y ≠ 0) ⊸ (z:int[pure], prop:z = x/y) [pure]
Lt  : (x:int[pure], y:int[pure]) ⊸ (z:bool[pure], prop:z = x < y) [pure]
Le  : (x:int[pure], y:int[pure]) ⊸ (z:bool[pure], prop:z = x ≤ y) [pure]
Gt  : (x:int[pure], y:int[pure]) ⊸ (z:bool[pure], prop:z = x > y) [pure]
Ge  : (x:int[pure], y:int[pure]) ⊸ (z:bool[pure], prop:z = x ≥ y) [pure]
Eq[A] : (x:A[pure], y:A[pure]) ⊸ (z:bool[pure], prop:z = x = y) [pure]


New[A] : (x:A[pure]) ⊸ (p:Ptr A[pure], r:Own[A] p@x [res]) [impure]
Del[A] : (p:Ptr A [pure], x:A[spec], r:Own[A]p@x [res]) ⊸ () [impure]
Get[A] : (p:Ptr A [pure], x:A[spec], r:Own[A]p@x [res]) ⊸ (v:A[pure], pf:x = v, r:Own[A]p@x [res]) [impure]
Set[A] : (p:Ptr A [pure], v:A[pure], x:A[spec], r:Own[A]p@x [res]) ⊸ (r:Own[A] p@v [res]) [impure]

