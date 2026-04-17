# Adding pattern matching to nanocn refinement types 

## Phase 1: Infallible patterns only 

In the simple refinement system, the grammar of patterns looks like this: 
q ::= (qbase1, ..., qbasen) 
qbase ::= cpat | res rpat | log lpat 
lpat ::= x
rpat ::= x | (x) y 
cpat ::=  x 

What we are going to do is to expand the grammar of patterns to encompass more stuff, 
but for patterns which always succeed in matching. 

q ::= (qbase1, ..., qbasen) 
qbase ::= cpat | res rpat-base | log lpat 
lpat ::= x
rpat-base ::= do x = rpat | rpat
rpat ::= x 
      | return lpat 
      | take(cpat, rpat); rpat 
      | fail 
      | let[a] cpat; rpat 
      | case L(p); rpat
      | iftrue; rpat
      | iffalse; rpat
      | unfold; rpat 
      | _; rpat
      
cpat ::=  x | (cpat1, ..., cpatn) | L cpat 





























## Extending the signature. 

The core signature must contain the full definition of all spec functions. (We have already
done this.) 


## Updating the context

The context should now be extended with logical assumptions (terms of type bool), and 
resource assumptions (pairs of terms of sort τ and Pred τ). Because resources are substructural,
resource assumptions are marked with a 0/1 usage flag indicating whether they can be used (1),
have already been used (0), or can optionally be used (?). 

u ::= 0 | 1 | ?

κ ::= eff | log | res(u) 
X ::= τ 
   | se          (se : Bool)
   | se@se'      (se : Pred τ, se' : τ)


Δ ::= · | Δ, x:X [κ]

Within Δ, eff can only be in {pure, spec}. 

For usage, we can think of ? as 0 ∨ 1 – it can be either used or ignored. 

We can define a partial meet operation on usages: 

u ⊓ u = u 
1 ⊓ ? = 1
? ⊓ 1 = 1
0 ⊓ ? = 0
? ⊓ 0 = 0


### Context Well-formedness

* Σ ⊢ Δ wf 

  - Inputs: Σ, Δ
  - Outputs: none

—————————
Σ ⊢ · wf 


Σ ⊢ Δ wf  eff ∈ {pure, spec}
——————————————————————————————
Σ ⊢ Δ, x:A [eff] wf


Σ ⊢ Δ wf    Σ; |Δ| ⊢ se ⇐ bool
———————————————————————————————
Σ ⊢ Δ, x:se [log] wf 


Σ ⊢ Δ wf   Σ; |Δ| ⊢ se ⇒ pred τ   Σ; |Δ| ⊢ se' ⇐ τ    
———————————————————————————————————————————————————
Σ ⊢ Δ, x:(se @ se') [res(u)] wf 


### Resource usage in contexts

#### affinize: make resource usage optional
 
aff(0) = 0 
aff(1) = ?
aff(?) = ?

affinize(·) = ·
affinize(Δ, x:τ[eff]) = affinize(Δ), x:τ[eff]
affinize(Δ, x:se[log]) = affinize(Δ), x:se[log]
affinize(Δ, x:se@se' [res(u)]) = affinize(Δ), x:se@se' [res(aff(u))]


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
zero(Δ, x:se[log])


zero(Δ)        u ∈ {0, ?}
——————————————————————————
zero(Δ, x:se@se' [res(u)])


### Context erasure 

There is an erasure judgement  |Δ| = Γ

|·|                      = ·
|Δ, x:(se@se') [res(u)]| = |Δ|
|Δ, x:ϕ [log]|           = |Δ|
|Δ, x:τ [eff]|           = |Δ|, x: τ [eff]

We use this to typecheck any core terms occurring inside refined terms, using our existing
judgemental machinery. 

## SMT Constraints 

The grammar of SMT constraint problems is as follows: 

C ::= ⊤ 
   |  C ∧ C 
   | ∀x:τ. C 
   | ϕ → C 
   | ϕ 

They are checked for well-formedness relative to a simple signature Σ and erased context Γ: 

* Σ; Γ ⊢ C wf 

  - Inputs: Σ, Γ, C 
  - Outputs: none 

—————————————
Σ; Γ ⊢ ⊤ wf 


Σ; Γ ⊢ C1 wf   Σ; Γ ⊢ C2 wf
————————————————————————————
Σ; Γ ⊢ C1 ∧ C2 wf


Σ; Γ, x:τ[spec] ⊢ C wf
———————————————————————
Σ; Γ ⊢ ∀x:τ.C wf


Σ; Γ ⊢[spec] ϕ <== Bool [eff']    Σ; Γ ⊢ C wf
———————————————————————————————————————————————
Σ; Γ ⊢ ϕ → C wf


Σ; Γ ⊢[spec] ϕ <== Bool [eff'] 
———————————————————————————————
Σ; Γ ⊢ ϕ wf

The auxiliary function Δ ⇒ C = C' is defined as follows:

·              	       ⇒ C = C
(Δ, x:τ [eff]) 	       ⇒ C = (Δ ⇒ ∀x:τ.C)
(Δ, x:ϕ [log])	       ⇒ C = (Δ ⇒ ϕ → C)
(Δ, x:se@se' [res(u)]) ⇒ C = (Δ ⇒ C)


The invariants we maintain about it is that if Σ ⊢ Δ wf and Σ; |Δ| ⊢ C wf then Σ; · ⊢ Δ ⇒ C wf. 
(This should be part of our implementation test suite!) 


## Refined terms 

Refined terms are terms, augmented with additional clauses for manipulating the proof state.

### Proof sorts/terms

I give the grammar of proof sorts as follows: 

    Pf ::= (ϕ [log]), Pf | (rs [res]), Pf | (x:τ [eff]), Pf | · 
    rs ::= se@se'
        |  (x).se     
 

(This is not the same as Δ, since (1) it associates the other way around, and (2) resources
don't have a flag.)

So a proof sort represents a list of logical facts, resources, and logical and computational values.
The computational erasure of this is

    Comp(Pf) = Prod {Pf}
    
    Prod [A1; ...;An] = (A1, ..., An)
    
    {ϕ[log], Pf}      = {Pf}
    {rs[res], Pf}     = {Pf}
    {x:τ[spec], Pf}   = {Pf}
    {x:A[pure], Pf}   = A :: {Pf}
    {·}               = []

So Comp(Pf) = A means that A is the computational content of the proof sort. 

We can also extract the binding content – the logical and computational bindings: 

——————————————————— 
Σ; Γ ⊢ Bind(·) = Γ


Σ; Γ, x:τ[eff] ⊢ Bind(Pf) = Γ'  
—————————————————————————————————————— 
Σ; Γ ⊢ Bind(x:τ[eff], Pf) = Γ'   

Σ; Γ ⊢ Bind(Pf) = Γ'
——————————————————————————————————————
Σ; Γ ⊢ Bind(se[log], Pf) = Γ'

Σ; Γ ⊢ Bind(Pf) = Γ'
——————————————————————————————————————
Σ; Γ ⊢ Bind(se@se'[res], Pf) = Γ'

Σ; Γ ⊢ se ⇒ Pred τ   Σ; Γ, y:τ[spec] ⊢ Bind(Pf) = Γ'
——————————————————————————————————————————————————————
Σ; Γ ⊢ Bind((y).se[res], Pf) = Γ'
    

Σ; Γ ⊢ Bind(Pf) = Γ' adds the computational and spec variables bound in Pf to Γ. 


* Σ; Γ ⊢[eff] Pf wf

  - Inputs: Σ, Γ, Pf 
  - Outputs: none

—————————————————
Σ; Γ ⊢[eff] · wf 


eff' ∈ {spec, ⌊eff⌋}    Σ; Γ, x:τ[eff'] ⊢ Pf wf  
————————————————————————————————————————————————
Σ; Γ ⊢[eff] x:τ [eff'], Pf wf


Σ; Γ ⊢[spec] se ⇐ bool [eff]    Σ; Γ ⊢ Pf wf    
———————————————————————————————————————————————————
Σ; Γ ⊢[eff] se [log], Pf wf


Σ; Γ ⊢[spec] se' ⇒ τ [eff']  Σ; Γ ⊢[spec] se ⇐ pred τ   Σ; Γ ⊢[eff] Pf wf   
——————————————————————————————————————————————————————————————————————————————
Σ; Γ ⊢[eff] (se @ se') [res], Pf wf


Σ; Γ ⊢[spec] se ⇒ pred τ [eff]   Σ; Γ, y:τ[spec] ⊢[eff] Pf wf   
——————————————————————————————————————————————————————————————————————————————
Σ; Γ ⊢[eff] (y).se [res], Pf wf



Σ; · ⊢[eff] Pf1 wf
Σ; · ⊢ Bind(Pf1) = Γ
Σ; Γ ⊢[eff] Pf2 wf   
—————————————————————————————————————————————————————
Σ ⊢ Pf1 ⊸ Pf2 [eff] wf 


These rules ensure that if a refined function has sort spec, it can only operates on spec
and resource arguments/returns values – it cannot return any computational (i.e., pure) values.
A refined computational (pure or impure) function can receive and return both pure and spec 
arguments. Return types can also depend upon the bindings of their inputs. 

### Grammar of core terms 

The grammar of core refined patterns is given as follows: 

qbase ::= res rpat | log lpat | pat 
lpat ::= x | auto 
rpat ::= x | ret lpat | take(pat, rpat1, rpat2) | fail 
      

q ::= (qbase1, ..., qbasen)

We use these to avoid introducing variables of arbitrary proof sort
in the context.

The grammar of core refined terms is as follows: 

crt ::=
    | let q = crt1; crt2 
    | let res x = rpf; crt
    | let log x = lpf; crt 
    | crt : Pf 
    | prim spine
    | f spine 
    | (spine)
    | iter(q = crt1) { crt2 } 
    | if[a] se then crt1 else crt2
    | case[a] se of {L1 x1 -> crt1 | ... } 
    | exfalso | open-take rpf 

spine ::= se, spine | rpf, spine | lpf, spine | · 

lpf ::= x | auto | unfold f(se) | open-ret rpf | lpf : ϕ

rpf ::= x | make-ret lpf | make-take crt | rpf : se@se'


There are 12 typechecking judgements, organized as follows: 

* Synthesizing logical facts: Σ; Δ ⊢ lpf ==> ϕ ⊣ Δ' ↝ C 

  - Inputs: Σ, Δ, lpf
  - Outputs: ϕ, Δ', C 

* Checking logical facts: Σ; Δ ⊢ lpf <== ϕ ⊣ Δ' ↝ C 

  - Inputs: Σ, Δ, lpf, ϕ
  - Outputs: C, Δ' 

* Resource variable lookup: Σ; Δ ⊢ x : se@se' ⊣ Δ'

  - Inputs: Σ, Δ, x
  - Outputs: se, se', Δ'

* Synthesizing resource usage: Σ; Δ ⊢ rpf ==> se@se' ⊣ Δ' ↝ C 

  - Inputs: Σ, Δ, rpf, se, se' 
  - Outputs: se@se', Δ', C

* Checking resource usage:  Σ; Δ ⊢ rpf <== se@se' ⊣ Δ' ↝ C 

  - Inputs: Σ, Δ, se, se' 
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

  - Inputs: Σ, Δ, eff, spine, Pf
  - Outputs: Δ', C 



1. The decision to emit constraints rather than solve them on the fly is 
   basically so that we can statically check the rough well-formedness
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


x:se [log] ∈ Δ
——————————————————————————————
Σ; Δ ⊢ x ==> se ⊣ Δ ↝ ⊤


Σ ⊢ f (x : τ) → τ' [eff] = se_body
eff ≤ spec 
Σ; |Δ| ⊢[spec] se <== τ 
——————————————————————————————————————————————————————
Σ; Δ ⊢ unfold f(se) ==> (f(se) = [se/x]se_body) ↝ ⊤


Σ; Δ ⊢ lpf ==> ϕ ⊣ Δ' ↝ C 
———————————————————————————————————
Σ; Δ ⊢ lpf <== ϕ' ⊣ Δ' ↝ C ∧ (ϕ → ϕ')


Σ; Δ ⊢ lpf <== ϕ ⊣ Δ' ↝ C
——————————————————————————————
Σ; Δ ⊢ (lpf : ϕ) ==> ϕ ⊣ Δ' ↝ C 


Σ; Δ ⊢ rpf ==> (return se)@se' ⊣ Δ' ↝ C
——————————————————————————————————————————
Σ; Δ ⊢ open-ret rpf ==> (se = se') ⊣ Δ' ↝ C 


#### Resource variable Lookup 

x ≠ y   Σ; Δ ⊢ x : se@se' ⊣ Δ'
————————————————————————————————————————————
Σ; Δ, y:X[κ] ⊢ x : se@se' ⊣ Δ', y:X[κ]


u ∈ {1, ?}
——————————————————————————————————————————————————————————————————————
Σ; Δ, x : (se@se') [res(u)] ⊢ x : (se@se') ⊣ Δ, x : (se@se') [res(0)]


These are the basic lookup rules, except that resource lookup only
succeeds if the usage is 1 or ?, and accessing it sets the usage to 0.

#### Checking/synthesizing resource facts: 

Σ; Δ ⊢ x : se@se' ⊣ Δ' 
————————————————————————————————————
Σ; Δ ⊢ x ==> se@se' ⊣ Δ' ↝ ⊤ 


Σ; Δ ⊢ lpf <== (ce1 = ce2) ⊣ Δ' ↝ C 
——————————————————————————————————————————————————————
Σ; Δ ⊢ make-ret lpf <== (return ce1)@ce2 ⊣ Δ' ↝ C 


Σ; Δ ⊢ crt <== (x:τ[spec], ce1@x [res], ce2@ce3 [res]) ⊣ Δ' ↝ C 
——————————————————————————————————————————————————————————————————
Σ; Δ ⊢ make-take crt <== (take x = ce1; ce2)@ce3 ⊣ Δ' ↝ C 


Σ; Δ ⊢ rpf <== se@se' ⊣ Δ' ↝ C
————————————————————————————————————————————
Σ; Δ ⊢ (rpf : se@se') ==> se@se' ⊣ Δ' ↝ C


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
Σ; Δ ⊢ (ce1 [log], Pf1) = (ce2 [log], Pf2) ↝ (ce1 = ce2) ∧ C


Σ; Δ ⊢ Pf1 = Pf2 ↝ C 
——————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢ ((ce1@ce1') [res], Pf1) = ((ce2@ce2') [res], Pf2) 
↝ (ce1 = ce2 ∧ ce1' = ce2') ∧ C


Σ;|Δ| ⊢ ce1 ==> Pred τ
Σ;|Δ| ⊢ ce2 ==> Pred τ
Σ; Δ, c:τ ⊢ [c/a]Pf1 = [c/b]Pf2 ↝ C 
——————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢ ((a).ce1 [res], Pf1) = ((b).ce2 [res], Pf2) ↝ (ce1 = ce2) ∧ ∀c:τ.C



#### Context merge

———————————————————
Σ; · ⊓ · = ·


Σ ⊢ Δ0 ⊓ Δ1 = Δ
——————————————————————————————————————————————————————
Σ; (Δ0, x:τ[eff]) ⊓ (Δ1, x:τ[eff]) = (Δ, x:τ[eff]) 


Σ ⊢ Δ0 ⊓ Δ1 = Δ
———————————————————————————————————————————————————————
Σ; (Δ0, x:se[log]) ⊓ (Δ1, x:se[log]) = Δ, x:se[log]


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


Σ; Γ, x:τ[eff] ⊢ q : [id(Γ), x/y]Pf ⊣ Δ
—————————————————————————————————————————————
Σ; Γ ⊢ (x,q) : (y:τ[eff], Pf) ⊣ x:τ[eff], Δ


Σ; Γ ⊢ q : Pf ⊣ Δ
————————————————————————————————————————————————————
Σ; Γ ⊢ (x,q) : (se[log], Pf) ⊣ x:se[log], Δ


Σ; Γ ⊢ q : Pf ⊣ Δ
————————————————————————————————————————————————————————————————
Σ; Γ ⊢ (x,q) : (se@se'[res], Pf) ⊣ x:(se@se')[res(1)], Δ

Σ; Γ ⊢[spec] se ==> Pred τ
Σ; Γ, x:τ ⊢ q : [id(Γ), x/z]Pf ⊣ Δ
—————————————————————————————————————————————————————————————————————
Σ; Γ ⊢ ((x,a),q) : ((z).se[res], Pf) ⊣ x:τ, a:(se@x)[res(1)], Δ



#### Typechecking



Σ; Δ0 ⊢[eff] crt ==> Pf' ⊣ Δ1 ↝ C
Σ; |Δ1| ⊢ q : Pf' ⊣ Δ' 
Σ; Δ1, Δ' ⊢[eff] crt2 <== Pf ⊣ Δ2, Δ'' ↝ C'
length(Δ1) = length(Δ2) 
length(Δ') = length(Δ'')
zero(Δ'')
————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let q = crt1; crt2 <== Pf ⊣ Δ2 ↝ C ∧ C'


Σ; Δ0 ⊢ lpf ==> ϕ ⊣ Δ1 ↝ C 
Σ; Δ1, x:ϕ [log] ⊢[eff] crt <== Pf ⊣ Δ2, x:ϕ [log] ↝ C'
—————————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let log x = lpf; crt <== Pf ⊣ Δ2 ↝ C ∧ C'


Σ; Δ0 ⊢ rpf ==> se@se' ⊣ Δ1 ↝ C 
Σ; Δ1, x:se@se' [res(1)] ⊢[eff] crt <== Pf ⊣ Δ2, x:se@se' [res(u)] ↝ C'
u ∈ {0, ?}
———————————————————————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let res x = rpf; crt <== Pf ⊣ Δ2 ↝ C ∧ C'



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
Σ; Δ ⊢[pure] crt1 <== (a:A, P @ Next a) ⊣ Δ' ↝ C 
Σ; Δ', x : A [pure], u : P@(Next x) [res(1)] ⊢[impure] crt2 <== ([pure] y:Step(A,B), [res] P@y) ⊣ Δ', x : A [pure], u : P@(Next x) [res(0)] ↝ C'
——————————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[impure] iter[P] ((x,u) = crt1) { crt2 } ==> (b:B[pure], P @ Done b) ⊣ Δ' ↝ C ∧ ∀x:A.C'


eff' = ⌊eff⌋
Σ; |Δ| ⊢[eff'] se ⇐ bool 
Σ; Δ, x : se = true [log] ⊢[eff'] crt1 <== Pf ⊣ Δ1, x : se = true ↝ C1
Σ; Δ, x : se = false [log] ⊢[eff'] crt2 <== Pf ⊣ Δ2, x : se = false  ↝ C2
Σ ⊢ Δ1 ⊓ Δ2 = Δ'
——————————————————————————————————————————————————————
Σ; Δ ⊢[eff] if[x] se then crt1 else crt2 <== Pf ⊣ Δ'
↝ (se = true → C1) ∧ (se = false → C2)


eff' = ⌊eff⌋
Σ; |Δ| ⊢[eff'] se ⇒ D(σ1, ..., σk) 
Σ ⊢ L1:τ1 in D(σ1, ..., σk) ... Σ ⊢ Ln:τn in D(σ1, ..., σk) 
Σ, Δ, x:τ1, y = L1(x) ⊢ crt1 <== Pf ⊣ Δ1, x:τ1, y = L1(x) ↝ C1 ... 
   Σ, Δ, x:τn, y = L1(x) ⊢ crtn <== Pf ⊣ Δn, x:τn, y = Ln(x) ↝ Cn
Σ ⊢ ⊓ (Δ1; ... Δn) = Δ'
———————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] case[y] se of {L1 x → crt1 | ... | Ln x → crtn} <== Pf ⊣ Δ'
↝ (∀x:τ1. se = L1 x → C1) ∧
   ...
   (∀x:τn. se = Ln x → Cn) 
   

Σ; |Δ| ⊢ Pf wf     Σ; Δ ⊢ crt <== Pf ⊣ Δ' ↝ C  
———————————————————–————————————————————————————
Σ; Δ ⊢ (crt : Pf) ==> Pf ⊣ Δ' ↝ C


#### Spine 


———————————————————————————————————————————————
Σ; Δ ⊢[eff] · : (· ⊸ Pf) >> Pf ⊣ Δ ↝ ⊤


Σ; |Δ| ⊢[eff] se <== τ 
Σ; Δ ⊢[eff] spine : ([se/x]Pf1 ⊸ [se/x]Pf2) >> Pf3 ⊣ Δ' ↝ C
—————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (se, spine) : (x:τ[eff], Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ' ↝ C 


Σ; Δ ⊢ lpf <== se ⊣ Δ' ↝ C 
Σ; Δ' ⊢[eff] spine : (Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ'' ↝ C'
——————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (lpf, spine) : (se[log], Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ'' ↝ C ∧ C'


Σ; Δ ⊢ rpf <== (ce1@ce2) ⊣ Δ' ↝ C
Σ; Δ' ⊢[eff] spine : (Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ'' ↝ C'
—————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (rpf, spine) : (ce1@ce2[res], Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ'' ↝ C ∧ C'


Σ; Δ ⊢ rpf ==> (ce2@se) ⊣ Δ' ↝ C
Σ; Δ' ⊢[eff] spine : ([se/y]Pf1 ⊸ [se/y]Pf2) >> Pf3 ⊣ Δ'' ↝ C'
————————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (rpf, spine) : ((y).ce1[res], Pf1 ⊸ Pf2) >> Pf3 ⊣ Δ'' ↝ C ∧ (ce1 = ce2) ∧ C'


#### Tuple (Σ; Δ ⊢[eff] spine : Pf ⊣ Δ' ↝ C) 

———————————————————————————————————————————————
Σ; Δ ⊢[eff] · : · ⊣ Δ ↝ ⊤


Σ; |Δ| ⊢[eff] se <== τ    Σ; Δ ⊢ spine : [id(|Δ|), se/x]Pf ⊣ Δ' ↝ C 
—————————————————————————————————————————————————————————
Σ; Δ ⊢ (se, spine) : (x:τ[pure], Pf) ⊣ Δ' ↝ C 

Σ; |Δ| ⊢[spec] se <== τ    Σ; Δ ⊢ spine : [id(|Δ|), se/x]Pf ⊣ Δ' ↝ C 
—————————————————————————————————————————————————————————
Σ; Δ ⊢ (se, spine) : (x:τ[spec], Pf) ⊣ Δ' ↝ C 


Σ; Δ ⊢ lpf <== se ⊣ Δ' ↝ C 
Σ; Δ' ⊢ spine : Pf ⊣ Δ'' ↝ C'
—————————————————————————————————————————————————————————
Σ; Δ ⊢ (lpf, spine) : (se[log], Pf) ⊣ Δ'' ↝ C ∧ C'


Σ; Δ ⊢[eff] rpf <== se@se' ⊣ Δ' ↝ C 
Σ; Δ' ⊢[eff] spine : Pf ⊣ Δ'' ↝ C'
———————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (rpf, spine) : (se@se'[res], Pf) ⊣ Δ'' ↝ C ∧ C'


Σ; Δ ⊢[eff] rpf ==> ce2@se ⊣ Δ' ↝ C 
Σ; Δ' ⊢[eff] spine : [id(|Δ|), se/y]Pf ⊣ Δ'' ↝ C'
———————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (rpf, spine) : ((y).ce1[res], Pf) ⊣ Δ'' ↝ C ∧ (ce1 = ce2) ∧ C'



#### Proof commands

Δ' = affinize(Δ)
—————————————————————————————————————
Σ; Δ ⊢[eff] exfalso <== Pf ⊣ Δ' ↝ ⊥  

(If we are in dead code, there are no more constraints on resource usage.)


Σ; Δ ⊢ rpf ==> ((take x = ce1; ce2)@ce3) ⊣ Δ' ↝ C
—————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] open-take rpf ==> (x:τ[spec], [res] ce1@x,  [res] ce2@ce3) ⊣ Δ' ↝ C 




* Commentary
   
   The proof commands range over a number of substructural proof term
   lemmas, with the following schemas for manipulating the proof states. The
   pcmd are intended to behave like: 
   
       open-ret     : (return ce1@ce2)              ⊸ ([log] ce1 = ce2)  
       make-ret     : (ce1 = ce2)                   ⊸ ([res] (return ce1)@ce2)
       open-take    : (take x = ce1; ce2)@ce3       ⊸ ([spec] x:τ, [res] ce1@x, [res] ce2@ce3) 
       make-take    : (x:τ, ce1@x, ce2@ce3)         ⊸ ([res] take x = ce1; ce2)@ce3)
       unfold(f,τ)  : ()                            ⊸ ([log] f(se) = se') 
   
   These are all schematic, and so cannot be primitives/functions unless
   we add type quantification. Furthermore, they live in different sorts
   – the unfold(f, t) command produces a logical fact, make-ret takes a
   logical fact and produces a resource, make-take takes a proof and
   produces a resource, and open-ret and open-take consume resources,
   but produce full proofs. So they all live in different judgements! 


#### Core refined signatures: 

Σr ::= · | Σr, f:F | Σr, fun f(x:τ) → τ'[eff] = se | Σr, f(q) : RF 

First, we do lookup in a signature: 

f:RF ∈ Σr
——————————————————
Σr ⊢ f : RF 


f:τ → τ' [eff] ∈ Σr
———————————————————————————————
Σr ⊢ f :  (x:τ) ⊸ (y:τ') [eff]


fun f (x:τ) → τ' [eff] = se ∈ Σr
—————————————————————————————————
Σr ⊢ f :  (x:τ) ⊸ (y:τ') [eff]

Next, we show how to erase refinements 

Comp(·)                              = ·
Comp(Σr, f:F)                        = Comp(Σr), f:F
Comp(Σr, fun f(x:τ) → τ'[eff] = se) = Comp(Σr), fun f(x:τ) → τ'[eff] = se
Comp(Σr, f : Pf1 ⊸ Pf2[eff])        = Comp(Σr), f : Comp(Pf1) → Comp(Pf2) [eff] 

Next, we give signature well-formedness ⊢ Σr wf 

———————
⊢ · wf 


⊢ Σr wf
——————————————————————————
⊢ Σr, f : τ → τ' [eff] wf 


⊢ Σr wf   Comp(Σr); x:τ ⊢[eff] se <== τ'
—————————————————————————————————————————
⊢ Σr, fun f (x : τ) → τ' [eff] = se wf 


⊢ Σr wf    Σ; · ⊢ RF wf 
——————————————————————————
⊢ Σr, f : RF wf 


#### Refined Programs: 

A refined program consists of the following declarations:

rprog ::= sort D(a1, ..., ak) = {L1 : τ1 | ... | Ln: τn} rprog
       |  type D(a1, ..., ak) = {L1 : A1 | ... | Ln: An} rprog
       |  fun f(x : τ) → τ' [eff] = { e } rprog
       |  fun f(Pf1) → Pf2 [eff] = { crt } rprog
       |  main : Pf [eff] = crt 


We typecheck these programs with the Σr ⊢ rprog ⊣ Σr' ↝ C judgement


Σr; · ⊢[eff] crt <== Pf ↝ C
—————————————————————————————————
Σr ⊢ main : Pf [eff] = crt ⊣ Σr ↝ C


Σc = Comp(Σr)
Σc ⊢ type D(a1, ..., ak) = {L1 : A1 | ... | Ln: An} ok
Σr, type D(a1, ..., ak) = {L1 : A1 | ... | Ln: An} ⊢ rprog ⊣ Σr' ↝ C
———————————————————————————————————————————————————————————————————————
Σr ⊢ type D(a1, ..., ak) = {L1 : A1 | ... | Ln: An} rprog ⊣ Σr' ↝ C


Σc = Comp(Σr)
Σc ⊢ sort D(a1, ..., ak) = {L1 : τ1 | ... | Ln: τn} ok
Σr, sort D(a1, ..., ak) = {L1 : τ1 | ... | Ln: τn} ⊢ rprog ⊣ Σr' ↝ C
———————————————————————————————————————————————————————————————————————
Σr ⊢ sort D(a1, ..., ak) = {L1 : τ1 | ... | Ln: τn} rprog ⊣ Σr' ↝ C


Σc = Comp(Σr)
Σc; x:A ⊢[pure] se <== B
Σr, fun f(x:A) → B [pure] = se ⊢ rprog ⊣ Σr' ↝ C
————————————————————————————————————————————————————————
Σr ⊢ fun f(x : A) → B [pure] = { se } rprog ⊣ Σr' ↝ C


Σc = Comp(Σr)
Σc; x:A ⊢[impure] se <== B
Σr, f : A → B [impure] ⊢ rprog ⊣ Σr' ↝ C
————————————————————————————————————————————————————————
Σr ⊢ fun f(x : A) → B [impure] = { se } rprog ⊣ Σr' ↝ C


Σc = Comp(Σr)
Σc, f : τ → τ' [spec]; x:τ ⊢[spec] se <== τ'
Σr, fun f(x:τ) → τ' [spec] = se ⊢ rprog ⊣ Σr' ↝ C
————————————————————————————————————————————————————————
Σr ⊢ fun f(x : τ) → τ' [spec] = { se } rprog ⊣ Σr' ↝ C


Σr; · ⊢ Pf1 ⊸ Pf2 [spec] wf 
Σr; · ⊢ q : Pf1 ⊣ Δ   
Σr, f : Pf1 ⊸ Pf2 [spec]; Δ ⊢[spec] crt <== Pf2 ↝ C
Σr, f:Pf1 ⊸ Pf2 [spec] ⊢ rprog ⊣ Σr' ↝ C'
——————————————————————————————————————————————————————————————
Σr ⊢ fun f(q) : Pf1 ⊸ Pf2 [spec] = { crt } rprog ⊣ Σr' ↝ C ∧ C'


Σr; · ⊢ Pf1 ⊸ Pf2 [impure] wf 
Σr; · ⊢ q : Pf1 ⊣ Δ   
Σr, f : Pf1 ⊸ Pf2 [impure]; Δ ⊢[impure] crt <== Pf2 ↝ C
Σr, f:Pf1 ⊸ Pf2 [impure] ⊢ rprog ⊣ Σr' ↝ C'
——————————————————————————————————————————————————————————————
Σr ⊢ fun f(q) : Pf1 ⊸ Pf2 [impure] = { crt } rprog ⊣ Σr' ↝ C ∧ C'


Σr; · ⊢ Pf1 ⊸ Pf2 [pure] wf 
Σr; · ⊢ q : Pf1 ⊣ Δ   
Σr; Δ ⊢[pure] crt <== Pf2 ↝ C
Σr, f:Pf1 ⊸ Pf2 [pure] ⊢ rprog ⊣ Σr' ↝ C'
——————————————————————————————————————————————————————————————
Σr ⊢ fun f(q) : Pf1 ⊸ Pf2 [pure] = { crt } rprog ⊣ Σr' ↝ C ∧ C'

## Refined primitives

Here are refined types for all of the primitives: 

Add : (x:int[pure], y:int[pure]) ⊸ (z:int[pure], z = x + y) [pure]
Mul : (x:int[pure], y:int[pure]) ⊸ (z:int[pure], z = x * y) [pure]
Sub : (x:int[pure], y:int[pure]) ⊸ (z:int[pure], z = x - y) [pure]
Div : (x:int[pure], y:int[pure], y ≠ 0) ⊸ (z:int[pure], z = x/y) [pure]
Lt  : (x:int[pure], y:int[pure]) ⊸ (z:bool[pure], z = x < y) [pure]
Le  : (x:int[pure], y:int[pure]) ⊸ (z:bool[pure], z = x ≤ y) [pure]
Gt  : (x:int[pure], y:int[pure]) ⊸ (z:bool[pure], z = x > y) [pure]
Ge  : (x:int[pure], y:int[pure]) ⊸ (z:bool[pure], z = x ≥ y) [pure]
Eq[A] : (x:A[pure], y:A[pure]) ⊸ (z:bool[pure], z = x = y) [pure]


New[A] : (x:A[pure]) ⊸ (p:Ptr A[pure], Own[A] p@x [res]) [impure]
Del[A] : (p:Ptr A [pure], (do x = Own[A]p) [res]) ⊸ () [impure]
Get[A] : (p:Ptr A [pure], (do x = Own[A]p) [res]) ⊸ (v:A[pure], v = x, Own[A]p@x [res]) [impure]
Set[A] : (p:Ptr A [pure], v:A[pure], do x:A = Own[A]p [res]) ⊸ (Own[A] p@v [res]) [impure]

