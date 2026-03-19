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

u ::= 0 | 1 

Δ ::= · | Δ, x:τ [eff] | log x : ϕ, res x :ᵘ ct@ct  

Within Δ, eff can only be in {pure, spec}. 

So now we need a context well-formedness judgement:

Σ ⊢ Δ wf 


Σ ⊢ · wf 

Σ ⊢ Δ wf  eff ∈ {pure, spec}
——————————————————————————————
Σ ⊢ Δ, x:A [eff] wf


Σ ⊢ Δ wf    Σ; Δ ⊢ ct ⇐ bool
———————————————————————————————
Σ ⊢ Δ, log x:ct wf


Σ ⊢ Δ wf   Σ; Δ ⊢ ct' ⇒ τ   Σ; Δ ⊢ ct ⇐ pred τ    
——————————————————————————————————————————————————
Σ ⊢ Δ, x:ᵘ (ct @ ct') 


## Context erasure 

There is an erasure judgement  |Δ| = Γ

|·|                 = ·
|Δ, res x:ᵘ(ct@ct)| = |Δ|
|Δ, log x:ϕ|        = |Δ|
|Δ, x:_eff τ]|      = |Δ|, x:_eff τ

This will let us typecheck core terms using our existing judgemental machinery. 

## Refined terms 

Refined terms are terms, augmented with additional clauses for manipulating the proof state.

### Proof sorts/terms

I give the grammar of proof sorts as follows: 

Pf ::= (log x:ϕ), Pf | (res x : ct@ct), Pf | (x:τ [eff]), Pf | · 

So a proof sort represents a list of logical facts, resources, and logical and computational values.
The computational erasure of this is

|Pf| = Prod {Pf}

Prod [A1; ...;An] = (A1, ..., An)

{ϕ ⊗ Pf}       = {Pf}
{ct@ct' ⊗ Pf}  = {Pf}
{∃:τ. Pf}       = {Pf}
{Σx:A. Pf}      = A :: {Pf}
{I}             = []

So |Pf| = A means that A is the computational content of the proof sort. 

A refined function type has the shape: 

f : Pf1 ⊸ Pf2 [eff]

The variables in Pf1 will be in scope in Pf2, like in a dependent function space. Because
it mention terms, it also need a well-formedness check. 

Σ; Δ ⊢ Pf wf


Σ; Δ ⊢ · wf 


Σ; Δ ⊢ Pf wf  eff ∈ {pure, spec}
——————————————————————————————
Σ; Δ ⊢ x:A [eff], Pf wf


Σ; Delta ⊢_spec ct ⇐ bool [eff]    Σ; Δ ⊢ Pf wf    
———————————————————————————————————————————————————
Σ; Δ ⊢ log x:ct, Pf wf


Σ; Pf ⊢_spec ct' ⇒ τ [eff']  Σ; Pf ⊢_spec ct ⇐ pred τ [eff]   Σ; Δ ⊢ Pf wf   
—————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢ x:ᵘ (ct @ ct'), Pf


Σ; · ⊢ Pf1 wf    Σ; ·, Pf1 ⊢ Pf2 wf   
—————————————————————————————————————
Σ ⊢ Pf1 ⊸ Pf2 [eff] wf 


## 

The grammar of proof patterns is given as follows: 

q ::= (x1, ..., xn)

We introduce these to avoid introducing variables of arbitrary proof sort
in the context.

Core refined terms will be as follows: 

crt ::=
    | lit 
    | blit 
    | x    
    | let q = crt1; crt2 
    | crt : Pf 
    | prim spine
    | f spine 
    | (spine)
    | if[a] ce then crt1 else crt2
    | case[a] ce of {L1 x1 -> crt1 | ... } 
    | pcmd crt 
    | (crt1, ..., crtn) 
    | iter(q = crt1) { crt2 } 

spine ::= ct, spine | crt, spine | ·





There are three typechecking judgements, organized as follows: 

Σ; Δ ⊢[eff0] crt1 ==> Pf [eff1] ⊣ Δ'
Σ; Δ ⊢[eff0] crt2 <== Pf [eff1] ⊣ Δ'
Σ; Δ ⊢[eff0] spine : Pf1 ⊸ Pf2 [eff1] ⊣ Δ'

———————————————————————————————————————
Σ; Δ ⊢ lit ⇒ (x:int, u: x = lit) ⊣ Δ 


—————————————————————————————————————————
Σ; Δ ⊢ blit ⇒ (x:bool, u: x = blit) ⊣ Δ 


TODO – lookup judgement
————————————————————————
Σ; Δ ⊢[eff] x ⇒ X ⊣ Δ'


Σ; Δ0 ⊢_eff0 crt ⇒ Pf' [eff1] ⊣ Δ1
eff2 = ⌊eff1⌋
Σ; Δ1 ⊢ q : Pf' [eff2] ⊣ Δ' 
Σ; Δ1, Δ' ⊢_eff0 crt2 <== Pf [eff3] ⊣ Δ2, Δ''
zero(Δ'')
eff4 = eff2 ∨ eff3 
————————————————————————————————————————————————————
Σ; Δ0 ⊢_eff0 let q = crt1; crt2 <== Pf [eff4] ⊣ Δ2



Σ; |Δ| ⊢_spec P ⇒ Pred (Step(A, B)) [eff'] 
Σ; Δ' ⊢_pure crt1 ⇒ (a:A, u: P @ Next a) ̣[pure] ⊣ Δ'
Σ; Δ', x : A, u : P@(Next x) ⊢_impure crt2 ⇐ (y:A+B. P@Done y) [eff] ⊣ Δ'
—————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢_impure iter[P] ((x,u) = crt1) { crt2 } ⇒ (b:B, u : P @ Done b) [impure] ⊣ Δ'



Σ; Δ ⊢[eff0] crt <== Pf [eff1] ⊣ Δ'
————————————————————————————————————————————————————
Σ; Δ ⊢[eff0] (crt : Pf) ==> Pf [eff1] ⊣ Δ'



The grammar of refined terms rt is as follows

rt ::= x | (rt, ..., rt) | (ct, rt) | pcmd rt | auto | exfalso | rt : Pf 



pcmd ::= auto | open-ret | make-ret | open-take | make-take | unfold[f,i]

The commands `pcmd` all range over a number of substructural proof term
formers, with the following schemas for manipulating the proof states. The
pcmd 

open-ret     : (u : return ct1@ct2)          ⊸ (x:τ, u : ct1 = ct2)
make-ret     : (u : ct1 = ct2)               ⊸ (u : (return ct1)@ct2)
open-take    : (u : (take x = ct1; ct2)@ct3) ⊸ (x:τ, u : ct1@x, ct2@ct3)
make-take    : (x:τ, ct1@x, ct2@ct3)         ⊸ (u : (take x = ct1; ct2)@ct3)



unfold[f, i] : Pf                      ⊸ Pf  // unfold the i-th occurence of f in Pf 


The typing rules will be as follows: 

log x : ϕ ∈ Δ
——————————————————
Σ; Δ ⊢ x ⇒ ϕ ⊣ Δ


Δ = Δ0, res x :¹ (ct@ct'), Δ1
————————————————————————————————————————
Σ; Δ ⊢ x ⇒ ϕ ⊣ Δ0, res x:⁰(ct@ct'), Δ1


Σ; Δ1 ⊢ pf1 ⇐ Pf1 ⊣ Δ1 ... Σ; Δn ⊣ pfn ⇐ Pfn ⊣ Δ_(n+1)
————————————————————————————————————————————————————————
Σ; Δ ⊢ (pf1, ..., pfn) ⇐ Pf1 ⊗ ... ⊗ Pfn ⊣ Δ_(n+1)


Σ; Δ ⊢ ct ⇐ τ    Σ; Δ ⊢ pf : [ct/x]Pf
———————————————————————————————————————
Σ; Δ ⊢ pack(ct, pf) ⇐ ∃x:τ. Pf


Σ; Δ ⊢ pf ⇒ Pf1    Σ; Δ ⊢ pcmd : Pf1 ⊸ Pf2   
———————————————————————————————————————————————
Σ; Δ ⊢ pcmd pf ⇒ Pf2 



Σ; Δ ⊢ open-ret : (return ct1@ct2) ⊸ ∃x:τ (ct1 = ct2) 

Σ; Δ ⊢ make-ret : (ct1 = ct2) ⊸ (return ct1)@ct2 

Σ; Δ ⊢ open-take : (take x = ct1; ct2)@ct3 ⊸ ∃x:τ. ct1@x ⊗ ct2@ct3

Σ; Δ ⊢ make-take : ∃x:τ. ct1@x ⊗ ct2@ct3  ⊸ (take x = ct1; ct2)@ct3


Σ; Δ ⊧ ⊥
———————————————————
Σ; Δ ⊢ exfalso ⇐ P 


Σ; Δ ⊧ ϕ
————————————————
Σ; Δ ⊢ auto ⇐ ϕ


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



