# Adding refinement types to nanocn 

Next, we are going to add support refinement types to nanoCN. 

## Extending the signature. 

The signature must now contain the full definition of all spec functions. 

## What are predicates?

## Refined type schemes

The unrefined type scheme is of the form A → B. 

Core refined type schemes take the form: 

    f : ∀a:tau. Πx:A. Pre → ∃b:τ. Σy:B. z:ct [eff]

Here, Pre is given by the grammar: 

Pre ::= take x = ct; Pre
     |  let x = ct; Pre
     |  let (x1, ..., xn) = ct; Pre 
     |  ·

This is the precondition of the function. It is a sequence of take-expressions asserting 
ownership of resources, and let-bindings naming subterms and destructuring tuples. It is 
not an assertion term, because we need its bindings to be visible in the postcondition. 

ct is a predicate term of type Pred () – i.e., a separation logic assertion. 

## Updating the context

The context should now be extended with logical assumptions (terms of type bool), and 
resource aassumptions (pairs of terms of sort τ and Pred τ). Because resources are substructural,
resource assumptions are marked with a 0/1 usage flag indicating whether they can be used (1)
or have already been used (0). 

u ::= 0 | 1 

Γ ::= ... | log x : ϕ, res x :ᵘ ct@ct 


So now we need a context well-formedness judgement:

Σ ⊢ Γ wf 


Σ ⊢ · wf 


Σ ⊢ Γ wf
———————————————————
Σ ⊢ Γ, comp x:A wf


Σ ⊢ Γ wf
———————————————————
Σ ⊢ Γ, spec x:τ wf


Σ ⊢ Γ wf    Σ; Γ ⊢ ct ⇐ bool
———————————————————————————————
Σ ⊢ Γ, log x:ct wf


Σ ⊢ Γ wf   Σ; Γ ⊢ ct' ⇒ τ   Σ; Γ ⊢ ct ⇐ pred τ    
——————————————————————————————————————————————————
Σ ⊢ Γ, x:ᵘ (ct @ ct') 


## Refined terms 

Refined terms are computational terms, plus some additional clauses for manipulating the 
proof state.

### Proof sorts/terms

The grammar of proof sorts is as follows: 

Pf ::= Pf ⊗ ... ⊗ Pf  | ϕ | ct @ ct | ∃x:τ. Pf 

The grammar of proof terms is as follows

pf ::= x | (pf, ..., pf) | pack(ct, pf) | cmd pf | auto | exfalso | pf : Pf 

cmd ::= auto | open-ret | make-ret | open-take | make-take | unfold[f,i]

The commands `cmd` all range over a number of substructural proof term
formers, with the following schemas for manipulating the proof states. The
cmd 

open-ret     : (return ct1@ct2)        ⊸ ∃x:τ (ct1 = ct2) 
make-ret     : (ct1 = ct2)             ⊸ (return ct1)@ct2 
open-take    : (take x = ct1; ct2)@ct3 ⊸ ∃x:τ. ct1@x ⊗ ct2@ct3
make-take    : ∃x:τ. ct1@x ⊗ ct2@ct3  ⊸ (take x = ct1; ct2)@ct3



unfold[f, i] : Pf                      ⊸ Pf  // unfold the i-th occurence of f in Pf 


The typing rules will be as follows: 

log x : ϕ ∈ Γ
——————————————————
Σ; Γ ⊢ x ⇒ ϕ ⊣ Γ


Γ = Γ0, res x :¹ (ct@ct'), Γ1
————————————————————————————————————————
Σ; Γ ⊢ x ⇒ ϕ ⊣ Γ0, res x:⁰(ct@ct'), Γ1


Σ; Γ1 ⊢ pf1 ⇐ Pf1 ⊣ Γ1 ... Σ; Γn ⊣ pfn ⇐ Pfn ⊣ Γ_(n+1)
————————————————————————————————————————————————————————
Σ; Γ ⊢ (pf1, ..., pfn) ⇐ Pf1 ⊗ ... ⊗ Pfn ⊣ Γ_(n+1)


Σ; Γ ⊢ ct ⇐ τ    Σ; Γ ⊢ pf : [ct/x]Pf
———————————————————————————————————————
Σ; Γ ⊢ pack(ct, pf) ⇐ ∃x:τ. Pf


Σ; Γ ⊢ pf ⇒ Pf1    Σ; Γ ⊢ cmd : Pf1 ⊸ Pf2   
———————————————————————————————————————————————
Σ; Γ ⊢ cmd pf ⇒ Pf2 



Σ; Γ ⊢ open-ret : (return ct1@ct2) ⊸ ∃x:τ (ct1 = ct2) 

Σ; Γ ⊢ make-ret : (ct1 = ct2) ⊸ (return ct1)@ct2 

Σ; Γ ⊢ open-take : (take x = ct1; ct2)@ct3 ⊸ ∃x:τ. ct1@x ⊗ ct2@ct3

Σ; Γ ⊢ make-take : ∃x:τ. ct1@x ⊗ ct2@ct3  ⊸ (take x = ct1; ct2)@ct3





Σ; Γ ⊧ ⊥
———————————————————
Σ; Γ ⊢ exfalso ⇐ P 


Σ; Γ ⊧ ϕ
————————————————
Σ; Γ ⊢ auto ⇐ ϕ



def(f(x) = e ∈ Σ)     unfold f(x) = e @ i in Pf1 ↝ Pf2 
————————————————————————————————————————————————————————
Σ; Γ ⊢ unfold[f,i] pf : Pf1 ⊸ Pf2 


unfold f(x) = e @ i in Pf1 ↝ Pf2 is a judgemenet finding the i-th occurence of 



















### Refined terms

The grammar of refined terms will be defined as follows. We will use ce for core terms, either pure or spec, and r for resource terms (to be defined later): 

Term variables and introduction forms are largely unchanged:

t ::= x 
   | (t1, ..., tn)
   | L t

Every binding form now needs an [a] binder to name the logical fact
tracking the equality of a value and its branches: 

   | let[a] x = t1; t2 
   | let[a] (x1, ..., xn) = t1; t2 
   | case([a] t, L1 x1 → t1 | ... | Ln xn → tn ) 
   | true | false | if[a] t1 then t2 else t3 

Since a spec type is now of the form `∀a:tau. Πx:A. P → ∃b:τ. Σy:B. z:ct`, we have to give it arguments 
for the assertion argument, the computational argument, and any resources it needs: 

   | let (a, x, u) = f(ce1, ce2, pf...)

Note that we have to pass in some terms proof terms pf which access the proof state. The
proof state contains both logical/pure facts and resource terms. 
Proof terms embed into refined terms as follows: 

t ::= ... 
   | let* x = pf; t 
   | let* (x1, .., xn) = pf; t 
   | let* pack(x, y) = pf; t 


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


## Typechecking

The checking/synthesis judgements is now of the form 

Σ; Γ; Ω ⊢ t ⇒ { x : A | ϕ(x) } [eff]

Σ; Γ; Ω ⊢ t ⇐ { x: A | ϕ(x) } [eff] 

