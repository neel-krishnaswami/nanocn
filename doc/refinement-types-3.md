# Adding refinement types to nanocn 

Next, we are going to add support refinement types to nanoCN. 

## What are predicates?

## Refined type schemes

The unrefined type scheme is of the form A → B. 

Core refined type schemes take the form: 

    f : ∀a:tau. Πx:A. take x = ct → ∃b:τ. Σy:B. z:ct [eff]

Here, P is given by the grammar: 

P ::= take x = ct; P 
   |  let x = ct; P 
   |  let (x1, ..., xn) = ct; P 

This is the precondition of the function. It is a sequence of take-expressions asserting 
ownership of resources, and let-bindings naming subterms and destructuring tuples. It is 
not an assertion term, because we need its bindings to be visible in the postcondition. 

ct is a predicate term of type Pred () – i.e., a separation logic assertion. 

## Updating the context

The context should now be extended with logical assumptions (terms of type bool). 

Γ ::= ... | log x : ϕ 

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



We also need a substructural resource context Ω ::= · | Ω, u : ct@ct' 

Resource context well-formedness is given by a judgement Σ; Γ ⊢ Ω wf 

Σ; Γ ⊢ · wf


Σ; Γ ⊢ Ω wf    Σ; Γ ⊢ ct' ⇒ τ   Σ; Γ ⊢ ct ⇐ pred τ    
—————————————————————————————————————————————————————
Σ; Γ ⊢ Ω, u : ct@ct' wf 


## Refined terms 

Refined terms are computational terms, plus some additional clauses for manipulating the 
proof state. 

## Typechecking refined computational terms 

The synthesis judgement is now of the form 

Σ; Γ; Ω ⊢ t ⇒ { x : A | ϕ(x) } [eff]

Σ; Γ; Ω ⊢ t ⇐ { x: A | ϕ(x) } [eff] 

The grammar of refined terms will be defined as follows. We will use ce for core terms, either pure or spec, and r for resource terms (to be defined later): 

Variables and introduction forms are unchanged:

t ::= x 
   | (t1, ..., tn)
   | L t

Every value eliminator now needs a [a] binder to name the logical fact
tracking the equality of a value and its branches:

   | let[a] (x1, ..., xn) = t1; t2 
   | case([a] t, L1 x1 → t1 | ... | Ln xn → tn ) 
   | true | false | if[a] t1 then t2 else t3 

Since a spec type is now of the form `∀a:tau. Πx:A. P → ∃b:τ. Σy:B. z:ct`, we have to give it arguments 
for the assertion argument, the computational argument, and any resources it needs: 

   | let* (a, x, u) = f(ce1, ce2, r...)

Next, we need terms for manipulating the proof state. 

Below is the syntax for unfolding a function call. It works by taking
the body of f, substituting ct for its formal parameters, and then
adding the equation `f(ct) = [ct/x]ct'` when `f(x) = ct'`.

   | let* log x' = unfold f ct; t 

Next, we have rules for destructuring resources. If r : (return ct1)@ct2 ∈ Ω, then we can turn it into a
logical fact: 

   | let*  x = ret u; t   // Γ; Ω, u:(return ct1)@ct2 ↝ Γ, log x:(ct1 = ct2); Ω 

We can pull asserts out of computations:

   | let* (x, u') = assert u; t // Γ; Ω, u:(assert ct1; ct2)@ct3 ↝ Γ, log x: ϕ; Ω, u':ct2@ct3 

We can split takes: 

   | let* (x, u1, u2) = bind u; Γ; Ω, u:(take x = ct1; ct2)@ct3 ↝ Γ, x:tau; Ω, u1:ct1@x, u2:ct2@ct3 

We will omit rules for performing reductions, because we will use the SMT solver to perform them. 

## Solver representation

We will declare a new sort for predicates:

(declare-sort Pred 1)

(declare-sort-parameter A)
(declare-sort-parameter B)

(declare-fun return (A) (Pred A))

(declare-fun bind ((Pred A) (-> A (Pred B))) (Pred B))


(assert (forall ((m (M A)))
  (= (bind m return) m)))

(assert (forall ((x A) (f (-> A (M B))))
  (= (bind (return x) f) (@ f x))))

(assert (forall ((m (M A)) (f (-> A (M B))) (g (-> B (M C))))
  (= (bind (bind m f) g)
     (bind m (lambda ((x A)) (bind (@ f x) g))))))