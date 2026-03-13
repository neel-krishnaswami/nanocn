# Adding refinement types to nanocn 

Next, we are going to add support refinement types to nanoCN. 

## What are predicates?

## Refined type schemes

The unrefined type scheme is of the form A → B. 

Refined type schemes take the form: 

    f : ∀a:tau. Πx:A. P → ∃b:τ. Σy:B. u:R [eff]

Here, P is given by the grammar: 

P ::= take x = ct; P 
   |  assert b; P 

This is the precondition of the function. It is a sequence of take-expressions and 



## Updating the context

The context should now be extended with logical assumptions (terms of type bool) and resources
(pairs R@e, where R : pred τ and e : τ)

Γ ::= ... | log x : ϕ | res x : R@e 

(return e)@e' ↝ e = e'
(take x:τ' = R1; R2)@t ↝ ∃x:τ'. R1@x * R2@t 

let (x, y, y') = z; 








## Refined terms 

Refined terms are computational terms, plus some additional clauses for manipulating the 
proof state. 

## Typechecking refined computational terms 

The synthesis judgement is now of the form 

Σ; Γ ⊢ e ⇒ { x : A | ϕ(x) } [eff]

Σ; Γ ⊢ e ⇐ { x: A | ϕ(x) } [eff] 

The grammar of refined expressions is (writing e for assertions): 

t ::= x 
   | (t1, ..., tn)
   | let (x1, ..., xn) = t1; t2 
   | L t
   | case(t, L1 x1 → t1 | ... | Ln xn → tn ) 
   | true | false | if[u] t1 then t2 else t3 
   | let [a1, ..., an] x with u1, ..., uk = f [e] t1; t2
   | let u' = unfold f at i in u; t 






## Converting core specification terms to SMT problems. 

TODO








