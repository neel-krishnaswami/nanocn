# Adding refinement types to nanocn 

Next, we are going to add support refinement types to nanoCN. 

## What are predicates?

1. Predicates are just specification terms of type bool. 
2. We restrict the equality predicate in specifications to non-predicate types. 

## Refined type schemes

The unrefined type scheme is of the form A → B. 

Refined type schemes take the form: 

    f : ∀a:tau. Πx:A. ϕ(x,y) → ∃b:τ. Σy:B. ψ(a, x, b, y) [eff]

There will be a similar declaration for every primitive, as well. 

## Updating the context

The context should now be extended with logical assumptions (terms of type bool) 

Γ ::= ... | log x : ϕ

## Typechecking refined computational terms 


## Converting core specification terms to SMT problems. 









