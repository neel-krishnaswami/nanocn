# Extending signatures to carry the full definitions of specs 

Currently, the sort \Sigma of signatures contains data and type declarations, plus prototypes of pure, spec, and impure functions. 

I want to extend the Ott file so that signatures contain not just the prototype, but also the code for pure and spec functions. So 

Σ ::= ·
   |  Σ, type D(...) = { ... } 
   |  Σ, data D(...) = { ... } 
   |  Σ, f : A → B [impure]
   |  Σ, f : τ → τ = body [eff]   (when eff ≤ spec)

There should be two variants of Σ, one containing surface definitions (which may do pattern matching), and one containing core definitions. 

1. Extend the grammar, and update the typing rules to use the appropriate sort of context. If there are overflows in the rules looking up spec function prototypes, you can introduce an auxilliary definition to do type lookups in the signature. 

2. Once you have done this, let the user review the changes and make any revisions they want/

3. Then, extend the elaboration algorithm to take in a surface signature and 
   produce a core signature: Σ_s; Γ ⊢ y; M ⇐eff τ ⇝ Σ_c; ce

4. Let the user review the changes and iterate until they are happy.
