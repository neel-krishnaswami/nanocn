# Adding Typed Holes to nanoCN 

Writing refined programs blank slate is difficult, because we want to
write programs incrementally. To fix this, I want to add *typed
holes*, which will let the typechecker operate on incomplete programs.

I want to extend the lexer and grammar with hole expressions, which are 
terms of the form 

    $name 

As a specific exception, this feature should NOT be part of
syntax.ott. This is a purely implementation-side feature.

## Typechecking

### Core typechecking

In the core typechecker, holes are treated as checking forms, with the typing
rule: 

—————————————————————————————————
Σ; Γ ⊢[eff] $name <== τ 

### Surface typechecking

In the surface typechecker, holes are treated as checking forms, with the typing
rule: 

—————————————————————————————————
Σ; Γ ⊢[eff] $name <== τ ↝ $name 

That is, holes elaborate to a hole of the same name in a core expression. 


### Refined typechecking

In the refined typechecker, holes are treated as checking forms, with the typing 
rule: 

Δ' = affinize(Δ) 
——————————————————————————————
Σ; Δ ⊢ $name <== Pf ⊣ Δ' ↝ ⊤

That is, checking always succeeds, and every linear variable in Δ becomes
affine, so that it can be used or not in subsequent code. 

## Editor support 

Holes are obviously not compilable, but they mainly exist to permit typechecking 
to continue even when part of the program does not yet exist. 

### Syntax highlighting 

Holes should are syntax highlighted with a yellow background. I don't know whether
this should be a lsp feature or an editor feature. For now either work. 

### Listing holes 

The LSP server should maintain a list of all active holes, and allow users
to see their types, and jump to them. 














