# Merging 

According to syntax.ott:

  * Pure computations are a subset of assertions. 
  * Pure computations are a subset of impure computations. 

As a result, we can merge these three classes into one syntactic sort, and then use the effect system
to distinguish them. 

We will do this in the following steps. 

## Extend the effect system

Right now we have 0 (pure) and 1 (impure) effects. 

We will change this to 'pure', 'impure', and 'spec' effects.

The subeffect ordering is now: 

   e ≤ e            
   pure ≤ impure
   pure ≤ spec      

This is also trivially transitive. It has a *partial* join operator, where 

e    ⊔ e                   = e 
pure ⊔ impure              = impure
impure ⊔ pure              = impure
pure ⊔ spec                = spec
spec ⊔ pure                = spec

impure ⊔ spec              = undefined
spec ⊔ impure              = undefined 


## Merge the type/sort grammars 

- There should be *one* grammar of sorts τ, containing all of the type formers of both sorts and types. 
- Type and sort declarations should remain separate (because computations are more restricted)
- Types A should be a subgrammar of sorts

## Merge the expression syntax 

- There should be one grammar of surface expressions se, and one grammar of core expressions ce. 
- The distinct assertion forms should be ascribed the 'spec' effect. 
- The distinct impure forms should be ascribed the 'impure' effect. 
- The pure forms should be ascribed the 'pure' effect. 

## Merge the toplevel signatures: 

Now sigma has sort declarations, type declarations, and function declarations. We do not 
need to distinguish between spec and computation declarations, since the effect will track that. 

## Merge the core typing rules. 

- Contexts continue to distinguish spec and computation variables. 
- We should have one set of bidirectional rules for expressions
- Looking up computation variables has the `pure` effect, as before. 
- Looking up spec variables now has the `spec` effect. 

## Merge the elaboration rules for surface expressions. 

- We have *one* elaboration judgement for all surface expressions, because the effect
  system tracks what is pure, impure or a spec. 
- In particular, pattern elaboration works uniformly. 

## Merge the toplevel function declarations. 

- Add a grammar of core programs cprog, corresponding to the core expression/surface expresion distinction.
  These are function declarations of the form

     fun (x : tau) → tau' [eff] = e 

- These are typechecked using the core typechecking routine. The only caveat is that if eff is not spec,
  we must additionally check that the types are in the computation subgrammar A.

- Update the grammar of surface programs prog, and elaborate it to a core program. 

- All function declarations now work the same way, distinguished by the spec/pure/impure effect. 
  All surface definitions are clausal, the way spec functions are now. 






