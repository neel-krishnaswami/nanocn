# Refactoring the typechecker 

Right now, the typechecker code is extremely annoying to read, because
of two primary factors: 

1. The code has a bunch of deeply-nested pattern matches.
2. The error-handling code is overly-verbose. 

These two issues need to be resolved together, because of the way that the 
problem arises. Consider the iter rule from the refined typechecker: 


    1. CS = comp RS
    2. type D(x, y) = { Next : a, Done : b } in CS
    3. G = erase Delta
    4. CS ; G ⊢[spec] ce ==> pred D(A, B)
    5. RS ; Delta ⊢[pure] crt1 r<== x : A[pure], ce @ Next x[res], pfnil ⊣ Delta' ~~> Ct
    6. RS ; Delta', x : A[pure], y : ce @ Next x[res(1)] ⊢[impure]
            crt2 r<== z : D(A, B)[pure], ce @ z[res], pfnil
        ⊣ Delta', x : A[pure], y : ce @ Next x[res(0)]
        ~~> Ct'
    —————————————————————————————————————————————————————————————————————————————————————————
    RS ; Delta ⊢[impure] iter[ce](q = crt1) { crt2 } r==> z : B[pure], ce @ Done z[res], pfnil
    ⊣ Delta' ~~> Ct ∧ ∀ x : A . Ct'

Look at premise 4 in the rule, which checks that ce is a D(A,B)-valued predicate. Now
compare this to the code from rCheck.ml, for checking the CIter constructor: 

```
    let* (ce_pred, pred_sort) = elab_and_synth rs delta Effect.Spec se_pred in
    (match Sort.shape pred_sort with
     | Sort.Pred inner_sort ->
       (match Sort.shape inner_sort with
        | Sort.App (_dsort_name, args) ->
           <<rest of the code>> 
        | _ ->
          ElabM.fail
            (Error.construct_sort_mismatch ~loc:binfo#loc
               ~construct:"iter" ~expected_shape:"D(A, B)"
               ~got:inner_sort))
     | _ ->
       ElabM.fail
         (Error.construct_sort_mismatch ~loc:binfo#loc
            ~construct:"iter" ~expected_shape:"Pred D(A, B)"
            ~got:pred_sort))
```
The way to fix this is to introduce *fallible extractors*, which are monadic 
functions which take a term (a sort, a core expression, a pattern, etc), and 
either return the subterms, or return a mismatch error of the right kind: 

   get_pred : Sort.sort -> (Sort.sort, Error.kind) result 
   get_app : Sort.sort -> (string * Sort.sort list, Error.kind) result 

These can then be lifted into the elaboration monad to avoid deep nesting: 

```
    let* (ce_pred, pred_sort) = elab_and_synth rs delta Effect.Spec se_pred in
    let* inner_sort = lift_at pos (Sort.get_pred pred_sort) in 
    let* (_dsort_name, args) = lift_at pos (Sort.get_app inner_sort) in 
    <<rest of code>> 
```

This also simplifies the elaborator code, since the error production is encapsulated
within the extractor. 
