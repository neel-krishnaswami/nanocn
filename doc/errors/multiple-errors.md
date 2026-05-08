# Reporting multiple type errors

At the moment, typechecking of an expression stops as soon as the
first error is detected.

This makes LSP services less useful, since a type-incorrect function
will have no diagnostic information at all.

Therefore, I want to adapt the typechecker to continue even after a
program has an incorrectly typed subterm, so elaboration returns a
potentially partially-typechecked term, plus a list of type errors.

The paper by Zhao et al, "Total Type Error Localization and Recover
with Holes", suggests that we want to think of this an elaboration
problem. A program with type errors is basically the original program,
with annotations marking erroneous subterms.

Currently, the type of a bidirectional elaborator has the following shape: 

```
val check : Ctx.t -> Term.untyped -> Type.t -> Expr.typed monad
val synth : Ctx.t -> Term.untyped -> (Type.t * Expr.typed) monad

```

where: 

0. `Ctx.t` is basically a list of bindings: 

   ```
   type binding =
   | Term of Var.t * Sort.sort * Effect.t
   | TVar of Tvar.t * Kind.t
   ```

1. `'a monad` is some kind of augmented result monad, which either 
   returns a value or an error: 

   ```
   type 'a monad = Var.supply -> ('a * Var.supply, Error.t) result 
   
   ```

2. Both the `Term` and `Expr` modules have a polymorphic type of values, and 
   the concrete terms here will be defined something like: 

   ```
   type Term.untyped = < loc:SourcePos.t > Term.t
   
   type Expr.typed = type_info Expr.t
   
   type type_info = < loc:SourcePos.t; context : Ctx.t; sort : Sort.sort > 
   ```
   
   The change in the info parameter shows how we are systematically augmenting 
   each subterm with information useful to a language server. 
   
   Note that if we are only doing typechecking, then `Term` and `Expr`
   will be the same module.


## The Aim 

What we want to do is to (1) change the type of `type_info` to record both 
success and failure, (2) update the type of contexts to record variables we 
know are in scope but don't have any information about, (3) update the 
type of the typechecker to allow for continuing past errors, 

### Updating the info type

We will want to change `type_info` type to record both success and failure: 

```
type type_info = < loc:SourcePos.t; context : Ctx.t; answer : (Sort.sort, Error.t) result > 
```

Now the `sort` field has changed to the `answer` field, and it stores *either* the result of 
a successful typechecking step, *or* it stores an error for that node. 

### Updating the context types

For example, if we have a let-binding `let x = <error>; ce`, we will want to check 
`ce` even though `<error>` is an erroneous term with no type. So `x` will be in 
scope during the check of `ce`, but we won't know anything else about it. 

Therefore, we will want to update the type of bindings with an "unknown variable"
binding. For example, we will want `Context.binding` to become: 

```
type binding =
| Term of Var.t * Sort.sort * Effect.t
| TVar of Tvar.t * Kind.t
| Unknown of Var.t 
```

Likewise, the type of `RCtx.entry` will now become: 

```
type entry =
  | Comp of { var : Var.t; sort : Sort.sort; eff : Effect.t }
  | Log of { var : Var.t; prop : CoreExpr.typed_ce }
  | Res of { var : Var.t; pred : CoreExpr.typed_ce; value : CoreExpr.typed_ce; usage : Usage.t }
  | Unknown of { var : Var.t }
```

Both of these functions will need a function to extend a context with an unknown 
variable, and to update the `lookup_` and `use_` functions to return an error 
rather than option (in particular, `Error.kind` now needs a case covering 
variables of unknown type). 


#### Updating the elaborator monad

The type of `'a ElabM.t` will need to change to: 

```
type 'a t = Var.supply -> 'a * Var.supply
```

Note that the `fail` function will no longer be part of the API, and `ElabM.from_supply`
is no longer needed either. 

Indeed, this monad is *only* needed if the typechecking or elaboration
needs to construct create new variables as part of the typechecking
process. So pattern elaboration (in `elaborate.ml`) will need to use
it, but core and refinement typechecking (in `typecheck.ml` and
`refinement.ml`) currently don't.


#### The New Types of Synth and Check

We will update the types of `check` and `synth` as follows: 

```
val check : Ctx.t -> Term.untyped -> Type.t option -> Expr.typed monad
val synth : Ctx.t -> Term.untyped -> Expr.typed monad
```

There are two changes, in addition to the monad no longer admitting failure:

1. The `check` function takes a `Type.t option` argument, so that `check`
can be called even if no type is available to be invoked. 

2. The `synth` function no longer returns a separate type as a result. This 
is  because we can always write `(info e)#result` to get the type of an 
elaborated expression `e` (if it has one!). 

#### From extractors to views

Many typing rules disassemble and reassemble terms and types. Now that 
type synthesis can return an error result, and type checking can receive
`None` arguments, we don't want to have to separately case analyse whether a 
result has succeeded or failed, since this blows up the number of control-flow 
paths. Instead, we'll introduce a *view* API to let us pretend that *always* 
returns a result of the right shape during typechecking/synthesis, but which 
secretly returns as many `None` or `Error kind` values as needed whenever the 
construction or destructuring should fail. 

For each AST module which is destructured as part of typechecking (at last
`Sort`, `CoreExpr`, and`RPat`, but there may be more), we introduce a 
corresponding view module (i.e., `SortView`, `CoreExprView`, and `RPatView`). 
Each module contains a `Get` and a `Make` submodule which expose 

```
type 'a t = ('a, Error.kind) Result.t

module Get : sig
  val int : 'a Sort.t t -> unit t 
  val bool : 'a Sort.t t -> unit t 
  val ptr : 'a Sort.t t -> 'a Sort.t t 
  val pred : 'a Sort.t t -> 'a Sort.t t 
  val record : int -> 'a Sort.t t -> 'a Sort.t t list 
  val app : 'a Sort.t t -> Dsort.t t * 'a Sort.t t list 
end

module Build : sig 
  val int : 'info -> unit t -> 'info Sort.t t
  val bool : 'info -> unit t -> 'info Sort.t t 
  val ptr : 'info -> 'info Sort.t t -> 'info Sort.t t 
  val pred : 'info -> 'info Sort.t t -> 'info Sort.t t 
  val record : int -> 'info -> 'info Sort.t t list -> 'info Sort.t t
  val app : 'info -> Dsort.t t * 'info Sort.t t list -> 'info Sort.t t 
end
```

The invariants we want to maintain are that: 

* If `e` is not a blah constructor, `Get.blah e = Error k` for some `k`. 
* If `e` is a blah constructor, `Get.blah e = Ok a` for some `a`, and `a` will have *no* `Error`s in it. 
* If the argument `a` has *any* `Error`s in it, then `Build.blah info a = Error k` for some `k`. 
* If the argument `a` has *no* `Error`s in it, then `Build.blah info a = Ok e` for some `e`. 
* If `e` is a blah constructor, we have `Build.blah (info e) (Get.blah (Ok e)) = Ok e` 

(These can be property tested!)

Here's what the implementation of some of the functions in `Get` might
look like: 

```
let (return, fail, (let+)) = Result.(ok, error, bind)

let int tp' = 
	let+ tp = tp' in 
    match tp with 
    | Int -> return () 
    | _ -> let o = object method loc = loc end in 
            fail (Error.blah ...) 

let ptr tp' = 
	let+ tp = tp' in 
    match tp with 
    | Ptr tpbody -> return tpbody 
    | _ -> let o = object method loc = loc end in 
            fail (Error.blah ...)

let record n tp' = 
    let+ tp = tp' in 
    match tp with 
    | Record tps when n = List.length tps -> 
        List.map return tps 
    | _ ->  List.init n (fun _ -> Error.blah ...)

let app tp' = 
    let+ tp = tp' in 
    match tp with 
    | App(d, ts) -> (ok d, List.map return ts)
    | _          -> (fail (Error.blah ...), [])
```

Note that `record` takes an integer saying how many arguments to return. Here's 
the corresponding `Build` implementations:

```
(* result_list : ('a, 'b) result list -> ('a list, 'b) result 

   returns an 'a list if none of the elements have an Error. 
*)
let rec result_list xs = 
	let (return, fail, (let+)) = Result.(ok, error, bind) in 
    match xs with 
    | [] -> return []
    | y' :: ys' -> let+ y = y' in 
                   let+ ys = result_list ys' in
                   return (y :: ys)

let int info tp' = 
	let (return, fail, (let+)) = Result.(ok, error, bind) in 
	let+ () = tp' in 
    return (Sort.mk info Int) 

let ptr info tp' = 
	let (return, fail, (let+)) = Result.(ok, error, bind) in 
	let+ tp = tp' in 
    return (Sort.mk info (Ptr tp))

let record info n tps' = 
    let (return, fail, (let+)) = Result.(ok, error, bind) in 
	if n = List.length tps then 
		let+ tps = result_list tps' in 
        return (Sort.make info tps)
    else 
        fail (Error.arity_mismatch ...) 

let app info (d', tps') = 
    let (return, fail, (let+)) = Result.(ok, error, bind) in 
    let+ d = d' in 
    let+ tps = result_list tps' in 
    return (Sort.mk info (App(d, tps)))

```

## Updating the typecheckers 

Updating the typecheckers is relatively straightforward.  The control
flow of the clauses should get simpler, because we want to keep
checking the types of subterms no matter what, and so branching that 
conditionally check subterms should tend go away. 

If control flow doesn't simplify, then that's a red flag and the user
should be consulted.

Here are some of the patterns to be used in refactoring the typecheckers,
using typecheck.ml as a model: 


### Replace calls to SortGet.blah with SortView.Get.blah. 


### Successful returns 

Successful returns of the form 

   ```return (mk ctx pos sort eff shape)```

get changed to 
   
   ```return (mk ... (SortView.Build.blah ...) ... shape)```
   
or
   ```
   let result = SortView.Build.blah ... in 
   return (mk ... result ... shape)
   ```
   
### Linearizing conditional tests. 

Many typing rules test a condition, and signal an error if they fail. Here's
how to fix them. 

#### Tuple checking 

In some cases (such as typechecking tuples), the conditionals are
simply no longer needed, since the view functions already check the
arity, and we know what the arity is from the shape of the tuple 
or tuple pattern. 

#### Effect checking

Many conditionals arise from effect checking, which generate
conditionals which look like this:

   ```
   if not (Effect.sub Effect.Spec eff0) then
      Error (Error.spec_context_required ~loc:pos ~construct:"return")
   else
      ... 
      return (mk ctx pos (SortView.Build.blah ...) eff shape) 
    ```

Instead, we want to write this in a more linear way. First, define a pair of functions
 `check` and `&&`: 

```
   let check : bool -> Error.t -> (unit, Error.t) result = 
     fun b err -> 
       if b then Ok () else Error err

    let (&&) : (_, Error.t) -> ('a, Error.t) -> ('a, Error.t) 
    let (&&) x e = 
      match x with
      | Ok _ -> e 
      | Error msg -> Error msg 

```

Then, we change the code to: 
```
   let eff_check = check (Effect.sub Effect.Spec eff0) 
                         Error (Error.spec_context_required ~loc:pos ~construct:"return") in 
   ... 
   return (mk ctx pos (eff_check && SortView.Build.blah ...) eff shape) 
```

#### Cases 

##### Injections

There is a bit of implicit branching when we check the incoming type to see if it is
a datatype, and again when we look up the constructor. We want to just keep going 
in both of those cases: 

 ```
 | CoreExpr.Inject (l, e_inner) ->
     let (d, args) = SortView.Get.app sort
     let tp = Error.at ~loc:pos (CtorLookup.lookup sig_ l d args) in
     let eff0 = Effect.purify eff0 in 
     let* e_inner' = check sig_ ctx e_inner tp eff0' in 
     let result = tp && SortView.Build.app (d, args) in
     return mk ctx pos e_inner'
 ```

The only monadic binding arises from the recursive call, in case it allocates new 
variables. 

##### Case analysis 

The existing code actually has a bug: it will not notice if there are any missing 
cases! 
  
It looks like this: 
```
  | CoreExpr.Case (scrut, branches) ->
    let eff0' = Effect.purify eff0 in
    let* scrut' = synth sig_ ctx eff0' scrut in
    let scrut_sort = (CoreExpr.info scrut')#sort in
    let* branches' =
      check_case_branches sig_ ctx branches scrut_sort sort eff0 eff0' pos in
    Ok (mk ctx pos sort eff0 (CoreExpr.Case (scrut', branches')))
	
and check_case_branches sig_ ctx branches scrut_sort result_sort eff0 bind_eff pos =
  match Sort.shape scrut_sort with
  | Sort.App (_d, args) ->
    let rec go = function
      | [] -> Ok []
      | (l, x, body, _) :: rest ->
        (match CtorLookup.lookup sig_ l args with
         | Ok ctor_sort ->
           let ctx' = Context.extend x ctor_sort bind_eff ctx in
           << typecheck the branch >> 
         | Error k ->
           Error (Error.structured ~loc:pos k))
    in
    go branches
  | _ -> Error (Error.scrutinee_not_data ~loc:pos ~got:scrut_sort)
```

So `check_case_branches` never checks to see if there are any
*missing* or *redundant* constructor cases!

We need to identify: 

1. Missing constructors
2. Redundant constructors
3. Constructors which are not in D(tau, ...). 

To do this, we introduce two functions. 

1.  `CtorLookup.lookup_all` which takes a datatype name and a list of
	arguments, and returns a list of constructors and the expected types
	at that type instantiation.

2. `merge_branches`, which takes the list of branches, and the list of label-type
   pairs, and pairs each branch up with its expected type, introducing extra branches 
   with a hole for the missing constructors, and a suitable errors instead of a good type 
   for redundant cases or cases not occuring in the case. 



```
    let eff0' = Effect.purify eff0 in
    let* scrut' = synth sig_ ctx eff0' scrut in
    let (d', args) = SortView.Get.app (CoreExpr.info scrut')#sort in
	let lts = (let+ d = d' in 
               let+ args = result_list args in 
               CtorLookup.lookup_all sig_ d args) in
    let (merged_branches, err) = merge_branches branches lts in 
	let branches' = List.map map (check_branch sig_ ctx eff) merged_branches in 
    let result = err && SortView.Get.build (CoreExpr.info scrut') (d', args) in
	return (mk ctx pos sort eff0 (CoreExpr.Case (scrut', branches')))
```


## Elaboration

The main (indeed, only) issue here is the pattern elaboration judgement. Do not rush 
in to implement this: instead, first update `typecheck.ml` and then lay out the issues. 


## Refined typechecking 

This should actually work out more easily than elaboration of surface patterns
do! 



                                                                               
## General design issue: Elaboration 

For a pure typechecker, the only difference between the original and the 
elaborated term is that the elaborated term has been annotated with 
type and context information. 

However, elaboration takes a term in source language, and produces a
term in a target language, and the elaboration process can depend upon
certain type invariants holding of the source term.

But if are continuing past errors, those invariants might not hold, 
and an erroneous program might not have any sensible elaboration.

So the question arises: what should we do when we elaborating terms
whose output substantially rearranges the input (eg, pattern matching), but
the input has errors in it?

This is where *holes* become useful. If we have no idea what to
generate, we can create a hole and put it into the core term. This
will be a placeholder upon which we can hang errors, and which will
let us continue typechecking, but we no longer have to creatively
invent terms if (for example) we miss some cases in a pattern match.

If any of the elaboration targets do not have a hole constructor, add it. 










