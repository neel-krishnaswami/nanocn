# SMT encoding design documentation, part 2

## Translating nanocn expressions to SMT-LIB format. 

Now, we want to extend nanoCN to turn constraints into SMT problems, and feed them to 
Z3 and CVC5. 

Phase 1 is to extend nanocn to take a file "foo.cn", elaborate and typecheck it, and produce a file "foo-constraints.smt" as output, and then invoke Z3 on it. 


### The global part of the translation 

Before any constraints are translated, we need to establish an
environment in which translations of all nanoCN types are available,
and declarations and definitions of spec and pure functions are
available to the constraints.

#### Translating Sorts 

The CN grammar of sorts is translated to SMT sorts as follows: 

##### Basic Types 

We translate Int to Int and Bool to Bool, so no action is needed in the 
prelude for these types. 

##### Pointer Types

We translate pointer types as: 

(declare-datatype Ptr
   (par (A)
      ((loc (addr Int)))))

This is a different type wrapping integers, to ensure that it is  a type error 
if pointers and integers are mixed. 

##### Tuple Types

For n = 0, declare

(declare-datatype Tuple-0
  ((tuple-0)))

FOr n=1, declare nothing. 

For n = 2 to 16, we declare: 

(declare-datatype Tuple-n 
   (par (T1 ... Tn)
     (tuple-n (prj-n-1 T1) ... (prj-n-n Tn))))

All of these are declared after the Ptr declaration. If 16-tuples turn out to be insufficient, we can extend it later.

##### Datatype declarations

After the tuple types are available, we collect all the type and sort definitions 
from the program, and translate them uniformly to SMT definitions.  

type D(a1, ..., an) = { L1 : τ1 | ... | Lk : τk }

gets translated to 

(declare-datatype D
   (par (a1 ... an)
     ((D-L1 (get-D-L1 τ1))
      ... 
      (D-Lk (get-D-Lk τk)))))

where the τi are the translated types of τ1 to τk. 

##### Monadic types

Last, we declare a sort

```
(declare-sort Pred 1)
```

in the prelude, after the translated datatypes, but before we give translations of
any program code. 

Then, let R be the set of types each return in the program occurs at,
and let T be the set of pairs of types each take in the program is
used at.

take x = return 3;
take y = return true; 
if y then 
  return "yes"
else 
  return "no"

In this program, R = {Int, Bool}, and T = {(Int, String), (Bool, String)}. 
Then let S = R ∪ {τ | (τ, _) ∈ T ∨ (_, τ) ∈ T} (that is, the set of all monadic
types in the program). 

Then, for each τ ∈ S, we add declarations:

(declare-const fail-τ (Pred τ))

(declare-fun return-τ (τ) (Pred τ))

For each (τ,σ) ∈ S × S, we add the declaration 

(declare-fun bind-τ-σ ((Pred τ) (-> τ (Pred σ))) (Pred σ))

Then, we want to declare a collection of equations for the monad laws.

For each τ ∈ S, we add the equation: 

(assert (forall ((m (Pred τ)))
  (! (= (bind-τ-τ m return-τ)
     	m)
     :pattern (bind-τ-τ m return-τ))))

For each (τ, σ) ∈ S × S, we add the equation: 

(assert (forall ((a τ) (f (-> τ (Pred σ))))
  (! (= (bind-τ-σ (return-τ a) f) 
     	(f a))
     :pattern (bind-τ-σ (return-τ a) f))))

For each (τ, σ, ρ) ∈ S³, we add the equation: 

(assert (forall ((m (Pred τ)) (f (-> τ (Pred σ))) (g (-> σ (Pred ρ))))
  (! (= (bind-σ-ρ (bind-τ-σ m f) g)
     	(bind-τ-ρ m (lambda ((x τ)) (bind-σ-ρ (f x) g))))
     :pattern (bind-σ-ρ (bind-τ-σ m f) g))))

These 3 families of equations should ensure predicate terms satisfy the monad laws. 

Additionally, we add the equations for each τ in S and (τ,σ) ∈ S²:

(assert (forall ((m (Pred τ)))
  (! (= (bind-τ-σ m (lambda ((x τ)) fail-σ))
        fail-σ)
     :pattern (bind-τ-σ m (lambda ((x τ)) fail-σ)))))

(assert (forall ((f (-> τ (Pred σ))))
  (! (= (bind-τ-σ fail-τ f)
        fail-σ)
     :pattern (bind-τ-σ fail-τ f))))

These implement the monad-fail laws. 

The pattern declarations orient the equations to rewrite programs to a normal 
form. The critical pairs are all confluent (I think, after a quick check). 

(As an aside, note that even though the arrow type is an array in Z3, we
don't need to write select in Z3 because locally-bound arrays can be
used as functions.)

##### Ownership predicates

Refined typechecking produces constraints that refer to the ownership
predicate `Own[τ] p`, which takes a pointer of type `Ptr τ` and yields a
predicate of type `Pred τ`. In SMT we model `Own` as a family of
uninterpreted functions, one per pointee sort τ that reaches `Own[τ]` in
the program:

    (declare-fun own-τ ((Ptr τ)) (Pred τ))

Let O be the set of pointee sorts τ such that `Own[τ]` occurs anywhere in
the program (in function bodies, constraint atoms, or implications).
For each τ ∈ O we emit one such declaration in the prelude, after the
monad declarations and before the user-defined function definitions.
Since `Own[τ] p` itself produces a `Pred τ`, τ is also added to S so
that the corresponding `return-τ`, `fail-τ`, and `bind-τ-_` / `bind-_-τ`
declarations are available.

No equations are asserted about `own-τ`: it is a genuine uninterpreted
symbol whose equational theory is provided by whatever refined-typing
axioms the user asserts at the constraint level.

#### Programs 

Programs are translated definition-by-definition, in order, and add information to 
the context.

##### Type and sort declarations

Remember that all of the type and sort definitions are hoisted from
where they occur in the program text up to the front of the SMT
prelude, after the tuple declarations.

##### Function declarations 

###### Impure functions

We skip these!

###### Pure functions

For each function fun f(x : τ) : τ' [pure] = { ce } in the program, in the prelude
we introduce a function definition: 

(define-fun f (x τ) τ' SMT(ce))

Because pure functions are nonrecursive, it is safe to make their whole definition
visible. 

###### Spec functions

For each fun f(x : τ) : τ' [pure] = { ce } in the program

we introduce a declaration: 

(declare-fun f (x τ) τ')

Because spec functions are possibly recursive, they have to be explicitly 
unfolded by the user, so they are just declarations in the SMT format.

## The local part of the translation. 

Both the translation of the whole program and the translation of
constraints uses the translation of expressions, which can be done
locally and compositionallly.

### Translating core terms to SMT terms. 

Note that we will have the types of subterms available. 

SMT(x : τ) = x 
SMT(n) = n 
SMT(b) = b 
SMT(let x = ce1; ce2) = (let ((x SMT(ce1))) SMT(ce2))
SMT((ce1, ..., cen)) = (tuple-n SMT(ce1) ... SMT(cen))
SMT(let (x1, ..., xn) = ce1; ce2) = 
  (match SMT(ce1) ((tuple-n x1 ... xn) SMT(ce2)))
SMT(L ce) = (L SMT(ce))
SMT(case ce of {L1 x1 → ce1 | ... Lk xk → cek}) = 
  (match SMT(ce)
    ((L1 x1) SMT(ce1))
    ... 
    ((Lk xk) SMT(cek)))
SMT(if ce then ce1 else ce2) = (ite SMT(ce) SMT(ce1) SMT(ce2))
SMT(prim ce) = (SMT(prim) ce)
SMT(f ce)    = (f SMT(ce))
SMT(ce1 = ce2) = (= SMT(ce1) SMT(ce2))
SMT(return ce : Pred τ) = (return-τ SMT(ce))
SMT(take x = (ce1 : Pred τ); (ce2 : Pred σ)) = 
  (bind-τ-σ SMT(ce1) (lambda ((x τ)) SMT(ce2)))
SMT(fail : Pred τ) = fail-τ
SMT(Own[τ] ce) = (own-τ SMT(ce))


## Translating constraints 

The grammar of constraints is as follows:

C ::= ⊤ | ⊥ | C1 ∧ C2 | ∀x :τ. C | ce ⇒ C | ce

We are trying to check validity of these formulas, and C is valid if ¬C is satisfiable. 
Here's how we translate each constraint into a list of SMT instructions. 

SMT(⊤) = <emit nothing>

SMT(⊥) = (check-sat)

SMT(C1 ∧ C2) = SMT(C1)
                SMT(C2)  

SMT(ce ⇒ C) = (assert SMT(ce))
              SMT(C)

SMT(ce) = (assert (not SMT(ce)))
          (check-sat)

SMT(∀x:τ.C) = (push)
              (declare-const x τ)
              SMT(C)
              (pop)

In the implementation, ensure that you put source position information as a comment
before each constraint. 
