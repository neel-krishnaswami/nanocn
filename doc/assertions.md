# Assertion language 

This is the language of nanocn assertions. 

## Contexts and Signatures 

### Signatures

The datasort declarations and specification function types are all stored in 
the context Σ, right alongside computational function definitions. 

Σ will now contain: 

   - function types f : A → B [eff]
   - specification function types f : τ1 → τ2 [spec]
   - datasort declarations sort D(a1, ..., an) = { C1 : τ1, ..., Cn : τn }

### Contexts

Assertion variables are stored in Γ, alongside computational variables. 

Γ will now contain 

   - computational variable declarations  x:A [comp]
   - specification variable declarations  x:A [spec]

##

## Assertion sorts

The sorts of assertions are given as follows: 

τ ::= Z | bool | Loc | (τ1 * ... * τn) | D(τ1, ..., τn) | Pred τ | A

X ::= τ | τ → τ 

The D(τ1, ..., τn) are references to datatype declarations, which
are all defined at the toplevel. 


## Toplevel declarations 

The toplevel declarations are a list of  specification function and 
datatype declarations. 

### Specification functions

Spec functions come in two forms:

1. Clausal spec functions define a function by pattern matching:

   spec f : τ1 → τ2 = { pat1 → e1 | ... | patn → en }

   Each pattern is checked against τ1, and each body against τ2.

2. Simple spec definitions bind a name to a spec expression:

   spec f : τ = e

   The expression e is checked against τ.

Spec functions are allowed to be recursive, though internally we track
whether a function is recursive (this may help SMT encodings).

Eventually we will need to termination check definitions, but for now,
omit it – the right balance of expressiveness and prover-encodability
is not yet clear. (Proof assistants like strictly structurally recursive
definitions, but this makes things like merge sort harder to express.)

### Datasort declarations

Where the datasort declarations are given at the toplevel as: 

sort D (a1, ..., an) = 
   | C1 of τ1      
   | ...
   | Cn of τn

The τi can refer to type variables ai in each branch, D(a1, ..., an)
or other types τ using datasorts D' defined earlier. There must also be
at least one constructor which is not recursive. 

For each datasort, we can construct a minimal term function, which
takes minimal terms for each of its parameter types and constructs a
value of the constructor Ci with the smallest type τi. Here, smallest
means the non-recursive constructor with the smallest type parameter. 
(In Ocaml, we can do this by annotating the type representing datasort
declarations with the preferred constructor for minimal examples.)

This will be used for error messages for pattern matching. 


### The signature 


### Surface Expressions


The pattern syntax of surface expressions looks like the following: 

pat ::= x | C p | (p1, ..., pn) | pat : τ  

Patterns cannot have repeated variables – (x, x) is forbidden

We use this to define the surface expression language: 

e ::= e = e
    | e && e
    | not e
    | take pat = e1; e2
    | return e
    | let p = e1; e2
    | case e1 of { pat1 -> e1 | ... | patn -> en }
    | f e
    | if e1 then e2 else e3
    | e                        (computational expression, must be pure)
    | e : τ

There is a pattern typing judgement: Σ ⊢ pat : τ ↝ Γ  used to learn the types of the variables in each branch 
of a pattern. 

———————————————————
Σ ⊢ x : τ ↝ x : τ [spec]

1. (sort D(a1, ..., an) = {C1 : τ1, ...}) ∈ Σ      
2. C = Ci 
3. Σ ⊢ pat : [τ1/a1, ..., τn/an]τi ↝ Γ
——————————————————————————————————————————————————
Σ ⊢ C pat : D(τ1, ..., τn) ↝ Γ


Σ ⊢ pat : τ ↝ Γ
——————————————————————————————————————————————————
Σ ⊢ (pat : τ) : τ ↝ Γ


Σ ⊢ pat1 : τ1 ↝ Γ1  ...  Σ ⊢ patk : τk ↝ Γk
———————————————————————————————————————————————————————
Σ ⊢ (pat1, ..., patk) : (τ1 * ... * τk) ↝ Γ1, ..., Γk


We typecheck surface expressions using a judgement Σ; Γ ⊢ e : τ. The main difference
is that the variable rule for assertion expressions is of the form: 

  x:τ [spec] ∈ Γ 
  ———————————————
  Σ; Γ ⊢ x : τ

(Similarly, the variable rule for computations checks that it is only using computation 
variables.) 

This will have its own set of bidirectional typing rules to improve error messages. 

### The core assertion expressions

We also elaborate patterns away to send surface expressions into a
core expression language. This will simplify things later. 

e ::= e = e
    | e && e
    | not e
    | Own<τ>
    | take x = e1; e2
    | return e
    | C e
    | case e of { C1 x1 -> e1 | ... | Cn xn -> en }
    | (e1, ..., en)
    | let (x1, ..., xn) = e1; e2
    | f e
    | let x = e1; e2
    | if e1 then e2 else e3
    | e : τ


I explain how to elaborate pattern matching away. In everything which
follows, all newly-introduced variables should be fresh.

"take pat = e1; e2"  should be rewritten to "take x = e1; case x of {pat -> e2}"

and similarly for let pat = e1; e2. 

After this elaboration step, complex pattern matching  
can be simplified to core terms using a matrix-style compilation algorithm.

We will introduce an auxilliary n-ary match function 

match es1;...; ek of { pats1 -> e1 | ... | patsn -> en } 

where 

es ::= · | e; es 
pats ::= · | pat; pats 
branches ::= · | pats → e "|" branches


(The infix syntax match es1;...; ek of { pats1 -> e1 | ... | patsn -> en } is intended to be
suggestive notation.) 

1. It recursively simplifies as follows, with the base cases: 

   match · { · → e1 | ... | · → en } simplifies to e1 

   If there are no branches match · { } then this is a sign of an incomplete pattern match! 

This implements left-to-right matching semantics. 

2. If we have no branches left: 

      match es { } 

   This is a coverage failure.  Generate a minimal term of each type
   in es and return it as an erroneous uncovered value list.


3. If all the leading patterns are variables, like this: 

   match e; es of { x1; pats1 → e1; ...; xn; patsn -> en}

   where e : τ, it gets rewritten to 

   let x = e; match es of { pats1 → let x1 = x; e1 | ... | patsn -> let xn = x; en }

   If the inner match returns an uncovered counterexample vs, generate a minimal example v of τ and 
   return v; vs. 

4. if we have 

   match e;es of { pats1 → e1 | ... } 

   and e is of the type (τ1 * ... * τk) 

   and every arm is either of the form

      (pat1, ..., patk); pats → e 

   or of the form 

      x; pats → e 

   Then we rewrite it to 

   let (x1, ..., xk) = e; 
   match x1; ...; xk; es of { branches }

   where (pat1, ..., patk); pats → e  gets sent to pat1; ...; patk; pats → e 
   and   y; pats → e  gets sent to u1; ...; uk; pats → let y = (u1, ..., uk); e 


5. If we have

   match e;es of { branches } 

   and e is of the type D(τ1, ... τk) with constructors

      C1 .. Cm 

   Then we group the branches by constructor. 
     - A branch Ci p; pats → e is sent to the Ci-th group as p; pats → e 
     - A branch x; pats → e is sent to the C-ith group as  y; pats → let x = Ci y; e   
       where y is fresh

   Then match e; es of {branches} gets rewritten to 

   case e { Ci xi → (match xi; es { C-th branch group}),  for all m branches } 

   If one of the xi; es returns a failing example v; vs, then return Ci v; vs as the
   coverage counterexample. 


