# SMT-LIB 2.7 Summary: Key Features

## 1. Polymorphic Datatype Declarations

Datatypes are introduced with `declare-datatype` (single) or `declare-datatypes` (mutually recursive). The general form of `declare-datatypes` is:

```smt2
(declare-datatypes ( (δ₁ k₁) ... (δₙ kₙ) ) ( d₁ ... dₙ ))
```

where each `(δᵢ kᵢ)` declares a sort symbol of arity `kᵢ`, and each `dᵢ` is the constructor list. **Polymorphic** datatypes wrap the constructor list in `(par (u₁ ... uₖ) ...)`:

```smt2
; Parametric lists
(declare-datatypes ( (List 1) ) (
  (par (T) ( (nil) (cons (car T) (cdr (List T))) ))))

; Option type
(declare-datatypes ( (Option 1) ) (
  (par (X) ( (none) (some (val X)) ))))

; Parametric pairs
(declare-datatypes ( (Pair 2) ) (
  (par (X Y) ( (pair (first X) (second Y)) ))))

; Mutually recursive parametric datatypes
(declare-datatypes ( (Tree 1) (TreeList 1) ) (
  (par (X) ( (node (value X) (children (TreeList X))) ))
  (par (Y) ( (empty) (insert (head (Tree Y)) (tail (TreeList Y))) ))))
```

Each constructor `c` gets a **tester** `(_ is c)` of rank `δ Bool` automatically. Each constructor argument `(s τ)` declares a **selector** `s` of rank `δ τ`. Constructors become function symbols of parametric rank `τ₁ ··· τₘ δ`. The datatype must be **well-founded**: at least one constructor's argument sorts must not mention any of the datatypes being declared (or only mention well-founded ones). **Nested patterns are not allowed** -- constructor arguments in patterns are always variables.

The shorthand `declare-datatype` covers the non-mutual case:

```smt2
(declare-datatype List (par (E)
  ( (nil) (cons (car E) (cdr (List E))) )))
```

## 2. Declaring Uninterpreted Sorts, Variables, and Functions

**Uninterpreted sorts** are introduced with `declare-sort`:

```smt2
(declare-sort U 0)          ; nullary sort (a simple uninterpreted sort)
(declare-sort MyArray 2)    ; binary sort constructor
```

**Sort parameters** (new in 2.7) enable prenex polymorphism in assertions:

```smt2
(declare-sort-parameter A)  ; global sort parameter
```

When `check-sat` runs, a polymorphic assertion is instantiated for *all* monomorphic sorts available. Sort parameters are **schematic variables** standing for monomorphic sorts, not universally quantified type variables in the usual sense.

**Constants and functions** are declared with `declare-const` and `declare-fun`:

```smt2
(declare-const x Int)                ; constant of sort Int
(declare-fun f (Int) Int)            ; unary function Int → Int
(declare-fun g (Int Int) Bool)       ; binary predicate
```

Sort parameters may appear in the sorts, making the function polymorphic:

```smt2
(declare-sort-parameter A)
(declare-sort-parameter B)
(declare-fun is-const ((Array A B)) Bool)   ; polymorphic function
```

**Defined** functions (with a body) use `define-fun`, `define-fun-rec`, `define-funs-rec`:

```smt2
(define-fun abs ((x Int)) Int (ite (>= x 0) x (- x)))
(define-fun-rec fact ((n Int)) Int
  (ite (= n 0) 1 (* n (fact (- n 1)))))
```

## 3. Lambda Expressions (new in 2.7)

The `lambda` binder creates function abstractions (maps). A multi-argument lambda abbreviates nested single-argument lambdas:

```smt2
(lambda ((x₁ τ₁) (x₂ τ₂) ... (xₙ τₙ)) t)
;; desugars to:
(lambda ((x₁ τ₁)) (lambda ((x₂ τ₂)) (... (lambda ((xₙ τₙ)) t) ...)))
```

Lambdas create values of **map sort** `(-> τ₁ τ₂)`, provided by the new `HO-Core` theory. The `->` sort constructor is **right-associative**: `(-> τ₁ τ₂ τ₃)` means `(-> τ₁ (-> τ₂ τ₃))`.

**Application** uses the `_` operator (from `HO-Core`), which is **left-associative**:

```smt2
(_ f x)            ; apply map f to x
(_ (_ f x) y)      ; curried application, can be written (_ f x y)
(f x)              ; shorthand for (_ f x) when unambiguous
```

Key distinction: **functions** (declared with `declare-fun`) have rank `τ₁ ··· τₙ τ` and cannot be passed as arguments. **Maps** (values of sort `(-> τ₁ τ₂)`) can be passed and returned. Conversion between them:

```smt2
; function → map
(define-const c_f (lambda ((x τ₁)) (f x)))
; map → function
(define-fun f_c ((x τ₁)) τ₂ (_ e x))
```

The logic remains **first-order** in syntax -- you cannot write `(f g)` where `g` is a function symbol of rank > 0. You must wrap it: `(f (lambda ((x τ₁)) (g x)))`.

## 4. The `let` and `match` Forms

**`let`** introduces **parallel** local bindings:

```smt2
(let ((x₁ t₁) ... (xₙ tₙ)) t)
```

This simultaneously substitutes each `tᵢ` for `xᵢ` in `t`. Variables `x₁, ..., xₙ` **must be pairwise distinct**. There is **no sequential `let`**; sequential binding is achieved by nesting:

```smt2
(let ((x t₁)) (let ((y (+ x 1))) body))
```

**`match`** performs pattern matching on algebraic datatype values:

```smt2
(match t ( (p₁ t₁) ... (pₘ₊₁ tₘ₊₁) ))
```

where `t` has some datatype sort `δ`. Each pattern `p` is one of:
- A **variable** `x` (of sort `δ`)
- The **wildcard** `_` (each occurrence is a fresh variable)
- A **nullary constructor** `c`
- A **constructor application** `(c x₁ ... xₖ)` where `x₁, ..., xₖ` are **distinct** variables (or `_`)

**No nested patterns**: in `(c x₁ ... xₖ)`, each `xᵢ` must be a variable or `_`, never another constructor. The pattern list must include a variable/wildcard pattern unless every constructor of `δ` is covered. The scope of variables in pattern `pᵢ` is `tᵢ`.

Example:

```smt2
; Axiom for list append
(forall ((l1 (List Int)) (l2 (List Int)))
  (= (append l1 l2)
     (match l1 (
       (nil l2)
       ((cons h t) (cons h (append t l2)))))))

; Using wildcard for unused fields
(forall ((l (List Int)))
  (= (length l)
     (match l (
       (nil 0)
       ((cons _ t) (+ 1 (length t)))))))
```

`match` can be desugared into `ite`/`let` chains using testers and selectors:

```smt2
(ite ((_ is nil) t) t₁
  (let ((h (car t)) (tl (cdr t))) t₂))
```

## 5. If-then-else (`ite`)

`ite` is part of the **Core theory**. Declared as a polymorphic function:

```smt2
(par (A) (ite Bool A A A))
```

Takes a Boolean condition, a "then" branch, and an "else" branch (same sort). Returns the second argument if true, the third if false.

```smt2
(ite (> x 0) x (- x))          ; absolute value
```

## 6. Integer Sort

The sort for integers is `Int`, nullary. Literals are numerals; negatives written `(- n)`.

```smt2
(- Int Int)             ; negation
(- Int Int Int)         ; subtraction
(+ Int Int Int)         ; addition (:left-assoc)
(* Int Int Int)         ; multiplication (:left-assoc)
(<= Int Int Bool)       ; (:chainable)
(<  Int Int Bool)       ; (:chainable)
(>= Int Int Bool)       ; (:chainable)
(>  Int Int Bool)       ; (:chainable)
```

## 7. BNF for SMT-LIB Term Syntax

```bnf
⟨sort⟩ ::= ⟨identifier⟩ | ( ⟨identifier⟩ ⟨sort⟩+ )

⟨identifier⟩ ::= ⟨symbol⟩ | ( _ ⟨symbol⟩ ⟨index⟩+ )

⟨index⟩ ::= ⟨numeral⟩ | ⟨symbol⟩

⟨qual_identifier⟩ ::= ⟨identifier⟩ | ( as ⟨identifier⟩ ⟨sort⟩ )

⟨sorted_var⟩ ::= ( ⟨symbol⟩ ⟨sort⟩ )

⟨var_binding⟩ ::= ( ⟨symbol⟩ ⟨term⟩ )

⟨pattern⟩ ::= ⟨symbol⟩ | ( ⟨symbol⟩ ⟨symbol⟩+ )

⟨match_case⟩ ::= ( ⟨pattern⟩ ⟨term⟩ )

⟨term⟩ ::= ⟨spec_constant⟩
         | ⟨qual_identifier⟩
         | ( ⟨qual_identifier⟩ ⟨term⟩+ )
         | ( let ( ⟨var_binding⟩+ ) ⟨term⟩ )
         | ( lambda ( ⟨sorted_var⟩+ ) ⟨term⟩ )
         | ( forall ( ⟨sorted_var⟩+ ) ⟨term⟩ )
         | ( exists ( ⟨sorted_var⟩+ ) ⟨term⟩ )
         | ( match ⟨term⟩ ( ⟨match_case⟩+ ) )
         | ( ! ⟨term⟩ ⟨attribute⟩+ )
```

## 8. Key Commands

```bnf
⟨command⟩ ::= ( declare-sort ⟨symbol⟩ ⟨numeral⟩ )
            | ( declare-sort-parameter ⟨symbol⟩ )
            | ( declare-fun ⟨symbol⟩ ( ⟨sort⟩* ) ⟨sort⟩ )
            | ( declare-const ⟨symbol⟩ ⟨sort⟩ )
            | ( declare-datatype ⟨symbol⟩ ⟨datatype_dec⟩ )
            | ( declare-datatypes ( ⟨sort_dec⟩+ ) ( ⟨datatype_dec⟩+ ) )
            | ( define-fun ⟨function_def⟩ )
            | ( define-fun-rec ⟨function_def⟩ )
            | ( define-funs-rec ( ⟨function_dec⟩+ ) ( ⟨term⟩+ ) )
            | ( define-const ⟨symbol⟩ ⟨sort⟩ ⟨term⟩ )
            | ( define-sort ⟨symbol⟩ ( ⟨symbol⟩* ) ⟨sort⟩ )
            | ( assert ⟨term⟩ )
            | ( check-sat )
            | ( check-sat-assuming ( ⟨prop_literal⟩* ) )
            | ( set-logic ⟨symbol⟩ )
            | ( get-model ) | ( get-value ( ⟨term⟩+ ) )
            | ( push ⟨numeral⟩ ) | ( pop ⟨numeral⟩ )
            | ( reset ) | ( reset-assertions ) | ( exit )

⟨datatype_dec⟩ ::= ( ⟨constructor_dec⟩+ )
                  | ( par ( ⟨symbol⟩+ ) ( ⟨constructor_dec⟩+ ) )

⟨constructor_dec⟩ ::= ( ⟨symbol⟩ ⟨selector_dec⟩* )

⟨selector_dec⟩ ::= ( ⟨symbol⟩ ⟨sort⟩ )
```
