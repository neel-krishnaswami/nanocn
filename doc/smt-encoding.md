# SMT Encoding

## Datatype encoding

### Base types

- Translate int to the SMT sort Int.

- Translate bool to the SMT sort Bool. 

- Translate Ptr A with the following 

(declare-datatype (Ptr 1) 
   (par (X) ((null) 
             (ptr (addr Int)))))

### Declare tuple types tuple2 through tuple16. 

  (declare-datatypes ((Tuple2 2)) 
    (par (X1 X2) (tuple2 (prj-2-1 X1) (prj-2-2 X2))))


  (declare-datatypes ((Tuple3 3)) 
    (par (X1 X2 X3) (tuple3 (prj-3-1 X1) (prj-3-2 X2) (prj-3-3 X3)))

  and so on. 

### Datatype declarations

Go through a signature, and translate each CN datatype into the
corresponding SMT datatype declaration. Translate CN tuples into SMT 
tuples above. 


### Declare an uninterpreted sort for the Pred monad, and polymorphic return and
   bind operations 

  (declare-sort pred 1)  ; unary sort of separation logic predicates 

  (declare-sort-parameter A)
  (declare-sort-parameter B)

  (declare-fun return (A) (pred A))
  (declare-fun take (A (-> A (pred B))) (pred B))

#### Maybe

Add quantified assertions (with a :pattern) to implement the monad laws? It's not clear
how to orient them. 

Otherwise we will need to add more syntax to simplify them manually.  

### Nonrecursive functions

Translate pure and nonrecursive spec functions f : (x : τ) → τ' = ce as

 (define-fun f (x τ) τ' [ce])

where [ce] is the SMT-LIB translation of ce, defined below. 

### Recursive functions 

Translate a recursive spec functions f : τ \to τ' via 

(declare-fun f (τ) τ') 

We will **never** use `define-fun-rec`! All recursive functions are translated by 

## Translating expressions 

The syntax of core spec expressions is close to SMT-LIB 2.7 terms. If we start with the 
core terms after elaboration, we will have all the type information we need to generate 
the constraints. 

- [n] = n
- [b] => b
- [x] => x
- [prim e] => ([prim] [e])
- [f e] => (f [e])
- [e1 = e2] => (= [e1] [e2])
- [let x = e1; e2] => (let ((x [e1])) [e2])
- [(e1, ..., en)] => (tuplen [e1] ... [en])
- [let (x1, ..., xn) = e1; e2] => (match [e1] ((tuplen x1 ... xn) [e2]))
- [L e] => (L [e])
- [case ce of {L1 x1 -> e1 ...}] => (match [ce] (((L1 x1) [e1]) ... ((Ln xn) [en])))
- [if e1 then e2 else e3] => (ite [e1] [e2] [e3])
- [return e] => (return [e])
- [take x = e1; e2] => (bind [e1] (lambda (x τ1) [e2]))  where we get the type from the info
- [e : τ] => [e] 

Primitives get translated as follows:

- Add => + 
- Sub => - 
- Lt => <
- Le => <= 
- Gt => > 
– Ge => >= 
- Eq[A] => = 

We can also translate addition and multiplication, but these need special handling: 

- Mul => * 
- Div => div 

Mul (n, ce) and Mul(ce, n), where n is a numeric literal, should be translated without
issue, but there should be an error or warning when nonlinear multiplications are detected.

Likewise, Div(ce, n) should be accepted, but uses of nonlinear
division should be flagged to the user.

## Translating constraints 

The grammar of constraints is 

C ::= 
   | ⊤ 
   |  C ∧ C 
   | ∀x:τ. C 
   | ϕ → C 
   | ϕ 

To translate a constraint, 

1. Declare all the datatypes and toplevel functions. 

Then, recursively translate the constraints as follows: 

- Translating ⊤ 

  Generate nothing

- Translating ⊥ 

  (check-sat)

  If we get unsat, then the assumptions are unsatisfiable, and hence imply ⊥ 

- Translating ϕ 

  (assert (not [ϕ]))
  (check-sat)

If we get unsat, then ϕ is valid, and we return success. 

- Translating C1 ∧ C2 

  Translate C1. 
  Trasnlate C2. 

  We succeed if both C1 and C2 succeed. 

- Translating ϕ → C 

  (push)
  (assert [ϕ])
  Translate C 
  (pop)

- Translating [∀x:τ. C]:

  (push)
  (declare-const x τ)
  Translate C 
  (pop)


  











