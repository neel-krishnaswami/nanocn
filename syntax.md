Here is the syntax for the core language:

Types:

A ::= [A1, ..., An]   // records
   |  {L1:A1 |  ... | Ln: An}    // labelled sums
   | int                         // integers
   | ptr A                       // pointer type
   | ( A )                       // parenthesis (for parsing only)

Γ ::= · | Γ, x:A    // Contexts

ϵ ::= pure | impure  // Effects


e ::= x | n                                                // variables and integer literals
   | let x = e1; e2                                        // sequencing
   | [e1, ..., en] | let [x1, ..., xn] = e1; e2           // tuples and destructuring
   | L e | case e { L1 x1 -> e1 |  ... | Ln xn -> en }    // labelled sums and cases
   | iter (x = e) { e }                                    // iteration
   | f(e)                                                  // arithmetic primitives
   | g[A](e)                                               // state primitives (with type annotation)
   | e : A [ϵ]                                             // type and effect annotation
   | ( e )                                                 // parenthesis (for parsing only)



Labels L should be of the form [A-Z][a-zA-Z0-9_']+.


f ::= Add | Mul | Sub | Div         // arithmetic (monomorphic)
g ::= New | Del | Get | Set         // state (polymorphic, requires type annotation)


Bidirectional typechecking judgements

ϵ ≤ ϵ'             Subeffecting

Γ ⊢ e ⇐ A[ϵ]     Checking:   In context Γ, e checks against type A and effect ϵ
Γ ⊢ e ⇒ A[ϵ]     Synthesis:  In context Γ, e synthesizes type A and effect ϵ

The judgements are defined below:

pure ≤ impure
pure ≤ pure
impure ≤ impure


Arithmetic primitives:

Add : [Int, Int] → Int [pure]
Sub : [Int, Int] → Int [pure]
Mul : [Int, Int] → Int [pure]
Div : [Int, Int] → Int [impure]

State primitives (type parameter A given explicitly):

New[A] : A → Ptr A [impure]
Del[A] : Ptr A → [] [impure]
Get[A] : Ptr A → A [impure]
Set[A] : [Ptr A, A] → [] [impure]


————————————
Γ ⊢ n ⇒ Int [pure]


x:A ∈ Γ
——————————————
Γ ⊢ x ⇒ A [pure]


Γ ⊢ e ⇐ A[ϵ]
————————————————————
Γ ⊢ (e : A [ϵ]) ⇒ A[ϵ]


Γ ⊢ e1 ⇒ A[ϵ']   ϵ' ≤ ϵ   Γ, x:A ⊢ e2 ⇐ B[ϵ]
—————————————————————————————————————————————
Γ ⊢ let x = e1; e2 ⇐ B[ϵ]


f : A → B [ϵ]    Γ ⊢ e ⇐ A [pure]
————————————————————————————————————
Γ ⊢ f(e) ⇒ B[ϵ]


g[A] : B → C [ϵ]    Γ ⊢ e ⇐ B [pure]
————————————————————————————————————————
Γ ⊢ g[A](e) ⇒ C[ϵ]


Γ ⊢ e1 ⇒ A[ϵ₁]    Γ, x:A ⊢ e2 ⇐ {Next:A | Done:B} [impure]    impure ≤ ϵ
————————————————————————————————————————————————————————————————
Γ ⊢ iter (x = e1) { e2 } ⇐ B[ϵ]


Γ ⊢ e ⇐ Ai [ϵ]
———————————————————————————————————
Γ ⊢ L(e) ⇐ {L1:A1| ...| Ln:An} [ϵ]


Γ ⊢ e ⇒ {L1:A1|...|Ln:An} [ϵ₁]   ϵ₁ ≤ ϵ    Γ, x1:A1 ⊢ e1 ⇐ C[ϵ] ... Γ, xn:An ⊢ en ⇐ C[ϵ]
——————————————————————————————————————————————————————————————————————————————————————————
Γ ⊢ case e { L1 x1 → e1 | ... | Ln xn -> en } ⇐ C[ϵ]


Γ ⊢ e1 ⇐ A1 [ϵ] ... Γ ⊢ en ⇐ An [ϵ]
——————————————————————————————————————
Γ ⊢ [e1, ..., en] ⇐ [A1, ..., An] [ϵ]


Γ ⊢ e1 ⇒ [A1, ..., An] [ϵ₁]   ϵ₁ ≤ ϵ   Γ, x1:A1, ..., xn:An ⊢ e2 ⇐ C[ϵ]
———————————————————————————————————————————————————————————————————————
Γ ⊢ let [x1, ..., xn] = e1; e2 ⇐ C[ϵ]
