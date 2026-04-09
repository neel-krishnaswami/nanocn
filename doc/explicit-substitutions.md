# Contexts and substitutions, uniformly 

Right now, nanoCN has two issues: 

1. Contexts only contain term variables, and type variables (used in datatype declarations) are handled in 
   an ad-hoc fashion. 
2. The implementation has an incorrect implementation of substitution, which is a bit awkward to correct.


## The grammar

kind ::= type | sort 

type ≤ type
type ≤ sort 
sort ≤ sort 

Γ ::= · | Γ, x:τ[eff]  | Γ, a:kind

## Well-kindedness 

### Judgement: 

The type/sort well-formedness judgement is: Σ; Γ ⊢ τ : kind 

 - Inputs: Σ, Γ, τ, kind
 - Outputs: none

 
a:kind' ∈ Gamma    kind' ≤ kind
—————————————————————————————————
Σ; Γ ⊢ a : kind 


Σ; Γ ⊢ τi : kind for i ∈ [1...n]
——————————————————————————————————
Σ; Γ ⊢ (τ1, ..., τn) : kind


——————————————————
Σ; Γ ⊢ Int : kind


——————————————————
Σ; Γ ⊢ Bool : kind


Σ; Γ ⊢ τ : sort
——————————————————————
Σ; Γ ⊢ Pred τ : sort 


Σ; Γ ⊢ τ : type
——————————————————————
Σ; Γ ⊢ Ptr τ : kind


type D(a1, ..., ak) = {...} ∈ Σ
n = k 
Σ; Γ ⊢ τi : type for i ∈ [1...n]
———————————————————————————————————————–—
Σ; Γ ⊢ D(τ1, ..., τn) : type


sort D(a1, ..., ak) = {...} ∈ Σ
n = k 
Σ; Γ ⊢ τi : sort for i ∈ [1...n]
———————————————————————————————————————–—
Σ; Γ ⊢ D(τ1, ..., τn) : sort



## Signature checking rules 

Here's how the type declaration judgements change: 

Σ, sort D(a1,...,an) = {}; a1:sort, ..., an:sort ⊢ τ1 : sort
...
Σ, sort D(a1,...,an) = {}; a1:sort, ..., an:sort ⊢ τk : sort 
————————————————————————————————————————————————————————————— SDWf_Ok
Σ ⊢ sort D(a1,...,an) = {L1 : τ1 ... Lk : τk } ok


Σ; a1:type, ..., an:type; D(a1,...,an) ⊢ τ1 guarded
...
Σ; a1:type, ..., an:type; D(a1,...,an) ⊢ τk guarded
—————————————————————————————————————————————————————————————  TDWf_Ok
Σ ⊢ type D(a1,...,an) = {L1 : τ1 ... Lk : τk } ok


By omitting the type declaration of D from Σ fixes a bug in the existing TDWf_Ok rule that breaks the cycle
checking. 


The guardedness judgement is changed as follows:

  Σ; Γ; D(a1, ..., an) ⊢ A guarded

  - Inputs: Σ, Γ, D, n, A 
  - Outputs: none


The rules are mostly as in syntax.ott, except that we change the TWfG_Ptr rule to 

Σ, type D(a1, ..., an) = {}; Γ ⊢ τ : type
————————————————————————————————————————————— TwfG_Ptr
Σ; Γ; D(a1, ..., an) ⊢ Ptr τ guarded


In conjunction with how the rule was invoked, this enables recursive occurences under pointers, but does not allow
them as parameters to other datatypes (since we don't know if the parameters are guarded). 

## Explicit substitutions: 

### Grammar 

γ ::= · | γ, (t/x) | γ, (τ/a) 


### Well-formedness: 

The substitution well-formedness judgement is: Σ; Γ ⊢ γ : Γ' 

   - Inputs: Σ, Γ, γ, Γ'
   - Outputs: none

The rules are: 

—————————————
Σ; Γ ⊢ · : ·


Σ; Γ ⊢ γ : Γ'    Σ; Γ ⊢[eff] e ==> τ'   τ' = τ
————————————————————————————————————————————————
Σ; Γ ⊢ (γ, e/x) : (Γ', x:τ[eff])


Σ; Γ ⊢ γ : Γ'    Σ; Γ ⊢ τ :kind
—————————————————————————————————
Σ; Γ ⊢ (γ, τ/a) : (Γ', a:kind)


### Applying substitutions: 

We define the application γ(a) of a substitution to a variable as follows: 

(id)(a)      = a 
(γ, τ/a)(b)  = τ     when a = b 
(γ, τ/a)(b)  = γ(b)  when a ≠ b 
(γ, ce/a)(b) = ce    when a = b 
(γ, ce/a)(b) = γ(b)  when a ≠ b 
·(a)         = undefined 


#### Applying substitutions to types: 

Substitution on variables lifts homomorphically to a full application on types and sort [γ]τ as follows:

[γ] a              = τ          when γ(a) = τ
                   = undefined  when γ(a) = ce
[γ] (τ1, ..., τn)  = ([γ]τ1, ..., [γ]τn)
[γ] Int            = Int
[γ] Bool           = Bool
[γ] (Pred τ)       = Pred ([γ]τ)
[γ] (Ptr τ)        = Ptr ([γ]τ)
[γ] D(τ₁, ..., τn) = D([γ]τ1, ..., [γ]τn)


The well-formedness condition on substitutions is: 

- If Σ; Γ ⊢ γ : Γ' and Σ; Γ' ⊢ τ : kind then Σ; Γ ⊢ [γ]τ : kind 


#### Applying substitutions to terms: 

Substitution on variables lifts homomorphically to a full application of substitutions to terms [γ]e as follows: 

[γ](x) = ce        when γ(x) = ce
       = undefined when γ(x) = τ
[γ](n) = n
[γ](b) = b
[γ](if ce1 then ce2 else ce3) = if [γ]ce1 then [γ]ce2 else [γ]ce3
[γ](let x = ce1; ce2) = let x = [γ]ce1; [γ, x/x]ce2
[γ](ce1, ..., cen) = ([γ]ce1, ..., [γ]cen)
[γ](let (x1, ..., xn) = ce1; ce2) = 
   let (x1, ..., xn) = [γ]ce1; [γ, x1/x1, ..., xn/xn]ce2
[γ](L ce) = L ([γ]ce)
[γ](case ce of { L1 x1 → ce1 | ... | Ln xn → cen })
   = case [γ]ce of { L1 x1 → [γ, x1/x1]ce1 | ... | Ln xn → [γ, xn/xn] cen }
[γ](iter (x = ce1) { ce2 }) = iter (x = [γ]ce1) { [γ, x/x]ce2 }
[γ](prim ce) = prim ([γ]ce)
[γ](f ce) = f ([γ]ce)
[γ](ce : τ) = ([γ]ce : [γ]τ)
[γ](ce1 = ce2) = [γ]ce1 = [γ]ce2
[γ](take x = ce1; ce2) = take x = [γ]ce1; [γ,x/x]ce2 
[γ](return ce) = return ([γ]ce)

The well-formedness condition on substitutions on terms is: 

- If Σ; Γ ⊢ γ : Γ' and Σ; Γ' ⊢[eff] ce <== τ then Σ; Γ ⊢[eff] [γ]ce <== [γ]τ
- If Σ; Γ ⊢ γ : Γ' and Σ; Γ' ⊢[eff] ce ==> τ then Σ; Γ ⊢[eff] [γ]ce ==> [γ]τ


#### Applying substitutions to proof sorts:

Proof sorts are lists of entries that bind variables. Substitution applies γ to the 
types and core expressions within each entry, and extends γ with identity mappings
for each bound variable (so that bound variables are not captured).

[γ](·) = ·
[γ](x : τ [eff], Pf) = x : [γ]τ [eff], [γ, x/x]Pf
[γ](x : ce [log], Pf) = x : [γ]ce [log], [γ]Pf
[γ](x : ce @ ce' [res], Pf) = x : [γ]ce @ [γ]ce' [res], [γ]Pf
[γ](x : (y).ce [res], Pf) = x : (y).[γ, y/y]ce [res], [γ]Pf

The x in the log and res cases are not part of the substitution, because
they do not occur in Γ = |Δ|. 


The well-formedness condition on substitutions on proof sorts is significantly more
complex, and stating it requires (a) defining substitutions δ on refined contexts from
Δ to Δ', (b) an erasure operation γ = |δ|, and a number of coherence theorems 
about them. Stating these properties will be deferred until we define refined substitutions. 

### The identity substitution:   

id(·) = ·
id(Γ, x:τ[eff]) = id(Γ), x/x
id(Γ, a:kind) = id(Γ), a/a

This satisfies the following properties: 

     - Σ; Γ ⊢ id(Γ) : Γ
     - [id(Γ)]τ = τ
     - [id(Γ)]e = e

### Composition of substitutions: 

The composition of substitutions γ0; γ1 is defined as follows: 

γ0; (·)       = · 
γ0; (γ1, e/x) = (γ0;γ1, [γ0]e/x)
γ0; (γ1, τ/a) = (γ0;γ1, [γ0]τ/a)

It satisfies the following properties: 

1. If Σ; Γ0 ⊢ γ0 : Γ1  and Σ; Γ1 ⊢ γ1 : Γ2 then
   - Σ; Γ0 ⊢ (γ0; γ1) : Γ2 
   - If Σ; Γ2 ⊢ τ : kind then [γ0;γ1]τ = [γ0]([γ1]τ)
   - If Σ; Γ2 ⊢[eff] e <== τ then [γ0; γ1]e = [γ0]([γ1]e)
   - If Σ; Γ2 ⊢[eff] e ==> τ then [γ0; γ1]e = [γ0]([γ1]e)

2. If Σ; Γ0 ⊢ γ0 : Γ1  and Σ; Γ1 ⊢ γ1 : Γ2 and Σ; Γ2 ⊢ γ2 : Γ3 then
   (γ0; γ1); γ2 = γ0; (γ1; γ2) 


## Single variable substitution: 

- Single variable substitutions of the form [τ/a] can be rewritten as [id(Γ), τ/a]
- Single variable substitutions of the form [ce/x] can be rewritten as [id(Γ), ce/x]

Obviously this requires knowing the context the substitution is performed in, but in fact we always do. 














