# Adding pattern matching to nanocn refinement types 

## Phase 1: Infallible patterns only 

In the simple refinement system, the grammar of patterns looks like this: 
q ::= (qbase1, ..., qbasen) 
qbase ::= cpat | res rpat-base | log lpat 
lpat ::= x
rpat ::= x | (x) y 
cpat ::=  x 

We are going to do is to expand the grammar of patterns to encompass more stuff, 
but for patterns which always succeed in matching. 

q ::= · | qbase, q
qbase ::= cpat | res rpat-base | log lpat 
lpat ::= x | auto 
rpat-base ::= do cpat = rpat | rpat
rpat ::= x 
      | return lpat 
      | take(cpat, rpat); rpat 
      | fail[lpat] 
      | let[lpat] cpat; rpat 
      | case L(cpat); lpat; rpat
      | iftrue; rpat
      | iffalse; rpat
      | unfold; rpat 
      | annot; rpat
cpat ::=  x | (cpat1, ..., cpatn) 

In Phase 2, the core patterns will need an effect-sensitive matching judgement, because we 
must ensure computational control flow is not affected by spec-level matching. For phase 1, 
though, this does not happen. 

### The Pattern Matching Judgement

However the pattern matching judgement Σ; Δ ⊢[eff] q : Pf ⊣ Δ' ↝ C will now grow an effect eff to 
prepare for this eventuality, plus a constraint C to track the facts which must be true in order 
for the pattern match to succeed. Because of this, we also need to change the input context to Δ, so that we can look up any facts we need! 

First, the empty case: 

—————————————————————————————
Σ; Δ ⊢[eff] · : · ⊣ Δ ↝ ⊤



Next, the rules for core patterns: 

Σ; Δ, x:τ[eff'] ⊢[eff] q : [x/y]Pf ⊣ Δ' ↝ C
—————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (x, q) : (y:τ[eff'], Pf) ⊣ Δ' ↝ C


Σ; Δ ⊢[eff] cpat1, ..., cpatn, q  : (x1:τ1[eff'], ..., xn:τn[eff'], [(x1, ..., xn)/x]Pf) ⊣ Δ'  ↝ C
—————————————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (cpat1, ..., cpatn), q : (x:(τ1, ..., τn)[eff'], Pf) ⊣ Δ'  ↝ C

Next, the rules for the logical patterns: 

Σ; Δ, x : ϕ [log] ⊢[eff] q : Pf ⊣ Δ'  ↝ C
———————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] x, q : (ce [log], Pf) ⊣ Δ'  ↝ C


Σ; Δ ⊢[eff] q : Pf ⊣ Δ'  ↝ C
———————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] auto, q : (ϕ [log], Pf) ⊣ Δ'  ↝ ϕ ∧ C 

(auto in pattern position asserts the constraint is already satisfied by the context)


Finally, the rules for the resource patterns: 

Σ;|Δ| ⊢[spec] ce ==> Pred τ 
Σ; Δ ⊢[eff] (cpat, rpat, q) : (x:τ[spec], ce@x [res], Pf) ⊣ Δ' ↝ C
—————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (do cpat=rpat, q) : ((x).ce[res], Pf) ⊣ Δ' ↝ C


Σ; Δ, x:ce@ce'[res(1)]  ⊢[eff] q : Pf ⊣ Δ' ↝ C
—————————————————————————————————————————————————————
Σ; Δ ⊢[eff] x, q : (ce@ce'[res], Pf) ⊣ Δ' ↝ C


Σ; Δ ⊢[eff] lpat, q : (ce = ce' [log], Pf) ⊣ Δ' ↝ C
————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] return lpat, q : ((return ce)@ce'[res], Pf) ⊣ Δ' ↝ C


Σ; |Δ| ⊢[spec] ce1 ==> Pred τ
Σ; Δ ⊢[eff] (cpat, rpat, rpat', q) : (x:τ[spec], ce1@x [res], ce2@ce'[res], Pf) ⊣ Δ' ↝ C 
——————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (take(cpat, rpat); rpat'), q : ((take x=ce1; ce2)@ce'[res], Pf) ⊣ Δ'  ↝ C


Σ; Δ ⊢[eff] (lpat, q) : (⊥ [log], Pf) ⊣ Δ' ↝ C
——————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (fail[lpat], q) : ((fail@ce)[res], Pf) ⊣ Δ' ↝ ⊥


Σ; |Δ| ⊢[spec] ce1 ==> Pred τ
Σ; Δ ⊢[eff] (cpat, lpat, rpat, q) : (x:τ[spec], x=ce1[log], ce2@ce[res], Pf) ⊣ Δ' ↝ C
——————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] ((let[lpat]cpat; rpat), q) : ((let x=ce1;ce2)@ce[res], Pf) ⊣ Δ' ↝ C


Σ; Δ ⊢[eff] (rpat, q) : (ce1@ce'[res], Pf) ⊣ Δ' ↝  C
——————————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] ((iftrue; rpat), q) : ((if ce then ce1 else ce2)@ce'[res], Pf) ⊣ Δ' ↝ (ce ∧ C)


Σ; Δ ⊢[eff] (rpat, q) : (ce2@ce'[res], Pf) ⊣ Δ' ↝ C
——————————————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] ((iffalse; rpat), q) : ((if ce then ce1 else ce2)@ce'[res], Pf) ⊣ Δ' ↝ (¬ce ∧ C)


Σ; |Δ| ⊢[⌊eff⌋] ce ==> D(σ1, ..., σm)
Lk:τ ∈ D(σ1, ..., σm) in Σ
Σ; Δ ⊢[eff] (cpat, lpat, rpat, q) : (xk : τ[spec], L xk = ce [log], cek@ce' [res], Pf) ⊣ Δ' ↝ C 
——————————————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] ((case Lk(cpat); lpat; rpat), q) : (case(ce, L[i] xi → cei)@ce'[res], Pf) ⊣ Δ' 
↝ is(ce, L) ∧ C


fun f(x:τ) → τ' [eff'] = ce0 ∈ Σ 
Σ; Δ ⊢[eff] (rpat, q) : ([ce/x]ce0@ce' [res], Pf) ⊣ Δ' ↝ C
———————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (unfold; rpat, q) : ((f(ce)@ce')[res], Pf) ⊣ Δ' ↝ C


Σ; Δ ⊢[eff] (rpat, q) : ((ce@ce')[res], Pf) ⊣ Δ' ↝ C
———————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] (annot; rpat, q) : ((ce : Pred τ@ce')[res], Pf) ⊣ Δ' ↝ C


### Invocations of the Pattern Matching Judgement

#### The Let Rule

Σ; Δ0 ⊢[eff] crt1 ==> Pf' ⊣ Δ1 ↝ C
Σ; Δ1 ⊢[⌊eff⌋] q : Pf' ⊣ Δ2 ↝ C1
Σ; Δ2 ⊢[eff] crt2 <== Pf ⊣ Δ3 ↝ C2
Δ3 = Δ4, Δ''
length(Δ1) = length(Δ4) 
zero(Δ'')
(Δ'' ⇒ (C1 ∧ C2)) = C'
—————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let q = crt1; crt2 <== Pf ⊣ Δ4 ↝ C ∧ C'


#### Let-res/core/log 

Σ; Δ0 ⊢ rpf ==> ce@ce' ⊣ Δ1 ↝ C
Σ; Δ1 ⊢[⌊eff⌋] (rpat, ·) : (ce@ce' [res], ·) ⊣ Δ2 ↝ C1
Σ; Δ2 ⊢[eff] crt <== Pf ⊣ Δ3 ↝ C2
Δ3 = Δ4, Δ''
|Δ1| = |Δ4|
zero(Δ'')
(Δ'' ⇒ (C1 ∧ C2)) = C'
————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let res rpat = rpf; crt <== Pf ⊣ Δ4 ↝ C ∧ C'


Σ; Δ0 ⊢ lpf ==> ce ⊣ Δ1 ↝ C
Σ; Δ1 ⊢[⌊eff⌋] (lpat, ·) : (ce [log], ·) ⊣ Δ2 ↝ C1
Σ; Δ2 ⊢[eff] crt <== Pf ⊣ Δ3 ↝ C2
Δ3 = Δ4, Δ''
|Δ1| = |Δ4|
zero(Δ'')
(Δ'' ⇒ (C1 ∧ C2)) = C'
————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let log lpat = lpf; crt <== Pf ⊣ Δ4 ↝ C ∧ C'



Σ; |Δ0| ⊢[⌊eff⌋] ce ==> τ 
Σ; Δ0 ⊢[⌊eff⌋] (cpat, lpat) : (y:τ[⌊eff⌋], y = ce [log], ·) ⊣ Δ1 ↝ C1
Σ; Δ1 ⊢[eff] crt <== Pf ⊣ Δ2 ↝ C2
Δ2 = Δ3, Δ''
|Δ0| = |Δ3|
zero(Δ'')
(Δ'' ⇒ (C1 ∧ C2)) = C
——————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let core[lpat] cpat = ce; crt <== Pf ⊣ Δ3 ↝ C




#### Refined Function Declarations


Σr; · ⊢ Pf1 ⊸ Pf2 [spec] wf 
Σr; · ⊢[spec] q : Pf1 ⊣ Δ  ↝ C1
Σr, f : Pf1 ⊸ Pf2 [spec]; Δ ⊢[spec] crt <== Pf2 ⊣ Δ' ↝ C2
zero(Δ')
(Δ' ⇒ (C1 ∧ C2)) = C
Σr, f:Pf1 ⊸ Pf2 [spec] ⊢ rprog ⊣ Σr' ↝ C'
——————————————————————————————————————————————————————————————————————————
Σr ⊢ fun f(q) : Pf1 ⊸ Pf2 [spec] = { crt } rprog ⊣ Σr' ↝ C ∧ C' 


Σr; · ⊢ Pf1 ⊸ Pf2 [impure] wf 
Σr; · ⊢[pure] q : Pf1 ⊣ Δ ↝ C1
Σr, f : Pf1 ⊸ Pf2 [impure]; Δ ⊢[impure] crt <== Pf2 ⊣ Δ' ↝ C2
zero(Δ')
(Δ' ⇒ (C1 ∧ C2)) = C
Σr, f:Pf1 ⊸ Pf2 [impure] ⊢ rprog ⊣ Σr' ↝ C'
——————————————————————————————————————————————————————————————
Σr ⊢ fun f(q) : Pf1 ⊸ Pf2 [impure] = { crt } rprog ⊣ Σr' ↝ C ∧ C'


Σr; · ⊢ Pf1 ⊸ Pf2 [pure] wf 
Σr; · ⊢[pure] q : Pf1 ⊣ Δ ↝ C1 
Σr; Δ ⊢[pure] crt <== Pf2 ⊣ Δ' ↝ C2
zero(Δ')
(Δ' ⇒ (C1 ∧ C2)) = C
Σr, f:Pf1 ⊸ Pf2 [pure] ⊢ rprog ⊣ Σr' ↝ C'
——————————————————————————————————————————————————————————————
Σr ⊢ fun f(q) : Pf1 ⊸ Pf2 [pure] = { crt } rprog ⊣ Σr' ↝ C ∧ C'




# Phase 2 

The grammar of patterns is now: 

q ::= (qbase1, ..., qbasen) 
qbase ::= cpat | res rpat-base | log lpat 
lpat ::= x
rpat-base ::= do cpat = rpat | rpat
rpat ::= x 
      | return lpat 
      | take(cpat, rpat); rpat 
      | fail 
      | let[a] cpat; rpat 
      | case L(p); rpat
      | iftrue; rpat
      | iffalse; rpat
      | unfold; rpat 
      | annot; rpat
cpat ::=  x | (cpat1, ..., cpatn) | L cpat 

The only addition is L cpat, but this changes a LOT, because now we have to work with sets of 
clauses. 




 










 





















