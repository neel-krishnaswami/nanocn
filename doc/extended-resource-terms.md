# Improving the Resource Term Syntax. 

The resource term syntax (both terms and patterns) is awkward and requires
a lot of redundant type annotations. I want to improve this to make it easier
to write and destructure resource terms. 

## Richer Resource Terms [TODO]

In the simple refinement system, the grammar of logical and resource terms
looks like the following: 

Logical proof facts
lpf ::= x                  -- variable lookup
      | auto               -- discharge by SMT
      | unfold f(ce)       -- unfold spec function f at argument ce
      | open-ret rpf       -- extract equality from a return-form resource
      | lpf : ϕ            -- ascription

Resource proof facts
rpf ::= x                  -- variable lookup (consumes/marks resource)
      | make-ret lpf       -- build a return-form resource from an equality
      | make-take crt      -- build a take-form resource from a refined term
      | rpf : ce @ ce'     -- ascription
      | unfold rpf         -- check rpf against unfolded f(ce)@ce'


This is, honestly, very unpleasant to work with. Happily, we are in the 
process of fixing this. We have mostly redone the resource pattern matching,
and now are going to fix resource terms, by making them mirror the resource
patterns. This way, resource terms build up resources, and resource patterns
take them apart. 

ctm ::= ce | (ce1, ..., cen) 

rpf ::= x | rpf : ce@ce' 
      | return lpf
      | take(rpf, rpf)
      | fail[lpf] 
      | let[lpat] cpat; rpf 
      | case[lpat] L cpat; rpf
      | iftrue; rpf
      | iffalse; rpf
      | unfold; rpf 
      | annot; rpf
      
### The Judgemental Structure


The logical and resource well-formedness judgements remain as before: 

* Σ; Δ ⊢ lpf <== ϕ ⊣ Δ' ↝ C 
* Σ; Δ ⊢ lpf ==> ϕ ⊣ Δ' ↝ C 

* Σ; Δ ⊢ rpf <== ce@ce' ⊣ Δ' ↝ C 
* Σ; Δ ⊢ rpf ==> ce@ce' ⊣ Δ' ↝ C 

For any logical or resource terms in the grammar which do not have rules here, 
keep their existing rules from doc/refinement-types.md, and also keep the check/synth
switch rules from there. 

Any terms which do not have a rule either here or in
refinement-types.md should be flagged to me.

### Updated logical term rules 

Σ; Δ ⊢ lpf <== ce1 = ce2 ⊣ Δ' ↝ C 
———————————————————————————————————————————————————
Σ; Δ ⊢ return lpf <== (return ce1)@ce2 ⊣ Δ' ↝ C 

### Updated resource term rules 


Σ; Δ1 ⊢ rpf1 ==> ce1@ce ⊣ Δ2 ↝ C1 
Σ; Δ2 ⊢ rpf2 <== ([ce/x]ce2)@ce3 ⊣ Δ3 ↝ C2 
————————————————————————————————————————————————————————————————————————
Σ; Δ1 ⊢ take(rpf1, rpf2) <== (take x = ce1; ce2)@ce3 ⊣ Δ3 ↝ C1 ∧ C2


Σ; Δ ⊢ lpf <== ⊥ ⊣ Δ' ↝ C 
Δ'' = affinize(Δ')
———————————————————————————————————————
Σ; Δ ⊢ fail[lpf] <== fail@ce ⊣ Δ'' ↝ C 


Σ; |Δ0| ⊢[spec] ce1 ==> τ 
Σ; Δ0 ⊢[spec] cpat : τ ⊣ Δ1 ↝ ce 
Σ; Δ1 ⊢ lpat : (ce = ce1) ⊣ Δ2 ↝ C1 
Σ; Δ2 ⊢ rpf <== ([(ce:τ)/x]ce2)@ce3 ⊣ Δ3 ↝ C2
Δ3 = Δ4, Δ''
|Δ4| = |Δ0|
zero(Δ'')
(Δ'' ⇒ C1 ∧ C2) = C
————————————————————————————————————————————————————————————————————
Σ; Δ0 ⊢ let[lpat] cpat; rpf <== (let x = ce1; ce2)@ce3 ⊣ Δ4 ↝ C 


Σ; |Δ0| ⊢[spec] ce ==> (τ1, ..., τn)
Σ; Δ0 ⊢[spec] cpat1 : τ1 ⊣ Δ1 ↝ ce1 
...
Σ; Δ{n-1} ⊢[spec] cpatn : τn ⊣ Δ{n} ↝ cen

Σ; Δ{n} ⊢ lpat : (ce = (ce1, ..., cen)) ⊣ Δ{n+1} ↝ C1 
Σ; Δ{n+1} ⊢ rpf <== ([(ce1:τ1)/x1, ..., (cen:τn)/xn]ce')@ce'' ⊣ Δ{n+2} ↝ C2
Δ{n+2} = Δ{n+3}, Δ''
|Δ{n+3}| = |Δ0|
zero(Δ'')
(Δ'' ⇒ C1 ∧ C2) = C
————————————————————————————————————————————————————————————————————
Σ; Δ0 ⊢ let[lpat] (cpat1, ..., cpatn); rpf <== (let (x1, ..., xn) = ce; ce')@ce'' ⊣ Δ{n+3} ↝ C 




Σ; |Δ0| ⊢[spec] ce ==> D(τ1, .., τm) 
Σ ⊢ Lk : τ ∈ D(τ1, ..., τm) 
Σ; Δ0 ⊢[spec] cpat : τ ⊣ Δ1 ↝ ce'
Σ; Δ1 ⊢ lpat : (Lk ce' = ce) ⊣ Δ2 ↝ C1
Σ; Δ2 ⊢ rpf <== [(ce' : τ)/xk]cek@ce'' ⊣ Δ3 ↝ C2
Δ3 = Δ4, Δ''
|Δ4| = |Δ0|
zero(Δ'')
(Δ'' ⇒ C1 ∧ C2) = C
—————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ0 ⊢ case[lpat] Lk cpat; rpf <== (case ce of {Li xi → cei})@ce'' ⊣ Δ4 ↝ is(ce, Lk) ∧ C


Σ; Δ ⊢ rpf <== ce2@ce4 ⊣ Δ' ↝ C
———————————————————————————————————————————————————————————————————————
Σ; Δ ⊢ iftrue; rpf <== (if ce1 then ce2 else ce3)@ce4 ⊣ Δ' ↝ ce1 ∧ C


Σ; Δ ⊢ rpf <== ce3@ce4 ⊣ Δ' ↝ C
—————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢ iffalse; rpf <== (if ce1 then ce2 else ce3)@ce4 ⊣ Δ' ↝ ¬ce1 ∧ C


fun f(x:τ) → τ' [eff'] = ce0 ∈ Σ 
eff' ≤ spec 
Σ; Δ ⊢ rpf <== ([(ce:τ)/x]ce0)@ce' ⊣ Δ' ↝ C 
———————————————————————————————————————————————
Σ; Δ ⊢ unfold; rpf <== f(ce)@ce' ⊣ Δ' ↝ C 


Σ; Δ ⊢ rpf <== ce@ce' ⊣ Δ' ↝ C 
———————————————————————————————————————————————
Σ; Δ ⊢ annot; rpf <== (ce:τ)@ce' ⊣ Δ' ↝ C 



## Infallible resource patterns [REIMPLEMENT]

In the simple refinement system, the grammar of patterns looks like this: 
q ::= (qbase1, ..., qbasen) 
qbase ::= cpat | res rpat-base | log lpat 
lpat ::= x
rpat ::= x | (x) y 
cpat ::=  x 

We are going to do is to expand the grammar of patterns to encompass more stuff, 
but for patterns *which always succeed in matching*. 

q ::= · | qbase, q
qbase ::= cpat | res rpat-base | log lpat 
lpat ::= x | auto 
rpat-base ::= do cpat = rpat | rpat
rpat ::= x 
      | return lpat 
      | take(cpat, rpat); rpat 
      | fail[lpat] 
      | let[lpat] cpat; rpat 
      | case[lpat] L cpat; rpat
      | iftrue; rpat
      | iffalse; rpat
      | unfold; rpat 
      | annot; rpat
cpat ::=  x | (cpat1, ..., cpatn) 

In Phase 2, the core patterns will need an effect-sensitive matching judgement, because we 
must ensure computational control flow is not affected by spec-level matching. For phase 1, 
though, this does not happen. 

### Judgemental Structure of Patterns 

There are now 4 pattern matching judgements – one each for core, logical,
and resources terms; plus one for full refined patterns. 

* Σ; Δ ⊢[eff] cpat : τ ⊣ Δ' ↝ ce 

  Core pattern matching takes a signature Σ, a starting context Δ, an
  effect eff, a pattern cpat, and a sort τ as inputs, and produces an extended
  context Δ' and a term ce as an output. Δ' contains adds the
  variables cpat binds to Δ, and ce is a core term corresponding to
  cpat.

* Σ; Δ ⊢ lpat : ϕ ⊣ Δ' ↝ C

  Logical pattern matching takes a signature Σ, a starting context Δ, 
  a logical pattern lpat, and a proposition ϕ as inputs, and produces an extended
  context Δ' and a constraint C as an output. Δ' contains adds the
  variables lpat binds to Δ, and C is a constraint containing
  any of the logical contraints required by the pattern. 

* Σ; Δ ⊢ rpat : ce@ce' ⊣ Δ' ↝ C

  Resource pattern matching takes a signature Σ, a starting context Δ, 
  a resource pattern rpat, and a resource type ce@ce' as inputs, and produces an extended
  context Δ' and a constraint C as an output. Δ' contains adds the
  variables rpat binds to Δ, and C is a constraint containing
  any of the logical contraints required by the pattern. 

* Σ; Δ ⊢[eff] q : Pf ⊣ Δ' ↝ C

  Refined pattern matching takes a signature Σ, a starting context Δ, an
  effect eff, a refined pattern q, and a refined type Pf as inputs, and 
  produces an extended context Δ' and a constraint C as an output. Δ'  
  adds the variables q binds to Δ, and C is a constraint containing
  any of the logical contraints required by the pattern. 

  Eventually, we will use the [eff] annotation on this judgement to prevent
  pattern matching on spec terms in a computational context, so I want to 
  include this in the judgement. However, it will not be used until we support
  fallible patterns. 

#### Core pattern matching 


—————————————————————————————————————————————————————
Σ; Δ ⊢[eff] x : τ ⊣ Δ, x:τ[⌊eff⌋] ↝ x 


Σ; Δ1 ⊢[eff] cpat1 : τ1 ⊣ Δ2 ↝ ce1 
...
Σ; Δn ⊢[eff] cpatn : τn ⊣ Δ(n+1) ↝ cen
————————————————————————————————————————————————————————————————————————————
Σ; Δ1 ⊢[eff] (cpat1, ..., cpatn), q : (x:(τ1, ..., τn)[eff'], Pf) ⊣ Δ(n+1) ↝ (ce1, ..., cen)


This produces a term corresponding to the pattern as well, with the property that if 
Σ; Δ ⊢[eff] cpat : τ ⊣ Δ' ↝ ce then Σ; |Δ'| ⊢[eff] ce <== τ. 


#### Logical patterns 

———————————————————————————————————————————————————————————
Σ; Δ ⊢ x : ϕ  ⊣ Δ, x : ϕ [log] ↝ ⊤ 


———————————————————————————————————————————————————————————————————
Σ; Δ ⊢ auto : ϕ ⊣ Δ  ↝ ϕ 

auto in the pattern position asserts the constraint is already entailed by the context,
and so we don't need to add it as a hypothesis. 

#### Resource patterns 

Σ;|Δ0| ⊢[spec] ce ==> Pred τ 
Σ; Δ0 ⊢[spec] cpat : τ ⊣ Δ1 ↝ ce'
Σ; Δ1 ⊢ rpat : ce@ce' ⊣ Δ2 ↝ C
—————————————————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] do cpat=rpat : (x).ce[res] ⊣ Δ2 ↝ C


————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] x : ce@ce'[res] ⊣ Δ, x:ce@ce'[res(1)] ↝ ⊤ 


Σ; Δ ⊢[eff] lpat : ce = ce' ⊣ Δ' ↝ C
————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] return lpat : (return ce)@ce'[res] ⊣ Δ' ↝ C


Σ; |Δ| ⊢[spec] ce1 ==> Pred τ 
Σ; Δ0 ⊢[spec] cpat : τ ⊣ Δ1 ↝ ce 
Σ; Δ1 ⊢ rpat : ce1@ce ⊣ Δ2 ↝ C1 
Σ; Δ2 ⊢ rpat' : ce2@ce' ⊣ Δ3 ↝ C2 
——————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] take(cpat, rpat); rpat' : (take x=ce1; ce2)@ce' ⊣ Δ3 ↝ C1 ∧ C2 


Σ; Δ ⊢[eff] lpat : ⊥ ⊣ Δ' ↝ C
———————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] fail[lpat] : fail@ce ⊣ Δ' ↝ C


Σ; |Δ0| ⊢[spec] ce1 ==> τ
Σ; Δ0 ⊢[spec] cpat : τ ⊣ Δ1 ↝ ce 
Σ; Δ1 ⊢ lpat : ce = ce1 ⊣ Δ2 ↝ C1 
Σ; Δ2 ⊢ rpat : [(ce:τ)/x]ce2@ce3 ⊣ Δ3 ↝ C2
——————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let[lpat]cpat; rpat : (let x=ce1;ce2)@ce3 ⊣ Δ3 ↝ C1 ∧ C2


Σ; |Δ0| ⊢[spec] ce ==> (τ1, ..., τn)
Σ; Δ0     ⊢[spec] cpat1 : τ1 ⊣ Δ1     ↝ ce1
...
Σ; Δ{n-1} ⊢[spec] cpatn : τn ⊣ Δn     ↝ cen
Σ; Δn     ⊢ lpat : ce = (ce1, ..., cen) ⊣ Δ{n+1} ↝ C1
Σ; Δ{n+1} ⊢ rpat : [(ce1:τ1)/x1, ..., (cen:τn)/xn]ce'@ce'' ⊣ Δ{n+2} ↝ C2
——————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let[lpat](cpat1, ..., cpatn); rpat : (let (x1, ..., xn)=ce;ce')@ce'' ⊣ Δ{n+2} ↝ C1 ∧ C2






Σ; Δ ⊢[eff] rpat : ce1@ce' ⊣ Δ' ↝  C
——————————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] iftrue; rpat : (if ce then ce1 else ce2)@ce' ⊣ Δ' ↝ (ce ∧ C)


Σ; Δ ⊢[eff] rpat : ce2@ce' ⊣ Δ' ↝  C
——————————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] iffalse; rpat : (if ce then ce1 else ce2)@ce' ⊣ Δ' ↝ (¬ce ∧ C)


Σ; |Δ| ⊢[⌊eff⌋] ce ==> D(σ1, ..., σm)
Lk:τ ∈ D(σ1, ..., σm) in Σ
Σ; Δ0 ⊢[spec] cpat : τ ⊣ Δ1 ↝ ce' 
Σ; Δ1 ⊢ lpat : ce = L ce' ⊣ Δ2 ↝ C1 
Σ; Δ2 ⊢ rpat : cek@ce'' ⊣ Δ3 ↝ C2 
————————————————————————————————————————————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] case[lpat] Lk cpat; rpat : (case(ce, L[i] xi → cei)@ce'' ⊣ Δ3
↝ is(ce, Lk) ∧ C1 ∧ C2 


fun f(x:τ) → τ' [eff'] = ce0 ∈ Σ 
eff' ≤ spec 
Σ; Δ ⊢[eff] rpat : [(ce:τ)/x]ce0@ce' ⊣ Δ' ↝ C
———————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] unfold; rpat : (f(ce)@ce')[res] ⊣ Δ' ↝ C


Σ; Δ ⊢[eff] rpat : ce@ce' ⊣ Δ' ↝ C
———————————————————————————————————————————————————————————————————————
Σ; Δ ⊢[eff] annot; rpat : (ce : Pred τ)@ce' ⊣ Δ' ↝ C



#### Full refined patterns 

The full refined pattern matching judgement now invokes each of the three auxilliary 
pattern matching judgements: 

—————————————————————————————
Σ; Δ ⊢[eff] · : · ⊣ Δ ↝ ⊤


Σ; Δ0 ⊢[eff'] cpat : τ ⊣ Δ1 ↝ ce 
Σ; Δ1 ⊢ q : [(ce:τ)/x]Pf ⊣ Δ2 ↝ C 
——————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] (cpat, q) : (x : τ[eff'], Pf) ⊣ Δ2 ↝ C

[NOTE: in this rule, the [eff] input effect is not used. This will change 
once fallible patterns are added to the language.]


Σ; Δ0 ⊢ rpat-base : rs ⊣ Δ1 ↝ ce 
Σ; Δ1 ⊢[eff] q : Pf ⊣ Δ2 ↝ C 
——————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] (res rpat-base, q) : (rs [res], Pf) ⊣ Δ2 ↝ C


Σ; Δ0 ⊢ lpat : ϕ ⊣ Δ1 ↝ ce 
Σ; Δ1 ⊢ q : Pf ⊣ Δ2 ↝ C 
——————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] (log lpat, q) : (ϕ [log], Pf) ⊣ Δ2 ↝ C


### Invocations of the Pattern Matching Judgement

#### The Let Rule [UPDATE]

Σ; Δ0 ⊢[eff] crt1 ==> Pf' ⊣ Δ1 ↝ C
Σ; Δ1 ⊢[eff] q : Pf' ⊣ Δ2 ↝ C1
Σ; Δ2 ⊢[eff] crt2 <== Pf ⊣ Δ3 ↝ C2
Δ3 = Δ4, Δ''
length(Δ1) = length(Δ4) 
zero(Δ'')
(Δ'' ⇒ (C1 ∧ C2)) = C'
—————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let q = crt1; crt2 <== Pf ⊣ Δ4 ↝ C ∧ C'


#### Let-res/core/log [UPDATE TO ADD PATTERN MATCHING]

Σ; Δ0 ⊢ rpf ==> ce@ce' ⊣ Δ1 ↝ C
Σ; Δ1 ⊢ rpat : ce@ce' [res] ⊣ Δ2 ↝ C1
Σ; Δ2 ⊢[eff] crt <== Pf ⊣ Δ3 ↝ C2
Δ3 = Δ4, Δ''
|Δ1| = |Δ4|
zero(Δ'')
(Δ'' ⇒ (C1 ∧ C2)) = C'
————————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let res rpat = rpf; crt <== Pf ⊣ Δ4 ↝ C ∧ C'


Σ; Δ0 ⊢ lpf ==> ϕ ⊣ Δ1 ↝ C
Σ; Δ1 ⊢ lpat : ϕ ⊣ Δ2 ↝ C1
Σ; Δ2 ⊢[eff] crt <== Pf ⊣ Δ3 ↝ C2
Δ3 = Δ4, Δ''
|Δ1| = |Δ4|
zero(Δ'')
(Δ'' ⇒ (C1 ∧ C2)) = C'
————————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let log lpat = lpf; crt <== Pf ⊣ Δ4 ↝ C ∧ C'



Σ; |Δ0| ⊢[⌊eff⌋] ce ==> τ 
Σ; Δ0 ⊢[⌊eff⌋] cpat : τ ⊣ Δ1 ↝ ce'
Σ; Δ1 ⊢ lpat : ce' = ce ⊣ Δ2 ↝ C1
Σ; Δ2 ⊢[eff] crt <== Pf ⊣ Δ3 ↝ C2
Δ3 = Δ4, Δ''
|Δ0| = |Δ4|
zero(Δ'')
(Δ'' ⇒ (C1 ∧ C2)) = C
—————————————————————————————————————————————————————————————
Σ; Δ0 ⊢[eff] let core[lpat] cpat = ce; crt <== Pf ⊣ Δ3 ↝ C




#### Refined Function Declarations [MAY NEED UPDATING]


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



# Phase 2 [DO NOT IMPLEMENT YET]

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

The only addition is L cpat, but this changes a LOT, because now we
have to move to work with pattern matrices. 

