In both the core expression and surface expression typing rules: 

* Change the variable lookup rule as follows 

x:τ[eff'] ∈ Γ    eff' ≤ eff
————————————————————————————
Σ; Γ ⊢[eff] x ⇒ τ [eff]


* Change the Syn_Call rule as follows: 

f : τ → τ' [eff0] ∈ Σ
eff0 ≤ eff
eff1 = ⌊eff⌋
Σ; Γ ⊢[eff1] ce <== τ [eff2]
——————————————————————————
Σ; Γ ⊢[eff] f ce ⇒ τ' [eff]


* Change the Prim rule similarly. 


The idea is to ensure that in function and primitive calls: 

* If eff = pure: 

  ce can only use pure variables, since ⌊pure⌋ = pure 

* If eff = impure 

  ce can only use pure variables, since ⌊impure⌋ = pure 

* If eff = spec 

  ce can use pure and spec variables, since ⌊spec⌋ = spec 


