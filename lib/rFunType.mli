(** Refined function types: [Pf₁ ⊸ Pf₂ \[eff\]].

    Parameterized by ['e], the type of embedded expressions. *)

type 'e t = {
  domain : 'e ProofSort.t;
  codomain : 'e ProofSort.t;
  eff : Effect.t;
}

val map : ('a -> 'b) -> 'a t -> 'b t
val print : (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e t -> unit
val print_ce : Format.formatter -> CoreExpr.ce t -> unit

module Test : sig
  val test : QCheck.Test.t list
end
