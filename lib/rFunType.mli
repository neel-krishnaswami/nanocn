(** Refined function types: [Pf₁ ⊸ Pf₂ \[eff\]].

    Parameterized by ['e], the type of embedded expressions,
    and ['var], the type of variable names. *)

type ('e, 'var) t = {
  domain : ('e, 'var) ProofSort.t;
  codomain : ('e, 'var) ProofSort.t;
  eff : Effect.t;
}

val map : ('a -> 'b) -> ('a, 'var) t -> ('b, 'var) t
val print_gen : (Format.formatter -> Var.t -> unit) -> (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, Var.t) t -> unit
val print : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, Var.t) t -> unit
val print_ce : Format.formatter -> (CoreExpr.ce, Var.t) t -> unit

val to_string : (Format.formatter -> 'e -> unit) -> ('e, Var.t) t -> string
val to_string_ce : (CoreExpr.ce, Var.t) t -> string

module Test : sig
  val test : QCheck.Test.t list
end
