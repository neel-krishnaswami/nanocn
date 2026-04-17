(** Refined function types: [Pf₁ ⊸ Pf₂ \[eff\]].

    Parameterized by ['e], the type of embedded expressions,
    ['b], auxiliary info on proof sort entries,
    and ['var], the type of variable names. *)

type ('e, 'b, 'var) t = {
  domain : ('e, 'b, 'var) ProofSort.t;
  codomain : ('e, 'b, 'var) ProofSort.t;
  eff : Effect.t;
}

val map : ('a -> 'b) -> ('a, 'info, 'var) t -> ('b, 'info, 'var) t
val map_info : ('b -> 'c) -> ('e, 'b, 'var) t -> ('e, 'c, 'var) t

val print_gen : (Format.formatter -> Var.t -> unit) -> (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, 'b, Var.t) t -> unit
val print : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, 'b, Var.t) t -> unit
val print_ce : Format.formatter -> (CoreExpr.ce, 'b, Var.t) t -> unit

val to_string : (Format.formatter -> 'e -> unit) -> ('e, 'b, Var.t) t -> string
val to_string_ce : (CoreExpr.ce, 'b, Var.t) t -> string

module Test : sig
  val test : QCheck.Test.t list
end
