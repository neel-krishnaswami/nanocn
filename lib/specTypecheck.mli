(** Bidirectional typechecker for core assertion expressions.

    Implements [core_synth] and [core_check] from the Ott spec.
    Operates on core expressions (patterns already compiled away). *)

val synth : Sig.t -> Context.t -> CoreExpr.ce -> (Sort.sort, string) result
(** [synth sig ctx ce] synthesizes the sort of [ce]. *)

val check : Sig.t -> Context.t -> CoreExpr.ce -> Sort.sort -> (unit, string) result
(** [check sig ctx ce sort] checks [ce] against [sort]. *)
