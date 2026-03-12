(** Bidirectional typechecker for core assertion expressions.

    Implements [core_synth] and [core_check] from the Ott spec.
    Operates on core expressions (patterns already compiled away).
    Returns annotated core expressions carrying context and sort
    information at each node. *)

type typed_info = < loc : SourcePos.t; ctx : Context.t; sort : Sort.sort >
type typed_ce = typed_info CoreExpr.t

val synth : Sig.t -> Context.t -> CoreExpr.ce -> (typed_ce, string) result
(** [synth sig ctx ce] synthesizes the sort of [ce] and returns an
    annotated core expression. *)

val check : Sig.t -> Context.t -> CoreExpr.ce -> Sort.sort -> (typed_ce, string) result
(** [check sig ctx ce sort] checks [ce] against [sort] and returns an
    annotated core expression. *)
