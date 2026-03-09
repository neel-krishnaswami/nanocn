(** Bidirectional typechecker.

    Implements synthesis and checking judgements, returning a fully
    annotated expression tree where each node carries its source location,
    typing context, type, and effect.
*)

type typed_info = < loc : SourcePos.t; ctx : Context.t; typ : Typ.ty; eff : Effect.t >
type typed_expr = typed_info Expr.t

val synth : Context.t -> Expr.expr -> (typed_expr, string) result
(** Synthesize a type and effect for the given expression, returning
    the fully annotated tree. *)

val check : Context.t -> Expr.expr -> Typ.ty -> Effect.t -> (typed_expr, string) result
(** Check an expression against a given type and effect, returning
    the fully annotated tree. *)
