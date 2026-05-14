(** Fallible extractors for core expressions.

    Each extractor inspects [CoreExpr.shape] and either returns the
    sub-components of the matching constructor or fails with
    [[Error.wrong_pred_shape]] carrying the [~construct] label and an
    [expected_shape] hint.  The [got] field is the pretty-printed
    input expression.  Callers thread the [(_, Error.t) result]
    through the View pipeline (e.g. via [Result.map] /
    [Constraint.atom']) rather than failing the elaboration monad. *)

val get_return : construct:string -> 'b CoreExpr.t -> ('b CoreExpr.t, Error.t) result
val get_fail   : construct:string -> 'b CoreExpr.t -> (unit, Error.t) result
val get_take   : construct:string -> 'b CoreExpr.t -> (Var.t * 'b CoreExpr.t * 'b CoreExpr.t, Error.t) result
val get_let    : construct:string -> 'b CoreExpr.t -> (Var.t * 'b CoreExpr.t * 'b CoreExpr.t, Error.t) result
val get_if     : construct:string -> 'b CoreExpr.t -> ('b CoreExpr.t * 'b CoreExpr.t * 'b CoreExpr.t, Error.t) result
val get_case   : construct:string -> 'b CoreExpr.t -> ('b CoreExpr.t * (Label.t * Var.t * 'b CoreExpr.t * 'b) list, Error.t) result
val get_call   : construct:string -> 'b CoreExpr.t -> (string * 'b CoreExpr.t, Error.t) result
