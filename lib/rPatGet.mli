(** Fallible extractors for refined patterns.

    Each extractor inspects a [RPat] shape and either returns the
    sub-components of the matching constructor or fails with
    [Error.K_wrong_pred_shape] carrying the [~construct] label. Lift via
    [ElabM.lift_at] at the call site. *)

val get_cvar : construct:string -> ('b, Var.t) RPat.cpat -> (Var.t, Error.kind) result
(** Extract the bound variable from a [CVar] cpat. Fails on [CTuple]. *)
