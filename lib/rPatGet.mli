(** Fallible extractors for refined patterns.

    Each extractor inspects a [RPat] shape and either returns the
    sub-components of the matching constructor or fails with
    [[Error.wrong_pred_shape]] carrying the [~construct] label.
    Callers thread the [(_, Error.t) result] through the
    typechecker's errkind plumbing rather than failing the
    elaboration monad. *)

val get_cvar : construct:string -> ('b, Var.t) RPat.cpat -> (Var.t, Error.t) result
(** Extract the bound variable from a [CVar] cpat. Fails on [CTuple]. *)
