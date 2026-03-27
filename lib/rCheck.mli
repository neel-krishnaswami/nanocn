(** Refined typechecker.

    Implements all refined typing judgements from the Ott specification.
    Delegates core expression checking to [Typecheck.synth]/[Typecheck.check]
    via [RSig.comp] and [RCtx.erase]. *)

val check_rprog :
  RProg.parsed ->
  (RSig.t * Constraint.t) ElabM.t
(** [check_rprog prog] typechecks a refined program,
    returning the final signature and constraint tree. *)

module Test : sig
  val test : QCheck.Test.t list
  val pf_eq : RSig.t -> RCtx.t -> (CoreExpr.ce, Var.t) ProofSort.t -> (CoreExpr.ce, Var.t) ProofSort.t -> Constraint.t ElabM.t
  val with_delta_check : (unit -> 'a) -> 'a
  (** [with_delta_check f] runs [f] with the Delta ⊓ Delta' = Delta'
      monotonicity assertion enabled in [check_crt] and [synth_crt]. *)
end
