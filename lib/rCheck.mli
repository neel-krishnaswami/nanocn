(** Refined typechecker.

    Implements all refined typing judgements from the Ott specification.
    Elaboration delegates to [Elaborate.synth]/[Elaborate.check] via
    [RSig.comp] and [RCtx.erase]. Uses [typed_ce] throughout to
    preserve sort/ctx/eff info from elaboration. *)

val check_rprog :
  RProg.parsed ->
  (RSig.t * Constraint.typed_ct) ElabM.t
(** [check_rprog prog] typechecks a refined program,
    returning the final signature and typed constraint tree. *)

val check_rdecl :
  RSig.t -> Constraint.typed_ct ->
  (SurfExpr.se, Var.t) RProg.decl ->
  (RSig.t * Constraint.typed_ct) ElabM.t
(** [check_rdecl rsig ct d] typechecks a single refined declaration
    against [rsig], conjoining any new constraints with [ct].
    Returns the updated signature and constraint tree. *)

module Test : sig
  val test : QCheck.Test.t list
  val pf_eq : SourcePos.t -> RSig.t -> RCtx.t -> (CoreExpr.typed_ce, Var.t) ProofSort.t -> (CoreExpr.typed_ce, Var.t) ProofSort.t -> Constraint.typed_ct ElabM.t
  val with_delta_check : (unit -> 'a) -> 'a
  (** [with_delta_check f] runs [f] with the Delta ⊓ Delta' = Delta'
      monotonicity assertion enabled in [check_crt] and [synth_crt]. *)
end
