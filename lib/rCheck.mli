(** Refined typechecker.

    Implements all refined typing judgements from the Ott specification.
    Delegates core expression checking to [Typecheck.synth]/[Typecheck.check]
    via [RSig.comp] and [RCtx.erase]. *)

val synth_crt :
  RSig.t -> RCtx.t -> Effect.t -> RefinedExpr.located_crt ->
  (CoreExpr.ce ProofSort.t * RCtx.t * Constraint.t, string) result

val check_crt :
  RSig.t -> RCtx.t -> Effect.t -> RefinedExpr.located_crt ->
  CoreExpr.ce ProofSort.t ->
  (RCtx.t * Constraint.t, string) result

val synth_lpf :
  RSig.t -> RCtx.t -> RefinedExpr.located_lpf ->
  (CoreExpr.ce * RCtx.t * Constraint.t, string) result

val check_lpf :
  RSig.t -> RCtx.t -> RefinedExpr.located_lpf -> CoreExpr.ce ->
  (RCtx.t * Constraint.t, string) result

val synth_rpf :
  RSig.t -> RCtx.t -> RefinedExpr.located_rpf ->
  (CoreExpr.ce * CoreExpr.ce * RCtx.t * Constraint.t, string) result

val check_rpf :
  RSig.t -> RCtx.t -> RefinedExpr.located_rpf -> CoreExpr.ce -> CoreExpr.ce ->
  (RCtx.t * Constraint.t, string) result

val check_spine :
  RSig.t -> RCtx.t -> Effect.t -> RefinedExpr.located_spine ->
  CoreExpr.ce RFunType.t ->
  (CoreExpr.ce ProofSort.t * RCtx.t * Constraint.t, string) result

val check_rprog :
  RProg.parsed ->
  (RSig.t * Constraint.t, string) result
(** [check_rprog prog] typechecks a refined program,
    returning the final signature and constraint tree. *)

module Test : sig
  val test : QCheck.Test.t list
end
