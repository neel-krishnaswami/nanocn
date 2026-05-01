(** Refined typechecker.

    Implements all refined typing judgements from the Ott specification.
    Elaboration delegates to [Elaborate.synth]/[Elaborate.check] via
    [RSig.comp] and [RCtx.erase]. Uses [typed_ce] throughout to
    preserve sort/ctx/eff info from elaboration. *)

val check_rprog :
  RProg.parsed ->
  (RProg.typed * RSig.t * Constraint.typed_ct) ElabM.t
(** [check_rprog prog] typechecks a refined program,
    returning the fully annotated program, final signature,
    and typed constraint tree.  Every refined expression node in
    the output carries [CoreExpr.typed_info] (context, sort, effect). *)

val check_rdecl :
  RSig.t -> Constraint.typed_ct ->
  (SurfExpr.se, < loc : SourcePos.t >, Var.t) RProg.decl ->
  ((CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RProg.decl * RSig.t * Constraint.typed_ct) ElabM.t
(** [check_rdecl rsig ct d] typechecks a single refined declaration
    against [rsig], conjoining any new constraints with [ct].
    Returns the typed declaration, updated signature, and constraint tree. *)

val collect_errors_rprog : RProg.typed -> Error.t list
(** [collect_errors_rprog prog] returns every error recorded on
    [info#answer] anywhere in the typed refined program — across
    typed_rinfo nodes (RPat, ProofSort, RefinedExpr.{lpf,rpf,crt,
    spine}) and embedded typed_ce sub-trees (via
    [Typecheck.collect_errors]).

    Pre-order over decls then main; within each tree, source order.
    The walker is currently a manual O(n) traversal; once slices
    C.2-C.5 populate the [subterm_errors] field on every rinfo,
    this can be reduced to an O(1) read at the root. *)

module Test : sig
  val test : QCheck.Test.t list
  val pf_eq : SourcePos.t -> RSig.t -> RCtx.t -> (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t -> (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t -> Constraint.typed_ct ElabM.t
  val with_delta_check : (unit -> 'a) -> 'a
  (** [with_delta_check f] runs [f] with the Delta ⊓ Delta' = Delta'
      monotonicity assertion enabled in [check_crt] and [synth_crt]. *)
end
