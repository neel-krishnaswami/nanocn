(** Surface expression elaboration and pattern match compilation.

    Implements [spec_synth] and [spec_check] (surface→core elaboration)
    and the coverage/pattern match compilation judgement, as mutually
    recursive functions. *)

(** {1 Coverage types} *)

(** A single binding: pattern with its sort. *)
type binding = Pat.pat * Sort.sort

(** A match branch.
    [spec_bindings] accumulates [(x, tau)] pairs during coverage;
    at Cov_done these are added to the outer context via
    [Context.extend_spec_list]. *)
type branch = {
  bindings : binding list;
  spec_bindings : (Var.t * Sort.sort) list;
  ectx : EvalCtx.t;
  body : SurfExpr.se;
}

(** {1 Elaboration} *)

val synth : Sig.t -> Context.t -> SurfExpr.se ->
  (CoreExpr.ce * Sort.sort) ElabM.t
(** [synth sig ctx se] synthesizes the sort of [se] and elaborates it
    to a core expression. *)

val check : Sig.t -> Context.t -> SurfExpr.se -> Sort.sort ->
  CoreExpr.ce ElabM.t
(** [check sig ctx se sort] checks [se] against [sort] and elaborates
    it to a core expression. *)

(** {1 Coverage} *)

val coverage_check : Sig.t -> Context.t -> Var.t list -> branch list ->
  Sort.sort -> CoreExpr.ce ElabM.t
(** [coverage_check sig ctx scrutinees branches sort] compiles the
    match matrix into a core expression. *)
