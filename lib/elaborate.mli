(** Unified surface expression elaboration and pattern match compilation.

    Implements synth and check (surface -> core elaboration)
    and the coverage/pattern match compilation judgement, as mutually
    recursive functions. Handles both computation and assertion expressions
    via the effect parameter. *)

(** {1 Coverage types} *)

(** A single binding: pattern with its sort. *)
type binding = Pat.pat * Sort.sort

(** A match branch.
    [ctx_bindings] accumulates [(x, sort, eff)] triples during coverage;
    at Cov_done these are added to the outer context. *)
type branch = {
  bindings : binding list;
  ctx_bindings : (Var.t * Sort.sort * Effect.t) list;
  ectx : < loc : SourcePos.t > EvalCtx.t;
  body : SurfExpr.se;
}

(** {1 Elaboration} *)

val synth : _ Sig.t -> Context.t -> Effect.t -> SurfExpr.se ->
  (CoreExpr.ce * Sort.sort * Effect.t) ElabM.t
(** [synth sig ctx eff0 se] synthesizes the sort and effect of [se] and
    elaborates it to a core expression. [eff0] is the ambient effect. *)

val check : _ Sig.t -> Context.t -> SurfExpr.se -> Sort.sort -> Effect.t ->
  CoreExpr.ce ElabM.t
(** [check sig ctx se sort eff0] checks [se] against [sort] at ambient
    effect [eff0] and elaborates it to a core expression. *)

(** {1 Coverage} *)

val coverage_check : _ Sig.t -> Context.t -> Var.t list -> branch list ->
  Effect.t -> Sort.sort -> Effect.t -> CoreExpr.ce ElabM.t
(** [coverage_check sig ctx scrutinees branches eff_b sort eff0] compiles the
    match matrix into a core expression. [eff_b] is the binding effect for
    scrutinee variables; [eff0] is the ambient effect. *)
