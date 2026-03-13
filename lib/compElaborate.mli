(** Surface computation elaboration and pattern match compilation.

    Implements elaboration from [SurfComp.se] to [Expr.expr]
    and the coverage/pattern match compilation judgement, as mutually
    recursive functions. *)

(** {1 Coverage types} *)

(** A single binding: pattern with its type. *)
type binding = Pat.pat * Typ.ty

(** A match branch.
    [comp_bindings] accumulates [(x, A)] pairs during coverage;
    at Cov_done these are added to the outer context via
    [Context.extend_comp_list]. *)
type branch = {
  bindings : binding list;
  comp_bindings : (Var.t * Typ.ty) list;
  ectx : CompEvalCtx.t;
  body : SurfComp.se;
}

(** {1 Elaboration} *)

val synth : Sig.t -> Context.t -> SurfComp.se ->
  (Expr.expr * Typ.ty * Effect.t) ElabM.t
(** [synth sig ctx se] synthesizes the type and effect of [se] and
    elaborates it to a core expression. *)

val check : Sig.t -> Context.t -> SurfComp.se -> Typ.ty -> Effect.t ->
  Expr.expr ElabM.t
(** [check sig ctx se ty eff] checks [se] against [ty] and [eff] and
    elaborates it to a core expression. *)

(** {1 Coverage} *)

val coverage_check : Sig.t -> Context.t -> Var.t list -> branch list ->
  Typ.ty -> Effect.t -> Expr.expr ElabM.t
(** [coverage_check sig ctx scrutinees branches ty eff] compiles the
    match matrix into a core expression. *)
