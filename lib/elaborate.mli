(** Unified surface expression elaboration and pattern match compilation.

    Implements synth and check (surface -> typed core elaboration)
    and the coverage/pattern match compilation judgement, as mutually
    recursive functions. Produces fully typed core expressions directly,
    with context, sort, and effect at every node. *)

(** {1 Typed node types} *)

type typed_info = CoreExpr.typed_info
type typed_ce = typed_info CoreExpr.t

val lift_sort : Sort.sort -> typed_info Sort.t
(** Upgrade a plain [Sort.sort] to a [typed_info Sort.t] suitable for use in
    [CoreExpr.Annot]. The lifted sort carries empty context, [Effect.Pure],
    and uses each node's existing location. *)

(** {1 Coverage types} *)

(** A single binding: pattern with its sort. *)
type binding = Pat.pat * Sort.sort

(** A let-binding accumulated during coverage checking. *)
type let_binding = {
  var : Var.t;
  rhs : Var.t;
  sort : Sort.sort;
  eff : Effect.t;
  loc : SourcePos.t;
}

(** A match branch.
    [let_bindings] accumulates let-bindings during coverage;
    at Cov_done these are wrapped around the body. *)
type branch = {
  bindings : binding list;
  let_bindings : let_binding list;
  body : SurfExpr.se;
}

(** {1 Elaboration} *)

val synth : _ Sig.t -> Context.t -> Effect.t -> SurfExpr.se ->
  typed_ce ElabM.t
(** [synth sig ctx eff0 se] synthesizes the sort of [se] and
    elaborates it to a typed core expression.  [eff0] is the ambient
    effect.  The synthesized sort lives on the result's
    [info#answer] and can be read with [CoreExpr.sort_of_info]. *)

val check : _ Sig.t -> Context.t -> SurfExpr.se ->
  (Sort.sort, Error.kind) result -> Effect.t -> typed_ce ElabM.t
(** [check sig ctx se sort eff0] checks [se] against [sort] at ambient
    effect [eff0] and elaborates it to a typed core expression.  The
    expected sort is itself a result so callers can pass
    [Error K_cannot_synthesize] (or any other reason) when no expected
    sort is available; the term still elaborates and any failures
    land on [info#answer] of the offending nodes. *)

(** {1 Coverage} *)

val coverage_check : _ Sig.t -> Context.t -> Var.t list -> branch list ->
  Effect.t -> Sort.sort -> Effect.t ->
  cov_loc:SourcePos.t ->
  (PatWitness.t list -> PatWitness.t) ->
  typed_ce ElabM.t
(** [coverage_check sig ctx scrutinees branches eff_b sort eff0
    ~cov_loc rebuilder] compiles the match matrix into a typed core
    expression. [ctx] must include the scrutinee variables. [eff_b]
    is the binding effect for scrutinee variables; [eff0] is the
    ambient effect. [~cov_loc] is the source position of the
    construct that started the coverage check (case/let/take/iter),
    used for reporting a non-exhaustive-match error. [rebuilder]
    lifts witnesses for the current scrutinees back to a witness for
    the original scrutinee(s); typical initial callers pass
    [function [w] -> w | _ -> PatWitness.Wild]. *)
