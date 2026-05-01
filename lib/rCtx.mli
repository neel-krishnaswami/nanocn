(** Refined contexts (Δ).

    A refined context extends ordinary typing contexts with logical
    facts and resource ownership entries, each with usage tracking. *)

type entry =
  | Comp of { var : Var.t; sort : Sort.sort; eff : Effect.t }
  | Log of { var : Var.t; prop : CoreExpr.typed_ce }
  | Res of { var : Var.t; pred : CoreExpr.typed_ce; value : CoreExpr.typed_ce; usage : Usage.t }
  | Unknown of { var : Var.t }
    (** A refined-context binding whose kind / sort could not be
        determined because the rCheck judgement that introduced it
        had errors recorded on its typed AST.  The multi-error
        rCheck binds the var so downstream uses still resolve, but
        any [lookup_comp] / [lookup_log] / [use_resource] for an
        [Unknown] entry returns a structured error.  [Unknown]
        entries do NOT carry a usage flag, so [use_resource] on
        them does not affect linearity tracking — otherwise an
        upstream error would silently cascade as a linear-resource
        leak. *)

type t

val empty : t
val extend_comp : Var.t -> Sort.sort -> Effect.t -> t -> t
val extend_log : Var.t -> CoreExpr.typed_ce -> t -> t
val extend_res : Var.t -> CoreExpr.typed_ce -> CoreExpr.typed_ce -> Usage.t -> t -> t
val extend_unknown : Var.t -> t -> t
(** [extend_unknown x ctx] adds an [Unknown] binding for [x].  Used
    by the multi-error rCheck when an introduction-form errored and
    no concrete kind / sort is available. *)

val concat : t -> t -> t

val lookup_comp :
  Var.t -> t -> (Sort.sort * Effect.t, Error.kind) result
(** [lookup_comp x ctx] returns sort and effect for computational
    binding [x].  Errors:
    - [K_unbound_var x] when [x] is not bound at all.
    - [K_unknown_var_type {var=x}] when [x] is bound as [Unknown].
    - [K_internal_invariant] when [x] is bound as [Log] or [Res]
      (caller should have used the appropriate lookup variant). *)

val lookup_log :
  Var.t -> t -> (CoreExpr.typed_ce, Error.kind) result
(** [lookup_log x ctx] returns the proposition for logical binding
    [x].  Errors as for [lookup_comp]. *)

val use_resource :
  Var.t -> t ->
  (CoreExpr.typed_ce * CoreExpr.typed_ce * t, Error.kind) result
(** [use_resource x ctx] checks [x] is available, sets it to used,
    and returns [(pred, value, ctx')]. Fails with
    [Error.K_resource_not_found] if [x] has no binding, or
    [Error.K_resource_already_used] if it does but is non-[Avail]. *)

val erase : t -> Context.t
(** [erase Δ] drops log/res entries, keeping only comp entries. *)

val affinize : t -> t
(** [affinize Δ] converts [Avail] resources to [Opt]. *)

val zero : t -> bool
(** [zero Δ] is true when all resources are consumed or optional. *)

val merge : t -> t -> (t, Error.kind) result
(** [merge Δ₁ Δ₂] pointwise merges usage flags. Fails with a
    [Error.K_branch_merge_failure] whose [reason] describes the
    specific obstruction (length mismatch, entry-kind mismatch,
    or incompatible usage). *)

val merge_n : t list -> (t, Error.kind) result
(** [merge_n [Δ₁; ...; Δₙ]] folds [merge] left-to-right. Fails on
    an empty input or on the first [merge] failure. *)

val lattice_merge : t -> t -> (t, Error.kind) result
(** [lattice_merge Δ₁ Δ₂] merges using [Usage.lattice_meet] (total order
    [Used ≤ Opt ≤ Avail]). Cannot fail due to usage; only on length
    or entry-kind mismatches. *)

val usage_equal : t -> t -> bool
(** [usage_equal Δ₁ Δ₂] checks that the two contexts have the same
    length and matching usage flags on every resource entry. *)

val length : t -> int
val split : int -> t -> t * t
(** [split n Δ] returns [(Δ₁, Δ₂)] where [Δ₁] has [n] entries. *)

val entries : t -> entry list
(** [entries Δ] returns the underlying entry list. *)

val print_gen : (Format.formatter -> Var.t -> unit) -> Format.formatter -> t -> unit
val print : Format.formatter -> t -> unit
val to_string : t -> string

module Test : sig
  val test : QCheck.Test.t list
end
