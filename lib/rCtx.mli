(** Refined contexts (Δ).

    A refined context extends ordinary typing contexts with logical
    facts and resource ownership entries, each with usage tracking. *)

type entry =
  | Comp of { var : Var.t; sort : Sort.sort; eff : Effect.t }
  | Log of { var : Var.t; prop : CoreExpr.typed_ce }
  | Res of { var : Var.t; pred : CoreExpr.typed_ce; value : CoreExpr.typed_ce; usage : Usage.t }

type t

val empty : t
val extend_comp : Var.t -> Sort.sort -> Effect.t -> t -> t
val extend_log : Var.t -> CoreExpr.typed_ce -> t -> t
val extend_res : Var.t -> CoreExpr.typed_ce -> CoreExpr.typed_ce -> Usage.t -> t -> t
val concat : t -> t -> t

val lookup_comp : Var.t -> t -> (Sort.sort * Effect.t) option
(** [lookup_comp x ctx] returns sort and effect for computational binding [x]. *)

val lookup_log : Var.t -> t -> CoreExpr.typed_ce option
(** [lookup_log x ctx] returns the proposition for logical binding [x]. *)

val use_resource : Var.t -> t -> (CoreExpr.typed_ce * CoreExpr.typed_ce * t, string) result
(** [use_resource x ctx] checks [x] is available, sets it to used,
    and returns [(pred, value, ctx')]. *)

val erase : t -> Context.t
(** [erase Δ] drops log/res entries, keeping only comp entries. *)

val affinize : t -> t
(** [affinize Δ] converts [Avail] resources to [Opt]. *)

val zero : t -> bool
(** [zero Δ] is true when all resources are consumed or optional. *)

val merge : t -> t -> (t, string) result
(** [merge Δ₁ Δ₂] pointwise merges usage flags. *)

val merge_n : t list -> (t, string) result
(** [merge_n [Δ₁; ...; Δₙ]] folds [merge] left-to-right. *)

val lattice_merge : t -> t -> (t, string) result
(** [lattice_merge Δ₁ Δ₂] merges using [Usage.lattice_meet] (total order
    [Used ≤ Opt ≤ Avail]). Never fails due to usage incompatibility. *)

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
