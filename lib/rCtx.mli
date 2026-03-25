(** Refined contexts (Δ).

    A refined context extends ordinary typing contexts with logical
    facts and resource ownership entries, each with usage tracking. *)

type entry =
  | Comp of { var : Var.t; sort : Sort.sort; eff : Effect.t }
  | Log of { var : Var.t; prop : CoreExpr.ce }
  | Res of { var : Var.t; pred : CoreExpr.ce; value : CoreExpr.ce; usage : Usage.t }

type t

val empty : t
val extend_comp : Var.t -> Sort.sort -> Effect.t -> t -> t
val extend_log : Var.t -> CoreExpr.ce -> t -> t
val extend_res : Var.t -> CoreExpr.ce -> CoreExpr.ce -> Usage.t -> t -> t
val concat : t -> t -> t

val lookup_comp : Var.t -> t -> (Sort.sort * Effect.t) option
(** [lookup_comp x ctx] returns sort and effect for computational binding [x]. *)

val lookup_log : Var.t -> t -> CoreExpr.ce option
(** [lookup_log x ctx] returns the proposition for logical binding [x]. *)

val use_resource : Var.t -> t -> (CoreExpr.ce * CoreExpr.ce * t, string) result
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

val length : t -> int
val split : int -> t -> t * t
(** [split n Δ] returns [(Δ₁, Δ₂)] where [Δ₁] has [n] entries. *)

val entries : t -> entry list
(** [entries Δ] returns the underlying entry list. *)

val print : Format.formatter -> t -> unit

module Test : sig
  val test : QCheck.Test.t list
end
