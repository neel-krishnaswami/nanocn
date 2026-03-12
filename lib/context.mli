(** Typing contexts mapping variables to types or sorts.

    Computational variables carry [Typ.ty], specification variables
    carry [Sort.sort]. Lookup functions enforce the separation. *)

type t

val empty : t

val extend_comp : Var.t -> Typ.ty -> t -> t
(** [extend_comp x ty ctx] adds a computational variable binding. *)

val extend_spec : Var.t -> Sort.sort -> t -> t
(** [extend_spec x sort ctx] adds a specification variable binding. *)

val lookup_comp : Var.t -> t -> Typ.ty option
(** [lookup_comp x ctx] looks up a computational variable. *)

val lookup_spec : Var.t -> t -> Sort.sort option
(** [lookup_spec x ctx] looks up a specification variable. *)

val extend_comp_list : (Var.t * Typ.ty) list -> t -> t
(** [extend_comp_list bindings ctx] extends [ctx] with multiple computational bindings. *)

val extend_spec_list : (Var.t * Sort.sort) list -> t -> t
(** [extend_spec_list bindings ctx] extends [ctx] with multiple spec bindings. *)

val print : Format.formatter -> t -> unit

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
