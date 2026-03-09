(** Typing contexts mapping variables to types. *)

type t

val empty : t
val extend : Var.t -> Typ.ty -> t -> t
val lookup : Var.t -> t -> Typ.ty option

val extend_list : (Var.t * Typ.ty) list -> t -> t
(** [extend_list bindings ctx] extends [ctx] with multiple bindings. *)

val print : Format.formatter -> t -> unit

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
