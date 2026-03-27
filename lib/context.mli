(** Typing contexts mapping variables to sorts and effects.

    All bindings carry [Sort.sort * Effect.t]. The effect is [purify eff]
    of the ambient effect where the variable was bound. *)

type t

val empty : t

val extend : Var.t -> Sort.sort -> Effect.t -> t -> t
(** [extend x sort eff ctx] adds a binding for [x]. *)

val lookup : Var.t -> t -> (Sort.sort * Effect.t) option
(** [lookup x ctx] returns the sort and effect for [x]. *)

val extend_list : (Var.t * Sort.sort * Effect.t) list -> t -> t
(** [extend_list bindings ctx] extends [ctx] with multiple bindings. *)

val print_gen : (Format.formatter -> Var.t -> unit) -> Format.formatter -> t -> unit
val print : Format.formatter -> t -> unit
val to_string : t -> string

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
