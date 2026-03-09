(** Function signature context mapping function names to signatures. *)

type entry = {
  arg : Typ.ty;
  ret : Typ.ty;
  eff : Effect.t;
}

type t

val empty : t
val extend : Var.t -> entry -> t -> t
val lookup : Var.t -> t -> entry option
val print : Format.formatter -> t -> unit

module Test : sig
  val test : QCheck.Test.t list
end
