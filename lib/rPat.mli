(** Refined patterns (q): flat variable tuples. *)

type t = Var.t list

val print : Format.formatter -> t -> unit

module Test : sig
  val test : QCheck.Test.t list
end
