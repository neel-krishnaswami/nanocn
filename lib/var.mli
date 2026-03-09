(** Variables (identifiers starting with lowercase). *)

type t

val of_string : string -> t
val to_string : t -> string
val compare : t -> t -> int
val print : Format.formatter -> t -> unit

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
