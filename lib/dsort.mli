(** Datasort names (identifiers starting with lowercase).

    Datasort names like [list], [tree] identify user-defined datasort
    declarations. They start with a lowercase letter, like type names. *)

type t

val of_string : string -> (t, string) result
val to_string : t -> string
val compare : t -> t -> int
val print : Format.formatter -> t -> unit
val json : t -> Json.t

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
