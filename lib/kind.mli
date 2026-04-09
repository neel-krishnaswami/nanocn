(** Kinds classify sorts and types.

    [Type] classifies computational types (no [Pred]).
    [Sort] classifies assertion sorts (may contain [Pred]).
    Subkinding: [Type <= Sort]. *)

type t = Type | Sort

val compare : t -> t -> int
val subkind : t -> t -> bool
(** [subkind k k'] is [true] when [k <= k']. *)

val print : Format.formatter -> t -> unit

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
