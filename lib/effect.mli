(** Effects: pure (0) or effectful (1). *)

type t = Pure | Impure

val sub : t -> t -> bool
(** [sub e1 e2] is true when [e1 ≤ e2] in the subeffecting order. *)

val join : t -> t -> t
(** [join e1 e2] is the least upper bound. *)

val compare : t -> t -> int
val print : Format.formatter -> t -> unit

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
