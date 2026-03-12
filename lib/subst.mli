(** Type variable substitution for sorts.

    A substitution maps type variables to sorts.
    [apply] recursively replaces [TVar a] with the bound sort. *)

type t

val empty : t
val extend : Tvar.t -> Sort.sort -> t -> t
val apply : t -> Sort.sort -> Sort.sort
val of_lists : Tvar.t list -> Sort.sort list -> (t, string) result

val compare : t -> t -> int
val print : Format.formatter -> t -> unit

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
