(** Type variable substitution for computational types.

    A substitution maps type variables to types.
    [apply] recursively replaces [TVar a] with the bound type. *)

type t

val empty : t
val extend : Tvar.t -> Typ.ty -> t -> t
val apply : t -> Typ.ty -> Typ.ty
val of_lists : Tvar.t list -> Typ.ty list -> (t, string) result

val compare : t -> t -> int
val print : Format.formatter -> t -> unit

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
