(** Primitive operations.

    Arithmetic primitives are monomorphic.
    State primitives carry an explicit type parameter. *)

type t =
  | Add | Mul | Sub | Div
  | New of Typ.ty | Del of Typ.ty | Get of Typ.ty | Set of Typ.ty

val compare : t -> t -> int
val print : Format.formatter -> t -> unit

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
