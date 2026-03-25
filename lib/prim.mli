(** Primitive operations.

    Arithmetic primitives are monomorphic.
    State primitives carry an explicit type parameter. *)

type t =
  | Add | Mul | Sub | Div
  | Lt | Le | Gt | Ge
  | And | Or | Not
  | Eq of Typ.ty
  | New of Typ.ty | Del of Typ.ty | Get of Typ.ty | Set of Typ.ty
  | Own of Typ.ty

val compare : t -> t -> int
val print : Format.formatter -> t -> unit
val json : t -> Json.t

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
