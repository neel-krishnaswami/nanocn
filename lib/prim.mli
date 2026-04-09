(** Primitive operations.

    Arithmetic primitives are monomorphic.
    State primitives carry an explicit type parameter. *)

type t =
  | Add | Mul | Sub | Div
  | Lt | Le | Gt | Ge
  | And | Or | Not
  | Eq of Sort.sort
  | New of Sort.sort | Del of Sort.sort | Get of Sort.sort | Set of Sort.sort
  | Own of Sort.sort

val compare : t -> t -> int
val print : Format.formatter -> t -> unit
val json : t -> Json.t

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
