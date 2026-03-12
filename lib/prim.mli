(** Primitive operations.

    Arithmetic primitives are monomorphic.
    State primitives carry an explicit type parameter. *)

type t =
  | Add | Mul | Sub | Div
  | And | Or | Not
  | Eq of Typ.ty
  | New of Typ.ty | Del of Typ.ty | Get of Typ.ty | Set of Typ.ty

val compare : t -> t -> int
val print : Format.formatter -> t -> unit

(** [spec_sort p] returns [Some (arg_sort, ret_sort)] for pure,
    non-type-parameterized primitives usable in the assertion language.
    Returns [None] for impure or type-parameterized primitives. *)
val spec_sort : t -> (Sort.sort * Sort.sort) option

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
