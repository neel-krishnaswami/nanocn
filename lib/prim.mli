(** Primitive operations.

    Arithmetic primitives are monomorphic.
    State primitives are polymorphic and require an explicit type annotation. *)

type arith = Add | Mul | Sub | Div

val compare_arith : arith -> arith -> int
val print_arith : Format.formatter -> arith -> unit
val arith_to_string : arith -> string

type state = New | Del | Get | Set

val compare_state : state -> state -> int
val print_state : Format.formatter -> state -> unit
val state_to_string : state -> string

module Test : sig
  val gen_arith : arith QCheck.Gen.t
  val gen_state : state QCheck.Gen.t
  val test : QCheck.Test.t list
end
