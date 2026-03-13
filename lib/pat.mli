(** Patterns for the assertion language surface syntax.

    Patterns bind variables in case, let, and take expressions. *)

(** The shape functor for patterns. ['a] is the recursive occurrence. *)
type 'a patF =
  | Var of Var.t
  | Con of Label.t * 'a
  | Tuple of 'a list

val map : ('a -> 'b) -> 'a patF -> 'b patF

(** A pattern tree annotated with ['b] at each node. *)
type 'b t = In of 'b t patF * 'b

val extract : 'b t -> 'b
val shape : 'b t -> 'b t patF

(** Concrete located pattern. *)
type pat = < loc : SourcePos.t > t

val compare : pat -> pat -> int
val print : Format.formatter -> _ t -> unit

val linear_check : pat -> (unit, string) result
(** [linear_check p] returns [Ok ()] if no variable appears more than once
    in [p], or [Error msg] if a variable is repeated. *)

val vars : pat -> Var.t list
(** [vars p] returns the list of variables bound by [p], in left-to-right order. *)

module Test : sig
  val gen : pat QCheck.Gen.t
  val test : QCheck.Test.t list
end
