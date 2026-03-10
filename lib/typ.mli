(** Types in the language. *)

(** The shape functor for types. ['a] is the recursive occurrence. *)
type 'a tF =
  | Record of 'a list
  | Sum of (Label.t * 'a) list
  | Int
  | Bool
  | Ptr of 'a

val map : ('a -> 'b) -> 'a tF -> 'b tF
val compare_tF : ('a -> 'a -> int) -> 'a tF -> 'a tF -> int
val print_tF : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a tF -> unit

(** A type tree annotated with ['b] at each node. *)
type 'b t = In of 'b t tF * 'b

val extract : 'b t -> 'b
val shape : 'b t -> 'b t tF

(** Concrete located type. *)
type ty = < loc : SourcePos.t > t

val compare : ty -> ty -> int
val print : Format.formatter -> ty -> unit

module Test : sig
  val gen_tF : 'a QCheck.Gen.t -> 'a tF QCheck.Gen.t
  val gen : ty QCheck.Gen.t
  val test : QCheck.Test.t list
end
