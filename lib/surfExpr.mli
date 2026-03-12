(** Surface assertion expressions.

    Surface expressions retain complex patterns in let, take, and case.
    They are elaborated to core expressions during typechecking. *)

(** The shape functor for surface expressions. ['a] is the recursive occurrence. *)
type 'a seF =
  | Var of Var.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Take of Pat.pat * 'a * 'a
  | Return of 'a
  | Let of Pat.pat * 'a * 'a
  | Tuple of 'a list
  | Inject of Label.t * 'a
  | Case of 'a * (Pat.pat * 'a) list
  | Call of Var.t * 'a
  | Const of Var.t
  | If of 'a * 'a * 'a
  | Annot of 'a * Sort.sort
  | IntLit of int
  | BoolLit of bool
  | Prim of Prim.t * 'a

val map : ('a -> 'b) -> 'a seF -> 'b seF

(** A surface expression tree annotated with ['b] at each node. *)
type 'b t = In of 'b t seF * 'b

val extract : 'b t -> 'b
val shape : 'b t -> 'b t seF

(** Concrete located surface expression. *)
type se = < loc : SourcePos.t > t

val print : Format.formatter -> se -> unit

module Test : sig
  val gen : se QCheck.Gen.t
  val test : QCheck.Test.t list
end
