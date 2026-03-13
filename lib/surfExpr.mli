(** Surface expressions (unified computation + assertion).

    Surface expressions retain complex patterns in let, take, case, and iter.
    They are elaborated to core expressions during typechecking. *)

(** The shape functor for surface expressions. ['a] is the recursive occurrence. *)
type 'a seF =
  | Var of Var.t
  | IntLit of int
  | BoolLit of bool
  | Let of Pat.pat * 'a * 'a
  | Tuple of 'a list
  | Inject of Label.t * 'a
  | Case of 'a * (Pat.pat * 'a) list
  | Iter of Pat.pat * 'a * 'a
  | App of Prim.t * 'a
  | Call of Var.t * 'a
  | If of 'a * 'a * 'a
  | Annot of 'a * Sort.sort * Effect.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Own of Sort.sort
  | Take of Pat.pat * 'a * 'a
  | Return of 'a

val map : ('a -> 'b) -> 'a seF -> 'b seF

(** A surface expression tree annotated with ['b] at each node. *)
type 'b t = In of 'b t seF * 'b

val extract : 'b t -> 'b
val shape : 'b t -> 'b t seF

(** Concrete located surface expression. *)
type se = < loc : SourcePos.t > t

val print : Format.formatter -> _ t -> unit

module Test : sig
  val gen : se QCheck.Gen.t
  val test : QCheck.Test.t list
end
