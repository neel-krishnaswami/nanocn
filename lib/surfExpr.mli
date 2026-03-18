(** Surface expressions (unified computation + assertion).

    Surface expressions retain complex patterns in let, take, case, and iter.
    They are elaborated to core expressions during typechecking. *)

(** The shape functor for surface expressions.
    ['a] = recursive positions, ['b] = auxiliary info.
    Embeds ['b Pat.t] and ['b Sort.t] so that annotation info flows through. *)
type ('a, 'b) seF =
  | Var of Var.t
  | IntLit of int
  | BoolLit of bool
  | Let of 'b Pat.t * 'a * 'a
  | Tuple of 'a list
  | Inject of Label.t * 'a
  | Case of 'a * ('b Pat.t * 'a * 'b) list
  | Iter of 'b Pat.t * 'a * 'a
  | App of Prim.t * 'a
  | Call of Var.t * 'a
  | If of 'a * 'a * 'a
  | Annot of 'a * 'b Sort.t * Effect.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Own of 'b Sort.t
  | Take of 'b Pat.t * 'a * 'a
  | Return of 'a

val map_shape : ('a -> 'c) -> ('a, 'b) seF -> ('c, 'b) seF
val map_info : ('b -> 'c) -> ('a, 'b) seF -> ('a, 'c) seF

(** A surface expression tree annotated with ['b] at each node. *)
type 'b t

val mk : 'b -> ('b t, 'b) seF -> 'b t
val info : 'b t -> 'b
val shape : 'b t -> ('b t, 'b) seF
val map : ('b -> 'c) -> 'b t -> 'c t

(** Concrete located surface expression. *)
type se = < loc : SourcePos.t > t

val print : Format.formatter -> _ t -> unit
val json : ('b -> Json.t) -> 'b t -> Json.t

module Test : sig
  val gen : se QCheck.Gen.t
  val test : QCheck.Test.t list
end
