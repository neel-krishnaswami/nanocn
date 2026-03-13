(** Core expressions (unified computation + assertion).

    Core expressions have patterns compiled away — case branches bind
    a single variable, and let/take bind a single variable. *)

(** The shape functor for core expressions. ['a] is the recursive occurrence. *)
type 'a ceF =
  | Var of Var.t
  | IntLit of int
  | BoolLit of bool
  | Let of Var.t * 'a * 'a
  | Tuple of 'a list
  | LetTuple of Var.t list * 'a * 'a
  | Inject of Label.t * 'a
  | Case of 'a * (Label.t * Var.t * 'a) list
  | Iter of Var.t * 'a * 'a
  | App of Prim.t * 'a
  | Call of Var.t * 'a
  | If of 'a * 'a * 'a
  | Annot of 'a * Sort.sort * Effect.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Own of Sort.sort
  | Take of Var.t * 'a * 'a
  | Return of 'a

val map : ('a -> 'b) -> 'a ceF -> 'b ceF

(** A core expression tree annotated with ['b] at each node. *)
type 'b t = In of 'b t ceF * 'b

val extract : 'b t -> 'b
val shape : 'b t -> 'b t ceF

(** Concrete located core expression. *)
type ce = < loc : SourcePos.t > t

val print : Format.formatter -> _ t -> unit

module Test : sig
  val gen : ce QCheck.Gen.t
  val test : QCheck.Test.t list
end
