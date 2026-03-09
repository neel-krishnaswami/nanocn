(** Expressions in the language. *)

(** The shape functor for expressions. ['a] is the recursive occurrence. *)
type 'a exprF =
  | Var of Var.t
  | IntLit of int
  | Let of Var.t * 'a * 'a
  | Tuple of 'a list
  | LetTuple of Var.t list * 'a * 'a
  | Inject of Label.t * 'a
  | Case of 'a * (Label.t * Var.t * 'a) list
  | Iter of Var.t * 'a * 'a
  | App of Prim.arith * 'a
  | StateOp of Prim.state * Typ.ty * 'a
  | Annot of 'a * Typ.ty * Effect.t

val map : ('a -> 'b) -> 'a exprF -> 'b exprF

val print_exprF : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a exprF -> unit

(** An expression tree annotated with ['b] at each node. *)
type 'b t = In of 'b t exprF * 'b

val extract : 'b t -> 'b
val shape : 'b t -> 'b t exprF

(** Concrete located expression. *)
type expr = < loc : SourcePos.t > t

val print : Format.formatter -> expr -> unit

module Test : sig
  val gen : expr QCheck.Gen.t
  val test : QCheck.Test.t list
end
