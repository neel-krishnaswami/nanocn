(** Surface expressions (unified computation + assertion).

    Surface expressions retain complex patterns in let, take, case, and iter.
    They are elaborated to core expressions during typechecking.

    Parameterized by ['var] — the type of variable names
    ([string] at parse time, [Var.t] after scope resolution). *)

(** The shape functor for surface expressions.
    ['a] = recursive positions, ['b] = auxiliary info, ['var] = variable type.
    Embeds [('b, 'var) Pat.t] and ['b Sort.t] so that annotation info flows through. *)
type ('a, 'b, 'var) seF =
  | Var of 'var
  | IntLit of int
  | BoolLit of bool
  | Let of ('b, 'var) Pat.t * 'a * 'a
  | Tuple of 'a list
  | Inject of Label.t * 'a
  | Case of 'a * (('b, 'var) Pat.t * 'a * 'b) list
  | Iter of ('b, 'var) Pat.t * 'a * 'a
  | App of Prim.t * 'a
  | Call of string * 'a
  | If of 'a * 'a * 'a
  | Annot of 'a * 'b Sort.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Take of ('b, 'var) Pat.t * 'a * 'a
  | Return of 'a

val map_shape : ('a -> 'c) -> ('a, 'b, 'var) seF -> ('c, 'b, 'var) seF
val map_info : ('b -> 'c) -> ('a, 'b, 'var) seF -> ('a, 'c, 'var) seF

(** A surface expression tree annotated with ['b] at each node. *)
type ('b, 'var) t

val mk : 'b -> (('b, 'var) t, 'b, 'var) seF -> ('b, 'var) t
val info : ('b, 'var) t -> 'b
val shape : ('b, 'var) t -> (('b, 'var) t, 'b, 'var) seF
val map : ('b -> 'c) -> ('b, 'var) t -> ('c, 'var) t

(** Concrete located surface expression (after scope resolution). *)
type se = (< loc : SourcePos.t >, Var.t) t

(** Parsed surface expression (before scope resolution). *)
type parsed_se = (< loc : SourcePos.t >, string) t

val print : Format.formatter -> (_, Var.t) t -> unit
val json : ('b -> Json.t) -> ('b, Var.t) t -> Json.t

module Test : sig
  val gen : se QCheck.Gen.t
  val test : QCheck.Test.t list
end
