(** Assertion sorts (τ).

    Sorts classify assertion-language values: integers, booleans, locations,
    tuples, datasort applications, and predicates. *)

(** The shape functor for sorts. ['a] is the recursive occurrence. *)
type 'a sortF =
  | Int
  | Bool
  | Loc
  | Record of 'a list
  | App of Dsort.t * 'a list
  | Pred of 'a
  | TVar of Tvar.t

val map : ('a -> 'b) -> 'a sortF -> 'b sortF

(** A sort tree annotated with ['b] at each node. *)
type 'b t = In of 'b t sortF * 'b

val extract : 'b t -> 'b
val shape : 'b t -> 'b t sortF

(** Concrete located sort. *)
type sort = < loc : SourcePos.t > t

val compare : sort -> sort -> int
val print : Format.formatter -> sort -> unit

val is_spec_type : sort -> bool
(** [is_spec_type s] is [true] if [s] contains no [Pred] anywhere.
    Enforces the σ ⊂ τ subrule: equality is only defined on spec types. *)

module Test : sig
  val gen : sort QCheck.Gen.t
  val test : QCheck.Test.t list
end
