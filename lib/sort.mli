(** Assertion sorts (τ).

    Sorts classify assertion-language values: integers, booleans, pointers,
    tuples, datasort applications, and predicates. *)

(** The shape functor for sorts.
    ['a] = recursive positions, ['b] = auxiliary info (phantom here). *)
type ('a, 'b) sortF =
  | Int
  | Bool
  | Ptr of 'a
  | Record of 'a list
  | App of Dsort.t * 'a list
  | Pred of 'a
  | TVar of Tvar.t

val map_shape : ('a -> 'c) -> ('a, 'b) sortF -> ('c, 'b) sortF
val map_info : ('b -> 'c) -> ('a, 'b) sortF -> ('a, 'c) sortF

(** A sort tree annotated with ['b] at each node. *)
type 'b t

val mk : 'b -> ('b t, 'b) sortF -> 'b t
val info : 'b t -> 'b
val shape : 'b t -> ('b t, 'b) sortF
val map : ('b -> 'c) -> 'b t -> 'c t

(** Concrete located sort. *)
type sort = < loc : SourcePos.t > t

val compare : sort -> sort -> int
val print : Format.formatter -> _ t -> unit
val json : ('b -> Json.t) -> 'b t -> Json.t

val is_spec_type : sort -> bool
(** [is_spec_type s] is [true] if [s] contains no [Pred] anywhere.
    Enforces the σ ⊂ τ subrule: equality is only defined on spec types. *)

val typ_to_sort : Typ.ty -> sort
(** [typ_to_sort ty] converts a type to the corresponding sort.
    Types are a subset of sorts (no Pred). *)

val sort_to_typ : sort -> (Typ.ty, string) result
(** [sort_to_typ s] converts a sort to a type, failing if [s] contains Pred. *)

module Test : sig
  val gen : sort QCheck.Gen.t
  val test : QCheck.Test.t list
end
