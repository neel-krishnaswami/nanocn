(** Types in the language. *)

(** The shape functor for types.
    ['a] = recursive positions, ['b] = auxiliary info (phantom here). *)
type ('a, 'b) tF =
  | Record of 'a list
  | App of Dsort.t * 'a list
  | TVar of Tvar.t
  | Int
  | Bool
  | Ptr of 'a

val map_shape : ('a -> 'c) -> ('a, 'b) tF -> ('c, 'b) tF
val map_info : ('b -> 'c) -> ('a, 'b) tF -> ('a, 'c) tF
val compare_tF : ('a -> 'a -> int) -> ('a, 'b) tF -> ('a, 'b) tF -> int
val print_tF : (Format.formatter -> 'a -> unit) -> Format.formatter -> ('a, 'b) tF -> unit

(** A type tree annotated with ['b] at each node. *)
type 'b t

val mk : 'b -> ('b t, 'b) tF -> 'b t
val info : 'b t -> 'b
val shape : 'b t -> ('b t, 'b) tF
val map : ('b -> 'c) -> 'b t -> 'c t

(** Concrete located type. *)
type ty = < loc : SourcePos.t > t

val compare : ty -> ty -> int
val print : Format.formatter -> _ t -> unit
val json : _ t -> Json.t

val is_eqtype : ty -> bool
(** [is_eqtype t] returns [true] if [t] is [int], [bool], or [ptr _]. *)

module Test : sig
  val gen_tF : 'a QCheck.Gen.t -> ('a, 'b) tF QCheck.Gen.t
  val gen : ty QCheck.Gen.t
  val test : QCheck.Test.t list
end
