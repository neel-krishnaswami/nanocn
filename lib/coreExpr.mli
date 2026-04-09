(** Core expressions (unified computation + assertion).

    Core expressions have patterns compiled away — case branches bind
    a single variable, and let/take bind a single variable. *)

(** The shape functor for core expressions.
    ['a] = recursive positions, ['b] = auxiliary info.
    Embeds ['b Sort.t] so that annotation info flows through sorts. *)
type ('a, 'b) ceF =
  | Var of Var.t
  | IntLit of int
  | BoolLit of bool
  | Let of (Var.t * 'b) * 'a * 'a
  | Tuple of 'a list
  | LetTuple of (Var.t * 'b) list * 'a * 'a
  | Inject of Label.t * 'a
  | Case of 'a * (Label.t * Var.t * 'a * 'b) list
  | Iter of Var.t * 'a * 'a
  | App of Prim.t * 'a
  | Call of string * 'a
  | If of 'a * 'a * 'a
  | Annot of 'a * 'b Sort.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Take of (Var.t * 'b) * 'a * 'a
  | Return of 'a

val map_shape : ('a -> 'c) -> ('a, 'b) ceF -> ('c, 'b) ceF
val map_info : ('b -> 'c) -> ('a, 'b) ceF -> ('a, 'c) ceF

(** A core expression tree annotated with ['b] at each node. *)
type 'b t

val mk : 'b -> ('b t, 'b) ceF -> 'b t
val info : 'b t -> 'b
val shape : 'b t -> ('b t, 'b) ceF
val map : ('b -> 'c) -> 'b t -> 'c t

(** Concrete located core expression. *)
type ce = < loc : SourcePos.t > t

(** Typed core expression, carrying context, sort, and effect at every node. *)
type typed_info = < loc : SourcePos.t; ctx : Context.t; sort : Sort.sort; eff : Effect.t >
type typed_ce = typed_info t


val print_gen : (Format.formatter -> Var.t -> unit) -> Format.formatter -> _ t -> unit
val print : Format.formatter -> _ t -> unit
val to_string : _ t -> string
val json : ('b -> Json.t) -> 'b t -> Json.t

module Test : sig
  val gen : ce QCheck.Gen.t
  val test : QCheck.Test.t list
end
