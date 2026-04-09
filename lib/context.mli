(** Typing contexts with term variables and type variables.

    Term bindings carry [Sort.sort * Effect.t].
    Type variable bindings carry [Kind.t]. *)

type t

val empty : t

val extend : Var.t -> Sort.sort -> Effect.t -> t -> t
(** [extend x sort eff ctx] adds a term variable binding. *)

val lookup : Var.t -> t -> (Sort.sort * Effect.t) option
(** [lookup x ctx] returns the sort and effect for term variable [x]. *)

val extend_tvar : Tvar.t -> Kind.t -> t -> t
(** [extend_tvar a kind ctx] adds a type variable binding. *)

val lookup_tvar : Tvar.t -> t -> Kind.t option
(** [lookup_tvar a ctx] returns the kind for type variable [a]. *)

val extend_list : (Var.t * Sort.sort * Effect.t) list -> t -> t
(** [extend_list bindings ctx] extends [ctx] with multiple term bindings. *)

type binding =
  | Term of Var.t * Sort.sort * Effect.t
  | TVar of Tvar.t * Kind.t

val to_list : t -> binding list
(** [to_list ctx] returns the bindings as a list. *)

val print_gen : (Format.formatter -> Var.t -> unit) -> Format.formatter -> t -> unit
val print : Format.formatter -> t -> unit
val to_string : t -> string

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
