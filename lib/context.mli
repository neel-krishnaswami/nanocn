(** Typing contexts with term variables and type variables.

    Term bindings carry [Sort.sort * Effect.t].
    Type variable bindings carry [Kind.t].
    [Unknown] bindings record a variable that is in scope but whose
    sort could not be determined — its binder's right-hand side had a
    typing error and the multi-error typechecker chose to continue. *)

type t

val empty : t

val extend : Var.t -> Sort.sort -> Effect.t -> t -> t
(** [extend x sort eff ctx] adds a term variable binding. *)

val extend_unknown : Var.t -> t -> t
(** [extend_unknown x ctx] adds [x] as a variable in scope whose sort
    is unknown.  Used by the multi-error typechecker when a binder's
    right-hand side erred. *)

val extend_or_unknown :
  Var.t -> (Sort.sort, _) result -> Effect.t -> t -> t
(** [extend_or_unknown x sort_result eff ctx] is [extend x s eff ctx]
    when [sort_result] is [Ok s], and [extend_unknown x ctx] otherwise.
    The single helper hides the case-analysis from typechecker clauses
    so they can stay branch-free on result values (View pattern). *)

val lookup : Var.t -> t -> (Sort.sort * Effect.t, Error.kind) result
(** [lookup x ctx] returns [Ok (sort, eff)] when [x] is a [Term]
    binding, [Error (K_unknown_var_type {var=x})] when it is an
    [Unknown] binding, and [Error (K_unbound_var x)] when [x] is not
    bound at all. *)

val extend_tvar : Tvar.t -> Kind.t -> t -> t
(** [extend_tvar a kind ctx] adds a type variable binding. *)

val lookup_tvar : Tvar.t -> t -> Kind.t option
(** [lookup_tvar a ctx] returns the kind for type variable [a]. *)

val extend_list : (Var.t * Sort.sort * Effect.t) list -> t -> t
(** [extend_list bindings ctx] extends [ctx] with multiple term bindings. *)

type binding =
  | Term of Var.t * Sort.sort * Effect.t
  | TVar of Tvar.t * Kind.t
  | Unknown of Var.t

val to_list : t -> binding list
(** [to_list ctx] returns the bindings as a list. *)

val print_gen : (Format.formatter -> Var.t -> unit) -> Format.formatter -> t -> unit
val print : Format.formatter -> t -> unit
val to_string : t -> string

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
