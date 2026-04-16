(** Explicit substitutions.

    A substitution maps type variables to sorts and term variables to
    core expressions. Application is capture-avoiding: at each binder,
    the substitution is extended with an identity mapping for the bound
    variable. *)

type t

val empty : t
val extend_tvar : Tvar.t -> Sort.sort -> t -> t
val extend_var : Var.t -> CoreExpr.typed_ce -> t -> t

val apply : t -> Sort.sort -> Sort.sort
(** [apply gamma tau] is [[gamma]]tau. *)

val apply_ce : t -> CoreExpr.typed_ce -> CoreExpr.typed_ce
(** [apply_ce gamma ce] is [[gamma]]ce. *)

val of_lists :
  Tvar.t list -> Sort.sort list -> (t, Error.kind) result
(** [of_lists tvars sorts] builds a type-variable substitution.
    Fails with [Error.K_subst_arity_mismatch] if the two lists
    differ in length. *)

val id : Context.t -> t
(** [id gamma] is the identity substitution on [gamma]. *)

val compose : t -> t -> t
(** [compose gamma0 gamma1] is [gamma0 ; gamma1]. *)

val compare : t -> t -> int
val print : Format.formatter -> t -> unit

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
