(** Constructor resolution (unified sort + type).

    Implements the [L : τ in D(τ1,...,τi) in S] judgement: given a head
    sort/type name [D], a label [L], a signature, and sort arguments,
    resolves the constructor's argument sort with type variables
    substituted.

    Dispatches on whether [D] is declared as a datasort or a datatype. *)

val lookup :
  'a Sig.t -> Dsort.t -> Label.t -> Sort.sort list ->
  (Sort.sort, Error.kind) result
(** [lookup sig d l args] finds the constructor [l] of [d] in the
    signature, then substitutes [args] for the declaration's type
    parameters in the constructor's raw sort. Fails with:
    - [Error.K_unbound_sort d] if [d] is not declared in [sig];
    - [Error.K_ctor_not_in_decl] if [d] is declared but [l] is not
      one of its constructors;
    - [Error.K_subst_arity_mismatch] if [args] has a different
      arity than the decl's type parameters. *)

module Test : sig
  val test : QCheck.Test.t list
end
