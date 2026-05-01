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

val lookup_all :
  'a Sig.t -> Dsort.t -> Sort.sort list ->
  ((Label.t * Sort.sort) list, Error.kind) result
(** [lookup_all sig d args] returns every [(label, payload_sort)]
    pair declared at head [d], with [args] substituted into each
    payload sort.  Used by case completeness checking to compare the
    given branches against the full ctor set.

    Fails the same way as [lookup] for unbound head sort and
    arity mismatches; never fails for a missing/extra ctor (that
    is the caller's responsibility — reported via
    [K_missing_ctor] / [K_redundant_ctor]). *)

val lookup_all_observed :
  'a Sig.t ->
  (Sort.sort, Error.kind) result ->
  Label.t list ->
  (Label.t * (Sort.sort, Error.kind) result) list
(** [lookup_all_observed sig sort observed] unifies the case-dispatch
    decisions made by surface case checking and pattern-matrix
    elaboration.  [observed] is the list of constructor labels seen
    in a column's leading patterns (caller may pass duplicates; the
    wrapper dedupes internally, keeping the first occurrence).

    Behaviour:

    - When [sort] is [Error e]: returns [(L, Error e)] for each
      distinct observed label.  Exhaustiveness checking is
      suppressed — the upstream error has already been reported,
      and we don't know which constructors are declared.

    - When [sort] is [Ok s]: extracts the head dsort and type args
      via [SortView.Get.app], then calls [lookup_all] to obtain the
      declared ctor set.
      - Each observed label that is in the declared set yields
        [(L, Ok payload_sort)] (with type args substituted).
      - Each observed label that is not in the declared set yields
        [(L, Error (K_ctor_not_in_decl ...))].
      - Each declared label that was not observed yields
        [(L, Ok payload_sort)] appended after the observed entries.
        Callers synthesize a [Hole] branch for these.
      - If [Get.app] or [lookup_all] fails (head sort isn't a
        datasort/datatype application, dsort is unbound, type-arg
        arity mismatches), the failure is propagated to every
        observed label as in the [Error] case above; no
        exhaustiveness entries are emitted.

    Callers distinguish "observed but valid" from
    "declared-but-missing-from-observation" by looking up each
    output label in their own row map: present in the input ⇒ user
    wrote the branch; absent ⇒ wrapper synthesized the entry for
    completeness, body should be filled with a [Hole]. *)

module Test : sig
  val test : QCheck.Test.t list
end
