(** Constructor resolution (unified sort + type).

    Implements the [L : τ in D(τ1,...,τi) in S] judgement: given a label,
    a signature, and sort arguments, resolves the constructor's
    argument sort with type variables substituted.

    Dispatches to datasort declarations first, then datatype declarations
    (converting types to sorts). *)

val lookup :
  'a Sig.t -> Label.t -> Sort.sort list ->
  (Sort.sort, Error.kind) result
(** [lookup sig l args] finds constructor [l] in the signature, then
    substitutes [args] for the declaration's type parameters in the
    constructor's raw sort/type. Fails with:
    - [Error.K_unbound_ctor] if [l] is not a constructor of any
      sort or type in [sig];
    - [Error.K_ctor_sig_inconsistent] if [l] is indexed in [sig] but
      missing from the decl's own constructor table (signature
      invariant broken);
    - [Error.K_subst_arity_mismatch] if [args] has a different
      arity than the decl's type parameters. *)
