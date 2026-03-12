(** Constructor resolution.

    Implements the [L : τ in D(τ1,...,τi) in S] judgement: given a label,
    a signature, and datasort type arguments, resolves the constructor's
    argument sort with type variables substituted. *)

val lookup : Sig.t -> Label.t -> Sort.sort list -> (Sort.sort, string) result
(** [lookup sig l args] finds constructor [l] in the signature, then
    substitutes [args] for the datasort's type parameters in the
    constructor's raw sort. *)
