(** Constructor resolution (unified sort + type).

    Implements the [L : τ in D(τ1,...,τi) in S] judgement: given a label,
    a signature, and sort arguments, resolves the constructor's
    argument sort with type variables substituted.

    Dispatches to datasort declarations first, then datatype declarations
    (converting types to sorts). *)

val lookup : Sig.t -> Label.t -> Sort.sort list -> (Sort.sort, string) result
(** [lookup sig l args] finds constructor [l] in the signature, then
    substitutes [args] for the declaration's type parameters in the
    constructor's raw sort/type. *)
