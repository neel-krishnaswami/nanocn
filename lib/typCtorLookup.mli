(** Type constructor resolution.

    Implements the [L : A in D(A1,...,Ai) in S] judgement: given a label,
    a signature, and datatype type arguments, resolves the constructor's
    argument type with type variables substituted. *)

val lookup : Sig.t -> Label.t -> Typ.ty list -> (Typ.ty, string) result
(** [lookup sig l args] finds constructor [l] in the signature, then
    substitutes [args] for the datatype's type parameters in the
    constructor's raw type. *)
