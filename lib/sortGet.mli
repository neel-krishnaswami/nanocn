(** Fallible extractors for [Sort.sort].

    Each extractor inspects [Sort.shape] and either returns the
    sub-sorts of the matching constructor or fails with
    [[Error.construct_sort_mismatch]] carrying the [~construct] label
    and an [expected_shape] hint.  Callers thread the
    [(_, Error.t) result] through the View pipeline (e.g. via
    [Result.map] / [Constraint.atom']) rather than failing the
    elaboration monad. *)

val get_pred   : construct:string -> Sort.sort -> (Sort.sort, Error.t) result
val get_app    : construct:string -> Sort.sort -> (Dsort.t * Sort.sort list, Error.t) result
val get_record : construct:string -> Sort.sort -> (Sort.sort list, Error.t) result
