(** Fallible extractors for [Sort.sort].

    Each extractor inspects [Sort.shape] and either returns the
    sub-sorts of the matching constructor or fails with
    [Error.K_construct_sort_mismatch] carrying the [~construct] label
    and an [expected_shape] hint. Lift via [ElabM.lift_at] at the call
    site. *)

val get_pred   : construct:string -> Sort.sort -> (Sort.sort, Error.kind) result
val get_app    : construct:string -> Sort.sort -> (Dsort.t * Sort.sort list, Error.kind) result
val get_record : construct:string -> Sort.sort -> (Sort.sort list, Error.kind) result
