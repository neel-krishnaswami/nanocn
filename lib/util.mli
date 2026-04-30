(** Generic utilities used across nanoCN.

    Lives outside any specific data structure, so it must not depend on
    other modules in the [nanocn] library. *)

val result_list : ('a, 'e) result list -> ('a list, 'e) result
(** [result_list xs] sequences a list of results.  Returns [Ok ys] when
    every element is [Ok y] (preserving order), or the first [Error e]
    encountered while traversing [xs] left-to-right. *)

module Test : sig
  val test : QCheck.Test.t list
end
