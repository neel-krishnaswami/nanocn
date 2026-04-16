(** Structural diff of two [Sort.sort] values.

    Used by the type-error printer: when a sort mismatch occurs, we
    want to show both sides and call out which subterms disagree.
    A [shape_compare] is a tree that walks two sorts in lockstep,
    inserting [Diff] nodes at the first mismatch on each path and
    [Same] nodes wherever the heads agree. *)

type shape_compare =
  | Diff of Sort.sort * Sort.sort
  | Same of (shape_compare, < loc : SourcePos.t >) Sort.sortF

val diff : Sort.sort -> Sort.sort -> shape_compare
(** [diff s1 s2] walks the two sorts in lockstep:
    - at each node whose head constructors match (including arity
      and any leaf data like [Dsort.t] / [Tvar.t]), produces a
      [Same] wrapping the children's [diff]s;
    - at the first divergence, produces a [Diff (s1', s2')] carrying
      the two differing subterms. *)

val left : shape_compare -> Sort.sort
(** [left c] reconstructs the first argument of the [diff] that
    produced [c]. Matches [s1] bit-for-bit up to the phantom info. *)

val right : shape_compare -> Sort.sort
(** [right c] reconstructs the second argument of the [diff] that
    produced [c]. *)

val print_left : Format.formatter -> shape_compare -> unit
(** [print_left fmt c] prints the left sort, wrapping every [Diff]
    subterm in a semantic tag ["red;bold"] so that the mismatch is
    visually marked in the output. Context preserved via
    [Sort.print]-style boxing. *)

val print_right : Format.formatter -> shape_compare -> unit
(** [print_right fmt c] does the same for the right sort. *)

module Test : sig
  val test : QCheck.Test.t list
end
