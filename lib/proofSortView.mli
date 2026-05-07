(** A View on [ProofSort.t] for destructuring and rebuilding proof
    sorts in the typechecker.

    [type 'a t = 'a option] — every extractor and builder works in
    option-land.  [Get] consumes a wrapped proof sort and returns
    wrapped components for the head plus a wrapped tail; [Build]
    takes wrapped components plus a wrapped tail and produces a
    wrapped proof sort.  When the input is [None] (or the head's
    shape doesn't match the requested constructor), every output is
    [None] too.

    The View carries no error vocabulary: it just reports whether a
    head shape matched.  Consumers (typechecker clauses) define
    small *local* wrappers that lift the option output to a
    [(_, Error.kind) result] using the appropriate
    [K_pf_kind_mismatch] / [K_pf_expected_nil] error kind with the
    call-site's [construct] argument.

    Mirrors [SortView]'s and [CoreExprView]'s conventions exactly. *)

type 'a t = 'a option

(** Five Get extractors, one per proof-sort head shape.

    Invariants:
    - If [pf] starts with the matching head, every component is
      [Some _], with no [None]s in any of them.
    - Otherwise (input is [None], the proof sort is empty when not
      asking for [nil], or the head is a different shape), every
      component is [None]. *)
module Get : sig
  val nil    : ('e, 'b, 'var) ProofSort.t t -> unit t
  val comp   : ('e, 'b, 'var) ProofSort.t t ->
    'var t * Sort.sort t * Effect.t t * ('e, 'b, 'var) ProofSort.t t
  val log    : ('e, 'b, 'var) ProofSort.t t ->
    'e t * ('e, 'b, 'var) ProofSort.t t
  val res    : ('e, 'b, 'var) ProofSort.t t ->
    'e t * 'e t * ('e, 'b, 'var) ProofSort.t t
  val depres : ('e, 'b, 'var) ProofSort.t t ->
    'var t * 'e t * ('e, 'b, 'var) ProofSort.t t
end

(** Five Build constructors, one per proof-sort head shape.  Each
    takes an info ['b] for the head entry and option-typed components.

    Invariants:
    - If every component is [Some _], the result is [Some pf] for the
      reconstructed proof sort.
    - If any component is [None], the result is [None]. *)
module Build : sig
  val nil    : 'b -> unit t -> ('e, 'b, 'var) ProofSort.t t
  val comp   : 'b -> 'var t -> Sort.sort t -> Effect.t t ->
    ('e, 'b, 'var) ProofSort.t t -> ('e, 'b, 'var) ProofSort.t t
  val log    : 'b -> 'e t ->
    ('e, 'b, 'var) ProofSort.t t -> ('e, 'b, 'var) ProofSort.t t
  val res    : 'b -> 'e t -> 'e t ->
    ('e, 'b, 'var) ProofSort.t t -> ('e, 'b, 'var) ProofSort.t t
  val depres : 'b -> 'var t -> 'e t ->
    ('e, 'b, 'var) ProofSort.t t -> ('e, 'b, 'var) ProofSort.t t
end

module Test : sig
  val test : QCheck.Test.t list
end
