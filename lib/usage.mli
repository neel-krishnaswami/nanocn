(** Usage flags for refined context resource entries.

    [Used] = 0 (consumed), [Avail] = 1 (available), [Opt] = ? (optional). *)

type t = Used | Avail | Opt

val meet : t -> t -> t option
(** [meet u1 u2] is the greatest lower bound, or [None] for 1 ⊓ 0. *)

val lattice_meet : t -> t -> t
(** [lattice_meet u1 u2] is the greatest lower bound in the total order
    [Used ≤ Opt ≤ Avail]. Unlike [meet], this is always defined. *)

val is_avail : t -> bool
(** [is_avail u] is true when [u] is [Avail] or [Opt]. *)

val is_zero : t -> bool
(** [is_zero u] is true when [u] is [Used] or [Opt]. *)

val affinize : t -> t
(** [affinize Used = Used], [affinize Avail = Opt], [affinize Opt = Opt]. *)

val compare : t -> t -> int
val print : Format.formatter -> t -> unit

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
