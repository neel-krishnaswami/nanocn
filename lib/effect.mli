(** Effects: pure, impure, or spec.

    Three-element lattice: pure ≤ impure, pure ≤ spec.
    Impure and spec are incomparable. *)

type t = Pure | Impure | Spec

val sub : t -> t -> bool
(** [sub e1 e2] is true when [e1 ≤ e2] in the subeffecting order. *)

val join : t -> t -> t option
(** [join e1 e2] is the least upper bound, if it exists.
    Returns [None] when [e1] and [e2] are incomparable (impure ⊔ spec). *)

val purify : t -> t
(** [purify e] is the effect used for bindings:
    [purify pure = pure], [purify impure = pure], [purify spec = spec]. *)

val compare : t -> t -> int
val print : Format.formatter -> t -> unit
val json : t -> Json.t

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
