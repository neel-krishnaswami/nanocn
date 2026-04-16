(** A counter-example pattern shape for "non-exhaustive pattern match"
    errors.

    A [t] represents the *shape* of a value that the user's branches
    fail to cover. Variables are rendered as [_]; constructor
    applications and tuples are rendered the same way they appear in
    source patterns. The coverage checker in [Elaborate] produces a
    [t] when all branches have been ruled out by a particular
    constructor-sequence choice. *)

type t =
  | Wild
  | Ctor of Label.t * t
  | Tuple of t list

val print : Format.formatter -> t -> unit
(** [print fmt w] renders a witness using standard pattern syntax
    ([_], [Cons (_, _)], [(_, _, _)], ...). The printer emits the
    [Format] semantic tag ["red;bold"] around every [Ctor] constructor
    name so printed witnesses are visually anchored on the missing
    constructor. *)

module Test : sig
  val test : QCheck.Test.t list
end
