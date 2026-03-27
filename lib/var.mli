(** Variables (identifiers starting with lowercase).

    Variables carry their source position. For binding occurrences, this
    is where the variable is bound. For use occurrences, this is where
    the variable appears in source. Generated variables carry the position
    of the subpattern they originate from.

    Comparison is by unique ID only, ignoring names and source positions. *)

type t

val of_string : string -> SourcePos.t -> t
(** [of_string s pos] creates a user variable with name [s] at position [pos].
    Uses a sentinel ID (-1); for unique IDs, use [mk] with a supply instead.
    Only for backward compatibility in tests. *)

type supply
(** A purely functional supply of fresh generated variables. *)

val mk : string -> SourcePos.t -> supply -> t * supply
(** [mk name pos supply] creates a user variable with a unique ID from [supply]. *)

val to_string : t -> string
val compare : t -> t -> int
val print : Format.formatter -> t -> unit
val json : t -> Json.t

val is_generated : t -> bool
(** [is_generated v] is [true] if [v] was created by [fresh]. *)

val binding_site : t -> SourcePos.t
(** [binding_site v] returns the source position associated with [v]. *)

val empty_supply : supply
val fresh : SourcePos.t -> supply -> t * supply
(** [fresh pos s] returns a fresh generated variable and the updated supply.
    The variable's binding site is [pos]. *)

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
