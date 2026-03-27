(** Variables (identifiers starting with lowercase).

    Variables carry their source position. For binding occurrences, this
    is where the variable is bound. For use occurrences, this is where
    the variable appears in source. Generated variables carry the position
    of the subpattern they originate from.

    Comparison is by unique ID only, ignoring names and source positions. *)

type t

type supply
(** A purely functional supply of fresh generated variables. *)

val mk : string -> SourcePos.t -> supply -> t * supply
(** [mk name pos supply] creates a user variable with a unique ID from [supply]. *)

val name : t -> string
(** [name v] returns the bare name: ["x"] for [User(x, _)], ["_vN"] for [Generated N]. *)

val to_string : t -> string
(** [to_string v] returns a unique string: ["x_3"] for [User(x, 3)], ["_v5"] for [Generated 5]. *)

val print_unique : Format.formatter -> t -> unit
(** [print_unique fmt v] prints the unique form ["x_3"] for SMT/internal use. *)

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
