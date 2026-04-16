(** Structured typechecking errors.

    Replaces the previous [string]-typed errors from surface typechecking,
    elaboration, and refined checking. The ADT will grow in future phases
    (see [doc/errors/type-errors.md]); Phase 1 exposes only the [Legacy]
    bridge which carries a pre-formatted message and an optional source
    position so existing behaviour is preserved while the plumbing moves
    to structured errors. *)

type t

val legacy : SourcePos.t option -> string -> t
(** [legacy pos msg] builds a legacy error carrying a pre-formatted
    message. Used during the incremental migration; will be retired
    once every site emits a [Structured] variant. *)

val loc : t -> SourcePos.t option
(** [loc e] returns the error's source position, if known. *)

val to_string : t -> string
(** [to_string e] renders [e] through [print] with an empty source
    registry (no excerpt) and captures the result as a string.
    Primarily for tests and legacy callers that expect plain strings. *)

val print : SourceExcerpt.registry -> Format.formatter -> t -> unit
(** [print reg fmt e] prints an error with a high-level description, a
    source excerpt (from the registry) around the erroneous range, and
    any structured details the variant carries. Uses the [Format]
    semantic tags configured by [ErrorRender] so the output is coloured
    on a terminal and plain when piped. *)

module Test : sig
  val test : QCheck.Test.t list
end
