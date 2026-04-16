(** Structured typechecking errors.

    Replaces the previous [string]-typed errors from surface
    typechecking, elaboration, and refined checking. The ADT grows
    per phase (see [doc/errors/type-errors.md]):

    - Phase 1: [Legacy] bridge only — carries a pre-formatted message
      and an optional source position.
    - Phase 2: [Structured] variant with sort-mismatch kinds
      ([K_sort_mismatch], [K_annotation_disagrees]) carrying a
      [SortDiff.shape_compare] so printers can highlight differing
      subterms.
    - Later phases add more [kind] constructors. *)

type t

(** {1 Legacy bridge} *)

val legacy : SourcePos.t option -> string -> t
(** [legacy pos msg] builds a legacy error carrying a pre-formatted
    message. Used during the incremental migration; will be retired
    once every site emits a [Structured] variant. *)

(** {1 Structured errors} *)

type kind =
  | K_sort_mismatch of
      { expected : Sort.sort
      ; actual : Sort.sort
      ; diff : SortDiff.shape_compare }
  | K_annotation_disagrees of
      { inner_sort : Sort.sort
      ; annot : Sort.sort
      ; diff : SortDiff.shape_compare }
(** The kind of structured failure. Phase 2 exposes the two
    sort-mismatch kinds; later phases add more. *)

val structured : loc:SourcePos.t -> kind -> t
(** [structured ~loc kind] builds a structured error at [loc]. *)

val sort_mismatch :
  loc:SourcePos.t -> expected:Sort.sort -> actual:Sort.sort -> t
(** Convenience builder for [K_sort_mismatch]; computes the diff
    internally. *)

val annotation_disagrees :
  loc:SourcePos.t -> inner:Sort.sort -> annot:Sort.sort -> t
(** Convenience builder for [K_annotation_disagrees]. *)

(** {1 Accessors and printers} *)

val loc : t -> SourcePos.t option
(** [loc e] returns the error's source position, if known. *)

val to_string : t -> string
(** [to_string e] renders [e] through [print] with an empty source
    registry (no excerpt) and captures the result as a string.
    Primarily for tests and legacy callers that expect plain strings. *)

val print : SourceExcerpt.registry -> Format.formatter -> t -> unit
(** [print reg fmt e] prints an error with a high-level description,
    a source excerpt (from the registry) around the erroneous range,
    and any structured details the variant carries. Uses the
    [Format] semantic tags configured by [ErrorRender] so the output
    is coloured on a terminal and plain when piped. *)

module Test : sig
  val test : QCheck.Test.t list
end
