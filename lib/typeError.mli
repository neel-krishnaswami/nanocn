(** Structured typechecking errors.

    Replaces the previous [string]-typed errors from surface
    typechecking, elaboration, and refined checking. The ADT grows
    per phase (see [doc/errors/type-errors.md]):

    - Phase 1: [Legacy] bridge only — carries a pre-formatted message
      and an optional source position.
    - Phase 2: sort-mismatch kinds ([K_sort_mismatch],
      [K_annotation_disagrees]) carrying a
      [SortDiff.shape_compare] so printers can highlight differing
      subterms.
    - Phase 3: unbound names ([K_unbound_var], [K_unbound_ctor],
      [K_unbound_sort], [K_unbound_tvar]) and simple mismatches
      ([K_var_effect_mismatch], [K_prim_effect_mismatch],
      [K_fun_effect_mismatch], [K_scrutinee_not_data],
      [K_not_spec_type], [K_spec_context_required]).
    - Later phases add more [kind] constructors. *)

type t

(** {1 Legacy bridge} *)

val legacy : SourcePos.t option -> string -> t
(** [legacy pos msg] builds a legacy error carrying a pre-formatted
    message. Used during the incremental migration; will be retired
    once every site emits a [Structured] variant. *)

(** {1 Structured errors} *)

type kind =
  (* Phase 2 — sort mismatches *)
  | K_sort_mismatch of
      { expected : Sort.sort
      ; actual : Sort.sort
      ; diff : SortDiff.shape_compare }
  | K_annotation_disagrees of
      { inner_sort : Sort.sort
      ; annot : Sort.sort
      ; diff : SortDiff.shape_compare }

  (* Phase 3 — unbound names *)
  | K_unbound_var of Var.t
  | K_unbound_name of string
    (** A string-named reference that failed to resolve — used by
        the scope resolver, which sees variables before they gain
        their [Var.t] identity. *)
  | K_unbound_ctor of Label.t
  | K_unbound_sort of Dsort.t
  | K_unbound_tvar of Tvar.t

  (* Phase 3 — effect mismatches (three variants distinguish what
     supplies the incompatible effect) *)
  | K_var_effect_mismatch of
      { var : Var.t
      ; declared : Effect.t
      ; required : Effect.t }
  | K_prim_effect_mismatch of
      { prim : Prim.t
      ; declared : Effect.t
      ; required : Effect.t }
  | K_fun_effect_mismatch of
      { name : string
      ; declared : Effect.t
      ; required : Effect.t }

  (* Phase 3 — misc simple mismatches *)
  | K_scrutinee_not_data of { got : Sort.sort }
  | K_not_spec_type of { construct : string; got : Sort.sort }
  | K_spec_context_required of { construct : string }

  (* Phase 4 — pattern coverage *)
  | K_non_exhaustive of { witness : PatWitness.t }

  (* Phase 5 — refined checker *)
  | K_resource_leak of { name : Var.t option }
    (** A resource binding fell out of scope without being consumed
        (must be a linear consumption). [name] is the resource
        variable, when the checker can identify it. *)
  | K_iter_requires_impure of { actual : Effect.t }
    (** A refined [iter] was encountered at an effect that doesn't
        subsume [impure]. *)
  | K_internal_invariant of { rule : string; invariant : string }
    (** A check in the refined checker's internal logic failed —
        typically an "impossible" shape at a match arm, a
        previously-validated condition that no longer holds, or an
        early synthesise/check restriction. Not user-facing in
        normal use; reported with the [rule] (function or match arm
        that raised the check) and a short [invariant] description
        so that debugging can start from the exact failure point. *)
(** The kind of structured failure. *)

val structured : loc:SourcePos.t -> kind -> t
(** [structured ~loc kind] builds a structured error at [loc]. *)

val sort_mismatch :
  loc:SourcePos.t -> expected:Sort.sort -> actual:Sort.sort -> t
(** Convenience builder for [K_sort_mismatch]; computes the diff
    internally. *)

val annotation_disagrees :
  loc:SourcePos.t -> inner:Sort.sort -> annot:Sort.sort -> t
(** Convenience builder for [K_annotation_disagrees]. *)

val unbound_var : loc:SourcePos.t -> Var.t -> t
val unbound_name : loc:SourcePos.t -> string -> t
val unbound_ctor : loc:SourcePos.t -> Label.t -> t
val unbound_sort : loc:SourcePos.t -> Dsort.t -> t
val unbound_tvar : loc:SourcePos.t -> Tvar.t -> t

val var_effect_mismatch :
  loc:SourcePos.t -> var:Var.t ->
  declared:Effect.t -> required:Effect.t -> t

val prim_effect_mismatch :
  loc:SourcePos.t -> prim:Prim.t ->
  declared:Effect.t -> required:Effect.t -> t

val fun_effect_mismatch :
  loc:SourcePos.t -> name:string ->
  declared:Effect.t -> required:Effect.t -> t

val scrutinee_not_data : loc:SourcePos.t -> got:Sort.sort -> t

val not_spec_type :
  loc:SourcePos.t -> construct:string -> got:Sort.sort -> t
(** [not_spec_type ~loc ~construct ~got] is raised when a construct
    that requires a spec sort (no [Pred]) receives one that contains
    a [Pred]. [construct] names the construct ("equality", etc.). *)

val spec_context_required :
  loc:SourcePos.t -> construct:string -> t
(** [spec_context_required ~loc ~construct] is raised when an operator
    or form (e.g. [return], [fail], [take]) is used outside a [[spec]]
    context. *)

val non_exhaustive :
  loc:SourcePos.t -> witness:PatWitness.t -> t
(** [non_exhaustive ~loc ~witness] is raised when a pattern match
    misses a case. [witness] is an example of a value shape that
    would not be matched. *)

val resource_leak :
  loc:SourcePos.t -> name:Var.t option -> t
(** [resource_leak ~loc ~name] is raised when a resource binding
    (from [let res] / rfun domain / iter binder) is discarded
    without a linear consumer. *)

val iter_requires_impure :
  loc:SourcePos.t -> actual:Effect.t -> t
(** [iter_requires_impure ~loc ~actual] reports that a refined
    [iter] was used where the ambient effect does not allow
    [impure]. *)

val internal_invariant :
  loc:SourcePos.t -> rule:string -> invariant:string -> t
(** [internal_invariant ~loc ~rule ~invariant] records that an
    internal consistency check failed. [rule] names the function or
    rule-arm that raised the check; [invariant] describes the
    specific check that failed. *)

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
