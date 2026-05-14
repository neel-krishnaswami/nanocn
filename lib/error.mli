(** Structured typechecking errors.

    The error payload — a [t] — is the structured reason for a
    failure: a sort mismatch (with the [SortDiff.t] diff embedded), an
    unbound variable, a non-exhaustive pattern (with a [PatWitness.t]),
    and so on.  Source position is decoupled from the payload:
    term-attached errors get their position from the host
    [typed_info]'s [loc] field; standalone errors (lexer failures,
    missing-main) are attached to a position via [locate]/[locate_opt]
    only at the boundary where they leave the tree. *)

(** {1 Error payloads} *)

(** A structured error payload.  Abstract — clients build payloads via
    the smart constructors below; the printer is the only place that
    pattern-matches on the underlying variant. *)
type t

(** Why an [RCtx.merge] or [merge_n] failed. *)
type branch_merge_failure =
  | Mf_length_mismatch of { lhs : int; rhs : int }
  | Mf_entry_kind_mismatch
  | Mf_usage_incompatible of Var.t
  | Mf_empty_list

(** A coarse syntactic shape for a leading pattern.  Used by
    [incompatible_patterns] to describe why a column couldn'located be
    dispatched syntactically.  Variable patterns are universally
    compatible and never appear in a conflict report; they're
    represented for completeness but the typechecker only constructs
    [PS_Tuple] / [PS_Ctor] entries. *)
type pattern_shape_descriptor =
  | PS_Tuple of int
  | PS_Ctor of Label.t
  | PS_Var

(** {1 Kind builders} *)

val parse_error : msg:string -> t
(** A parse-time failure (Menhir-derived message for a syntactic error,
    or a [Failure] raised by the lexer). *)

val duplicate_pat_var : name:string -> t
(** A pattern binds the same name more than once. *)

val sort_mismatch :
  expected:Sort.sort -> actual:Sort.sort -> t
(** Convenience builder; computes the [SortDiff] internally. *)

val annotation_disagrees :
  inner:Sort.sort -> annot:Sort.sort -> t

val unbound_var : Var.t -> t
val unbound_name : string -> t
(** A string-named reference that failed to resolve — used by the scope
    resolver, which sees variables before they gain their [Var.t]
    identity. *)

val unknown_var_type : var:Var.t -> t
(** [unknown_var_type ~var] reports that [var] is in scope but its sort
    cannot be determined because its binding's right-hand side had a
    typing error.  Used by [Context.lookup] when the binding is
    [Context.Unknown _]. *)

val unbound_ctor : Label.t -> t
val unbound_sort : Dsort.t -> t
val unbound_tvar : Tvar.t -> t

val unknown_function : name:string -> t
(** Surface/core [f(e)] or refined [f @@ spine] calls a non-existent
    function. *)

val log_var_not_found : name:Var.t -> t
(** A refined [lpf]'s logical variable is not bound in the current
    refined context. *)

val var_effect_mismatch :
  var:Var.t -> declared:Effect.t -> required:Effect.t -> t

val prim_effect_mismatch :
  prim:Prim.t -> declared:Effect.t -> required:Effect.t -> t

val fun_effect_mismatch :
  name:string -> declared:Effect.t -> required:Effect.t -> t

val scrutinee_not_data : got:Sort.sort -> t

val not_spec_type : construct:string -> got:Sort.sort -> t
(** Raised when a construct that requires a spec sort (no [Pred])
    receives one that contains a [Pred]. *)

val spec_context_required : construct:string -> t
(** An operator or form ([return], [fail], [take], …) used outside a
    [[spec]] context. *)

val cannot_synthesize : construct:string -> t
(** Synthesis failed; the user needs to add an annotation.  [construct]
    is typically ["sort"] or ["proof sort"]. *)

val eq_not_equality_type : got:Sort.sort -> t

val construct_sort_mismatch :
  construct:string -> expected_shape:string -> got:Sort.sort -> t

val tuple_arity_mismatch :
  construct:string -> expected:int -> actual:int -> t

val subst_arity_mismatch : expected:int -> actual:int -> t
val resource_not_found : name:Var.t -> t
val resource_already_used : name:Var.t -> t
val branch_merge_failure : reason:branch_merge_failure -> t
val dep_res_not_pred : got:Sort.sort -> t
val ctor_not_in_decl : label:Label.t -> decl:Dsort.t -> t
val missing_ctor : label:Label.t -> decl:Dsort.t -> t
val redundant_ctor : label:Label.t -> t

val incompatible_patterns :
  shapes:(pattern_shape_descriptor * SourcePos.t) list -> t
(** Reports a pattern-matrix column whose leading patterns can'located be
    dispatched syntactically without type information.  [shapes] is the
    list of conflicting pattern shapes with their source positions, used
    by the renderer to point at the disagreeing patterns inline. *)

val tvar_kind_mismatch :
  tvar:Tvar.t -> got:Kind.t -> expected:Kind.t -> t

val dsort_arity_mismatch :
  dsort:Dsort.t -> expected:int -> actual:int -> t
(** An applied sort/type name has the wrong number of type arguments. *)

val pred_misuse : context:string -> t
val unguarded_recursion : dsort:Dsort.t -> t

val empty_decl : name:string -> is_type:bool -> t

val duplicate_ctor_in_decl :
  label:Label.t -> decl_name:string -> is_type:bool -> t

val non_exhaustive : witness:PatWitness.t -> t

val wrong_pred_shape :
  construct:string -> expected_shape:string -> got:string -> t

val unfold_not_spec : name:string -> t
val unfold_not_fundef : name:string -> t

val resource_leak : name:Var.t option -> t
val let_pattern_resource_leak : leftovers:string list -> t

val rpat_length_mismatch : pat_len:int -> pf_len:int -> t
val rpat_kind_mismatch : pat_kind:string -> pf_kind:string -> t

val iter_requires_impure : actual:Effect.t -> t

val spine_tag_mismatch :
  expected_tag:string -> expected_entry:string -> actual_tag:string -> t

val pf_structure_mismatch :
  synthesized_entry:string -> expected_entry:string -> t

val pf_effect_mismatch :
  sort:Sort.sort -> synthesized_eff:Effect.t -> expected_eff:Effect.t -> t

val iter_pattern_shape : got:string -> t

val rcase_label_not_in_branches :
  label:Label.t -> case_labels:Label.t list -> t

(** {1 Positioned errors} *)

(** A [t] paired with a source position.  Built only at boundaries
    where an error leaves the typed tree (top-level results,
    LSP/CLI diagnostics, stray lexer/parser failures). *)
type located

val locate : loc:SourcePos.t -> t -> located
(** [locate ~loc k] pairs a t with a source position. *)

val locate_opt : loc:SourcePos.t option -> t -> located
(** [locate_opt ~loc k] pairs a t with an optional position.  Used at
    stray-error sites — lexer [Failure] handlers, resilient-parse
    fallbacks, missing-main — where no position is available. *)

(** {1 Accessors and printers} *)

val loc : located -> SourcePos.t option
(** [loc e] returns the error's source position, if known. *)

val payload : located -> t
(** [payload e] returns the error's structured payload. *)

val header : t -> string
(** [header e] is a short, human-readable category for [e] —
    e.g., ["Type error: unbound variable"], ["Parse error"].  Used by
    [print] for the first line of error rendering and by tests for
    category identity (since [t] is abstract). *)

val to_string : located -> string
(** [to_string e] renders [e] through [print] with an empty source
    registry (no excerpt) and captures the result as a string.
    Primarily for tests and legacy callers that expect plain strings. *)

val print : SourceExcerpt.registry -> Format.formatter -> located -> unit
(** [print reg fmt e] prints an error with a high-level description,
    a source excerpt (from the registry) around the erroneous range,
    and any structured details the variant carries. Uses the
    [Format] semantic tags configured by [ErrorRender] so the output
    is coloured on a terminal and plain when piped. *)

module Test : sig
  val test : QCheck.Test.t list
end
