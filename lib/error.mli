(** Structured typechecking errors.

    A rule-structured ADT covering every failure reported by the
    lexer/parser, surface elaborator, core typechecker, and refined
    checker. Each [kind] carries enough structure for the printer to
    render a targeted message (sort/kind mismatches embed the
    structural diff produced by [SortDiff.diff]; pattern-coverage
    witnesses embed a [PatWitness.t] reconstructed by the coverage
    checker; etc.). *)

type t

(** {1 Structured errors} *)

(** Why an [RCtx.merge] or [merge_n] failed. *)
type branch_merge_failure =
  | Mf_length_mismatch of { lhs : int; rhs : int }
  | Mf_entry_kind_mismatch
  | Mf_usage_incompatible of Var.t
  | Mf_empty_list

type kind =
  (* Lexer / parser *)
  | K_parse_error of { msg : string }
    (** A parse-time failure (Menhir-derived message for a
        syntactic error, or a [Failure] raised by the lexer). *)

  (* Scope resolution *)
  | K_duplicate_pat_var of { name : string }
    (** A pattern binds the same name more than once. *)

  (* Sort mismatches (elaborator / core checker) *)
  | K_sort_mismatch of
      { expected : Sort.sort
      ; actual : Sort.sort
      ; diff : SortDiff.shape_compare }
  | K_annotation_disagrees of
      { inner_sort : Sort.sort
      ; annot : Sort.sort
      ; diff : SortDiff.shape_compare }

  (* Unbound names *)
  | K_unbound_var of Var.t
  | K_unbound_name of string
    (** A string-named reference that failed to resolve — used by
        the scope resolver, which sees variables before they gain
        their [Var.t] identity. *)
  | K_unbound_ctor of Label.t
  | K_unbound_sort of Dsort.t
  | K_unbound_tvar of Tvar.t
  | K_unknown_function of { name : string }
    (** Surface/core [f(e)] or refined [f @@ spine] calls a
        non-existent function. *)
  | K_log_var_not_found of { name : Var.t }
    (** A refined [lpf]'s logical variable is not bound in the
        current refined context. *)

  (* Effect mismatches *)
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
  | K_iter_requires_impure of { actual : Effect.t }
    (** An [iter] was encountered under an ambient effect that
        doesn't subsume [impure]. *)

  (* Expression-level sort mismatches *)
  | K_eq_not_equality_type of { got : Sort.sort }
    (** [==] used at a non-equality sort. *)
  | K_construct_sort_mismatch of
      { construct : string
      ; expected_shape : string
      ; got : Sort.sort }
    (** A construct needed a specific sort shape but got something
        else. [construct] names the construct ("return",
        "let-tuple scrutinee", "make-take bound expression", …),
        [expected_shape] gives a short phrase describing the
        required shape ("Pred _", "Record _",
        "datasort/datatype application"), and [got] is the actual
        sort. *)
  | K_tuple_arity_mismatch of
      { construct : string
      ; expected : int
      ; actual : int }
    (** A tuple / let-tuple expression has a different number of
        components from the target record sort. *)
  | K_scrutinee_not_data of { got : Sort.sort }
  | K_not_spec_type of { construct : string; got : Sort.sort }
  | K_spec_context_required of { construct : string }
  | K_cannot_synthesize of { construct : string }
    (** Synthesis failed; the user needs to add an annotation.
        [construct] is ["sort"] for surface/core terms or
        ["proof sort"] for refined terms. *)

  (* Submodule-originated failures (structured). Each case
     mirrors a specific failure mode in a helper module
     ([Subst], [RCtx], [ProofSort], [CtorLookup], [rpat_match]).
     The helper returns [(_, kind) result] and the caller lifts
     it with [Error.at ~loc]. *)
  | K_subst_arity_mismatch of { expected : int; actual : int }
    (** [Subst.of_lists] received type-parameter and sort-argument
        lists of different lengths. *)
  | K_resource_not_found of { name : Var.t }
    (** [RCtx.use_resource] tried to consume a resource with no
        binding in the refined context. *)
  | K_resource_already_used of { name : Var.t }
    (** [RCtx.use_resource] tried to consume a resource whose
        usage is already non-[Avail]. *)
  | K_branch_merge_failure of { reason : branch_merge_failure }
    (** [RCtx.merge] / [merge_n] / [lattice_merge] couldn't join
        two refined contexts at a branch's output. *)
  | K_dep_res_not_pred of { got : Sort.sort }
    (** [ProofSort.bind] or [rpat_match] encountered a [DepRes]
        whose predicate doesn't have [Pred _] sort. *)
  | K_ctor_sig_inconsistent of { label : Label.t; where : string }
    (** [CtorLookup] found [label] via [Sig]'s constructor index but
        the decl's own constructor table disagreed — signature
        invariant broken. [where] names the decl flavor
        ("datasort", "datatype"). *)

  (* Kind well-formedness (kind_wf / type_guarded) *)
  | K_tvar_kind_mismatch of
      { tvar : Tvar.t; got : Kind.t; expected : Kind.t }
  | K_dsort_arity_mismatch of
      { dsort : Dsort.t; expected : int; actual : int }
    (** An applied sort/type name has the wrong number of type
        arguments. Works for both [Sig.lookup_sort] and
        [Sig.lookup_type] lookups. *)
  | K_pred_misuse of { context : string }
    (** A [Pred _] sort appeared where it isn't allowed.
        [context] is ["kind sort"] (seen at kind [Type]) or
        ["type declaration"] (seen inside a datatype declaration's
        constructor sorts). *)
  | K_unguarded_recursion of { dsort : Dsort.t }
    (** A recursive reference inside a datatype declaration does
        not pass through [Ptr]. *)

  (* Declaration validators *)
  | K_empty_decl of { name : string; is_type : bool }
    (** A data[sort|type] declaration has no constructors. *)
  | K_duplicate_ctor_in_decl of
      { label : Label.t; decl_name : string; is_type : bool }
    (** A data[sort|type] declaration lists the same constructor
        label twice. *)

  (* Pattern coverage *)
  | K_non_exhaustive of { witness : PatWitness.t }

  (* Refined checker — shape checks *)
  | K_wrong_pred_shape of
      { construct : string
      ; expected_shape : string
      ; got : string }
    (** A refined construct ([open-ret], [make-ret], [open-take],
        [make-take]) expects its target predicate to have a
        specific shape. [got] is the pretty-printed form of the
        actual expression. *)
  | K_unfold_not_spec of { name : string }
    (** An [unfold f] references a function whose effect is not
        [spec]. *)
  | K_unfold_not_fundef of { name : string }
    (** An [unfold f] references a name that is not a defined
        (elaborated) function. *)
  | K_resource_leak of { name : Var.t option }
    (** A resource binding fell out of scope without being consumed
        (must be a linear consumption). [name] is the resource
        variable, when the checker can identify it. *)
  | K_let_pattern_resource_leak of { leftovers : string list }
    (** A refined [let] pattern bound resources that weren't all
        consumed by the body. [leftovers] lists the unconsumed
        entries in pretty-printed form (since they're structured
        [RCtx.entry]s with variable/pred/value/usage). *)

  | K_rpat_length_mismatch of { pat_len : int; pf_len : int }
    (** Refined pattern has a different number of elements than the
        proof sort it's being matched against. *)
  | K_rpat_kind_mismatch of { pat_kind : string; pf_kind : string }
    (** Refined pattern element kind (core/log/res/depres) does not
        match the corresponding proof sort entry kind. *)

  (* Last-resort escape hatches *)
  | K_internal_invariant of { rule : string; invariant : string }
    (** A check in the typechecker's internal logic failed —
        typically an "impossible" shape at a match arm, a
        previously-validated condition that no longer holds, or an
        early synthesise/check restriction. Not user-facing in
        normal use; reported with the [rule] (function or rule-arm
        that raised the check) and a short [invariant] description
        so that debugging can start from the exact failure point. *)
(** The kind of structured failure. *)

val structured : loc:SourcePos.t -> kind -> t
(** [structured ~loc kind] builds a structured error at [loc]. *)

val parse_error : loc:SourcePos.t option -> msg:string -> t
(** [parse_error ~loc ~msg] builds a [K_parse_error]. [loc] is
    optional so lexer-side [Failure] reports (which have no
    position) can still reach the same error pipeline. *)

val duplicate_pat_var : loc:SourcePos.t -> name:string -> t

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

val unknown_function : loc:SourcePos.t -> name:string -> t
val log_var_not_found : loc:SourcePos.t -> name:Var.t -> t

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

val cannot_synthesize : loc:SourcePos.t -> construct:string -> t
(** [cannot_synthesize ~loc ~construct] reports that synthesis failed
    and the user needs to add an annotation. [construct] is typically
    ["sort"] or ["proof sort"]. *)

val eq_not_equality_type : loc:SourcePos.t -> got:Sort.sort -> t

val construct_sort_mismatch :
  loc:SourcePos.t -> construct:string ->
  expected_shape:string -> got:Sort.sort -> t

val tuple_arity_mismatch :
  loc:SourcePos.t -> construct:string ->
  expected:int -> actual:int -> t

val subst_arity_mismatch :
  loc:SourcePos.t -> expected:int -> actual:int -> t
val resource_not_found : loc:SourcePos.t -> name:Var.t -> t
val resource_already_used : loc:SourcePos.t -> name:Var.t -> t
val branch_merge_failure :
  loc:SourcePos.t -> reason:branch_merge_failure -> t
val dep_res_not_pred : loc:SourcePos.t -> got:Sort.sort -> t
val ctor_sig_inconsistent :
  loc:SourcePos.t -> label:Label.t -> where:string -> t

val at : loc:SourcePos.t -> ('a, kind) result -> ('a, t) result
(** [at ~loc r] attaches [loc] to any [kind]-typed error in [r],
    producing an [Error.t]-typed result. Used at boundaries between
    submodule helpers ([Subst], [CtorLookup], [RCtx], [ProofSort])
    which report failures as a bare [kind], and the typechecker,
    which needs full [Error.t]s. *)

val tvar_kind_mismatch :
  loc:SourcePos.t -> tvar:Tvar.t ->
  got:Kind.t -> expected:Kind.t -> t

val dsort_arity_mismatch :
  loc:SourcePos.t -> dsort:Dsort.t ->
  expected:int -> actual:int -> t

val pred_misuse : loc:SourcePos.t -> context:string -> t

val unguarded_recursion : loc:SourcePos.t -> dsort:Dsort.t -> t

val empty_decl : loc:SourcePos.t -> name:string -> is_type:bool -> t

val duplicate_ctor_in_decl :
  loc:SourcePos.t -> label:Label.t ->
  decl_name:string -> is_type:bool -> t

val non_exhaustive :
  loc:SourcePos.t -> witness:PatWitness.t -> t
(** [non_exhaustive ~loc ~witness] is raised when a pattern match
    misses a case. [witness] is an example of a value shape that
    would not be matched. *)

val wrong_pred_shape :
  loc:SourcePos.t -> construct:string ->
  expected_shape:string -> got:string -> t
(** [wrong_pred_shape ~loc ~construct ~expected_shape ~got] reports
    that a refined construct's target predicate had the wrong shape.
    [got] is the pretty-printed form of the actual expression. *)

val unfold_not_spec : loc:SourcePos.t -> name:string -> t
val unfold_not_fundef : loc:SourcePos.t -> name:string -> t

val resource_leak :
  loc:SourcePos.t -> name:Var.t option -> t
(** [resource_leak ~loc ~name] is raised when a resource binding
    (from [let res] / rfun domain / iter binder) is discarded
    without a linear consumer. *)

val let_pattern_resource_leak :
  loc:SourcePos.t -> leftovers:string list -> t

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
