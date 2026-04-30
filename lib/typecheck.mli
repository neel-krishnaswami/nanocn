(** Unified bidirectional typechecker.

    Implements synthesis and checking judgements for core expressions,
    returning a fully annotated expression tree where each node carries
    its source location, typing context, sort, and effect. *)

type typed_info = CoreExpr.typed_info
type typed_ce = typed_info CoreExpr.t

val synth : _ Sig.t -> Context.t -> Effect.t -> CoreExpr.ce -> typed_ce
(** Synthesize a sort and effect for the given expression.
    [eff] is the ambient effect.

    Always returns a typed core expression — the multi-error
    typechecker continues past errors and records them on the
    offending nodes' [info#answer] field.  Use [collect_errors] to
    extract the error list from the resulting tree. *)

val check : _ Sig.t -> Context.t -> CoreExpr.ce ->
  (Sort.sort, Error.kind) result -> Effect.t -> typed_ce
(** Check an expression against an expected sort at ambient effect
    [eff0].  The expected sort is itself a result so the caller can
    pass [Error K_cannot_synthesize] (or any other [Error.kind]) when
    no expected sort is available; the term still elaborates and any
    failures land on [info#answer] of the offending nodes. *)

val collect_errors : typed_ce -> Error.t list
(** [collect_errors ce] returns every [Error e] recorded on
    [info#answer] anywhere in [ce]'s tree, in source-position order
    (left-to-right pre-order traversal).  An empty list means the
    expression typechecked without errors. *)

val prim_signature : Prim.t -> Sort.sort * Sort.sort * Effect.t
(** [prim_signature p] returns [(arg_sort, ret_sort, effect)] for primitive [p]. *)

val check_prog : Var.supply -> (SurfExpr.se, _, Var.t) Prog.t -> (typed_ce Sig.t * typed_ce Prog.core_prog, Error.t) result
(** Typecheck a complete program (elaborate + typecheck).
    Returns both the final core signature and the typed core program. *)

val check_decl : Var.supply -> _ Sig.t -> (SurfExpr.se, _, Var.t) Prog.decl -> (Var.supply * typed_ce Prog.core_decl, Error.t) result
(** Typecheck a single declaration against a signature.  Fail-fast
    contract — surfaces the first collected error. *)

val check_decl_multi : Var.supply -> _ Sig.t ->
  (SurfExpr.se, _, Var.t) Prog.decl ->
  (Var.supply * typed_ce Prog.core_decl * Error.t list, Error.t) result
(** Multi-error variant of [check_decl]: returns the typed
    declaration alongside every error recorded on its body's tree
    (via [collect_errors]).  The outer [Error _] is reserved for
    structural failures (e.g. sig validation, internal invariants
    that fire through [ElabM.fail] and short-circuit elaboration
    before any tree is produced). *)

val initial_sig : typed_ce Sig.t
(** The initial signature with built-in types (step). *)

val check_spec_decl : Var.supply -> typed_ce Sig.t -> (SurfExpr.se, _, Var.t) Prog.decl -> (Var.supply * typed_ce Sig.t, Error.t) result
(** [check_spec_decl supply sig d] validates a declaration,
    returning the updated supply and signature. *)

val extend_sig_with_header :
  typed_ce Sig.t ->
  (SurfExpr.se, _, Var.t) Prog.decl ->
  (typed_ce Sig.t, Error.t) result
(** [extend_sig_with_header sig d] validates the decl's header and
    extends [sig] with the declared signature entry.  For [FunDecl],
    this always succeeds (adds a [FunSig] with the declared sorts).
    For [SortDecl]/[TypeDecl], this runs the full validation (kind
    well-formedness, ctor uniqueness, guardedness) — if it fails
    the sig is unchanged.

    Used by the resilient compile driver: extend the sig with the
    header first so downstream decls can resolve call sites, then
    attempt body elaboration separately. *)
