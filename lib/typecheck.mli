(** Unified bidirectional typechecker.

    Implements synthesis and checking judgements for core expressions,
    returning a fully annotated expression tree where each node carries
    its source location, typing context, sort, and effect. *)

type typed_info = < loc : SourcePos.t; ctx : Context.t; sort : Sort.sort; eff : Effect.t >
type typed_ce = typed_info CoreExpr.t

val synth : _ Sig.t -> Context.t -> Effect.t -> CoreExpr.ce -> (typed_ce, string) result
(** Synthesize a sort and effect for the given expression.
    [eff] is the ambient effect. *)

val check : _ Sig.t -> Context.t -> CoreExpr.ce -> Sort.sort -> Effect.t -> (typed_ce, string) result
(** Check an expression against a given sort and effect. *)

val prim_signature : Prim.t -> Sort.sort * Sort.sort * Effect.t
(** [prim_signature p] returns [(arg_sort, ret_sort, effect)] for primitive [p]. *)

val check_prog : (SurfExpr.se, _) Prog.t -> (typed_ce Sig.t * typed_ce Prog.core_prog, string) result
(** Typecheck a complete program (elaborate + typecheck).
    Returns both the final core signature and the typed core program. *)

val check_decl : _ Sig.t -> (SurfExpr.se, _) Prog.decl -> (typed_ce Prog.core_decl, string) result
(** Typecheck a single declaration against a signature. *)

val initial_sig : typed_ce Sig.t
(** The initial signature with built-in types (step). *)

val check_spec_decl : typed_ce Sig.t -> (SurfExpr.se, _) Prog.decl -> (typed_ce Sig.t, string) result
(** [check_spec_decl sig d] validates a declaration,
    returning the updated signature. *)
