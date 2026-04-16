(** Unified bidirectional typechecker.

    Implements synthesis and checking judgements for core expressions,
    returning a fully annotated expression tree where each node carries
    its source location, typing context, sort, and effect. *)

type typed_info = < loc : SourcePos.t; ctx : Context.t; sort : Sort.sort; eff : Effect.t >
type typed_ce = typed_info CoreExpr.t

val synth : _ Sig.t -> Context.t -> Effect.t -> CoreExpr.ce -> (typed_ce, Error.t) result
(** Synthesize a sort and effect for the given expression.
    [eff] is the ambient effect. *)

val check : _ Sig.t -> Context.t -> CoreExpr.ce -> Sort.sort -> Effect.t -> (typed_ce, Error.t) result
(** Check an expression against a given sort at ambient effect [eff0]. *)

val prim_signature : Prim.t -> Sort.sort * Sort.sort * Effect.t
(** [prim_signature p] returns [(arg_sort, ret_sort, effect)] for primitive [p]. *)

val check_prog : Var.supply -> (SurfExpr.se, _, Var.t) Prog.t -> (typed_ce Sig.t * typed_ce Prog.core_prog, Error.t) result
(** Typecheck a complete program (elaborate + typecheck).
    Returns both the final core signature and the typed core program. *)

val check_decl : Var.supply -> _ Sig.t -> (SurfExpr.se, _, Var.t) Prog.decl -> (Var.supply * typed_ce Prog.core_decl, Error.t) result
(** Typecheck a single declaration against a signature. *)

val initial_sig : typed_ce Sig.t
(** The initial signature with built-in types (step). *)

val check_spec_decl : Var.supply -> typed_ce Sig.t -> (SurfExpr.se, _, Var.t) Prog.decl -> (Var.supply * typed_ce Sig.t, Error.t) result
(** [check_spec_decl supply sig d] validates a declaration,
    returning the updated supply and signature. *)
