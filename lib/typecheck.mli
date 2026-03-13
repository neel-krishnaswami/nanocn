(** Unified bidirectional typechecker.

    Implements synthesis and checking judgements for core expressions,
    returning a fully annotated expression tree where each node carries
    its source location, typing context, sort, and effect. *)

type typed_info = < loc : SourcePos.t; ctx : Context.t; sort : Sort.sort; eff : Effect.t >
type typed_ce = typed_info CoreExpr.t

val synth : Sig.t -> Context.t -> Effect.t -> CoreExpr.ce -> (typed_ce, string) result
(** Synthesize a sort and effect for the given expression.
    [eff] is the ambient effect. *)

val check : Sig.t -> Context.t -> CoreExpr.ce -> Sort.sort -> Effect.t -> (typed_ce, string) result
(** Check an expression against a given sort and effect. *)

val prim_signature : Prim.t -> Sort.sort * Sort.sort * Effect.t
(** [prim_signature p] returns [(arg_sort, ret_sort, effect)] for primitive [p]. *)

val check_prog : SurfExpr.se Prog.t -> (typed_ce Prog.core_prog, string) result
(** Typecheck a complete program (elaborate + typecheck). *)

val check_decl : Sig.t -> SurfExpr.se Prog.decl -> (typed_ce Prog.core_decl, string) result
(** Typecheck a single declaration against a signature. *)

val initial_sig : Sig.t
(** The initial signature with built-in types (step). *)

val check_spec_decl : Sig.t -> SurfExpr.se Prog.decl -> (Sig.t, string) result
(** [check_spec_decl sig d] validates a declaration,
    returning the updated signature. *)
