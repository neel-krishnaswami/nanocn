(** Bidirectional typechecker.

    Implements synthesis and checking judgements, returning a fully
    annotated expression tree where each node carries its source location,
    typing context, type, and effect.
*)

type typed_info = < loc : SourcePos.t; ctx : Context.t; typ : Typ.ty; eff : Effect.t >
type typed_expr = typed_info Expr.t

val synth : Sig.t -> Context.t -> Expr.expr -> (typed_expr, string) result
(** Synthesize a type and effect for the given expression. *)

val check : Sig.t -> Context.t -> Expr.expr -> Typ.ty -> Effect.t -> (typed_expr, string) result
(** Check an expression against a given type and effect. *)

val check_prog : Expr.expr Prog.t -> (typed_expr Prog.t, string) result
(** Typecheck a complete program. *)

val check_decl : Sig.t -> Expr.expr Prog.decl -> (typed_expr Prog.decl, string) result
(** Typecheck a single function declaration against a signature.
    The function's own signature is in scope for recursion. *)
