(** Scope resolution: transforms string-named ASTs to Var.t-named ASTs,
    assigning unique IDs from the ElabM supply.

    At binding sites (pattern variables, function params, proof sort binders):
    creates a fresh Var.t via ElabM.mk_var, extends env for the body.
    At use sites: searches env by string name, fails if not found. *)

type env = (string * Var.t) list

val resolve_pat :
  env -> Pat.parsed_pat -> (Pat.pat * env) ElabM.t
(** Resolve a pattern: checks linearity, creates fresh Var.t for each
    binding, returns the resolved pattern and the extended environment. *)

val resolve_expr :
  env -> SurfExpr.parsed_se -> SurfExpr.se ElabM.t
(** Resolve all variable references in an expression. *)

val resolve_decl :
  env -> (SurfExpr.parsed_se, SourcePos.t, string) Prog.decl ->
  ((SurfExpr.se, SourcePos.t, Var.t) Prog.decl * env) ElabM.t
(** Resolve a single declaration. *)

val resolve_prog :
  env -> (SurfExpr.parsed_se, SourcePos.t, string) Prog.t ->
  (SurfExpr.se, SourcePos.t, Var.t) Prog.t ElabM.t
(** Resolve a complete program. *)

val resolve_rprog :
  env -> RProg.raw_parsed -> RProg.parsed ElabM.t
(** Resolve a refined program. *)
