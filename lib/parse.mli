(** Parsing interface combining sedlex and menhir.

    Raw parse functions are pure and produce string-named trees.
    Resolved parse functions compose parsing with scope resolution
    and return ['a ElabM.t]. *)

(** {1 Raw parsing (pure, string-named trees)} *)

val parse_expr_raw : string -> file:string -> (SurfExpr.parsed_se, string) result
val parse_prog_raw : string -> file:string -> ((SurfExpr.parsed_se, SourcePos.t, string) Prog.t, string) result
val parse_decl_raw : string -> file:string -> ((SurfExpr.parsed_se, SourcePos.t, string) Prog.decl, string) result
val parse_rprog_raw : string -> file:string -> (RProg.raw_parsed, string) result
val parse_sort : string -> file:string -> (Sort.sort, string) result

(** {1 Parsed + resolved (monadic, Var.t-named trees)} *)

val parse_expr : ?env:Resolve.env -> string -> file:string -> SurfExpr.se ElabM.t
val parse_prog : ?env:Resolve.env -> string -> file:string -> (SurfExpr.se, SourcePos.t, Var.t) Prog.t ElabM.t
val parse_decl : ?env:Resolve.env -> string -> file:string -> (SurfExpr.se, SourcePos.t, Var.t) Prog.decl ElabM.t
val parse_let : ?env:Resolve.env -> string -> file:string -> (Var.t * SurfExpr.se) ElabM.t
val parse_rprog : ?env:Resolve.env -> string -> file:string -> RProg.parsed ElabM.t
