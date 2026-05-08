(** Parsing interface combining sedlex and menhir.

    Raw parse functions are pure and produce string-named trees.
    Resolved parse functions compose parsing with scope resolution
    and return ['a ElabM.t]. *)

(** {1 Raw parsing (pure, string-named trees)} *)

val parse_expr_raw : string -> file:string -> (SurfExpr.parsed_se, Error.t) result
val parse_prog_raw : string -> file:string -> ((SurfExpr.parsed_se, SourcePos.t, string) Prog.t, Error.t) result
val parse_decl_raw : string -> file:string -> ((SurfExpr.parsed_se, SourcePos.t, string) Prog.decl, Error.t) result
val parse_rprog_raw : string -> file:string -> (RProg.raw_parsed, Error.t) result
val parse_sort : string -> file:string -> (Sort.sort, Error.t) result

(** {1 Parsed + resolved (monadic, Var.t-named trees)}

    Each function returns [('a, Error.t) result ElabM.t]: a parse
    failure from [parse_raw] is surfaced as a [Result] value inside
    the elaboration monad rather than escaping through the monad's
    own failure channel.  The [ElabM] effect remains because the
    resolve step consumes fresh variables from [ElabM]'s state. *)

val parse_expr  : ?env:Resolve.env -> string -> file:string ->
  (SurfExpr.se, Error.t) result ElabM.t
val parse_prog  : ?env:Resolve.env -> string -> file:string ->
  ((SurfExpr.se, SourcePos.t, Var.t) Prog.t, Error.t) result ElabM.t
val parse_decl  : ?env:Resolve.env -> string -> file:string ->
  ((SurfExpr.se, SourcePos.t, Var.t) Prog.decl, Error.t) result ElabM.t
val parse_let   : ?env:Resolve.env -> string -> file:string ->
  (Var.t * SurfExpr.se, Error.t) result ElabM.t
val parse_rprog : ?env:Resolve.env -> string -> file:string ->
  (RProg.parsed, Error.t) result ElabM.t
