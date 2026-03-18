(** Parsing interface combining sedlex and menhir. *)

val parse_expr : string -> file:string -> (SurfExpr.se, string) result
(** [parse_expr s ~file] parses a string as an expression. *)

val parse_sort : string -> file:string -> (Sort.sort, string) result
(** [parse_sort s ~file] parses a string as a sort. *)

val parse_prog : string -> file:string -> ((SurfExpr.se, SourcePos.t) Prog.t, string) result
(** [parse_prog s ~file] parses a string as a complete program. *)

val parse_decl : string -> file:string -> ((SurfExpr.se, SourcePos.t) Prog.decl, string) result
(** [parse_decl s ~file] parses a single declaration. *)

val parse_let : string -> file:string -> (Var.t * SurfExpr.se, string) result
(** [parse_let s ~file] parses a let-binding (without body). *)
