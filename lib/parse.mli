(** Parsing interface combining sedlex and menhir. *)

val parse_expr : string -> file:string -> (Expr.expr, string) result
(** [parse_expr s ~file] parses a string as an expression. *)

val parse_typ : string -> file:string -> (Typ.ty, string) result
(** [parse_typ s ~file] parses a string as a type. *)

val parse_prog : string -> file:string -> (Expr.expr Prog.t, string) result
(** [parse_prog s ~file] parses a string as a complete program. *)

val parse_decl : string -> file:string -> (Expr.expr Prog.decl, string) result
(** [parse_decl s ~file] parses a single declaration. *)

val parse_let : string -> file:string -> (Var.t * Expr.expr, string) result
(** [parse_let s ~file] parses a let-binding (without body). *)
