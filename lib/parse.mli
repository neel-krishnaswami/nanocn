(** Parsing interface combining sedlex and menhir. *)

val parse_expr : string -> file:string -> (Expr.expr, string) result
(** [parse_expr s ~file] parses a string as an expression. *)

val parse_typ : string -> file:string -> (Typ.ty, string) result
(** [parse_typ s ~file] parses a string as a type. *)
