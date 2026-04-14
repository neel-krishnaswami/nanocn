(** Parsing entry points combining [SmtLexer] and [SmtParser].

    Errors (lexical and syntactic) are returned as
    [Result.Error "parse error at <pos>"]; no exceptions escape. *)

val parse_sexp  : string -> file:string -> (SmtSexp.sexp,      string) result
(** Parse a single top-level s-expression followed by EOF. *)

val parse_sexps : string -> file:string -> (SmtSexp.sexp list, string) result
(** Parse a sequence of top-level s-expressions up to EOF. This is
    the common solver-response entry point: a response like
    ["sat\n((x 5))\n"] contains two top-level s-expressions. *)
