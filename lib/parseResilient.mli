(** Resilient parser: tokenizes the whole file, splits at top-level
    keywords (fun/rfun/sort/type/main), and parses each chunk
    independently with the existing Menhir entry points.

    Reports every parse error rather than stopping at the first one.
    Used by both the CLI (multi-error reporting) and the LSP server
    (per-decl diagnostics).

    The splitter uses only keyword occurrence as sync points — no
    delimiter-depth counting — so a malformed declaration with
    unbalanced parentheses cannot break subsequent declarations. *)

(** {1 Per-chunk results} *)

type 'a chunk_result =
  | Parsed of 'a
  | Failed of Error.t

(** {1 Surface programs (.cn)} *)

type parsed_file = {
  decls : (SurfExpr.parsed_se, SourcePos.t, string) Prog.decl chunk_result list;
  main  : ((SurfExpr.parsed_se, SourcePos.t, string) Prog.t, Error.t) result option;
    (** [None] when the file has no [main] keyword.
        [Some (Ok prog)] when the main section parsed; [prog.decls]
        is always empty since declarations are split out.
        [Some (Error e)] on parse failure. *)
  errors : Error.t list;
    (** Union of all chunk failures plus any missing-main error. *)
}

val parse_prog_resilient : string -> file:string -> parsed_file

(** {1 Refined programs (.rcn)} *)

type parsed_rfile = {
  rdecls : (SurfExpr.parsed_se, < loc : SourcePos.t >, string) RProg.decl chunk_result list;
  rmain  : (RProg.raw_parsed, Error.t) result option;
  errors : Error.t list;
}

val parse_rprog_resilient : string -> file:string -> parsed_rfile

module Test : sig
  val test : QCheck.Test.t list
end
