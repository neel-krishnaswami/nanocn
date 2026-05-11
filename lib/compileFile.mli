(** Per-decl compile driver for a whole file.

    Consumes [ParseResilient] output, threads [Sig.t] / [RSig.t]
    through per-decl header-check + body-check, and collects all
    diagnostics.  A broken body does not roll back the header — the
    declared signature survives so downstream call sites still
    resolve.

    Used by both the CLI (multi-error reporting) and the LSP server
    (per-file diagnostics). *)

(** {1 Surface programs (.cn)} *)

type file_outcome = {
  final_sig  : Typecheck.typed_ce Sig.t;
  typed_decls : Typecheck.typed_ce Prog.core_decl list;
  diagnostics : Error.t list;
  warnings    : Warning.t list;
}

val compile_file :
  string -> file:string ->
  file_outcome
(** [compile_file source ~file] parses resiliently, resolves names,
    typechecks per-decl with header-first sig extension, and returns
    all results. *)

(** {1 Refined programs (.rcn)} *)

type rfile_outcome = {
  final_rsig  : RSig.t;
  constraints : Constraint.typed_ct;
  diagnostics : Error.t list;
  hover       : HoverIndex.t;
  decls       : RProg.raw_parsed_decl list;
    (** Successfully-parsed top-level declarations, in source order.
        Populated regardless of typechecking outcome so consumers like
        [documentSymbol] can locate decls even in broken files. *)
  main_loc    : SourcePos.t option;
    (** Source location of the [main] block, if it parsed. *)
}

val compile_rfile :
  string -> file:string ->
  rfile_outcome
(** [compile_rfile source ~file] is the refined-program analogue. *)

module Test : sig
  val test : QCheck.Test.t list
end
