(** One-shot file-based solver invocation.

    [run_z3 ~exe ~smt_path] spawns [exe <smt_path>], captures stdout,
    parses the output via [SmtParse.parse_sexps], and classifies each
    top-level s-expression as [Sat], [Unsat], [Unknown], or [Error].
    The resulting [answer list] has one entry per [(check-sat)]
    command in the input, in order.

    Errors (nonzero exit, parse failure) are returned as [Error _];
    no exception escapes the library. *)

type answer = Sat | Unsat | Unknown | Error of string

val run_z3 :
  exe:string -> smt_path:string -> (answer list, string) result

val print_answer : Format.formatter -> answer -> unit
