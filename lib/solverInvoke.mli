(** One-shot file-based solver invocation.

    [run_z3 ~exe ~smt_path] spawns [exe <smt_path>], captures stdout,
    parses the output via [SolverOutput.classify_output], and returns
    the answer list.  One entry per [(check-sat)] command, in order.

    Errors (nonzero exit, parse failure) are returned as [Error _];
    no exception escapes the library. *)

type answer = SolverOutput.answer = Sat | Unsat | Unknown | Error of string

val run_z3 :
  exe:string -> smt_path:string -> (answer list, string) result

val print_answer : Format.formatter -> answer -> unit
