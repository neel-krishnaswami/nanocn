(** Solver answer classification.

    Shared between the synchronous [SolverInvoke] and the
    asynchronous [SmtAsync] paths. *)

type answer = Sat | Unsat | Unknown | Error of string

val print : Format.formatter -> answer -> unit

val classify_sexp : SmtSexp.sexp -> answer
(** Classify a single top-level s-expression from solver output. *)

val classify_output : string -> file:string -> (answer list, string) result
(** [classify_output text ~file] parses the full solver stdout as
    a sequence of s-expressions and classifies each one. *)

module Test : sig
  val test : QCheck.Test.t list
end
