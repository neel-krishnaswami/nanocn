(** Generic utilities used across nanoCN.

    Lives outside any specific data structure; depends only on
    [SourcePos] (a low-level position primitive). *)

val result_list : ('a, 'e) result list -> ('a list, 'e) result
(** [result_list xs] sequences a list of results.  Returns [Ok ys] when
    every element is [Ok y] (preserving order), or the first [Error e]
    encountered while traversing [xs] left-to-right. *)

(** {1 Compiler-bug invariant failures}

    [Invariant_failure] is raised when an internal consistency check
    fires — i.e. a condition the typechecker's own logic should have
    ruled out, no matter how ill-typed the user program.  Every such
    failure is a compiler bug and must be treated specially: the
    top-level driver (CLI or LSP) catches the exception, reports it
    with a "compiler bug, please file a report" framing, and exits or
    returns a synthesized [K_internal_invariant] diagnostic.

    Distinct from regular type errors: those flow through the typed
    tree's [info#answer] field and are reported as ordinary
    diagnostics.  An [Invariant_failure] always means the typechecker
    itself is broken. *)

type invariant_failure_info = {
  rule : string;
  invariant : string;
  loc : SourcePos.t;
}

exception Invariant_failure of invariant_failure_info

val raise_invariant :
  loc:SourcePos.t -> rule:string -> string -> 'a
(** [raise_invariant ~loc ~rule msg] raises [Invariant_failure].
    [rule] is the function or rule-arm that fired the check;
    [msg] describes the specific check that failed. *)

module Test : sig
  val test : QCheck.Test.t list
end
