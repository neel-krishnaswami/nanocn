(** Build the SMT-LIB prelude for a nanoCN program: the fixed
    declarations (logic, [Ptr], tuples, [Pred]), the user's
    datatype/sort declarations from the refined signature, the
    per-sort / per-pair monad operations and axioms, and finally the
    pure-function definitions and spec-function declarations.

    See the "Prelude assembly" section of
    [doc/smt-encoding-plan.md] for the full ordering. *)

type config = {
  position_trace : bool;
  (** When [true], the prelude emits the [Pos] and [PosList]
      datatype declarations used by [SmtConstraint]'s position
      tracing. *)
}

val default_config : config

val build :
  ?config:config -> RSig.t -> SmtMonadSorts.t ->
  (SmtSexp.sexp list, string) result
