(** State+error monad for elaboration.

    Threads a fresh variable supply and an accumulating list of
    [Warning.t]s, and may fail with a structured [Error.t]. *)

type 'a t

val return : 'a -> 'a t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val fail : Error.t -> 'a t
(** [fail e] aborts the computation with the given structured error. *)

val lift : ('a, Error.t) result -> 'a t
(** [lift r] promotes a plain result into the monad. *)

val lift_at : SourcePos.t -> ('a, Error.kind) result -> 'a t
(** [lift_at pos r] forwards a submodule-structured
    [(_, Error.kind) result] into the monad, attaching [pos] via
    [Error.at]. Used at the boundary between elaboration/typechecking
    and helper modules ([Sig], [RSig], [CtorLookup], [Subst], [RCtx],
    [ProofSort], etc.). *)

val fresh : SourcePos.t -> Var.t t
(** [fresh pos] generates a fresh variable with binding site [pos]. *)

val mk_var : string -> SourcePos.t -> Var.t t
(** [mk_var name pos] creates a user variable with a unique id from
    the supply. *)

val record_warning : Warning.t -> unit t
(** [record_warning w] appends [w] to the monad's warning list.
    Use [run_full] to retrieve the accumulated warnings; [run]
    discards them. *)

val sequence : 'a t list -> 'a list t
(** [sequence ms] runs each computation in order, collecting results. *)

val run : Var.supply -> 'a t -> ('a * Var.supply, Error.t) result
(** [run supply m] executes [m] starting from [supply], returning the
    result and the final supply.  Discards any accumulated warnings;
    use [run_full] when warnings should be surfaced. *)

val run_full :
  Var.supply -> 'a t ->
  ('a * Var.supply * Warning.t list, Error.t) result
(** [run_full supply m] is like [run] but also returns the warnings
    accumulated by [record_warning] during the run, in source order
    (first recorded → first in the list). *)

module Test : sig
  val test : QCheck.Test.t list
end
