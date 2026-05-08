(** State monad for elaboration.

    Threads a fresh variable supply and an accumulating list of
    [Warning.t]s.  The monad has no failure mode: callers that need to
    surface errors do so as ordinary values (typically a [Result] in
    the carrier type). *)

type 'a t

val return : 'a -> 'a t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

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

val run : Var.supply -> 'a t -> 'a * Var.supply
(** [run supply m] executes [m] starting from [supply], returning the
    result and the final supply.  Discards any accumulated warnings;
    use [run_full] when warnings should be surfaced. *)

val run_full :
  Var.supply -> 'a t -> 'a * Var.supply * Warning.t list
(** [run_full supply m] is like [run] but also returns the warnings
    accumulated by [record_warning] during the run, in source order
    (first recorded → first in the list). *)

module Test : sig
  val test : QCheck.Test.t list
end
