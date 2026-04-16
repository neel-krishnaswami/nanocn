(** State+error monad for elaboration.

    Threads a fresh variable supply and may fail with a structured
    [TypeError.t]. During the migration to structured errors the helper
    [legacy_fail] wraps a pre-formatted message in [TypeError.legacy];
    per-variant migrations replace [legacy_fail] call sites with direct
    [TypeError] constructors. *)

type 'a t

val return : 'a -> 'a t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val fail : TypeError.t -> 'a t
(** [fail e] aborts the computation with the given structured error. *)

val legacy_fail : SourcePos.t option -> string -> 'a t
(** [legacy_fail pos msg] aborts with a [TypeError.legacy pos msg].
    Transitional helper retired at the end of the error-refactor. *)

val lift : ('a, TypeError.t) result -> 'a t
(** [lift r] promotes a plain result into the monad. *)

val from_supply : (Var.supply -> ('a * Var.supply, TypeError.t) result) -> 'a t
(** [from_supply f] creates a monadic computation from a
    supply-threading function. *)

val fresh : SourcePos.t -> Var.t t
(** [fresh pos] generates a fresh variable with binding site [pos]. *)

val mk_var : string -> SourcePos.t -> Var.t t
(** [mk_var name pos] creates a user variable with a unique id from
    the supply. *)

val sequence : 'a t list -> 'a list t
(** [sequence ms] runs each computation in order, collecting results. *)

val run : Var.supply -> 'a t -> ('a * Var.supply, TypeError.t) result
(** [run supply m] executes [m] starting from [supply], returning the
    result and the final supply. *)

module Test : sig
  val test : QCheck.Test.t list
end
