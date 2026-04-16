(** State+error monad for elaboration.

    Threads a fresh variable supply and may fail with a structured
    [Error.t]. *)

type 'a t

val return : 'a -> 'a t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val fail : Error.t -> 'a t
(** [fail e] aborts the computation with the given structured error. *)

val lift : ('a, Error.t) result -> 'a t
(** [lift r] promotes a plain result into the monad. *)

val from_supply : (Var.supply -> ('a * Var.supply, Error.t) result) -> 'a t
(** [from_supply f] creates a monadic computation from a
    supply-threading function. *)

val fresh : SourcePos.t -> Var.t t
(** [fresh pos] generates a fresh variable with binding site [pos]. *)

val mk_var : string -> SourcePos.t -> Var.t t
(** [mk_var name pos] creates a user variable with a unique id from
    the supply. *)

val sequence : 'a t list -> 'a list t
(** [sequence ms] runs each computation in order, collecting results. *)

val run : Var.supply -> 'a t -> ('a * Var.supply, Error.t) result
(** [run supply m] executes [m] starting from [supply], returning the
    result and the final supply. *)

module Test : sig
  val test : QCheck.Test.t list
end
