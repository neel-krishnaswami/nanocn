(** State+error monad for elaboration.

    Threads a fresh variable supply and may fail with an error message.
    Used by Coverage, Elaborate, and spec declaration typechecking. *)

type 'a t

val return : 'a -> 'a t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val fail : string -> 'a t

val fresh : SourcePos.t -> Var.t t
(** [fresh pos] generates a fresh variable with binding site [pos]. *)

val run : 'a t -> ('a, string) result
(** [run m] executes [m] starting from an empty supply. *)

module Test : sig
  val test : QCheck.Test.t list
end
