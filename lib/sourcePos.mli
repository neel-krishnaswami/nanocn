(** Source positions tracking file, line, and column ranges. *)

type t

val create : file:string -> start_line:int -> start_col:int -> end_line:int -> end_col:int -> t
val file : t -> string
val start_line : t -> int
val start_col : t -> int
val end_line : t -> int
val end_col : t -> int

val merge : t -> t -> t
(** [merge p1 p2] returns the span from the start of [p1] to the end of [p2]. *)

val dummy : t
(** A dummy position for generated code. *)

val compare : t -> t -> int
val print : Format.formatter -> t -> unit
val json : t -> Json.t

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
