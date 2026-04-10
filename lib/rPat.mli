(** Refined patterns (q): variable tuples with optional pair elements.
    Parameterized by ['var] — [string] at parse time, [Var.t] after resolution. *)

type 'var pat_elem = Single of 'var | Pair of 'var * 'var

type 'var t = 'var pat_elem list

val map_var : ('v -> 'w) -> 'v t -> 'w t
val print_gen : (Format.formatter -> 'var -> unit) -> Format.formatter -> 'var t -> unit
val print : Format.formatter -> Var.t t -> unit
val to_string : Var.t t -> string

module Test : sig
  val test : QCheck.Test.t list
end
