(** Refined patterns (q): variable tuples with optional pair elements.
    Parameterized by ['var] — [string] at parse time, [Var.t] after resolution,
    and ['b] — auxiliary info (location at parse time, typing info after checking). *)

type ('var, 'b) pat_elem = Single of 'b * 'var | Pair of 'b * 'var * 'var

type ('var, 'b) t = ('var, 'b) pat_elem list

val elem_info : ('var, 'b) pat_elem -> 'b

val map_var_elem : ('v -> 'w) -> ('v, 'b) pat_elem -> ('w, 'b) pat_elem
val map_var : ('v -> 'w) -> ('v, 'b) t -> ('w, 'b) t

val map_info_elem : ('b -> 'c) -> ('var, 'b) pat_elem -> ('var, 'c) pat_elem
val map_info : ('b -> 'c) -> ('var, 'b) t -> ('var, 'c) t

val print_gen : (Format.formatter -> 'var -> unit) -> Format.formatter -> ('var, _) t -> unit
val print : Format.formatter -> (Var.t, _) t -> unit
val to_string : (Var.t, _) t -> string

module Test : sig
  val test : QCheck.Test.t list
end
