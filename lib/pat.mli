(** Patterns for the assertion language surface syntax.

    Patterns bind variables in case, let, and take expressions.
    Parameterized by ['var] — the type of variable names
    ([string] at parse time, [Var.t] after scope resolution). *)

(** The shape functor for patterns.
    ['a] = recursive positions, ['b] = auxiliary info, ['var] = variable type. *)
type ('a, 'b, 'var) patF =
  | Var of 'var
  | Con of Label.t * 'a
  | Tuple of 'a list

val map_shape : ('a -> 'c) -> ('a, 'b, 'var) patF -> ('c, 'b, 'var) patF
val map_info : ('b -> 'c) -> ('a, 'b, 'var) patF -> ('a, 'c, 'var) patF

(** A pattern tree annotated with ['b] at each node. *)
type ('b, 'var) t

val mk : 'b -> (('b, 'var) t, 'b, 'var) patF -> ('b, 'var) t
val info : ('b, 'var) t -> 'b
val shape : ('b, 'var) t -> (('b, 'var) t, 'b, 'var) patF
val map : ('b -> 'c) -> ('b, 'var) t -> ('c, 'var) t

val map_var : ('v -> 'w) -> ('b, 'v) t -> ('b, 'w) t
(** [map_var f p] replaces every variable in [p] by applying [f]. *)

(** Concrete located pattern (after scope resolution). *)
type pat = (< loc : SourcePos.t >, Var.t) t

(** Parsed pattern (before scope resolution). *)
type parsed_pat = (< loc : SourcePos.t >, string) t

val compare : pat -> pat -> int
val print_gen : (Format.formatter -> 'var -> unit) -> Format.formatter -> (_, 'var) t -> unit
val print : Format.formatter -> (_, Var.t) t -> unit
val to_string : (_, Var.t) t -> string
val json : ('b -> Json.t) -> ('b, Var.t) t -> Json.t

val linear_check : pat -> (unit, string) result
(** [linear_check p] returns [Ok ()] if no variable appears more than once
    in [p], or [Error msg] if a variable is repeated. *)

val vars : pat -> Var.t list
(** [vars p] returns the list of variables bound by [p], in left-to-right order. *)

(** Collect string variable names from a parsed pattern. *)
val parsed_vars : parsed_pat -> string list

module Test : sig
  val gen : pat QCheck.Gen.t
  val test : QCheck.Test.t list
end
