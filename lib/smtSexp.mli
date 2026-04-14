(** SMT-LIB 2.7 s-expressions.

    Structured per [doc/instructions/syntax-trees.md]: a shape functor
    [('a, 'b) sexpF] exposes the structural recursion, and the knot
    [type 'b t] ties the recursion with an info annotation at every
    node. The concrete syntax has only proper lists (no dotted-pair
    form), so [sexpF] has a [List of 'a list] constructor rather than
    cons cells. *)

(** Shape functor.
    ['a] is the recursive child position, ['b] is auxiliary info. *)
type ('a, 'b) sexpF =
  | Atom of SmtAtom.t
  | List of 'a list

val map_shape : ('a -> 'c) -> ('a, 'b) sexpF -> ('c, 'b) sexpF
val map_info  : ('b -> 'c) -> ('a, 'b) sexpF -> ('a, 'c) sexpF

(** An s-expression annotated with ['b] at every node. *)
type 'b t

val mk    : 'b -> ('b t, 'b) sexpF -> 'b t
val info  : 'b t -> 'b
val shape : 'b t -> ('b t, 'b) sexpF
val map   : ('b -> 'c) -> 'b t -> 'c t

(** Concrete located s-expression, parallel to [CoreExpr.ce]. *)
type sexp = < loc : SourcePos.t > t

(** {1 Smart constructors}

    Info is an explicit parameter so callers pick their own
    annotation type. The parser supplies a [< loc : SourcePos.t >]
    object; programmatic construction from nanoCN may thread the
    originating source position or use [unit]. *)

val atom       : 'b -> SmtAtom.t -> 'b t
val symbol     : 'b -> string -> 'b t
val keyword    : 'b -> string -> 'b t
val numeral_s  : 'b -> string -> 'b t
val string_lit : 'b -> string -> 'b t
val reserved   : 'b -> SmtAtom.reserved -> 'b t
val list       : 'b -> 'b t list -> 'b t

val app : 'b -> 'b t -> 'b t list -> 'b t
(** [app info head args = list info (head :: args)]. The canonical
    parenthesised-application form. *)

(** {1 Views}

    [as_atom] returns [Some] exactly for [Atom] nodes; [as_list]
    returns [Some] exactly for [List] nodes. [as_symbol] / [as_keyword]
    succeed only when the node is an atom of the matching flavour. *)

val as_atom    : 'b t -> SmtAtom.t option
val as_list    : 'b t -> 'b t list option
val as_symbol  : 'b t -> string option
val as_keyword : 'b t -> string option

(** {1 Comparison and printing} *)

val compare   : ('b -> 'b -> int) -> 'b t -> 'b t -> int
val print     : Format.formatter -> 'b t -> unit
val to_string : 'b t -> string
val json      : ('b -> Json.t) -> 'b t -> Json.t

module Test : sig
  val gen  : sexp QCheck.Gen.t
  val test : QCheck.Test.t list
end
