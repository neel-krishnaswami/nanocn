(** Evaluation contexts for computational pattern match compilation.

    An evaluation context accumulates let-bindings during coverage
    checking. When a branch is selected, the context is filled with
    the branch body to produce the final core expression. *)

type t =
  | Hole
  | Let of Var.t * Expr.expr * t

val fill : t -> Expr.expr -> Expr.expr
(** [fill ctx e] substitutes [e] for the hole in [ctx]. *)

val extend : t -> Var.t -> Expr.expr -> t
(** [extend ctx x e] appends [let x = e] at the innermost position. *)

val print : Format.formatter -> t -> unit
