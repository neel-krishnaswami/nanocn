(** Evaluation contexts for pattern match compilation.

    An evaluation context accumulates let-bindings during coverage
    checking. When a branch is selected, the context is filled with
    the branch body to produce the final core expression. *)

type t =
  | Hole
  | Let of Var.t * CoreExpr.ce * t

val fill : t -> CoreExpr.ce -> CoreExpr.ce
(** [fill ctx ce] substitutes [ce] for the hole in [ctx]. *)

val extend : t -> Var.t -> CoreExpr.ce -> t
(** [extend ctx x e] appends [let x = e] at the innermost position. *)

val print : Format.formatter -> t -> unit
