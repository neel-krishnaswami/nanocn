(** Evaluation contexts for pattern match compilation.

    An evaluation context accumulates let-bindings during coverage
    checking. When a branch is selected, the context is filled with
    the branch body to produce the final core expression. *)

type 'b t =
  | Hole
  | Let of (Var.t * 'b) * 'b CoreExpr.t * 'b t

val fill : 'b t -> 'b CoreExpr.t -> 'b CoreExpr.t
(** [fill ctx ce] substitutes [ce] for the hole in [ctx]. *)

val extend : 'b t -> Var.t -> 'b -> 'b CoreExpr.t -> 'b t
(** [extend ctx x info e] appends [let (x,info) = e] at the innermost position. *)

val print : _ t -> Format.formatter -> unit
