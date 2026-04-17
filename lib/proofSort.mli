(** Proof sorts (Pf).

    A proof sort classifies the logical content of a refined function's
    argument or return: computational bindings, logical facts, and
    resource ownership entries.

    Parameterized by ['e], the type of embedded expressions
    (SurfExpr.se at parse time, CoreExpr.ce after elaboration),
    ['b], auxiliary info on each entry (location at parse time,
    typing info after checking),
    and ['var], the type of variable names
    (string at parse time, Var.t after scope resolution). *)

type ('e, 'b, 'var) entry =
  | Comp of { info : 'b; var : 'var; sort : Sort.sort; eff : Effect.t }
  | Log of { info : 'b; prop : 'e }
  | Res of { info : 'b; pred : 'e; value : 'e }
  | DepRes of { info : 'b; bound_var : 'var; pred : 'e }

type ('e, 'b, 'var) t = ('e, 'b, 'var) entry list

val entry_info : ('e, 'b, 'var) entry -> 'b

val map_entry : ('a -> 'b) -> ('a, 'info, 'var) entry -> ('b, 'info, 'var) entry
val map : ('a -> 'b) -> ('a, 'info, 'var) t -> ('b, 'info, 'var) t

val map_info_entry : ('b -> 'c) -> ('e, 'b, 'var) entry -> ('e, 'c, 'var) entry
val map_info : ('b -> 'c) -> ('e, 'b, 'var) t -> ('e, 'c, 'var) t

val map_var_entry : ('v -> 'w) -> ('e, 'b, 'v) entry -> ('e, 'b, 'w) entry
val map_var : ('v -> 'w) -> ('e, 'b, 'v) t -> ('e, 'b, 'w) t

val pf_types : ('e, 'b, 'var) t -> Sort.sort list
(** [pf_types pf] extracts the pure computational sorts from [pf]. *)

val comp : ('e, 'b, 'var) t -> Sort.sort
(** [comp pf] is the product of [pf_types pf] (erasure to a single sort). *)

val bind :
  Context.t -> (CoreExpr.typed_ce, 'b, Var.t) t ->
  (Context.t, Error.kind) result
(** [bind Γ pf] extends Γ with comp/spec variable bindings from [pf].
    For [DepRes] entries, reads the sort directly from the typed info.
    Fails with [Error.K_dep_res_not_pred] if a [DepRes] predicate
    doesn't have [Pred _] sort. *)

val apply_subst : Subst.t -> (CoreExpr.typed_ce, 'b, Var.t) t -> (CoreExpr.typed_ce, 'b, Var.t) t
(** [apply_subst gamma pf] is [[gamma]]pf. *)

val subst : Var.t -> CoreExpr.typed_ce -> (CoreExpr.typed_ce, 'b, Var.t) t -> (CoreExpr.typed_ce, 'b, Var.t) t
(** [subst x e pf] is [[e/x]pf], shorthand for [apply_subst (extend_var x e empty)]. *)

val print_gen : (Format.formatter -> Var.t -> unit) -> (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, 'b, Var.t) t -> unit
val print : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, 'b, Var.t) t -> unit
val print_ce : Format.formatter -> (CoreExpr.typed_ce, 'b, Var.t) t -> unit

val to_string : (Format.formatter -> 'e -> unit) -> ('e, 'b, Var.t) t -> string
val to_string_ce : (CoreExpr.typed_ce, 'b, Var.t) t -> string

module Test : sig
  val test : QCheck.Test.t list
end
