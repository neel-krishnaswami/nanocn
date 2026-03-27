(** Proof sorts (Pf).

    A proof sort classifies the logical content of a refined function's
    argument or return: computational bindings, logical facts, and
    resource ownership entries.

    Parameterized by ['e], the type of embedded expressions
    (SurfExpr.se at parse time, CoreExpr.ce after elaboration),
    and ['var], the type of variable names
    (string at parse time, Var.t after scope resolution). *)

type ('e, 'var) entry =
  | Comp of { var : 'var; sort : Sort.sort; eff : Effect.t }
  | Log of { var : 'var; prop : 'e }
  | Res of { var : 'var; pred : 'e; value : 'e }
  | DepRes of { var : 'var; bound_var : 'var; pred : 'e }

type ('e, 'var) t = ('e, 'var) entry list

val map_entry : ('a -> 'b) -> ('a, 'var) entry -> ('b, 'var) entry
val map : ('a -> 'b) -> ('a, 'var) t -> ('b, 'var) t

val map_var_entry : ('v -> 'w) -> ('e, 'v) entry -> ('e, 'w) entry
val map_var : ('v -> 'w) -> ('e, 'v) t -> ('e, 'w) t

val pf_types : ('e, 'var) t -> Sort.sort list
(** [pf_types pf] extracts the pure computational sorts from [pf]. *)

val comp : ('e, 'var) t -> Sort.sort
(** [comp pf] is the product of [pf_types pf] (erasure to a single sort). *)

val bind : _ Sig.t -> Context.t -> (CoreExpr.ce, Var.t) t -> (Context.t, string) result
(** [bind cs Γ pf] extends Γ with comp/spec variable bindings from [pf].
    For DepRes entries, synthesizes the pred to recover the bound sort. *)

val pf_to_ctx : _ Sig.t -> RCtx.t -> (CoreExpr.ce, Var.t) t -> (RCtx.t, string) result
(** [pf_to_ctx cs Δ pf] appends proof sort entries to Δ;
    resource entries get [Avail] usage.
    For DepRes entries, synthesizes the pred to recover the bound sort. *)

val subst : Var.t -> CoreExpr.ce -> (CoreExpr.ce, Var.t) t -> (CoreExpr.ce, Var.t) t
(** [subst x e pf] is [[e/x]pf]. *)

val print : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, Var.t) t -> unit
val print_ce : Format.formatter -> (CoreExpr.ce, Var.t) t -> unit

module Test : sig
  val test : QCheck.Test.t list
end
