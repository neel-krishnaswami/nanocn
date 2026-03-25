(** Proof sorts (Pf).

    A proof sort classifies the logical content of a refined function's
    argument or return: computational bindings, logical facts, and
    resource ownership entries.

    Parameterized by ['e], the type of embedded expressions
    (SurfExpr.se at parse time, CoreExpr.ce after elaboration). *)

type 'e entry =
  | Comp of { var : Var.t; sort : Sort.sort; eff : Effect.t }
  | Log of { var : Var.t; prop : 'e }
  | Res of { var : Var.t; pred : 'e; value : 'e }

type 'e t = 'e entry list

val map_entry : ('a -> 'b) -> 'a entry -> 'b entry
val map : ('a -> 'b) -> 'a t -> 'b t

val pf_types : 'e t -> Sort.sort list
(** [pf_types pf] extracts the pure computational sorts from [pf]. *)

val comp : 'e t -> Sort.sort
(** [comp pf] is the product of [pf_types pf] (erasure to a single sort). *)

val bind : Context.t -> 'e t -> Context.t
(** [bind Γ pf] extends Γ with comp/spec variable bindings from [pf]. *)

val pf_to_ctx : RCtx.t -> CoreExpr.ce t -> RCtx.t
(** [pf_to_ctx Δ pf] appends proof sort entries to Δ;
    resource entries get [Avail] usage. *)

val subst : Var.t -> CoreExpr.ce -> CoreExpr.ce t -> CoreExpr.ce t
(** [subst x e pf] is [[e/x]pf]. *)

val print : (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e t -> unit
val print_ce : Format.formatter -> CoreExpr.ce t -> unit

module Test : sig
  val test : QCheck.Test.t list
end
