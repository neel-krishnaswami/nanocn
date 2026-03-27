(** Refined signatures (RS).

    Extends core signatures with refined function type entries.
    Provides erasure to core signatures via [comp]. *)

type entry =
  | FunSig of { arg : Sort.sort; ret : Sort.sort; eff : Effect.t }
  | FunDef of { param : Var.t; arg : Sort.sort; ret : Sort.sort; eff : Effect.t; body : CoreExpr.ce }
  | RFunSig of (CoreExpr.ce, Var.t) RFunType.t
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type t

val empty : t
val extend : string -> entry -> t -> t
val extend_sort : t -> DsortDecl.t -> t
val extend_type : t -> DtypeDecl.t -> t

val lookup_rf : string -> t -> (CoreExpr.ce, Var.t) RFunType.t option
(** [lookup_rf f sig] returns the refined function type for [f].
    Lifts plain [FunSig]/[FunDef] entries to trivial RF. *)

val lookup_fun : string -> t -> (Sort.sort * Sort.sort * Effect.t) option
(** [lookup_fun f sig] returns [(arg, ret, eff)] for any function. *)

val lookup_fundef : string -> t -> (Var.t * Sort.sort * Sort.sort * Effect.t * CoreExpr.ce) option

val lookup_sort : Dsort.t -> t -> DsortDecl.t option
val lookup_type : Dsort.t -> t -> DtypeDecl.t option
val lookup_ctor : Label.t -> t -> (Dsort.t * DsortDecl.t) option
val lookup_type_ctor : Label.t -> t -> (Dsort.t * DtypeDecl.t) option

val comp : t -> CoreExpr.ce Sig.t
(** [comp rs] erases refined entries to core signature.
    RFunSig entries become FunSig with product sorts. *)

val print_gen : (Format.formatter -> Var.t -> unit) -> Format.formatter -> t -> unit
val print : Format.formatter -> t -> unit
val to_string : t -> string

module Test : sig
  val test : QCheck.Test.t list
end
