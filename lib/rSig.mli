(** Refined signatures (RS).

    Extends core signatures with refined function type entries.
    Provides erasure to core signatures via [comp]. *)

type entry =
  | FunSig of { arg : Sort.sort; ret : Sort.sort; eff : Effect.t }
  | FunDef of { param : Var.t; arg : Sort.sort; ret : Sort.sort; eff : Effect.t; body : CoreExpr.typed_ce }
  | RFunSig of (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RFunType.t
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type t

val empty : t
val extend : string -> entry -> t -> t
val extend_sort : t -> DsortDecl.t -> t
val extend_type : t -> DtypeDecl.t -> t

(** {1 Lookups}

    All lookups return [(_, Error.kind) result]. Failure produces the
    canonical "not found" error for that kind of name; lift to the
    elaboration monad with [ElabM.lift_at]. *)

val lookup_rf :
  string -> t ->
  ((CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RFunType.t, Error.kind) result
(** [lookup_rf f rs] looks up the refined function type for [f]; only
    returns [Ok] if [f] has an [RFunSig] entry. Returns
    [Error (K_unknown_function { name = f })] otherwise. *)

val lookup_fun :
  string -> t -> (Sort.sort * Sort.sort * Effect.t, Error.kind) result
(** [lookup_fun f rs] returns [(arg, ret, eff)] for any function;
    [RFunSig] entries are projected through [ProofSort.comp] of their
    domain/codomain. Returns [Error (K_unknown_function { name = f })]
    if [f] is not bound. *)

val lookup_fundef :
  string -> t ->
  (Var.t * Sort.sort * Sort.sort * Effect.t * CoreExpr.typed_ce, Error.kind) result
(** Returns [Error (K_unfold_not_fundef { name = f })] if [f] is not
    a [FunDef] entry. *)

val lookup_sort :
  Dsort.t -> t -> (DsortDecl.t, Error.kind) result

val lookup_type :
  Dsort.t -> t -> (DtypeDecl.t, Error.kind) result

val lookup_ctor :
  Label.t -> t -> (Dsort.t * DsortDecl.t, Error.kind) result

val lookup_type_ctor :
  Label.t -> t -> (Dsort.t * DtypeDecl.t, Error.kind) result

val lookup_dsort_or_type :
  Dsort.t -> t -> (Sig.sort_or_type, Error.kind) result
(** Combined datasort-or-datatype lookup. Returns [Error
    (K_unbound_sort dsort)] if neither flavor has a declaration for
    [dsort]. *)

(** Source-order listing of all signature entries. Named function
    entries are [LFun (name, entry)]; anonymous sort/type declarations
    are [LSort]/[LType]. Used by consumers that need to iterate the
    full signature (e.g. the SMT encoder). *)
type listed_entry =
  | LFun  of string * entry
  | LSort of DsortDecl.t
  | LType of DtypeDecl.t

val entries : t -> listed_entry list

val comp : t -> CoreExpr.typed_ce Sig.t
(** [comp rs] erases refined entries to core signature.
    RFunSig entries become FunSig with product sorts. *)

val print_gen : (Format.formatter -> Var.t -> unit) -> Format.formatter -> t -> unit
val print : Format.formatter -> t -> unit
val to_string : t -> string

module Test : sig
  val test : QCheck.Test.t list
end
