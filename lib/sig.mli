(** Signature context for function, spec function, spec value, and datasort declarations. *)

type entry =
  | FunSig of { arg : Typ.ty; ret : Typ.ty; eff : Effect.t }
  | SpecFun of { arg : Sort.sort; ret : Sort.sort }
  | SpecVal of { sort : Sort.sort }
  | SortDecl of DsortDecl.t

type t

val empty : t
val extend : Var.t -> entry -> t -> t

val lookup_fun : Var.t -> t -> (Typ.ty * Typ.ty * Effect.t) option
(** [lookup_fun f sig] returns [(arg, ret, eff)] for a computational function. *)

val lookup_spec_fun : Var.t -> t -> (Sort.sort * Sort.sort) option
(** [lookup_spec_fun f sig] returns [(arg, ret)] for a spec function. *)

val lookup_spec_val : Var.t -> t -> Sort.sort option
(** [lookup_spec_val f sig] returns the sort for a spec value. *)

val lookup_sort : Dsort.t -> t -> DsortDecl.t option
(** [lookup_sort d sig] returns the datasort declaration for [d]. *)

val lookup_ctor : Label.t -> t -> (Dsort.t * DsortDecl.t) option
(** [lookup_ctor l sig] searches all datasort declarations for constructor [l]. *)

val extend_sort : t -> DsortDecl.t -> t
(** [extend_sort sig d] adds a datasort declaration directly. *)

val print : Format.formatter -> t -> unit

module Test : sig
  val test : QCheck.Test.t list
end
