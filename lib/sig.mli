(** Signature context for function, datasort, and datatype declarations.

    All functions use a unified [FunSig] entry with a sort-level signature
    and effect. Spec functions have [eff = Spec]. *)

type entry =
  | FunSig of { arg : Sort.sort; ret : Sort.sort; eff : Effect.t }
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type t

val empty : t
val extend : Var.t -> entry -> t -> t

val lookup_fun : Var.t -> t -> (Sort.sort * Sort.sort * Effect.t) option
(** [lookup_fun f sig] returns [(arg, ret, eff)] for any function. *)

val lookup_sort : Dsort.t -> t -> DsortDecl.t option
(** [lookup_sort d sig] returns the datasort declaration for [d]. *)

val lookup_ctor : Label.t -> t -> (Dsort.t * DsortDecl.t) option
(** [lookup_ctor l sig] searches all datasort declarations for constructor [l]. *)

val extend_sort : t -> DsortDecl.t -> t
(** [extend_sort sig d] adds a datasort declaration directly. *)

val lookup_type : Dsort.t -> t -> DtypeDecl.t option
(** [lookup_type d sig] returns the datatype declaration for [d]. *)

val lookup_type_ctor : Label.t -> t -> (Dsort.t * DtypeDecl.t) option
(** [lookup_type_ctor l sig] searches all datatype declarations for constructor [l]. *)

val extend_type : t -> DtypeDecl.t -> t
(** [extend_type sig d] adds a datatype declaration directly. *)

val print : Format.formatter -> t -> unit

module Test : sig
  val test : QCheck.Test.t list
end
