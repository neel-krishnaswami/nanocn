(** Signature context for function, datasort, and datatype declarations.

    The signature is parameterized by ['a], the type of function bodies
    stored in [FunDef] entries.  Pure and spec functions store their full
    definitions; impure functions store only prototypes ([FunSig]). *)

type 'a entry =
  | FunSig of { arg : Sort.sort; ret : Sort.sort; eff : Effect.t }
  | FunDef of { param : Var.t; arg : Sort.sort; ret : Sort.sort; eff : Effect.t; body : 'a }
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type 'a t

val empty : 'a t
val extend : Var.t -> 'a entry -> 'a t -> 'a t

val lookup_fun : Var.t -> 'a t -> (Sort.sort * Sort.sort * Effect.t) option
(** [lookup_fun f sig] returns [(arg, ret, eff)] for any function,
    extracting the prototype from either [FunSig] or [FunDef]. *)

val lookup_fundef : Var.t -> 'a t -> (Var.t * Sort.sort * Sort.sort * Effect.t * 'a) option
(** [lookup_fundef f sig] returns [(param, arg, ret, eff, body)] for
    functions stored as [FunDef].  Returns [None] for [FunSig] entries. *)

val lookup_sort : Dsort.t -> 'a t -> DsortDecl.t option
(** [lookup_sort d sig] returns the datasort declaration for [d]. *)

val lookup_ctor : Label.t -> 'a t -> (Dsort.t * DsortDecl.t) option
(** [lookup_ctor l sig] searches all datasort declarations for constructor [l]. *)

val extend_sort : 'a t -> DsortDecl.t -> 'a t
(** [extend_sort sig d] adds a datasort declaration directly. *)

val lookup_type : Dsort.t -> 'a t -> DtypeDecl.t option
(** [lookup_type d sig] returns the datatype declaration for [d]. *)

val lookup_type_ctor : Label.t -> 'a t -> (Dsort.t * DtypeDecl.t) option
(** [lookup_type_ctor l sig] searches all datatype declarations for constructor [l]. *)

val extend_type : 'a t -> DtypeDecl.t -> 'a t
(** [extend_type sig d] adds a datatype declaration directly. *)

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

module Test : sig
  val test : QCheck.Test.t list
end
