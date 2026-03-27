(** Signature context for function, datasort, and datatype declarations.

    The signature is parameterized by ['a], the type of function bodies
    stored in [FunDef] entries.  Pure and spec functions store their full
    definitions; impure functions store only prototypes ([FunSig]).

    Function names are plain strings (resolved against the signature,
    not lexically scoped). *)

type 'a entry =
  | FunSig of { arg : Sort.sort; ret : Sort.sort; eff : Effect.t }
  | FunDef of { param : Var.t; arg : Sort.sort; ret : Sort.sort; eff : Effect.t; body : 'a }
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type 'a t

val empty : 'a t
val extend : string -> 'a entry -> 'a t -> 'a t

val lookup_fun : string -> 'a t -> (Sort.sort * Sort.sort * Effect.t) option
val lookup_fundef : string -> 'a t -> (Var.t * Sort.sort * Sort.sort * Effect.t * 'a) option

val lookup_sort : Dsort.t -> 'a t -> DsortDecl.t option
val lookup_ctor : Label.t -> 'a t -> (Dsort.t * DsortDecl.t) option
val extend_sort : 'a t -> DsortDecl.t -> 'a t

val lookup_type : Dsort.t -> 'a t -> DtypeDecl.t option
val lookup_type_ctor : Label.t -> 'a t -> (Dsort.t * DtypeDecl.t) option
val extend_type : 'a t -> DtypeDecl.t -> 'a t

val print_gen : (Format.formatter -> Var.t -> unit) -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val to_string : (Format.formatter -> 'a -> unit) -> 'a t -> string

module Test : sig
  val test : QCheck.Test.t list
end
