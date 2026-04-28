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

(** {1 Lookups}

    All lookups return [(_, Error.kind) result]. Failure produces the
    canonical "not found" error for that kind of name; lift to the
    elaboration monad with [ElabM.lift_at]. *)

val lookup_fun :
  string -> 'a t -> (Sort.sort * Sort.sort * Effect.t, Error.kind) result
(** Returns [Error (K_unknown_function { name })] if [name] is not
    bound. *)

val lookup_fundef :
  string -> 'a t ->
  (Var.t * Sort.sort * Sort.sort * Effect.t * 'a, Error.kind) result
(** Returns [Error (K_unfold_not_fundef { name })] if [name] is not
    a [FunDef] entry. *)

val lookup_sort :
  Dsort.t -> 'a t -> (DsortDecl.t, Error.kind) result
(** Returns [Error (K_unbound_sort dsort)] if [dsort] is not declared
    as a datasort. *)

val lookup_type :
  Dsort.t -> 'a t -> (DtypeDecl.t, Error.kind) result
(** Returns [Error (K_unbound_sort dsort)] if [dsort] is not declared
    as a datatype. *)

val lookup_ctor :
  Label.t -> 'a t -> (Dsort.t * DsortDecl.t, Error.kind) result
(** Returns [Error (K_unbound_ctor label)] if [label] is not a
    constructor of any datasort. *)

val lookup_type_ctor :
  Label.t -> 'a t -> (Dsort.t * DtypeDecl.t, Error.kind) result
(** Returns [Error (K_unbound_ctor label)] if [label] is not a
    constructor of any datatype. *)

(** Result of looking up a name that may be either a datasort or a
    datatype. *)
type sort_or_type =
  | LSortDecl of DsortDecl.t
  | LTypeDecl of DtypeDecl.t

val lookup_dsort_or_type :
  Dsort.t -> 'a t -> (sort_or_type, Error.kind) result
(** Combined lookup: returns [Ok (LSortDecl _)] if [dsort] is declared
    as a datasort, [Ok (LTypeDecl _)] if as a datatype, and
    [Error (K_unbound_sort dsort)] otherwise. *)

val extend_sort : 'a t -> DsortDecl.t -> 'a t
val extend_type : 'a t -> DtypeDecl.t -> 'a t

val print_gen : (Format.formatter -> Var.t -> unit) -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val to_string : (Format.formatter -> 'a -> unit) -> 'a t -> string

module Test : sig
  val test : QCheck.Test.t list
end
