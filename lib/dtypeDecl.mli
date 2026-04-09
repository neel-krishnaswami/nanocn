(** Datatype declarations.

    A datatype declaration introduces a named type with parameters and
    a list of labeled constructors, each carrying a type. *)

type t = {
  name : Dsort.t;
  params : Tvar.t list;
  ctors : (Label.t * Sort.sort) list;
  loc : SourcePos.t;
}

val lookup_ctor : Label.t -> t -> Sort.sort option
(** [lookup_ctor l d] returns the type associated with constructor [l]
    in declaration [d], or [None] if not found. *)

val ctor_labels : t -> Label.t list
(** [ctor_labels d] returns the list of constructor labels in [d]. *)

val compare : t -> t -> int
val print : Format.formatter -> t -> unit
val json : t -> Json.t

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
