(** Datasort declarations.

    A datasort declaration introduces a named type with parameters and
    a list of labeled constructors, each carrying a sort. *)

type t = {
  name : Dsort.t;
  params : Tvar.t list;
  ctors : (Label.t * Sort.sort) list;
  loc : SourcePos.t;
}

val lookup_ctor : Label.t -> t -> Sort.sort option
(** [lookup_ctor l d] returns the sort associated with constructor [l]
    in declaration [d], or [None] if not found. *)

val ctor_labels : t -> Label.t list
(** [ctor_labels d] returns the list of constructor labels in [d]. *)

val resolve_tvars : t -> t
(** [resolve_tvars d] replaces [App(name, [])] with [TVar name] in
    each constructor sort, when [name] matches one of [d.params].
    This resolves the parse-time ambiguity between datasort names and
    type variables. *)

val compare : t -> t -> int
val print : Format.formatter -> t -> unit

module Test : sig
  val gen : t QCheck.Gen.t
  val test : QCheck.Test.t list
end
