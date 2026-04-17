(** Spatial index over a typed core expression tree.

    Maps a cursor position [(line, col)] to the smallest enclosing
    typed node, returning its source location, typing context, sort,
    and effect.  Built from the output of [CompileFile], which
    produces [typed_ce] trees where every node carries
    [Typecheck.typed_info]. *)

type t

val empty : t

val of_typed_decls : Typecheck.typed_ce Prog.core_decl list -> t
(** Build an index from a list of typed core declarations. *)

val add_typed_expr : Typecheck.typed_ce -> t -> t
(** Add a single typed expression (e.g. the main body) to the index. *)

val lookup :
  t -> line:int -> col:int ->
  (SourcePos.t * Context.t * Sort.sort * Effect.t) option
(** [lookup idx ~line ~col] finds the smallest typed node whose
    source span covers the given 1-based line and 0-based column.
    Returns [None] if no node covers the position. *)

module Test : sig
  val test : QCheck.Test.t list
end
