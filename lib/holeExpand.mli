(** Type-directed hole-expansion code actions for the LSP.

    Given a typed refined program and a source position, produces a
    list of code actions that replace a hole under the cursor with a
    structurally richer expression driven by the hole's expected
    type.

    See [doc/lsp-improvements.md] §"Hole expansion" for the
    user-facing description.  Unlike [PatternExpand], no witness
    substitution is needed — holes don't bind variables — so every
    action is a single edit replacing the hole's source text. *)

(** {1 Public types} *)

type edit = {
  range : SourcePos.t;
  new_text : string;
}

type action = {
  title : string;
  edits : edit list;
}

(** {1 Entry point} *)

val actions_at :
  RProg.typed -> line:int -> col:int -> action list
(** [actions_at prog ~line ~col] walks [prog] looking for a hole
    leaf whose source span covers the cursor at [(line, col)].
    Returns the list of available code actions (typically [[]] or a
    single-element list).

    Hole flavors handled:
    - [CoreExpr.Hole h] at a [Record _] sort → tuple of holes
    - [RefinedExpr.LHole h]                  → [auto]
    - [RefinedExpr.RHole h] at one of the five resource shapes
      ([return]/[take]/[let]/[let-tuple]/[call])
    - [RefinedExpr.CHole h] at a proof sort → parenthesised
      hole sequence indexed from 1

    All other shapes (e.g. an Error answer on a synth-only core
    hole, an unrecognised resource predicate, a non-record sort)
    return no action. *)
