(** Type-directed pattern-expansion code actions for the LSP.

    Given a typed refined program and a source position, produces a
    list of code actions that expand a variable pattern under the
    cursor into a structurally richer pattern, with corresponding
    witness substitutions throughout the pattern's scope.

    See [doc/lsp-improvements.md] for the user-facing description. *)

(** {1 Public types} *)

(** A single source-text replacement. *)
type edit = {
  range : SourcePos.t;
  new_text : string;
}

(** A code action bundles a title and the edits it would apply.
    All edits target the same file (the one the cursor is in). *)
type action = {
  title : string;
  edits : edit list;
}

(** {1 Entry point} *)

val actions_at :
  RProg.typed -> file:string -> line:int -> col:int -> action list
(** [actions_at prog ~file ~line ~col] walks [prog] looking for a
    pattern variable whose source span covers the cursor at
    [(line, col)].  Returns the list of available code actions
    (typically [[]] or a single-element list).

    Currently supported:
    - [CVar v] with a [Record [τ₁; …; τₙ]] sort → tuple destructuring.

    Future cases (planned but not yet wired): all five refined-resource
    pattern expansions in [doc/lsp-improvements.md]. *)
