(** Render a source excerpt around a [SourcePos.t].

    A registry holds the source text of files the driver has loaded; the
    error printer queries it by filename to produce "±N lines of context
    with the erroneous range highlighted" output.

    The registry must be populated explicitly; if a position's file is
    not registered (typical for the REPL or synthesized code), the
    excerpt call is a no-op — the caller still prints the position
    line itself. *)

type registry

val create : unit -> registry
(** A fresh empty registry. *)

val register : registry -> file:string -> source:string -> unit
(** [register reg ~file ~source] records [source] as the contents of
    [file] for later excerpt rendering. If [file] is already present,
    it is overwritten. *)

val excerpt : registry -> context:int -> SourcePos.t -> Format.formatter -> unit
(** [excerpt reg ~context pos fmt] prints [pos]'s surrounding source to
    [fmt]: up to [context] lines before and after the erroneous range,
    with a gutter of line numbers and the erroneous span highlighted
    via ocolor-style [Format] semantic tags (["red;reverse"] for the
    erroneous span, ["faint"] for the gutter).

    If [pos]'s file is not in the registry, prints nothing. *)

val text_at : registry -> SourcePos.t -> string option
(** [text_at reg pos] returns the raw source substring spanning
    [pos], if [pos]'s file is in the registry and the position is
    in range.  For multi-line spans, the returned string contains
    the literal newlines.  Returns [None] when the file isn't
    registered or the position is malformed. *)

module Test : sig
  val test : QCheck.Test.t list
end
