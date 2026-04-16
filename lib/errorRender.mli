(** Configure a [Format.formatter] for coloured error output via ocolor.

    The error printer (see [TypeError.print]) emits its output with
    [Format] semantic tags for colour and emphasis — [Format] itself
    ignores unknown tags unless the formatter is set up to interpret
    them. This module does that set-up, and conditionally disables
    colour when the formatter is writing to something that isn't a
    terminal (so piping to a file still yields plain text). *)

val configure_formatter : ?force:[ `Auto | `Always | `Never ] -> Format.formatter -> unit
(** [configure_formatter fmt] enables ocolor-style tag interpretation
    on [fmt]. The optional [?force] argument overrides auto-detection:
    [`Auto] (the default) enables colour when the formatter's output
    is a terminal; [`Always] forces colour on regardless; [`Never]
    turns colour off (tags still parse but emit nothing).

    Once configured, a formatter passes tags like [@{<red;bold>...@}]
    through ocolor. If the colour decision is [`Never], tags become
    no-ops and the printer's output is pure text. *)

val is_tty : Format.formatter -> bool
(** [is_tty fmt] is a best-effort check for whether [fmt] writes to
    a terminal. Used by [configure_formatter] when the force mode is
    [`Auto]. *)
