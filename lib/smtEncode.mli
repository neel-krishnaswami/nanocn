(** End-to-end encoding: build prelude + constraint commands for a
    typechecked refined nanoCN program, and write them to an output
    channel with source-position comments before each constraint
    command. *)

type config = {
  position_trace : bool;
  (** Passes through to [SmtPrelude] and [SmtConstraint]. *)
}

val default_config : config

val encode :
  ?config:config -> RSig.t -> Constraint.typed_ct ->
  (SmtSexp.sexp list * SmtConstraint.located_cmd list, string) result

val write_file :
  out_channel ->
  prelude:SmtSexp.sexp list ->
  constraints:SmtConstraint.located_cmd list ->
  unit
(** Emits each prelude command (separated by a blank line) followed by
    each constraint command preceded by a [; file:line:col] comment. *)
