(** Translate a typed constraint tree to a flat list of top-level
    SMT commands per the SMT(C) rules of [doc/smt-encoding.md]. Each
    returned command carries the source position of the originating
    constraint node so a caller can emit a preceding comment. *)

type located_cmd = { pos : SourcePos.t; cmd : SmtSexp.sexp }

type config = {
  position_trace : bool;
  (** When [true], the translator emits commands that maintain a
      [pos-N] chain of [PosList Pos] values tracking the nested
      scope that the current [(check-sat)] is emitted in. The
      supporting [Pos] / [PosList] datatype declarations are
      emitted by [SmtPrelude]; the root [pos-0] declaration and
      its head-constraining [assert] are prepended to the constraint
      commands by this function. *)
}

val default_config : config
(** [{ position_trace = true }]. *)

val of_ct :
  ?config:config -> Constraint.typed_ct ->
  (located_cmd list, string) result

val is_check_sat : located_cmd -> bool
(** [is_check_sat c] is [true] iff [c.cmd] is the bare s-expression
    [(check-sat)]. Used by callers that pair Z3's per-query answers with
    the originating constraint's source position: only [(check-sat)]
    commands produce answers, so the positions list passed to the
    solver driver must be restricted to those. *)
