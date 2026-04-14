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
