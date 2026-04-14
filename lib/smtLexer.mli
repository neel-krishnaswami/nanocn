(** Sedlex tokeniser for SMT-LIB 2.7 s-expressions. *)

type token = SmtParser.token

val token : Sedlexing.lexbuf -> token

val from_string : string -> file:string -> Sedlexing.lexbuf

val pos_of_lexbuf : Sedlexing.lexbuf -> SourcePos.t
