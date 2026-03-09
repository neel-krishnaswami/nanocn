(** Lexer for the language, using sedlex. *)

type token = Parser.token

val token : Sedlexing.lexbuf -> token
(** [token buf] returns the next token from [buf]. *)

val from_string : string -> file:string -> Sedlexing.lexbuf
(** [from_string s ~file] creates a lexbuf for string [s] with filename [file]. *)

val pos_of_lexbuf : Sedlexing.lexbuf -> SourcePos.t
(** [pos_of_lexbuf buf] returns the current source position. *)
