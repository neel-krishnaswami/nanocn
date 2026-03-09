let supplier buf =
  fun () ->
    let tok = Lexer.token buf in
    let start_pos, end_pos = Sedlexing.lexing_positions buf in
    (tok, start_pos, end_pos)

let parse_with entry s ~file =
  let buf = Lexer.from_string s ~file in
  let supplier = supplier buf in
  try Ok (MenhirLib.Convert.Simplified.traditional2revised entry supplier)
  with
  | Parser.Error ->
    let pos = Lexer.pos_of_lexbuf buf in
    Error (Format.asprintf "parse error at %a" SourcePos.print pos)
  | Failure msg ->
    Error msg

let parse_expr s ~file = parse_with Parser.program s ~file
let parse_typ s ~file = parse_with Parser.typ_eof s ~file
let parse_prog s ~file = parse_with Parser.prog_eof s ~file
let parse_decl s ~file = parse_with Parser.repl_decl s ~file
let parse_let s ~file = parse_with Parser.repl_let s ~file
