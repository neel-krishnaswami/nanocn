let supplier buf =
  fun () ->
    let tok = SmtLexer.token buf in
    let start_pos, end_pos = Sedlexing.lexing_positions buf in
    (tok, start_pos, end_pos)

let parse_raw entry s ~file =
  let buf = SmtLexer.from_string s ~file in
  let supplier = supplier buf in
  try Ok (MenhirLib.Convert.Simplified.traditional2revised entry supplier)
  with
  | SmtParser.Error ->
    let pos = SmtLexer.pos_of_lexbuf buf in
    Error (Format.asprintf "parse error at %a" SourcePos.print pos)
  | Failure msg ->
    Error msg

let parse_sexp  s ~file = parse_raw SmtParser.sexp_eof s ~file
let parse_sexps s ~file = parse_raw SmtParser.sexps_eof s ~file
