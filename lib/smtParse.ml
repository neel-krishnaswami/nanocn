(** SMT-LIB parser driver: feeds [SmtLexer] into the Menhir
    incremental SMT parser and maps error checkpoints to per-state
    diagnostics from [smtParser.messages]. *)

let supplier buf =
  fun () ->
    let tok = SmtLexer.token buf in
    let start_pos, end_pos = Sedlexing.lexing_positions buf in
    (tok, start_pos, end_pos)

let lookup_message state =
  try SmtParser_messages.message state
  with Not_found -> "PLACEHOLDER\n"

let parse_raw start s ~file =
  let buf = SmtLexer.from_string s ~file in
  let supplier = supplier buf in
  let module I = SmtParser.MenhirInterpreter in
  let rec loop (checkpoint : 'a I.checkpoint) =
    match checkpoint with
    | I.InputNeeded _ ->
      let tok = supplier () in
      loop (I.offer checkpoint tok)
    | I.Shifting _ | I.AboutToReduce _ ->
      loop (I.resume checkpoint)
    | I.HandlingError env ->
      let state = I.current_state_number env in
      let msg = String.trim (lookup_message state) in
      let pos = SmtLexer.pos_of_lexbuf buf in
      Error (Format.asprintf "@[<v>%a: parse error@ %s@]"
               SourcePos.print pos msg)
    | I.Accepted v -> Ok v
    | I.Rejected ->
      let pos = SmtLexer.pos_of_lexbuf buf in
      Error (Format.asprintf "%a: parser rejected input" SourcePos.print pos)
  in
  try
    let start_pos, _ = Sedlexing.lexing_positions buf in
    loop (start start_pos)
  with Failure msg -> Error msg

let parse_sexp  s ~file = parse_raw SmtParser.Incremental.sexp_eof  s ~file
let parse_sexps s ~file = parse_raw SmtParser.Incremental.sexps_eof s ~file
