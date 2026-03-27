let supplier buf =
  fun () ->
    let tok = Lexer.token buf in
    let start_pos, end_pos = Sedlexing.lexing_positions buf in
    (tok, start_pos, end_pos)

let parse_raw entry s ~file =
  let buf = Lexer.from_string s ~file in
  let supplier = supplier buf in
  try Ok (MenhirLib.Convert.Simplified.traditional2revised entry supplier)
  with
  | Parser.Error ->
    let pos = Lexer.pos_of_lexbuf buf in
    Error (Format.asprintf "parse error at %a" SourcePos.print pos)
  | Failure msg ->
    Error msg

(* {1 Raw (pure) parse functions} *)

let parse_expr_raw s ~file = parse_raw Parser.program s ~file
let parse_prog_raw s ~file = parse_raw Parser.prog_eof s ~file
let parse_decl_raw s ~file = parse_raw Parser.repl_decl s ~file
let parse_rprog_raw s ~file = parse_raw Parser.rprog_eof s ~file
let parse_sort s ~file = parse_raw Parser.sort_eof s ~file

(* {1 Parsed + resolved} *)

let parse_and_resolve entry resolve s ~file =
  let open ElabM in
  let* raw = lift (parse_raw entry s ~file) in
  resolve raw

let parse_expr ?(env=[]) s ~file =
  parse_and_resolve Parser.program (Resolve.resolve_expr env) s ~file

let parse_prog ?(env=[]) s ~file =
  parse_and_resolve Parser.prog_eof (Resolve.resolve_prog env) s ~file

let parse_decl ?(env=[]) s ~file =
  let open ElabM in
  let* raw = lift (parse_raw Parser.repl_decl s ~file) in
  let* (d, _env) = Resolve.resolve_decl env raw in
  return d

let parse_let ?(env=[]) s ~file =
  let open ElabM in
  let* (name, pos, raw_se) = lift (parse_raw Parser.repl_let s ~file) in
  let* v = mk_var name pos in
  let* se = Resolve.resolve_expr env raw_se in
  return (v, se)

let parse_rprog ?(env=[]) s ~file =
  parse_and_resolve Parser.rprog_eof (Resolve.resolve_rprog env) s ~file
