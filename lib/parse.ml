(** Parser driver: feeds the Sedlex lexer into the Menhir incremental
    parser and maps error checkpoints to per-state diagnostics from
    [parser.messages]. *)

let supplier buf =
  fun () ->
    let tok = Lexer.token buf in
    let start_pos, end_pos = Sedlexing.lexing_positions buf in
    (tok, start_pos, end_pos)

let lookup_message state =
  try Parser_messages.message state
  with Not_found -> "PLACEHOLDER\n"

(* Drive a Menhir incremental parser for a single start symbol. We
   keep offering tokens until the parser accepts, rejects, or hands
   us a [HandlingError env]; the state number extracted from [env]
   keys into [Parser_messages.message]. *)
let parse_raw start s ~file =
  let buf = Lexer.from_string s ~file in
  let supplier = supplier buf in
  let module I = Parser.MenhirInterpreter in
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
      let pos = Lexer.pos_of_lexbuf buf in
      Error (Error.parse_error ~loc:(Some pos) ~msg)
    | I.Accepted v -> Ok v
    | I.Rejected ->
      let pos = Lexer.pos_of_lexbuf buf in
      Error (Error.parse_error ~loc:(Some pos)
               ~msg:"parser rejected input")
  in
  try
    let start_pos, _ = Sedlexing.lexing_positions buf in
    loop (start start_pos)
  with Failure msg -> Error (Error.parse_error ~loc:None ~msg)

(** {1 Raw (pure) parse functions} *)

let parse_expr_raw  s ~file = parse_raw Parser.Incremental.program   s ~file
let parse_prog_raw  s ~file = parse_raw Parser.Incremental.prog_eof  s ~file
let parse_decl_raw  s ~file = parse_raw Parser.Incremental.repl_decl s ~file
let parse_rprog_raw s ~file = parse_raw Parser.Incremental.rprog_eof s ~file
let parse_sort      s ~file = parse_raw Parser.Incremental.sort_eof  s ~file

(** {1 Parsed + resolved} *)

let parse_and_resolve entry resolve s ~file =
  let open ElabM in
  let* raw = lift (parse_raw entry s ~file) in
  resolve raw

let parse_expr ?(env=[]) s ~file =
  parse_and_resolve Parser.Incremental.program (Resolve.resolve_expr env) s ~file

let parse_prog ?(env=[]) s ~file =
  parse_and_resolve Parser.Incremental.prog_eof (Resolve.resolve_prog env) s ~file

let parse_decl ?(env=[]) s ~file =
  let open ElabM in
  let* raw = lift (parse_raw Parser.Incremental.repl_decl s ~file) in
  let* (d, _env) = Resolve.resolve_decl env raw in
  return d

let parse_let ?(env=[]) s ~file =
  let open ElabM in
  let* (name, pos, raw_se) = lift (parse_raw Parser.Incremental.repl_let s ~file) in
  let* v = mk_var name pos in
  let* se = Resolve.resolve_expr env raw_se in
  return (v, se)

let parse_rprog ?(env=[]) s ~file =
  parse_and_resolve Parser.Incremental.rprog_eof (Resolve.resolve_rprog env) s ~file
