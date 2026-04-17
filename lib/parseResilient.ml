(** Resilient parser — see parseResilient.mli for documentation. *)

type 'a chunk_result =
  | Parsed of 'a
  | Failed of Error.t

(* ================================================================== *)
(* Tokenization                                                        *)
(* ================================================================== *)

type located_token = Parser.token * Lexing.position * Lexing.position

(** Tokenize the entire input, collecting [(token, start, end)] triples.
    The final [EOF] triple is included. *)
let tokenize s ~file : located_token list =
  let buf = Lexer.from_string s ~file in
  let rec loop acc =
    let tok = Lexer.token buf in
    let sp, ep = Sedlexing.lexing_positions buf in
    let triple = (tok, sp, ep) in
    match tok with
    | Parser.EOF -> List.rev (triple :: acc)
    | _ -> loop (triple :: acc)
  in
  try loop []
  with Failure msg ->
    (* Lexer failure (unknown character, etc.).  Return what we have
       plus a synthetic EOF so the splitter has something to work with.
       The lexer-error chunk will fail at parse time. *)
    let sp, ep = Sedlexing.lexing_positions buf in
    List.rev ((Parser.EOF, sp, ep) :: (Parser.IDENT msg, sp, ep) :: [])

(* ================================================================== *)
(* Splitting at top-level keywords                                     *)
(* ================================================================== *)

type chunk_kind = Decl | Main

type chunk = {
  kind   : chunk_kind;
  tokens : located_token list;
}

(** Is this token a top-level sync keyword? *)
let is_sync_token = function
  | Parser.FUN | Parser.RFUN | Parser.SORT | Parser.TYPE | Parser.MAIN -> true
  | _ -> false

(** Is this token the [main] keyword? *)
let is_main_token = function
  | Parser.MAIN -> true
  | _ -> false

(** Split a token list into chunks.  Every occurrence of a sync
    keyword starts a new chunk.  No depth counting.  The chunk
    that starts with [MAIN] is tagged [Main]; all others are
    tagged [Decl]. *)
let split_into_chunks (tokens : located_token list) : chunk list =
  let rec loop acc current_rev = function
    | [] ->
      (* Flush any pending chunk. *)
      flush acc current_rev
    | ((tok, _, _) as triple) :: rest when is_sync_token tok ->
      (* Start of a new chunk.  Flush the previous one (if any). *)
      let acc = flush acc current_rev in
      loop acc [triple] rest
    | triple :: rest ->
      loop acc (triple :: current_rev) rest
  and flush acc = function
    | [] -> acc
    | rev_tokens ->
      let tokens = List.rev rev_tokens in
      let kind = match tokens with
        | (tok, _, _) :: _ when is_main_token tok -> Main
        | _ -> Decl
      in
      acc @ [{ kind; tokens }]
  in
  loop [] [] tokens

(* ================================================================== *)
(* Parse a token chunk via Menhir's incremental API                    *)
(* ================================================================== *)

let lookup_message state =
  try Parser_messages.message state
  with Not_found -> "PLACEHOLDER\n"

(** Append a synthetic [EOF] token at the end of a chunk (using the
    last token's end position), so the Menhir parser sees end-of-input. *)
let append_eof (tokens : located_token list) : located_token list =
  let last_pos = match List.rev tokens with
    | (_, _, ep) :: _ -> ep
    | [] -> { Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  (* Drop any existing trailing EOF to avoid duplicates. *)
  let without_eof = List.filter (fun (tok, _, _) ->
    match tok with Parser.EOF -> false | _ -> true) tokens in
  without_eof @ [(Parser.EOF, last_pos, last_pos)]

(** Drive Menhir's incremental parser on a list of tokens.
    Returns [Ok value] on success, [Error _] on parse error. *)
let parse_from_tokens
    (start : Lexing.position -> 'a Parser.MenhirInterpreter.checkpoint)
    (tokens : located_token list)
  : ('a, Error.t) result =
  let tokens = append_eof tokens in
  let remaining = ref tokens in
  (* Track the last offered position for error reporting. *)
  let last_pos = ref (match tokens with
    | (_, sp, _) :: _ -> Some sp
    | [] -> None) in
  let module I = Parser.MenhirInterpreter in
  let first_pos = match tokens with
    | (_, sp, _) :: _ -> sp
    | [] -> { Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
  in
  let rec loop (checkpoint : 'a I.checkpoint) =
    match checkpoint with
    | I.InputNeeded _ ->
      begin match !remaining with
      | triple :: rest ->
        let (_, sp, _) = triple in
        last_pos := Some sp;
        remaining := rest;
        loop (I.offer checkpoint triple)
      | [] ->
        (* Should not happen — we appended EOF. *)
        let pos = match !last_pos with
          | Some p -> SourcePos.create ~file:p.Lexing.pos_fname
                        ~start_line:p.pos_lnum ~start_col:0
                        ~end_line:p.pos_lnum ~end_col:0
          | None -> SourcePos.dummy
        in
        Error (Error.parse_error ~loc:(Some pos) ~msg:"unexpected end of input")
      end
    | I.Shifting _ | I.AboutToReduce _ ->
      loop (I.resume checkpoint)
    | I.HandlingError env ->
      let state = I.current_state_number env in
      let msg = String.trim (lookup_message state) in
      let pos = match !last_pos with
        | Some p -> SourcePos.create ~file:p.Lexing.pos_fname
                      ~start_line:p.pos_lnum
                      ~start_col:(p.pos_cnum - p.pos_bol)
                      ~end_line:p.pos_lnum
                      ~end_col:(p.pos_cnum - p.pos_bol)
        | None -> SourcePos.dummy
      in
      Error (Error.parse_error ~loc:(Some pos) ~msg)
    | I.Accepted v -> Ok v
    | I.Rejected ->
      let pos = match !last_pos with
        | Some p -> SourcePos.create ~file:p.Lexing.pos_fname
                      ~start_line:p.pos_lnum
                      ~start_col:(p.pos_cnum - p.pos_bol)
                      ~end_line:p.pos_lnum
                      ~end_col:(p.pos_cnum - p.pos_bol)
        | None -> SourcePos.dummy
      in
      Error (Error.parse_error ~loc:(Some pos) ~msg:"parser rejected input")
  in
  try loop (start first_pos)
  with Failure msg -> Error (Error.parse_error ~loc:None ~msg)

(* ================================================================== *)
(* Surface programs (.cn)                                              *)
(* ================================================================== *)

type parsed_file = {
  decls  : (SurfExpr.parsed_se, SourcePos.t, string) Prog.decl chunk_result list;
  main   : ((SurfExpr.parsed_se, SourcePos.t, string) Prog.t, Error.t) result option;
  errors : Error.t list;
}

let parse_prog_resilient s ~file =
  let tokens = tokenize s ~file in
  let chunks = split_into_chunks tokens in
  let decls = ref [] in
  let main = ref None in
  let errors = ref [] in
  List.iter (fun chunk ->
    match chunk.kind with
    | Decl ->
      begin match parse_from_tokens Parser.Incremental.repl_decl chunk.tokens with
      | Ok d -> decls := Parsed d :: !decls
      | Error e ->
        decls := Failed e :: !decls;
        errors := e :: !errors
      end
    | Main ->
      (* Parse the main chunk with prog_eof.  Since we split out
         decls, this chunk starts with MAIN and list(decl) is empty. *)
      begin match parse_from_tokens Parser.Incremental.prog_eof chunk.tokens with
      | Ok prog -> main := Some (Ok prog)
      | Error e ->
        main := Some (Error e);
        errors := e :: !errors
      end
  ) chunks;
  (if !main = None then
     let e = Error.parse_error ~loc:None ~msg:"missing `main` declaration" in
     errors := e :: !errors);
  { decls = List.rev !decls;
    main = !main;
    errors = List.rev !errors }

(* ================================================================== *)
(* Refined programs (.rcn)                                             *)
(* ================================================================== *)

type parsed_rfile = {
  rdecls : (SurfExpr.parsed_se, string) RProg.decl chunk_result list;
  rmain  : (RProg.raw_parsed, Error.t) result option;
  errors : Error.t list;
}

let parse_rprog_resilient s ~file =
  let tokens = tokenize s ~file in
  let chunks = split_into_chunks tokens in
  let rdecls = ref [] in
  let rmain = ref None in
  let errors = ref [] in
  List.iter (fun chunk ->
    match chunk.kind with
    | Decl ->
      begin match parse_from_tokens Parser.Incremental.repl_rdecl chunk.tokens with
      | Ok d -> rdecls := Parsed d :: !rdecls
      | Error e ->
        rdecls := Failed e :: !rdecls;
        errors := e :: !errors
      end
    | Main ->
      begin match parse_from_tokens Parser.Incremental.rprog_eof chunk.tokens with
      | Ok prog -> rmain := Some (Ok prog)
      | Error e ->
        rmain := Some (Error e);
        errors := e :: !errors
      end
  ) chunks;
  (if !rmain = None then
     let e = Error.parse_error ~loc:None ~msg:"missing `main` declaration" in
     errors := e :: !errors);
  { rdecls = List.rev !rdecls;
    rmain = !rmain;
    errors = List.rev !errors }

(* ================================================================== *)
(* Tests                                                               *)
(* ================================================================== *)

module Test = struct
  let test = []
end
