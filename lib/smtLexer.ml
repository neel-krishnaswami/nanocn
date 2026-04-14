type token = SmtParser.token

let pos_of_lexbuf buf =
  let start_pos, end_pos = Sedlexing.lexing_positions buf in
  let open Lexing in
  SourcePos.create
    ~file:start_pos.pos_fname
    ~start_line:start_pos.pos_lnum
    ~start_col:(start_pos.pos_cnum - start_pos.pos_bol)
    ~end_line:end_pos.pos_lnum
    ~end_col:(end_pos.pos_cnum - end_pos.pos_bol)

let from_string s ~file =
  let buf = Sedlexing.Utf8.from_string s in
  Sedlexing.set_filename buf file;
  buf

(* §3.1 lexical classes *)
let digit       = [%sedlex.regexp? '0' .. '9']
let letter      = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let sym_punct   = [%sedlex.regexp?
  '~' | '!' | '@' | '$' | '%' | '^' | '&' | '*' | '_'
  | '+' | '=' | '<' | '>' | '.' | '?' | '/' | '-']
let sym_char    = [%sedlex.regexp? letter | digit | sym_punct]
(* simple_symbol: non-empty, first char not a digit. *)
let simple_sym  = [%sedlex.regexp? (letter | sym_punct), Star sym_char]
let numeral     = [%sedlex.regexp? '0' | ('1' .. '9', Star digit)]
(* decimal: <numeral> . <digit>+ (per §3.1, trailing part is digits). *)
let decimal     = [%sedlex.regexp? numeral, '.', Plus digit]
let hex_digit   = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let hexadecimal = [%sedlex.regexp? "#x", Plus hex_digit]
let binary      = [%sedlex.regexp? "#b", Plus ('0' | '1')]
let whitespace  = [%sedlex.regexp? ' ' | '\t' | '\n' | '\r']

(* Read the contents of a string literal, starting just after the
   opening double-quote. The only recognised escape inside SMT-LIB
   string literals is the doubled quote [""], which stands for a
   single quote character; anything else (including raw newlines) is
   emitted verbatim. *)
let rec read_string acc buf =
  match%sedlex buf with
  | '"', '"' ->
    Buffer.add_char acc '"';
    read_string acc buf
  | '"' ->
    SmtParser.STRING (Buffer.contents acc)
  | eof ->
    failwith (Format.asprintf "unterminated string literal at %a"
                SourcePos.print (pos_of_lexbuf buf))
  | any ->
    Buffer.add_string acc (Sedlexing.Utf8.lexeme buf);
    read_string acc buf
  | _ ->
    failwith "SmtLexer.read_string: unreachable"

(* Read the contents of a quoted symbol, starting just after the
   opening pipe. Quoted symbols may not contain a backslash or pipe
   per §3.1. *)
let rec read_quoted_symbol acc buf =
  match%sedlex buf with
  | '|' ->
    SmtParser.SYMBOL (Buffer.contents acc)
  | '\\' ->
    failwith (Format.asprintf "backslash is not allowed in a quoted symbol at %a"
                SourcePos.print (pos_of_lexbuf buf))
  | eof ->
    failwith (Format.asprintf "unterminated quoted symbol at %a"
                SourcePos.print (pos_of_lexbuf buf))
  | any ->
    Buffer.add_string acc (Sedlexing.Utf8.lexeme buf);
    read_quoted_symbol acc buf
  | _ ->
    failwith "SmtLexer.read_quoted_symbol: unreachable"

let rec token buf =
  match%sedlex buf with
  | Plus whitespace -> token buf
  | ';', Star (Compl ('\n' | '\r')) -> token buf
  | '(' -> SmtParser.LPAREN
  | ')' -> SmtParser.RPAREN
  | decimal ->
    SmtParser.DECIMAL (Sedlexing.Utf8.lexeme buf)
  | numeral ->
    SmtParser.NUMERAL (Sedlexing.Utf8.lexeme buf)
  | hexadecimal ->
    let s = Sedlexing.Utf8.lexeme buf in
    SmtParser.HEXADECIMAL (String.sub s 2 (String.length s - 2))
  | binary ->
    let s = Sedlexing.Utf8.lexeme buf in
    SmtParser.BINARY (String.sub s 2 (String.length s - 2))
  | '"' ->
    read_string (Buffer.create 16) buf
  | '|' ->
    read_quoted_symbol (Buffer.create 16) buf
  | ':', simple_sym ->
    let s = Sedlexing.Utf8.lexeme buf in
    SmtParser.KEYWORD (String.sub s 1 (String.length s - 1))
  | simple_sym ->
    let s = Sedlexing.Utf8.lexeme buf in
    (match SmtAtom.reserved_of_string s with
     | Some r -> SmtParser.RESERVED r
     | None -> SmtParser.SYMBOL s)
  | eof -> SmtParser.EOF
  | _ ->
    failwith (Format.asprintf "unexpected character at %a"
                SourcePos.print (pos_of_lexbuf buf))
