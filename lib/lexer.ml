type token = Parser.token

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

let digit = [%sedlex.regexp? '0' .. '9']
let lower = [%sedlex.regexp? 'a' .. 'z']
let upper = [%sedlex.regexp? 'A' .. 'Z']
let ident_rest = [%sedlex.regexp? lower | upper | digit | '_' | '\'']
let ident = [%sedlex.regexp? lower, Star ident_rest]
let label = [%sedlex.regexp? upper, Plus ident_rest]
let integer = [%sedlex.regexp? Plus digit]
let whitespace = [%sedlex.regexp? Plus (' ' | '\t')]
let newline = [%sedlex.regexp? '\n' | "\r\n"]

let keyword_or_ident s =
  match s with
  | "let" -> Parser.LET
  | "case" -> Parser.CASE
  | "iter" -> Parser.ITER
  | "fun" -> Parser.FUN
  | "main" -> Parser.MAIN
  | "int" -> Parser.INT_KW
  | "ptr" -> Parser.PTR
  | "pure" -> Parser.PURE
  | "impure" -> Parser.IMPURE
  | "bool" -> Parser.BOOL_KW
  | "true" -> Parser.TRUE
  | "false" -> Parser.FALSE
  | "if" -> Parser.IF
  | "then" -> Parser.THEN
  | "else" -> Parser.ELSE
  | "not" -> Parser.NOT_KW
  | "spec" -> Parser.SPEC
  | "sort" -> Parser.SORT
  | "take" -> Parser.TAKE
  | "return" -> Parser.RETURN
  | "pred" -> Parser.PRED
  | "of" -> Parser.OF
  | "type" -> Parser.TYPE
  | _ -> Parser.IDENT s

let keyword_or_label s =
  match s with
  | "Eq" -> Parser.EQ
  | "Set" -> Parser.SET
  | "Get" -> Parser.GET
  | "New" -> Parser.NEW
  | "Del" -> Parser.DEL
  | "Own" -> Parser.OWN
  | _ -> Parser.LABEL s

let rec token buf =
  match%sedlex buf with
  | whitespace -> token buf
  | newline -> token buf
  | "//" , Star (Compl '\n') -> token buf
  | integer -> Parser.INT_LIT (int_of_string (Sedlexing.Utf8.lexeme buf))
  | ident -> keyword_or_ident (Sedlexing.Utf8.lexeme buf)
  | label -> keyword_or_label (Sedlexing.Utf8.lexeme buf)
  | '[' -> Parser.LBRACKET
  | ']' -> Parser.RBRACKET
  | '(' -> Parser.LPAREN
  | ')' -> Parser.RPAREN
  | '{' -> Parser.LBRACE
  | '}' -> Parser.RBRACE
  | ',' -> Parser.COMMA
  | ';' -> Parser.SEMICOLON
  | "<=" -> Parser.LESSEQ
  | ">=" -> Parser.GREATEREQ
  | '<' -> Parser.LESS
  | '>' -> Parser.GREATER
  | "==" -> Parser.EQEQ
  | '=' -> Parser.EQUAL
  | ':' -> Parser.COLON
  | "->" -> Parser.ARROW
  | "||" -> Parser.BARBAR
  | "&&" -> Parser.AMPAMP
  | '|' -> Parser.BAR
  | '+' -> Parser.PLUS
  | '-' -> Parser.MINUS
  | '*' -> Parser.STAR
  | '/' -> Parser.SLASH
  | eof -> Parser.EOF
  | _ -> failwith (Format.asprintf "unexpected character at %a" SourcePos.print (pos_of_lexbuf buf))
