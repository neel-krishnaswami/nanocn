%{
  let mk_loc startpos endpos =
    let open Lexing in
    SourcePos.create
      ~file:startpos.pos_fname
      ~start_line:startpos.pos_lnum
      ~start_col:(startpos.pos_cnum - startpos.pos_bol)
      ~end_line:endpos.pos_lnum
      ~end_col:(endpos.pos_cnum - endpos.pos_bol)

  let loc_info startpos endpos =
    let loc = mk_loc startpos endpos in
    object method loc = loc end
%}

%token LPAREN RPAREN EOF
%token <string> NUMERAL
%token <string> DECIMAL
%token <string> HEXADECIMAL
%token <string> BINARY
%token <string> STRING
%token <string> SYMBOL
%token <string> KEYWORD
%token <SmtAtom.reserved> RESERVED

%start <SmtSexp.sexp>      sexp_eof
%start <SmtSexp.sexp list> sexps_eof

%%

sexp_eof:
  | s = sexp; EOF { s }

sexps_eof:
  | ss = list(sexp); EOF { ss }

sexp:
  | a = atom_token
      { SmtSexp.atom (loc_info $startpos $endpos) a }
  | LPAREN; ss = list(sexp); RPAREN
      { SmtSexp.list (loc_info $startpos $endpos) ss }

atom_token:
  | n = NUMERAL     { SmtAtom.Numeral n }
  | d = DECIMAL     { SmtAtom.Decimal d }
  | h = HEXADECIMAL { SmtAtom.Hexadecimal h }
  | b = BINARY      { SmtAtom.Binary b }
  | s = STRING      { SmtAtom.String s }
  | s = SYMBOL      { SmtAtom.Symbol s }
  | k = KEYWORD     { SmtAtom.Keyword k }
  | r = RESERVED    { SmtAtom.Reserved r }
