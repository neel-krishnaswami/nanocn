%{
  let mk_loc startpos endpos =
    let open Lexing in
    SourcePos.create
      ~file:startpos.pos_fname
      ~start_line:startpos.pos_lnum
      ~start_col:(startpos.pos_cnum - startpos.pos_bol)
      ~end_line:endpos.pos_lnum
      ~end_col:(endpos.pos_cnum - endpos.pos_bol)

  let loc_obj startpos endpos =
    let loc = mk_loc startpos endpos in
    object method loc = loc end

  let mk_typ startpos endpos s =
    Typ.In (s, loc_obj startpos endpos)

  let mk_expr startpos endpos s =
    Expr.In (s, loc_obj startpos endpos)

  let label s =
    match Label.of_string s with
    | Ok l -> l
    | Error _ -> failwith ("invalid label: " ^ s)

  let mk_var startpos endpos s =
    Var.of_string s (mk_loc startpos endpos)
%}

%token <int> INT_LIT
%token <string> IDENT
%token <string> LABEL
%token LET CASE ITER FUN MAIN
%token INT_KW BOOL_KW PTR
%token PURE IMPURE
%token SET GET NEW DEL
%token TRUE FALSE IF THEN ELSE NOT_KW
%token PLUS MINUS STAR SLASH
%token AMPAMP BARBAR
%token LBRACKET RBRACKET LPAREN RPAREN LBRACE RBRACE
%token COMMA SEMICOLON EQUAL COLON ARROW BAR
%token EOF

%start <Expr.expr> program
%start <Typ.ty> typ_eof
%start <Expr.expr Prog.t> prog_eof
%start <Expr.expr Prog.decl> repl_decl
%start <Var.t * Expr.expr> repl_let

%%

(* A variable occurrence with its source position. *)
%inline ident_var:
  | x = IDENT { mk_var $startpos $endpos x }

program:
  | e = expr; EOF { e }

typ_eof:
  | t = typ; EOF { t }

prog_eof:
  | ds = list(decl); MAIN; EQUAL; e = expr; EOF
    { { Prog.decls = ds; main = e; loc = mk_loc $startpos $endpos } }

repl_decl:
  | d = decl; EOF { d }

repl_let:
  | LET; x = ident_var; EQUAL; e = expr; EOF { (x, e) }

decl:
  | FUN; f = ident_var; LPAREN; x = ident_var; COLON; a = typ; RPAREN; ARROW; b = typ; LBRACKET; eff = effect; RBRACKET; LBRACE; body = expr; RBRACE
    { Prog.FunDecl { name = f; param = x;
        arg_ty = a; ret_ty = b; eff; body; loc = mk_loc $startpos $endpos } }

typ:
  | PTR; t = atomic_typ
    { mk_typ $startpos $endpos (Typ.Ptr t) }
  | t = atomic_typ
    { t }

atomic_typ:
  | LPAREN; RPAREN
    { mk_typ $startpos $endpos (Typ.Record []) }
  | LPAREN; t = typ; RPAREN
    { t }
  | LPAREN; t = typ; STAR; ts = separated_nonempty_list(STAR, typ); RPAREN
    { mk_typ $startpos $endpos (Typ.Record (t :: ts)) }
  | INT_KW
    { mk_typ $startpos $endpos Typ.Int }
  | BOOL_KW
    { mk_typ $startpos $endpos Typ.Bool }
  | LBRACE; cs = separated_nonempty_list(BAR, label_typ); RBRACE
    { mk_typ $startpos $endpos (Typ.Sum cs) }

label_typ:
  | l = LABEL; COLON; t = typ { (label l, t) }

effect:
  | PURE   { Effect.Pure }
  | IMPURE { Effect.Impure }

expr:
  | e = seq_expr; COLON; t = typ; LBRACKET; eff = effect; RBRACKET
    { mk_expr $startpos $endpos (Expr.Annot (e, t, eff)) }
  | e = seq_expr
    { e }

seq_expr:
  | LET; x = ident_var; EQUAL; e1 = expr; SEMICOLON; e2 = seq_expr
    { mk_expr $startpos $endpos (Expr.Let (x, e1, e2)) }
  | LET; LPAREN; xs = separated_list(COMMA, ident_var); RPAREN; EQUAL; e1 = expr; SEMICOLON; e2 = seq_expr
    { mk_expr $startpos $endpos (Expr.LetTuple (xs, e1, e2)) }
  | CASE; e = app_expr; LBRACE; bs = separated_nonempty_list(BAR, branch); RBRACE
    { mk_expr $startpos $endpos (Expr.Case (e, bs)) }
  | ITER; LPAREN; x = ident_var; EQUAL; e1 = expr; RPAREN; LBRACE; e2 = expr; RBRACE
    { mk_expr $startpos $endpos (Expr.Iter (x, e1, e2)) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = seq_expr
    { mk_expr $startpos $endpos (Expr.If (e1, e2, e3)) }
  | e = or_expr
    { e }

branch:
  | l = LABEL; x = ident_var; ARROW; e = expr
    { (label l, x, e) }

or_expr:
  | e1 = or_expr; BARBAR; e2 = and_expr
    { let tup = mk_expr $startpos $endpos (Expr.Tuple [e1; e2]) in
      mk_expr $startpos $endpos (Expr.App (Prim.Or, tup)) }
  | e = and_expr
    { e }

and_expr:
  | e1 = and_expr; AMPAMP; e2 = add_expr
    { let tup = mk_expr $startpos $endpos (Expr.Tuple [e1; e2]) in
      mk_expr $startpos $endpos (Expr.App (Prim.And, tup)) }
  | e = add_expr
    { e }

add_expr:
  | e1 = add_expr; PLUS; e2 = mul_expr
    { let tup = mk_expr $startpos $endpos (Expr.Tuple [e1; e2]) in
      mk_expr $startpos $endpos (Expr.App (Prim.Add, tup)) }
  | e1 = add_expr; MINUS; e2 = mul_expr
    { let tup = mk_expr $startpos $endpos (Expr.Tuple [e1; e2]) in
      mk_expr $startpos $endpos (Expr.App (Prim.Sub, tup)) }
  | e = mul_expr
    { e }

mul_expr:
  | e1 = mul_expr; STAR; e2 = app_expr
    { let tup = mk_expr $startpos $endpos (Expr.Tuple [e1; e2]) in
      mk_expr $startpos $endpos (Expr.App (Prim.Mul, tup)) }
  | e1 = mul_expr; SLASH; e2 = app_expr
    { let tup = mk_expr $startpos $endpos (Expr.Tuple [e1; e2]) in
      mk_expr $startpos $endpos (Expr.App (Prim.Div, tup)) }
  | e = app_expr
    { e }

app_expr:
  | l = LABEL; e = app_expr
    { mk_expr $startpos $endpos (Expr.Inject (label l, e)) }
  | p = state_prim; LBRACKET; t = typ; RBRACKET; e = simple_expr
    { mk_expr $startpos $endpos (Expr.App (p t, e)) }
  | NOT_KW; e = simple_expr
    { mk_expr $startpos $endpos (Expr.App (Prim.Not, e)) }
  | f = ident_var; e = simple_expr
    { mk_expr $startpos $endpos (Expr.Call (f, e)) }
  | e = simple_expr
    { e }

state_prim:
  | SET { fun ty -> Prim.Set ty }
  | GET { fun ty -> Prim.Get ty }
  | NEW { fun ty -> Prim.New ty }
  | DEL { fun ty -> Prim.Del ty }

simple_expr:
  | x = ident_var
    { mk_expr $startpos $endpos (Expr.Var x) }
  | n = INT_LIT
    { mk_expr $startpos $endpos (Expr.IntLit n) }
  | TRUE
    { mk_expr $startpos $endpos (Expr.BoolLit true) }
  | FALSE
    { mk_expr $startpos $endpos (Expr.BoolLit false) }
  | LPAREN; RPAREN
    { mk_expr $startpos $endpos (Expr.Tuple []) }
  | LPAREN; e = expr; RPAREN
    { e }
  | LPAREN; e = expr; COMMA; es = separated_nonempty_list(COMMA, expr); RPAREN
    { mk_expr $startpos $endpos (Expr.Tuple (e :: es)) }
