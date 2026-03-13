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

  let mk_surfexpr startpos endpos s =
    SurfExpr.In (s, loc_obj startpos endpos)

  let mk_sort startpos endpos s =
    Sort.In (s, loc_obj startpos endpos)

  let mk_pat startpos endpos s =
    Pat.In (s, loc_obj startpos endpos)

  let label s =
    match Label.of_string s with
    | Ok l -> l
    | Error _ -> failwith ("invalid label: " ^ s)

  let dsort s =
    match Dsort.of_string s with
    | Ok d -> d
    | Error _ -> failwith ("invalid datasort name: " ^ s)

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
%token SPEC SORT TYPE TAKE RETURN PRED OF OWN EQEQ EQ
%token PLUS MINUS STAR SLASH
%token AMPAMP BARBAR
%token LBRACKET RBRACKET LPAREN RPAREN LBRACE RBRACE
%token COMMA SEMICOLON EQUAL COLON ARROW BAR
%token EOF

%start <SurfExpr.se> program
%start <Sort.sort> sort_eof
%start <SurfExpr.se Prog.t> prog_eof
%start <SurfExpr.se Prog.decl> repl_decl
%start <Var.t * SurfExpr.se> repl_let

%%

(* A variable occurrence with its source position. *)
%inline ident_var:
  | x = IDENT { mk_var $startpos $endpos x }

program:
  | e = expr; EOF { e }

sort_eof:
  | s = sort; EOF { s }

prog_eof:
  | ds = list(decl); MAIN; COLON; ms = sort; LBRACKET; meff = effect; RBRACKET; EQUAL; e = expr; EOF
    { { Prog.decls = ds; main = e; main_sort = ms; main_eff = meff;
        loc = mk_loc $startpos $endpos } }

repl_decl:
  | d = decl; EOF { d }

repl_let:
  | LET; x = ident_var; EQUAL; e = expr; EOF { (x, e) }

(* ===== Declarations ===== *)

decl:
  | FUN; f = ident_var; COLON; a = sort; ARROW; b = sort; LBRACKET; eff = effect; RBRACKET; EQUAL; LBRACE; bs = separated_nonempty_list(BAR, branch); RBRACE
    { Prog.FunDecl { name = f; arg_sort = a; ret_sort = b; eff;
        branches = bs; loc = mk_loc $startpos $endpos } }
  | SORT; d = IDENT; LPAREN; params = separated_nonempty_list(COMMA, IDENT); RPAREN; EQUAL; LBRACE; cs = separated_nonempty_list(BAR, ctor_decl); RBRACE
    { let decl = DsortDecl.({
        name = dsort d;
        params = List.map Tvar.of_string params;
        ctors = cs;
        loc = mk_loc $startpos $endpos;
      }) in
      Prog.SortDecl (DsortDecl.resolve_tvars decl) }
  | SORT; d = IDENT; EQUAL; LBRACE; cs = separated_nonempty_list(BAR, ctor_decl); RBRACE
    { Prog.SortDecl DsortDecl.({
        name = dsort d;
        params = [];
        ctors = cs;
        loc = mk_loc $startpos $endpos;
      }) }
  | TYPE; d = IDENT; LPAREN; params = separated_nonempty_list(COMMA, IDENT); RPAREN; EQUAL; LBRACE; cs = separated_nonempty_list(BAR, type_ctor_decl); RBRACE
    { let decl = DtypeDecl.({
        name = dsort d;
        params = List.map Tvar.of_string params;
        ctors = cs;
        loc = mk_loc $startpos $endpos;
      }) in
      Prog.TypeDecl (DtypeDecl.resolve_tvars decl) }
  | TYPE; d = IDENT; EQUAL; LBRACE; cs = separated_nonempty_list(BAR, type_ctor_decl); RBRACE
    { Prog.TypeDecl DtypeDecl.({
        name = dsort d;
        params = [];
        ctors = cs;
        loc = mk_loc $startpos $endpos;
      }) }

branch:
  | p = pat; ARROW; e = expr
    { (p, e) }

ctor_decl:
  | l = LABEL; COLON; s = sort { (label l, s) }

type_ctor_decl:
  | l = LABEL; COLON; t = typ { (label l, t) }

(* ===== Computational types (for primitive type arguments only) ===== *)

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
  | d = IDENT; LPAREN; ts = separated_nonempty_list(COMMA, typ); RPAREN
    { mk_typ $startpos $endpos (Typ.App (dsort d, ts)) }
  | d = IDENT
    { mk_typ $startpos $endpos (Typ.App (dsort d, [])) }

effect:
  | PURE   { Effect.Pure }
  | IMPURE { Effect.Impure }
  | SPEC   { Effect.Spec }

(* ===== Sorts ===== *)

sort:
  | PRED; s = atomic_sort
    { mk_sort $startpos $endpos (Sort.Pred s) }
  | PTR; s = atomic_sort
    { mk_sort $startpos $endpos (Sort.Ptr s) }
  | s = atomic_sort
    { s }

atomic_sort:
  | LPAREN; RPAREN
    { mk_sort $startpos $endpos (Sort.Record []) }
  | LPAREN; s = sort; RPAREN
    { s }
  | LPAREN; s = sort; STAR; ss = separated_nonempty_list(STAR, sort); RPAREN
    { mk_sort $startpos $endpos (Sort.Record (s :: ss)) }
  | INT_KW
    { mk_sort $startpos $endpos Sort.Int }
  | BOOL_KW
    { mk_sort $startpos $endpos Sort.Bool }
  | d = IDENT; LPAREN; ss = separated_nonempty_list(COMMA, sort); RPAREN
    { mk_sort $startpos $endpos (Sort.App (dsort d, ss)) }
  | d = IDENT
    { mk_sort $startpos $endpos (Sort.App (dsort d, [])) }

(* ===== Patterns ===== *)

pat:
  | l = LABEL; p = atomic_pat
    { mk_pat $startpos $endpos (Pat.Con (label l, p)) }
  | p = atomic_pat
    { p }

atomic_pat:
  | x = ident_var
    { mk_pat $startpos $endpos (Pat.Var x) }
  | LPAREN; RPAREN
    { mk_pat $startpos $endpos (Pat.Tuple []) }
  | LPAREN; p = pat; RPAREN
    { p }
  | LPAREN; p = pat; COMMA; ps = separated_nonempty_list(COMMA, pat); RPAREN
    { mk_pat $startpos $endpos (Pat.Tuple (p :: ps)) }

(* ===== Unified expressions ===== *)

expr:
  | e = seq_expr; COLON; s = sort; LBRACKET; eff = effect; RBRACKET
    { mk_surfexpr $startpos $endpos (SurfExpr.Annot (e, s, eff)) }
  | e = seq_expr
    { e }

seq_expr:
  | LET; p = pat; EQUAL; e1 = expr; SEMICOLON; e2 = seq_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Let (p, e1, e2)) }
  | TAKE; p = pat; EQUAL; e1 = expr; SEMICOLON; e2 = seq_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Take (p, e1, e2)) }
  | RETURN; e = app_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Return e) }
  | CASE; e = app_expr; OF; LBRACE; bs = separated_nonempty_list(BAR, case_branch); RBRACE
    { mk_surfexpr $startpos $endpos (SurfExpr.Case (e, bs)) }
  | ITER; LPAREN; p = pat; EQUAL; e1 = expr; RPAREN; LBRACE; e2 = expr; RBRACE
    { mk_surfexpr $startpos $endpos (SurfExpr.Iter (p, e1, e2)) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = seq_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.If (e1, e2, e3)) }
  | e = or_expr
    { e }

case_branch:
  | p = pat; ARROW; e = expr
    { (p, e) }

or_expr:
  | e1 = or_expr; BARBAR; e2 = and_expr
    { let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.App (Prim.Or, tup)) }
  | e = and_expr
    { e }

and_expr:
  | e1 = and_expr; AMPAMP; e2 = eq_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.And (e1, e2)) }
  | e = eq_expr
    { e }

eq_expr:
  | e1 = add_expr; EQEQ; e2 = add_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Eq (e1, e2)) }
  | e = add_expr
    { e }

add_expr:
  | e1 = add_expr; PLUS; e2 = mul_expr
    { let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.App (Prim.Add, tup)) }
  | e1 = add_expr; MINUS; e2 = mul_expr
    { let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.App (Prim.Sub, tup)) }
  | e = mul_expr
    { e }

mul_expr:
  | e1 = mul_expr; STAR; e2 = app_expr
    { let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.App (Prim.Mul, tup)) }
  | e1 = mul_expr; SLASH; e2 = app_expr
    { let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.App (Prim.Div, tup)) }
  | e = app_expr
    { e }

app_expr:
  | l = LABEL; e = app_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Inject (label l, e)) }
  | p = state_prim; LBRACKET; t = typ; RBRACKET; e = simple_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.App (p t, e)) }
  | NOT_KW; e = simple_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Not e) }
  | f = ident_var; e = simple_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Call (f, e)) }
  | e = simple_expr
    { e }

state_prim:
  | EQ  { fun ty -> Prim.Eq ty }
  | SET { fun ty -> Prim.Set ty }
  | GET { fun ty -> Prim.Get ty }
  | NEW { fun ty -> Prim.New ty }
  | DEL { fun ty -> Prim.Del ty }

simple_expr:
  | x = ident_var
    { mk_surfexpr $startpos $endpos (SurfExpr.Var x) }
  | n = INT_LIT
    { mk_surfexpr $startpos $endpos (SurfExpr.IntLit n) }
  | TRUE
    { mk_surfexpr $startpos $endpos (SurfExpr.BoolLit true) }
  | FALSE
    { mk_surfexpr $startpos $endpos (SurfExpr.BoolLit false) }
  | OWN; LBRACKET; s = sort; RBRACKET
    { mk_surfexpr $startpos $endpos (SurfExpr.Own s) }
  | LPAREN; RPAREN
    { mk_surfexpr $startpos $endpos (SurfExpr.Tuple []) }
  | LPAREN; e = expr; RPAREN
    { e }
  | LPAREN; e = expr; COMMA; es = separated_nonempty_list(COMMA, expr); RPAREN
    { mk_surfexpr $startpos $endpos (SurfExpr.Tuple (e :: es)) }
