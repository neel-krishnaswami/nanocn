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

  let mk_sort startpos endpos s =
    Sort.In (s, loc_obj startpos endpos)

  let mk_pat startpos endpos s =
    Pat.In (s, loc_obj startpos endpos)

  let mk_surfexpr startpos endpos s =
    SurfExpr.In (s, loc_obj startpos endpos)

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

  (** Built-in spec function names for arithmetic desugaring *)
  let spec_arith_var name startpos endpos =
    Var.of_string name (mk_loc startpos endpos)
%}

%token <int> INT_LIT
%token <string> IDENT
%token <string> LABEL
%token LET CASE ITER FUN MAIN
%token INT_KW BOOL_KW PTR
%token PURE IMPURE
%token SET GET NEW DEL
%token TRUE FALSE IF THEN ELSE NOT_KW
%token SPEC SORT TAKE RETURN PRED LOC OF OWN EQEQ
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

(* ===== Declarations ===== *)

decl:
  | FUN; f = ident_var; LPAREN; x = ident_var; COLON; a = typ; RPAREN; ARROW; b = typ; LBRACKET; eff = effect; RBRACKET; LBRACE; body = expr; RBRACE
    { Prog.FunDecl { name = f; param = x;
        arg_ty = a; ret_ty = b; eff; body; loc = mk_loc $startpos $endpos } }
  | SPEC; f = ident_var; COLON; a = sort; ARROW; b = sort; EQUAL; LBRACE; bs = separated_nonempty_list(BAR, spec_branch); RBRACE
    { Prog.SpecFunDecl { name = f; arg_sort = a; ret_sort = b;
        branches = bs; loc = mk_loc $startpos $endpos } }
  | SPEC; f = ident_var; COLON; s = sort; EQUAL; body = spec_expr
    { Prog.SpecDefDecl { name = f; sort = s; body;
        loc = mk_loc $startpos $endpos } }
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

spec_branch:
  | p = pat; ARROW; e = spec_expr
    { (p, e) }

ctor_decl:
  | l = LABEL; COLON; s = sort { (label l, s) }

(* ===== Computational types ===== *)

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

(* ===== Sorts ===== *)

sort:
  | PRED; s = atomic_sort
    { mk_sort $startpos $endpos (Sort.Pred s) }
  | s = atomic_sort
    { s }

atomic_sort:
  | LPAREN; RPAREN
    { mk_sort $startpos $endpos (Sort.Record []) }
  | LPAREN; s = sort; RPAREN
    { s }
  | LPAREN; s = sort; COMMA; ss = separated_nonempty_list(COMMA, sort); RPAREN
    { mk_sort $startpos $endpos (Sort.Record (s :: ss)) }
  | INT_KW
    { mk_sort $startpos $endpos Sort.Int }
  | BOOL_KW
    { mk_sort $startpos $endpos Sort.Bool }
  | LOC
    { mk_sort $startpos $endpos Sort.Loc }
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

(* ===== Spec expressions ===== *)

spec_expr:
  | e = spec_seq_expr; COLON; s = sort
    { mk_surfexpr $startpos $endpos (SurfExpr.Annot (e, s)) }
  | e = spec_seq_expr
    { e }

spec_seq_expr:
  | LET; p = pat; EQUAL; e1 = spec_expr; SEMICOLON; e2 = spec_seq_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Let (p, e1, e2)) }
  | TAKE; p = pat; EQUAL; e1 = spec_expr; SEMICOLON; e2 = spec_seq_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Take (p, e1, e2)) }
  | RETURN; e = spec_app_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Return e) }
  | CASE; e = spec_app_expr; OF; LBRACE; bs = separated_nonempty_list(BAR, spec_case_branch); RBRACE
    { mk_surfexpr $startpos $endpos (SurfExpr.Case (e, bs)) }
  | IF; e1 = spec_expr; THEN; e2 = spec_expr; ELSE; e3 = spec_seq_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.If (e1, e2, e3)) }
  | e = spec_and_expr
    { e }

spec_case_branch:
  | p = pat; ARROW; e = spec_expr
    { (p, e) }

spec_and_expr:
  | e1 = spec_and_expr; AMPAMP; e2 = spec_eq_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.And (e1, e2)) }
  | e = spec_eq_expr
    { e }

spec_eq_expr:
  | e1 = spec_add_expr; EQEQ; e2 = spec_add_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Eq (e1, e2)) }
  | e = spec_add_expr
    { e }

spec_add_expr:
  | e1 = spec_add_expr; PLUS; e2 = spec_mul_expr
    { let name = spec_arith_var "__add" $startpos $endpos in
      let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.Call (name, tup)) }
  | e1 = spec_add_expr; MINUS; e2 = spec_mul_expr
    { let name = spec_arith_var "__sub" $startpos $endpos in
      let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.Call (name, tup)) }
  | e = spec_mul_expr
    { e }

spec_mul_expr:
  | e1 = spec_mul_expr; STAR; e2 = spec_app_expr
    { let name = spec_arith_var "__mul" $startpos $endpos in
      let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.Call (name, tup)) }
  | e1 = spec_mul_expr; SLASH; e2 = spec_app_expr
    { let name = spec_arith_var "__div" $startpos $endpos in
      let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.Call (name, tup)) }
  | e = spec_app_expr
    { e }

spec_app_expr:
  | NOT_KW; e = spec_simple_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Not e) }
  | l = LABEL; e = spec_simple_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Inject (label l, e)) }
  | f = ident_var; e = spec_simple_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Call (f, e)) }
  | e = spec_simple_expr
    { e }

spec_simple_expr:
  | x = ident_var
    { mk_surfexpr $startpos $endpos (SurfExpr.Var x) }
  | n = INT_LIT
    { mk_surfexpr $startpos $endpos (SurfExpr.IntLit n) }
  | TRUE
    { mk_surfexpr $startpos $endpos (SurfExpr.BoolLit true) }
  | FALSE
    { mk_surfexpr $startpos $endpos (SurfExpr.BoolLit false) }
  | OWN; LBRACKET; _s = sort; RBRACKET
    { mk_surfexpr $startpos $endpos (SurfExpr.Const (mk_var $startpos $endpos "__own")) }
  | LPAREN; RPAREN
    { mk_surfexpr $startpos $endpos (SurfExpr.Tuple []) }
  | LPAREN; e = spec_expr; RPAREN
    { e }
  | LPAREN; e = spec_expr; COMMA; es = separated_nonempty_list(COMMA, spec_expr); RPAREN
    { mk_surfexpr $startpos $endpos (SurfExpr.Tuple (e :: es)) }

(* ===== Computational expressions ===== *)

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
