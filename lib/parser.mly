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

  let mk_surfexpr startpos endpos s =
    SurfExpr.mk (loc_obj startpos endpos) s

  let mk_sort startpos endpos s =
    Sort.mk (loc_obj startpos endpos) s

  let mk_pat startpos endpos s =
    Pat.mk (loc_obj startpos endpos) s

  let label s =
    match Label.of_string s with
    | Ok l -> l
    | Error _ -> failwith ("invalid label: " ^ s)

  let dsort s =
    match Dsort.of_string s with
    | Ok d -> d
    | Error _ -> failwith ("invalid datasort name: " ^ s)

%}

%token <int> INT_LIT
%token <string> IDENT
%token <string> LABEL
%token LET CASE ITER FUN RFUN MAIN
%token INT_KW BOOL_KW PTR
%token PURE IMPURE
%token SET GET NEW DEL
%token TRUE FALSE IF THEN ELSE NOT_KW
%token SPEC SORT TYPE TAKE DO RETURN PRED OF OWN EQEQ EQ
%token PLUS MINUS STAR SLASH
%token AMPAMP BARBAR
%token LBRACKET RBRACKET LPAREN RPAREN LBRACE RBRACE
%token LESS LESSEQ GREATER GREATEREQ
%token COMMA SEMICOLON EQUAL COLON ARROW BAR
%token EXFALSO AUTO UNFOLD OPEN_RET OPEN_TAKE MAKE_RET MAKE_TAKE LOG RES FORALL AT
%token EOF

%start <SurfExpr.parsed_se> program
%start <Sort.sort> sort_eof
%start <(SurfExpr.parsed_se, SourcePos.t, string) Prog.t> prog_eof
%start <(SurfExpr.parsed_se, SourcePos.t, string) Prog.decl> repl_decl
%start <(string * SourcePos.t * SurfExpr.parsed_se)> repl_let
%start <RProg.raw_parsed> rprog_eof

%%

(* A variable occurrence — just the string name. *)
%inline ident_var:
  | x = IDENT { x }

program:
  | e = expr; EOF { e }

sort_eof:
  | s = sort; EOF { s }

prog_eof:
  | ds = list(decl); MAIN; COLON; ms = sort; LBRACKET; meff = eff_level; RBRACKET; EQUAL; e = expr; EOF
    { { Prog.decls = ds; main = e; main_sort = ms; main_eff = meff;
        loc = mk_loc $startpos $endpos } }

repl_decl:
  | d = decl; EOF { d }

repl_let:
  | LET; x = ident_var; EQUAL; e = expr; EOF
    { (x, mk_loc $startpos(x) $endpos(x), e) }

(* ===== Declarations ===== *)

decl:
  | FUN; f = ident_var; COLON; a = sort; ARROW; b = sort; LBRACKET; eff = eff_level; RBRACKET; EQUAL; LBRACE; bs = separated_nonempty_list(BAR, branch); RBRACE
    { Prog.FunDecl { name = f; arg_sort = a; ret_sort = b; eff;
        branches = bs; loc = mk_loc $startpos $endpos } }
  | SORT; d = LABEL; LPAREN; params = separated_nonempty_list(COMMA, IDENT); RPAREN; EQUAL; LBRACE; cs = separated_nonempty_list(BAR, ctor_decl); RBRACE
    { Prog.SortDecl DsortDecl.({
        name = dsort d;
        params = List.map Tvar.of_string params;
        ctors = cs;
        loc = mk_loc $startpos $endpos;
      }) }
  | SORT; d = LABEL; EQUAL; LBRACE; cs = separated_nonempty_list(BAR, ctor_decl); RBRACE
    { Prog.SortDecl DsortDecl.({
        name = dsort d;
        params = [];
        ctors = cs;
        loc = mk_loc $startpos $endpos;
      }) }
  | TYPE; d = LABEL; LPAREN; params = separated_nonempty_list(COMMA, IDENT); RPAREN; EQUAL; LBRACE; cs = separated_nonempty_list(BAR, type_ctor_decl); RBRACE
    { Prog.TypeDecl DtypeDecl.({
        name = dsort d;
        params = List.map Tvar.of_string params;
        ctors = cs;
        loc = mk_loc $startpos $endpos;
      }) }
  | TYPE; d = LABEL; EQUAL; LBRACE; cs = separated_nonempty_list(BAR, type_ctor_decl); RBRACE
    { Prog.TypeDecl DtypeDecl.({
        name = dsort d;
        params = [];
        ctors = cs;
        loc = mk_loc $startpos $endpos;
      }) }

branch:
  | p = pat; ARROW; e = expr
    { (p, e, mk_loc $startpos $endpos) }

ctor_decl:
  | l = LABEL; COLON; s = sort { (label l, s) }

type_ctor_decl:
  | l = LABEL; COLON; s = sort { (label l, s) }

eff_level:
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
  | d = LABEL; LPAREN; ss = separated_nonempty_list(COMMA, sort); RPAREN
    { mk_sort $startpos $endpos (Sort.App (dsort d, ss)) }
  | d = LABEL
    { mk_sort $startpos $endpos (Sort.App (dsort d, [])) }
  | a = IDENT
    { mk_sort $startpos $endpos (Sort.TVar (Tvar.of_string a)) }

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
  | e = seq_expr; COLON; s = sort
    { mk_surfexpr $startpos $endpos (SurfExpr.Annot (e, s)) }
  | e = seq_expr
    { e }

seq_expr:
  | LET; p = pat; COLON; s = sort; EQUAL; e1 = expr; SEMICOLON; e2 = seq_expr
    { let annot_e1 = mk_surfexpr $startpos $endpos (SurfExpr.Annot (e1, s)) in
      mk_surfexpr $startpos $endpos (SurfExpr.Let (p, annot_e1, e2)) }
  | LET; p = pat; EQUAL; e1 = expr; SEMICOLON; e2 = seq_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Let (p, e1, e2)) }
  | TAKE; p = pat; COLON; s = sort; EQUAL; e1 = expr; SEMICOLON; e2 = seq_expr
    { let annot_e1 = mk_surfexpr $startpos $endpos (SurfExpr.Annot (e1, s)) in
      mk_surfexpr $startpos $endpos (SurfExpr.Take (p, annot_e1, e2)) }
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
    { (p, e, loc_obj $startpos $endpos) }

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
  | e1 = cmp_expr; EQEQ; e2 = cmp_expr
    { mk_surfexpr $startpos $endpos (SurfExpr.Eq (e1, e2)) }
  | e = cmp_expr
    { e }

cmp_expr:
  | e1 = add_expr; LESS; e2 = add_expr
    { let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.App (Prim.Lt, tup)) }
  | e1 = add_expr; LESSEQ; e2 = add_expr
    { let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.App (Prim.Le, tup)) }
  | e1 = add_expr; GREATER; e2 = add_expr
    { let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.App (Prim.Gt, tup)) }
  | e1 = add_expr; GREATEREQ; e2 = add_expr
    { let tup = mk_surfexpr $startpos $endpos (SurfExpr.Tuple [e1; e2]) in
      mk_surfexpr $startpos $endpos (SurfExpr.App (Prim.Ge, tup)) }
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
  | p = state_prim; LBRACKET; t = sort; RBRACKET; e = simple_expr
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
  | OWN { fun ty -> Prim.Own ty }

simple_expr:
  | x = ident_var
    { mk_surfexpr $startpos $endpos (SurfExpr.Var x) }
  | n = INT_LIT
    { mk_surfexpr $startpos $endpos (SurfExpr.IntLit n) }
  | TRUE
    { mk_surfexpr $startpos $endpos (SurfExpr.BoolLit true) }
  | FALSE
    { mk_surfexpr $startpos $endpos (SurfExpr.BoolLit false) }
  | LPAREN; RPAREN
    { mk_surfexpr $startpos $endpos (SurfExpr.Tuple []) }
  | LPAREN; e = expr; RPAREN
    { e }
  | LPAREN; e = expr; COMMA; es = separated_nonempty_list(COMMA, expr); RPAREN
    { mk_surfexpr $startpos $endpos (SurfExpr.Tuple (e :: es)) }

(* ===== Refined programs ===== *)

rprog_eof:
  | ds = list(rdecl); MAIN; COLON; pf = pf_sort; LBRACKET; eff = eff_level; RBRACKET; EQUAL; e = crt_expr; EOF
    { RProg.{ decls = ds; main_pf = pf; main_eff = eff; main_body = e;
              loc = mk_loc $startpos $endpos } }

rdecl:
  | FUN; f = ident_var; LPAREN; x = ident_var; COLON; a = sort; RPAREN; ARROW; b = sort; LBRACKET; eff = eff_level; RBRACKET; EQUAL; body = expr
    { RProg.FunDecl { name = f; param = x; arg_sort = a; ret_sort = b; eff;
                       body; loc = mk_loc $startpos $endpos } }
  | RFUN; f = ident_var; dom = pf_domain; ARROW; pf2 = pf_sort; LBRACKET; eff = eff_level; RBRACKET; EQUAL; e = crt_expr
    { let (pat, pf1) = List.split dom in
      RProg.RFunDecl { name = f; pat; domain = pf1; codomain = pf2; eff;
                        body = e; loc = mk_loc $startpos $endpos } }
  | SORT; d = LABEL; LPAREN; params = separated_nonempty_list(COMMA, IDENT); RPAREN; EQUAL; LBRACE; cs = separated_nonempty_list(BAR, ctor_decl); RBRACE
    { RProg.SortDecl DsortDecl.({
        name = dsort d;
        params = List.map Tvar.of_string params;
        ctors = cs;
        loc = mk_loc $startpos $endpos;
      }) }
  | SORT; d = LABEL; EQUAL; LBRACE; cs = separated_nonempty_list(BAR, ctor_decl); RBRACE
    { RProg.SortDecl DsortDecl.({
        name = dsort d;
        params = [];
        ctors = cs;
        loc = mk_loc $startpos $endpos;
      }) }
  | TYPE; d = LABEL; LPAREN; params = separated_nonempty_list(COMMA, IDENT); RPAREN; EQUAL; LBRACE; cs = separated_nonempty_list(BAR, type_ctor_decl); RBRACE
    { RProg.TypeDecl DtypeDecl.({
        name = dsort d;
        params = List.map Tvar.of_string params;
        ctors = cs;
        loc = mk_loc $startpos $endpos;
      }) }
  | TYPE; d = LABEL; EQUAL; LBRACE; cs = separated_nonempty_list(BAR, type_ctor_decl); RBRACE
    { RProg.TypeDecl DtypeDecl.({
        name = dsort d;
        params = [];
        ctors = cs;
        loc = mk_loc $startpos $endpos;
      }) }

(* ===== Proof sorts =====

   Two grammars:
   - pf_sort: codomain/annotations — no binders for Log/Res/DepRes
   - pf_domain: domain — every entry carries a binder, parsed into a
     pattern element. Returns a list of (pat_elem, pf_entry) pairs. *)

pf_sort:
  | LPAREN; entries = separated_list(COMMA, pf_entry); RPAREN
    { entries }

pf_entry:
  | x = ident_var; COLON; s = sort
    { ProofSort.Comp { var = x; sort = s; eff = Effect.Pure } }
  | LBRACKET; eff = eff_level; RBRACKET; x = ident_var; COLON; s = sort
    { ProofSort.Comp { var = x; sort = s; eff } }
  | LBRACKET; LOG; RBRACKET; e = app_expr
    { ProofSort.Log { prop = e } }
  | LBRACKET; RES; RBRACKET; e1 = app_expr; AT; e2 = app_expr
    { ProofSort.Res { pred = e1; value = e2 } }
  | LBRACKET; RES; RBRACKET;
    LPAREN; DO; y = ident_var; COLON; s = sort; EQUAL; e = app_expr; RPAREN
    { let pred_sort = mk_sort $startpos $endpos (Sort.Pred s) in
      let annot_e = mk_surfexpr $startpos $endpos (SurfExpr.Annot (e, pred_sort)) in
      ProofSort.DepRes { bound_var = y; pred = annot_e } }
  | LBRACKET; RES; RBRACKET;
    LPAREN; DO; y = ident_var; EQUAL; e = app_expr; RPAREN
    { ProofSort.DepRes { bound_var = y; pred = e } }

pf_domain:
  | LPAREN; entries = separated_list(COMMA, pf_domain_entry); RPAREN
    { entries }

pf_domain_entry:
  | x = ident_var; COLON; s = sort
    { (RPat.Single x,
       ProofSort.Comp { var = x; sort = s; eff = Effect.Pure }) }
  | LBRACKET; eff = eff_level; RBRACKET; x = ident_var; COLON; s = sort
    { (RPat.Single x,
       ProofSort.Comp { var = x; sort = s; eff }) }
  | LBRACKET; LOG; RBRACKET; x = ident_var; COLON; e = app_expr
    { (RPat.Single x, ProofSort.Log { prop = e }) }
  | LBRACKET; RES; RBRACKET; x = ident_var; COLON; e1 = app_expr; AT; e2 = app_expr
    { (RPat.Single x, ProofSort.Res { pred = e1; value = e2 }) }
  | LBRACKET; RES; RBRACKET; x = ident_var; COLON;
    LPAREN; DO; y = ident_var; COLON; s = sort; EQUAL; e = app_expr; RPAREN
    { let pred_sort = mk_sort $startpos $endpos (Sort.Pred s) in
      let annot_e = mk_surfexpr $startpos $endpos (SurfExpr.Annot (e, pred_sort)) in
      (RPat.Pair (y, x), ProofSort.DepRes { bound_var = y; pred = annot_e }) }
  | LBRACKET; RES; RBRACKET; x = ident_var; COLON;
    LPAREN; DO; y = ident_var; EQUAL; e = app_expr; RPAREN
    { (RPat.Pair (y, x), ProofSort.DepRes { bound_var = y; pred = e }) }

(* ===== Core refined terms ===== *)

crt_expr:
  | e = crt_seq_expr; COLON; pf = pf_sort
    { RefinedExpr.mk_crt (loc_obj $startpos $endpos) (RefinedExpr.CAnnot (e, pf)) }
  | e = crt_seq_expr
    { e }

crt_seq_expr:
  | LET; q = rpat; EQUAL; e1 = crt_expr; SEMICOLON; e2 = crt_seq_expr
    { RefinedExpr.mk_crt (loc_obj $startpos $endpos) (RefinedExpr.CLet (q, e1, e2)) }
  | LET; LOG; x = ident_var; EQUAL; l = lpf_expr; SEMICOLON; e = crt_seq_expr
    { RefinedExpr.mk_crt (loc_obj $startpos $endpos) (RefinedExpr.CLetLog (x, l, e)) }
  | LET; LOG; x = ident_var; COLON; phi = app_expr; EQUAL; l = lpf_expr; SEMICOLON; e = crt_seq_expr
    { (* sugar: let log x : phi = lpf; e   ≡   let log x = (lpf : phi); e *)
      let loc = loc_obj $startpos $endpos in
      let annot_l = RefinedExpr.mk_lpf loc (RefinedExpr.LAnnot (l, phi)) in
      RefinedExpr.mk_crt loc (RefinedExpr.CLetLog (x, annot_l, e)) }
  | LET; RES; x = ident_var; EQUAL; r = rpf_expr; SEMICOLON; e = crt_seq_expr
    { RefinedExpr.mk_crt (loc_obj $startpos $endpos) (RefinedExpr.CLetRes (x, r, e)) }
  | LET; RES; x = ident_var; COLON; ce1 = app_expr; AT; ce2 = app_expr; EQUAL; r = rpf_expr; SEMICOLON; e = crt_seq_expr
    { (* sugar: let res x : ce@ce' = rpf; e   ≡   let res x = (rpf : ce@ce'); e *)
      let loc = loc_obj $startpos $endpos in
      let annot_r = RefinedExpr.mk_rpf loc (RefinedExpr.RAnnot (r, ce1, ce2)) in
      RefinedExpr.mk_crt loc (RefinedExpr.CLetRes (x, annot_r, e)) }
  | ITER; LBRACKET; ce = expr; RBRACKET; LPAREN; q = rpat; EQUAL; e1 = crt_expr; RPAREN; LBRACE; e2 = crt_expr; RBRACE
    { RefinedExpr.mk_crt (loc_obj $startpos $endpos) (RefinedExpr.CIter (ce, q, e1, e2)) }
  | IF; LBRACKET; x = ident_var; RBRACKET; ce = expr; THEN; e1 = crt_expr; ELSE; e2 = crt_seq_expr
    { RefinedExpr.mk_crt (loc_obj $startpos $endpos) (RefinedExpr.CIf (x, ce, e1, e2)) }
  | CASE; LBRACKET; y = ident_var; RBRACKET; ce = expr; OF; LBRACE; bs = separated_nonempty_list(BAR, crt_case_branch); RBRACE
    { RefinedExpr.mk_crt (loc_obj $startpos $endpos) (RefinedExpr.CCase (y, ce, bs)) }
  | EXFALSO
    { RefinedExpr.mk_crt (loc_obj $startpos $endpos) RefinedExpr.CExfalso }
  | OPEN_TAKE; LPAREN; r = rpf_expr; RPAREN
    { RefinedExpr.mk_crt (loc_obj $startpos $endpos) (RefinedExpr.COpenTake r) }
  | e = crt_spine_expr
    { e }
  | e = crt_app_expr
    { e }

(* Bare parenthesized tuple form: ( spine_arg,* ) → CTuple *)
crt_spine_expr:
  | LPAREN; args = separated_list(COMMA, spine_arg); RPAREN
    { let loc = loc_obj $startpos $endpos in
      let sp = List.fold_right (fun arg sp ->
        match arg with
        | `Core ce -> RefinedExpr.mk_spine loc (RefinedExpr.SCore (ce, sp))
        | `Log lpf -> RefinedExpr.mk_spine loc (RefinedExpr.SLog (lpf, sp))
        | `Res rpf -> RefinedExpr.mk_spine loc (RefinedExpr.SRes (rpf, sp)))
        args (RefinedExpr.mk_spine loc RefinedExpr.SNil) in
      RefinedExpr.mk_crt loc (RefinedExpr.CTuple sp) }

crt_case_branch:
  | l = LABEL; x = ident_var; ARROW; e = crt_expr
    { (label l, x, e) }

crt_app_expr:
  | p = state_prim; LBRACKET; t = sort; RBRACKET; sp = spine_expr
    { RefinedExpr.mk_crt (loc_obj $startpos $endpos) (RefinedExpr.CPrimApp (p t, sp)) }
  | f = ident_var; sp = spine_expr
    { RefinedExpr.mk_crt (loc_obj $startpos $endpos) (RefinedExpr.CCall (f, sp)) }

(* ===== Refined patterns ===== *)

rpat_elem:
  | x = ident_var
    { RPat.Single x }
  | LPAREN; x = ident_var; COMMA; y = ident_var; RPAREN
    { RPat.Pair (x, y) }

rpat:
  | LPAREN; RPAREN
    { [] }
  | LPAREN; xs = separated_nonempty_list(COMMA, rpat_elem); RPAREN
    { xs }
  | x = ident_var
    { [RPat.Single x] }

(* ===== Spine expressions ===== *)

spine_expr:
  | LPAREN; args = separated_list(COMMA, spine_arg); RPAREN
    { let loc = loc_obj $startpos $endpos in
      List.fold_right (fun arg sp ->
        match arg with
        | `Core ce -> RefinedExpr.mk_spine loc (RefinedExpr.SCore (ce, sp))
        | `Log lpf -> RefinedExpr.mk_spine loc (RefinedExpr.SLog (lpf, sp))
        | `Res rpf -> RefinedExpr.mk_spine loc (RefinedExpr.SRes (rpf, sp)))
        args (RefinedExpr.mk_spine loc RefinedExpr.SNil) }

spine_arg:
  | LOG; l = lpf_expr
    { `Log l }
  | RES; r = rpf_expr
    { `Res r }
  | e = expr
    { `Core e }

(* ===== Logical proof facts ===== *)

lpf_expr:
  | l = lpf_atom_expr; COLON; ce = expr
    { RefinedExpr.mk_lpf (loc_obj $startpos $endpos) (RefinedExpr.LAnnot (l, ce)) }
  | l = lpf_atom_expr
    { l }

lpf_atom_expr:
  | x = ident_var
    { RefinedExpr.mk_lpf (loc_obj $startpos $endpos) (RefinedExpr.LVar x) }
  | AUTO
    { RefinedExpr.mk_lpf (loc_obj $startpos $endpos) RefinedExpr.LAuto }
  | UNFOLD; f = ident_var; LPAREN; ce = expr; RPAREN
    { RefinedExpr.mk_lpf (loc_obj $startpos $endpos) (RefinedExpr.LUnfold (f, ce)) }
  | OPEN_RET; LPAREN; r = rpf_expr; RPAREN
    { RefinedExpr.mk_lpf (loc_obj $startpos $endpos) (RefinedExpr.LOpenRet r) }

(* ===== Resource proof facts ===== *)

rpf_expr:
  | r = rpf_atom_expr; COLON; e1 = simple_expr; AT; e2 = simple_expr
    { RefinedExpr.mk_rpf (loc_obj $startpos $endpos) (RefinedExpr.RAnnot (r, e1, e2)) }
  | r = rpf_atom_expr
    { r }

rpf_atom_expr:
  | x = ident_var
    { RefinedExpr.mk_rpf (loc_obj $startpos $endpos) (RefinedExpr.RVar x) }
  | MAKE_RET; LPAREN; l = lpf_expr; RPAREN
    { RefinedExpr.mk_rpf (loc_obj $startpos $endpos) (RefinedExpr.RMakeRet l) }
  | MAKE_TAKE; LPAREN; e = crt_expr; RPAREN
    { RefinedExpr.mk_rpf (loc_obj $startpos $endpos) (RefinedExpr.RMakeTake e) }
