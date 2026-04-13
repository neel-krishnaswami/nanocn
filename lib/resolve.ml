(* Scope resolution: string-named ASTs -> Var.t-named ASTs *)

open ElabM

type env = (string * Var.t) list

let lookup_env (name : string) (env : env) : Var.t option =
  let rec go = function
    | [] -> None
    | (n, v) :: _ when String.equal n name -> Some v
    | _ :: rest -> go rest
  in
  go env

let resolve_use pos env name =
  match lookup_env name env with
  | Some v -> return v
  | None -> fail (Format.asprintf "%a: unbound variable %s"
                    SourcePos.print pos name)

(* ===== Patterns ===== *)

let rec resolve_pat env (p : Pat.parsed_pat) : (Pat.pat * env) ElabM.t =
  let pos = (Pat.info p)#loc in
  match Pat.shape p with
  | Pat.Var name ->
    let* v = mk_var name pos in
    let p' = Pat.mk (Pat.info p) (Pat.Var v) in
    return (p', (name, v) :: env)
  | Pat.Con (l, sub) ->
    let* (sub', env') = resolve_pat env sub in
    return (Pat.mk (Pat.info p) (Pat.Con (l, sub')), env')
  | Pat.Tuple pats ->
    let* (pats', env') = resolve_pat_list env pats in
    return (Pat.mk (Pat.info p) (Pat.Tuple pats'), env')

and resolve_pat_list env = function
  | [] -> return ([], env)
  | p :: ps ->
    let* (p', env') = resolve_pat env p in
    let* (ps', env'') = resolve_pat_list env' ps in
    return (p' :: ps', env'')

(* Check that no string name appears more than once in the parsed pattern *)
let check_parsed_linearity (p : Pat.parsed_pat) : unit ElabM.t =
  let names = Pat.parsed_vars p in
  let rec check = function
    | [] -> return ()
    | n :: rest ->
      if List.exists (String.equal n) rest then
        fail (Format.asprintf "%a: duplicate variable %s in pattern"
                SourcePos.print (Pat.info p)#loc n)
      else check rest
  in
  check names

(* ===== Expressions ===== *)

let rec resolve_expr env (e : SurfExpr.parsed_se) : SurfExpr.se ElabM.t =
  let pos = (SurfExpr.info e)#loc in
  let mk s = return (SurfExpr.mk (SurfExpr.info e) s) in
  match SurfExpr.shape e with
  | SurfExpr.Var name ->
    let* v = resolve_use pos env name in
    mk (SurfExpr.Var v)
  | SurfExpr.IntLit n -> mk (SurfExpr.IntLit n)
  | SurfExpr.BoolLit b -> mk (SurfExpr.BoolLit b)
  | SurfExpr.Let (p, e1, e2) ->
    let* () = check_parsed_linearity p in
    let* e1' = resolve_expr env e1 in
    let* (p', env') = resolve_pat env p in
    let* e2' = resolve_expr env' e2 in
    mk (SurfExpr.Let (p', e1', e2'))
  | SurfExpr.Tuple es ->
    let* es' = resolve_expr_list env es in
    mk (SurfExpr.Tuple es')
  | SurfExpr.Inject (l, e1) ->
    let* e1' = resolve_expr env e1 in
    mk (SurfExpr.Inject (l, e1'))
  | SurfExpr.Case (scrut, branches) ->
    let* scrut' = resolve_expr env scrut in
    let* branches' = resolve_case_branches env branches in
    mk (SurfExpr.Case (scrut', branches'))
  | SurfExpr.Iter (p, e1, e2) ->
    let* () = check_parsed_linearity p in
    let* e1' = resolve_expr env e1 in
    let* (p', env') = resolve_pat env p in
    let* e2' = resolve_expr env' e2 in
    mk (SurfExpr.Iter (p', e1', e2'))
  | SurfExpr.App (prim, arg) ->
    let* arg' = resolve_expr env arg in
    mk (SurfExpr.App (prim, arg'))
  | SurfExpr.Call (name, arg) ->
    let _ = pos in
    let* arg' = resolve_expr env arg in
    mk (SurfExpr.Call (name, arg'))
  | SurfExpr.If (e1, e2, e3) ->
    let* e1' = resolve_expr env e1 in
    let* e2' = resolve_expr env e2 in
    let* e3' = resolve_expr env e3 in
    mk (SurfExpr.If (e1', e2', e3'))
  | SurfExpr.Annot (e1, s) ->
    let* e1' = resolve_expr env e1 in
    mk (SurfExpr.Annot (e1', s))
  | SurfExpr.Eq (e1, e2) ->
    let* e1' = resolve_expr env e1 in
    let* e2' = resolve_expr env e2 in
    mk (SurfExpr.Eq (e1', e2'))
  | SurfExpr.And (e1, e2) ->
    let* e1' = resolve_expr env e1 in
    let* e2' = resolve_expr env e2 in
    mk (SurfExpr.And (e1', e2'))
  | SurfExpr.Not e1 ->
    let* e1' = resolve_expr env e1 in
    mk (SurfExpr.Not e1')
  | SurfExpr.Take (p, e1, e2) ->
    let* () = check_parsed_linearity p in
    let* e1' = resolve_expr env e1 in
    let* (p', env') = resolve_pat env p in
    let* e2' = resolve_expr env' e2 in
    mk (SurfExpr.Take (p', e1', e2'))
  | SurfExpr.Return e1 ->
    let* e1' = resolve_expr env e1 in
    mk (SurfExpr.Return e1')
  | SurfExpr.Fail ->
    mk SurfExpr.Fail

and resolve_expr_list env = function
  | [] -> return []
  | e :: es ->
    let* e' = resolve_expr env e in
    let* es' = resolve_expr_list env es in
    return (e' :: es')

and resolve_case_branches env = function
  | [] -> return []
  | (p, body, info) :: rest ->
    let* () = check_parsed_linearity p in
    let* (p', env') = resolve_pat env p in
    let* body' = resolve_expr env' body in
    let* rest' = resolve_case_branches env rest in
    return ((p', body', info) :: rest')

(* ===== Programs ===== *)

let resolve_decl env (d : (SurfExpr.parsed_se, SourcePos.t, string) Prog.decl)
  : ((SurfExpr.se, SourcePos.t, Var.t) Prog.decl * env) ElabM.t =
  match d with
  | Prog.FunDecl { name; arg_sort; ret_sort; eff; branches; loc } ->
    let* branches' =
      sequence (List.map (fun (p, body, binfo) ->
        let* () = check_parsed_linearity p in
        let* (p', env') = resolve_pat env p in
        let* body' = resolve_expr env' body in
        return (p', body', binfo)
      ) branches)
    in
    return (Prog.FunDecl { name; arg_sort; ret_sort; eff;
                           branches = branches'; loc }, env)
  | Prog.SortDecl d -> return (Prog.SortDecl d, env)
  | Prog.TypeDecl d -> return (Prog.TypeDecl d, env)

let resolve_prog env (p : (SurfExpr.parsed_se, SourcePos.t, string) Prog.t)
  : (SurfExpr.se, SourcePos.t, Var.t) Prog.t ElabM.t =
  let rec resolve_decls env = function
    | [] -> return ([], env)
    | d :: ds ->
      let* (d', env') = resolve_decl env d in
      let* (ds', env'') = resolve_decls env' ds in
      return (d' :: ds', env'')
  in
  let* (decls', env') = resolve_decls env p.decls in
  let* main' = resolve_expr env' p.main in
  return { Prog.decls = decls'; main = main'; main_sort = p.main_sort;
           main_eff = p.main_eff; loc = p.loc }

(* ===== Proof sorts ===== *)

let resolve_pf_entry env (entry : (SurfExpr.parsed_se, string) ProofSort.entry)
  : ((SurfExpr.se, Var.t) ProofSort.entry * env) ElabM.t =
  match entry with
  | ProofSort.Comp { var = name; sort; eff } ->
    let* v = mk_var name (Sort.info sort)#loc in
    return (ProofSort.Comp { var = v; sort; eff }, (name, v) :: env)
  | ProofSort.Log { prop } ->
    let* prop' = resolve_expr env prop in
    return (ProofSort.Log { prop = prop' }, env)
  | ProofSort.Res { pred; value } ->
    let* pred' = resolve_expr env pred in
    let* value' = resolve_expr env value in
    return (ProofSort.Res { pred = pred'; value = value' }, env)
  | ProofSort.DepRes { bound_var = bname; pred } ->
    let pos = (SurfExpr.info pred)#loc in
    let* bv = mk_var bname pos in
    let env_with_bound = (bname, bv) :: env in
    let* pred' = resolve_expr env_with_bound pred in
    return (ProofSort.DepRes { bound_var = bv; pred = pred' },
            (bname, bv) :: env)

let resolve_pf env (pf : (SurfExpr.parsed_se, string) ProofSort.t)
  : ((SurfExpr.se, Var.t) ProofSort.t * env) ElabM.t =
  let rec go env = function
    | [] -> return ([], env)
    | entry :: rest ->
      let* (entry', env') = resolve_pf_entry env entry in
      let* (rest', env'') = go env' rest in
      return (entry' :: rest', env'')
  in
  go env pf

(* Domain resolution: pattern element and Pf entry are resolved together
   so that shared binder names get the same Var.t. *)
let resolve_pf_domain_entry env
    (pat_elem : string RPat.pat_elem)
    (entry : (SurfExpr.parsed_se, string) ProofSort.entry)
  : (Var.t RPat.pat_elem * (SurfExpr.se, Var.t) ProofSort.entry * env) ElabM.t =
  match pat_elem, entry with
  | RPat.Single name, ProofSort.Comp { var = name'; sort; eff } when name = name' ->
    let* v = mk_var name (Sort.info sort)#loc in
    return (RPat.Single v,
            ProofSort.Comp { var = v; sort; eff },
            (name, v) :: env)
  | RPat.Single name, ProofSort.Log { prop } ->
    let* prop' = resolve_expr env prop in
    let pos = (SurfExpr.info prop)#loc in
    let* v = mk_var name pos in
    return (RPat.Single v,
            ProofSort.Log { prop = prop' },
            (name, v) :: env)
  | RPat.Single name, ProofSort.Res { pred; value } ->
    let* pred' = resolve_expr env pred in
    let* value' = resolve_expr env value in
    let pos = (SurfExpr.info pred)#loc in
    let* v = mk_var name pos in
    return (RPat.Single v,
            ProofSort.Res { pred = pred'; value = value' },
            (name, v) :: env)
  | RPat.Pair (bname, rname), ProofSort.DepRes { bound_var = bname'; pred }
    when bname = bname' ->
    let pos = (SurfExpr.info pred)#loc in
    let* bv = mk_var bname pos in
    let env_with_bound = (bname, bv) :: env in
    let* pred' = resolve_expr env_with_bound pred in
    let* rv = mk_var rname pos in
    return (RPat.Pair (bv, rv),
            ProofSort.DepRes { bound_var = bv; pred = pred' },
            (rname, rv) :: (bname, bv) :: env)
  | _ ->
    fail "resolve_pf_domain_entry: pattern/entry mismatch"

let resolve_pf_domain env
    (pat : string RPat.t)
    (domain : (SurfExpr.parsed_se, string) ProofSort.t)
  : (Var.t RPat.t * (SurfExpr.se, Var.t) ProofSort.t * env) ElabM.t =
  let rec go env pat domain =
    match pat, domain with
    | [], [] -> return ([], [], env)
    | p :: ps, e :: es ->
      let* (p', e', env') = resolve_pf_domain_entry env p e in
      let* (ps', es', env'') = go env' ps es in
      return (p' :: ps', e' :: es', env'')
    | _ -> fail "resolve_pf_domain: pattern/domain length mismatch"
  in
  go env pat domain

(* ===== Refined patterns ===== *)

let resolve_rpat_elem env (elem : string RPat.pat_elem)
  : (Var.t RPat.pat_elem * env) ElabM.t =
  match elem with
  | RPat.Single name ->
    let* v = mk_var name SourcePos.dummy in
    return (RPat.Single v, (name, v) :: env)
  | RPat.Pair (name1, name2) ->
    let* v1 = mk_var name1 SourcePos.dummy in
    let* v2 = mk_var name2 SourcePos.dummy in
    return (RPat.Pair (v1, v2), (name2, v2) :: (name1, v1) :: env)

let resolve_rpat env (rp : string RPat.t)
  : (Var.t RPat.t * env) ElabM.t =
  let rec go env = function
    | [] -> return ([], env)
    | elem :: rest ->
      let* (elem', env') = resolve_rpat_elem env elem in
      let* (rest', env'') = go env' rest in
      return (elem' :: rest', env'')
  in
  go env rp

(* ===== Refined expressions ===== *)

let rec resolve_crt env (t : (SurfExpr.parsed_se, < loc : SourcePos.t >, string) RefinedExpr.crt)
  : (SurfExpr.se, < loc : SourcePos.t >, Var.t) RefinedExpr.crt ElabM.t =
  let b = RefinedExpr.crt_info t in
  match RefinedExpr.crt_shape t with
  | RefinedExpr.CLet (q, e1, e2) ->
    let* (q', env') = resolve_rpat env q in
    let* e1' = resolve_crt env e1 in
    let* e2' = resolve_crt env' e2 in
    return (RefinedExpr.mk_crt b (RefinedExpr.CLet (q', e1', e2')))
  | RefinedExpr.CLetLog (name, lpf, body) ->
    (* Resolve the lpf in the OUTER env (x is not yet in scope), then
       fresh-bind x for the body. *)
    let* lpf' = resolve_lpf env lpf in
    let* x = mk_var name b#loc in
    let env' = (name, x) :: env in
    let* body' = resolve_crt env' body in
    return (RefinedExpr.mk_crt b (RefinedExpr.CLetLog (x, lpf', body')))
  | RefinedExpr.CLetRes (name, rpf, body) ->
    let* rpf' = resolve_rpf env rpf in
    let* x = mk_var name b#loc in
    let env' = (name, x) :: env in
    let* body' = resolve_crt env' body in
    return (RefinedExpr.mk_crt b (RefinedExpr.CLetRes (x, rpf', body')))
  | RefinedExpr.CAnnot (e, pf) ->
    let* e' = resolve_crt env e in
    let* (pf', _env') = resolve_pf env pf in
    return (RefinedExpr.mk_crt b (RefinedExpr.CAnnot (e', pf')))
  | RefinedExpr.CPrimApp (p, sp) ->
    let* sp' = resolve_spine env sp in
    return (RefinedExpr.mk_crt b (RefinedExpr.CPrimApp (p, sp')))
  | RefinedExpr.CCall (name, sp) ->
    let* sp' = resolve_spine env sp in
    return (RefinedExpr.mk_crt b (RefinedExpr.CCall (name, sp')))
  | RefinedExpr.CTuple sp ->
    let* sp' = resolve_spine env sp in
    return (RefinedExpr.mk_crt b (RefinedExpr.CTuple sp'))
  | RefinedExpr.CIter (ce, q, e1, e2) ->
    let* ce' = resolve_expr env ce in
    let* (q', env') = resolve_rpat env q in
    let* e1' = resolve_crt env e1 in
    let* e2' = resolve_crt env' e2 in
    return (RefinedExpr.mk_crt b (RefinedExpr.CIter (ce', q', e1', e2')))
  | RefinedExpr.CIf (name, ce, e1, e2) ->
    let* v = mk_var name b#loc in
    let env' = (name, v) :: env in
    let* ce' = resolve_expr env' ce in
    let* e1' = resolve_crt env' e1 in
    let* e2' = resolve_crt env' e2 in
    return (RefinedExpr.mk_crt b (RefinedExpr.CIf (v, ce', e1', e2')))
  | RefinedExpr.CCase (name, ce, branches) ->
    let* v = mk_var name b#loc in
    let env' = (name, v) :: env in
    let* ce' = resolve_expr env' ce in
    let* branches' = resolve_crt_branches env' branches in
    return (RefinedExpr.mk_crt b (RefinedExpr.CCase (v, ce', branches')))
  | RefinedExpr.CExfalso ->
    return (RefinedExpr.mk_crt b RefinedExpr.CExfalso)
  | RefinedExpr.COpenTake rpf ->
    let* rpf' = resolve_rpf env rpf in
    return (RefinedExpr.mk_crt b (RefinedExpr.COpenTake rpf'))

and resolve_crt_branches env = function
  | [] -> return []
  | (l, name, body) :: rest ->
    let* v = mk_var name SourcePos.dummy in
    let env' = (name, v) :: env in
    let* body' = resolve_crt env' body in
    let* rest' = resolve_crt_branches env rest in
    return ((l, v, body') :: rest')

and resolve_lpf env (t : (SurfExpr.parsed_se, < loc : SourcePos.t >, string) RefinedExpr.lpf)
  : (SurfExpr.se, < loc : SourcePos.t >, Var.t) RefinedExpr.lpf ElabM.t =
  let b = RefinedExpr.lpf_info t in
  match RefinedExpr.lpf_shape t with
  | RefinedExpr.LVar name ->
    let* v = resolve_use b#loc env name in
    return (RefinedExpr.mk_lpf b (RefinedExpr.LVar v))
  | RefinedExpr.LAuto ->
    return (RefinedExpr.mk_lpf b RefinedExpr.LAuto)
  | RefinedExpr.LUnfold (name, ce) ->
    let* ce' = resolve_expr env ce in
    return (RefinedExpr.mk_lpf b (RefinedExpr.LUnfold (name, ce')))
  | RefinedExpr.LOpenRet rpf ->
    let* rpf' = resolve_rpf env rpf in
    return (RefinedExpr.mk_lpf b (RefinedExpr.LOpenRet rpf'))
  | RefinedExpr.LAnnot (lpf, ce) ->
    let* lpf' = resolve_lpf env lpf in
    let* ce' = resolve_expr env ce in
    return (RefinedExpr.mk_lpf b (RefinedExpr.LAnnot (lpf', ce')))

and resolve_rpf env (t : (SurfExpr.parsed_se, < loc : SourcePos.t >, string) RefinedExpr.rpf)
  : (SurfExpr.se, < loc : SourcePos.t >, Var.t) RefinedExpr.rpf ElabM.t =
  let b = RefinedExpr.rpf_info t in
  match RefinedExpr.rpf_shape t with
  | RefinedExpr.RVar name ->
    let* v = resolve_use b#loc env name in
    return (RefinedExpr.mk_rpf b (RefinedExpr.RVar v))
  | RefinedExpr.RMakeRet lpf ->
    let* lpf' = resolve_lpf env lpf in
    return (RefinedExpr.mk_rpf b (RefinedExpr.RMakeRet lpf'))
  | RefinedExpr.RMakeTake crt ->
    let* crt' = resolve_crt env crt in
    return (RefinedExpr.mk_rpf b (RefinedExpr.RMakeTake crt'))
  | RefinedExpr.RAnnot (rpf, ce1, ce2) ->
    let* rpf' = resolve_rpf env rpf in
    let* ce1' = resolve_expr env ce1 in
    let* ce2' = resolve_expr env ce2 in
    return (RefinedExpr.mk_rpf b (RefinedExpr.RAnnot (rpf', ce1', ce2')))

and resolve_spine env (t : (SurfExpr.parsed_se, < loc : SourcePos.t >, string) RefinedExpr.spine)
  : (SurfExpr.se, < loc : SourcePos.t >, Var.t) RefinedExpr.spine ElabM.t =
  let b = RefinedExpr.spine_info t in
  match RefinedExpr.spine_shape t with
  | RefinedExpr.SNil ->
    return (RefinedExpr.mk_spine b RefinedExpr.SNil)
  | RefinedExpr.SCore (ce, rest) ->
    let* ce' = resolve_expr env ce in
    let* rest' = resolve_spine env rest in
    return (RefinedExpr.mk_spine b (RefinedExpr.SCore (ce', rest')))
  | RefinedExpr.SLog (lpf, rest) ->
    let* lpf' = resolve_lpf env lpf in
    let* rest' = resolve_spine env rest in
    return (RefinedExpr.mk_spine b (RefinedExpr.SLog (lpf', rest')))
  | RefinedExpr.SRes (rpf, rest) ->
    let* rpf' = resolve_rpf env rpf in
    let* rest' = resolve_spine env rest in
    return (RefinedExpr.mk_spine b (RefinedExpr.SRes (rpf', rest')))

(* ===== Refined programs ===== *)

let resolve_rdecl env (d : (SurfExpr.parsed_se, string) RProg.decl)
  : ((SurfExpr.se, Var.t) RProg.decl * env) ElabM.t =
  match d with
  | RProg.FunDecl { name; param; arg_sort; ret_sort; eff; body; loc } ->
    let* param_v = mk_var param loc in
    let env_body = (param, param_v) :: env in
    let* body' = resolve_expr env_body body in
    return (RProg.FunDecl { name; param = param_v; arg_sort; ret_sort;
                            eff; body = body'; loc }, env)
  | RProg.RFunDecl { name; pat; domain; codomain; eff; body; loc } ->
    let* (pat', domain', env_dom) = resolve_pf_domain env pat domain in
    let* body' = resolve_crt env_dom body in
    let* (codomain', _env_cod) = resolve_pf env_dom codomain in
    return (RProg.RFunDecl { name; pat = pat'; domain = domain';
                             codomain = codomain'; eff; body = body'; loc }, env)
  | RProg.SortDecl d -> return (RProg.SortDecl d, env)
  | RProg.TypeDecl d -> return (RProg.TypeDecl d, env)

let resolve_rprog env (p : RProg.raw_parsed) : RProg.parsed ElabM.t =
  let rec resolve_rdecls env = function
    | [] -> return ([], env)
    | d :: ds ->
      let* (d', env') = resolve_rdecl env d in
      let* (ds', env'') = resolve_rdecls env' ds in
      return (d' :: ds', env'')
  in
  let* (decls', env') = resolve_rdecls env p.decls in
  let* (main_pf', env_pf) = resolve_pf env' p.main_pf in
  let* main_body' = resolve_crt env_pf p.main_body in
  return RProg.{ decls = decls'; main_pf = main_pf'; main_eff = p.main_eff;
                 main_body = main_body'; loc = p.loc }
