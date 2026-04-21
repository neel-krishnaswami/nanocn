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
  | None ->
    ElabM.fail (Error.unbound_name ~loc:pos name)

let invariant_at pos ~rule msg =
  ElabM.fail (Error.internal_invariant ~loc:pos ~rule ~invariant:msg)

(* Best-effort location for a ProofSort.entry: any embedded expression
   or sort carries a [SourcePos.t] via its info object. *)
let entry_loc (entry : (SurfExpr.parsed_se, _, string) ProofSort.entry) =
  (ProofSort.entry_info entry)#loc

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
        ElabM.fail
          (Error.duplicate_pat_var
             ~loc:(Pat.info p)#loc ~name:n)
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

let resolve_pf_entry env (entry : (SurfExpr.parsed_se, _, string) ProofSort.entry)
  : ((SurfExpr.se, _, Var.t) ProofSort.entry * env) ElabM.t =
  match entry with
  | ProofSort.Comp { info; var = name; sort; eff } ->
    let* v = mk_var name (Sort.info sort)#loc in
    return (ProofSort.Comp { info; var = v; sort; eff }, (name, v) :: env)
  | ProofSort.Log { info; prop } ->
    let* prop' = resolve_expr env prop in
    return (ProofSort.Log { info; prop = prop' }, env)
  | ProofSort.Res { info; pred; value } ->
    let* pred' = resolve_expr env pred in
    let* value' = resolve_expr env value in
    return (ProofSort.Res { info; pred = pred'; value = value' }, env)
  | ProofSort.DepRes { info; bound_var = bname; pred } ->
    let pos = (SurfExpr.info pred)#loc in
    let* bv = mk_var bname pos in
    let env_with_bound = (bname, bv) :: env in
    let* pred' = resolve_expr env_with_bound pred in
    return (ProofSort.DepRes { info; bound_var = bv; pred = pred' },
            (bname, bv) :: env)

let resolve_pf env (pf : (SurfExpr.parsed_se, _, string) ProofSort.t)
  : ((SurfExpr.se, _, Var.t) ProofSort.t * env) ElabM.t =
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
    (qb : (string, _) RPat.qbase)
    (entry : (SurfExpr.parsed_se, _, string) ProofSort.entry)
  : ((Var.t, _) RPat.qbase * (SurfExpr.se, _, Var.t) ProofSort.entry * env) ElabM.t =
  match qb, entry with
  | RPat.QCore (RPat.CVar (bi, name)), ProofSort.Comp { info; var = name'; sort; eff } when name = name' ->
    let* v = mk_var name (Sort.info sort)#loc in
    return (RPat.QCore (RPat.CVar (bi, v)),
            ProofSort.Comp { info; var = v; sort; eff },
            (name, v) :: env)
  | RPat.QLog (RPat.LVar (bi, name)), ProofSort.Log { info; prop } ->
    let* prop' = resolve_expr env prop in
    let pos = (SurfExpr.info prop)#loc in
    let* v = mk_var name pos in
    return (RPat.QLog (RPat.LVar (bi, v)),
            ProofSort.Log { info; prop = prop' },
            (name, v) :: env)
  | RPat.QLog (RPat.LAuto bi), ProofSort.Log { info; prop } ->
    let* prop' = resolve_expr env prop in
    return (RPat.QLog (RPat.LAuto bi),
            ProofSort.Log { info; prop = prop' },
            env)
  | RPat.QRes (RPat.RVar (bi, name)), ProofSort.Res { info; pred; value } ->
    let* pred' = resolve_expr env pred in
    let* value' = resolve_expr env value in
    let pos = (SurfExpr.info pred)#loc in
    let* v = mk_var name pos in
    return (RPat.QRes (RPat.RVar (bi, v)),
            ProofSort.Res { info; pred = pred'; value = value' },
            (name, v) :: env)
  | RPat.QDepRes (RPat.CVar (bi_c, bname), RPat.RVar (bi_r, rname)),
    ProofSort.DepRes { info; bound_var = bname'; pred }
    when bname = bname' ->
    let pos = (SurfExpr.info pred)#loc in
    let* bv = mk_var bname pos in
    let env_with_bound = (bname, bv) :: env in
    let* pred' = resolve_expr env_with_bound pred in
    let* rv = mk_var rname pos in
    return (RPat.QDepRes (RPat.CVar (bi_c, bv), RPat.RVar (bi_r, rv)),
            ProofSort.DepRes { info; bound_var = bv; pred = pred' },
            (rname, rv) :: (bname, bv) :: env)
  | _ ->
    invariant_at (entry_loc entry) ~rule:"resolve_pf_domain_entry"
      "pattern element does not match the Pf entry \
       (shared-binder name disagreement, or pattern shape mismatch)"

let resolve_pf_domain env
    (pat : (string, _) RPat.t)
    (domain : (SurfExpr.parsed_se, _, string) ProofSort.t)
  : ((Var.t, _) RPat.t * (SurfExpr.se, _, Var.t) ProofSort.t * env) ElabM.t =
  let rec go env pat domain =
    match pat, domain with
    | [], [] -> return ([], [], env)
    | p :: ps, e :: es ->
      let* (p', e', env') = resolve_pf_domain_entry env p e in
      let* (ps', es', env'') = go env' ps es in
      return (p' :: ps', e' :: es', env'')
    | pat_rest, dom_rest ->
      let loc = match dom_rest with
        | e :: _ -> entry_loc e
        | [] -> SourcePos.dummy
      in
      let _ = pat_rest in
      invariant_at loc ~rule:"resolve_pf_domain"
        "pattern and domain disagree on length (one still has entries \
         when the other is exhausted)"
  in
  go env pat domain

(* ===== Refined patterns ===== *)

let rec resolve_cpat env (cp : (string, _) RPat.cpat)
  : ((Var.t, _) RPat.cpat * env) ElabM.t =
  match cp with
  | RPat.CVar (b, name) ->
    let* v = mk_var name SourcePos.dummy in
    return (RPat.CVar (b, v), (name, v) :: env)
  | RPat.CTuple (b, cps) ->
    let* (cps', env') =
      List.fold_left (fun acc cp ->
        let* (cps_rev, env) = acc in
        let* (cp', env') = resolve_cpat env cp in
        return (cp' :: cps_rev, env'))
        (return ([], env)) cps
    in
    return (RPat.CTuple (b, List.rev cps'), env')

let resolve_lpat env (lp : (string, _) RPat.lpat)
  : ((Var.t, _) RPat.lpat * env) ElabM.t =
  match lp with
  | RPat.LVar (b, name) ->
    let* v = mk_var name SourcePos.dummy in
    return (RPat.LVar (b, v), (name, v) :: env)
  | RPat.LAuto b ->
    return (RPat.LAuto b, env)

let rec resolve_rpat_res env (rp : (string, _) RPat.rpat)
  : ((Var.t, _) RPat.rpat * env) ElabM.t =
  match rp with
  | RPat.RVar (b, name) ->
    let* v = mk_var name SourcePos.dummy in
    return (RPat.RVar (b, v), (name, v) :: env)
  | RPat.RReturn (b, lp) ->
    let* (lp', env') = resolve_lpat env lp in
    return (RPat.RReturn (b, lp'), env')
  | RPat.RTake (b, cp, rp1, rp2) ->
    let* (cp', env') = resolve_cpat env cp in
    let* (rp1', env'') = resolve_rpat_res env' rp1 in
    let* (rp2', env''') = resolve_rpat_res env'' rp2 in
    return (RPat.RTake (b, cp', rp1', rp2'), env''')
  | RPat.RFail (b, lp) ->
    let* (lp', env') = resolve_lpat env lp in
    return (RPat.RFail (b, lp'), env')
  | RPat.RLet (b, lp, cp, rp) ->
    let* (cp', env') = resolve_cpat env cp in
    let* (lp', env'') = resolve_lpat env' lp in
    let* (rp', env''') = resolve_rpat_res env'' rp in
    return (RPat.RLet (b, lp', cp', rp'), env''')
  | RPat.RCase (b, lp, l, cp, rp) ->
    let* (cp', env') = resolve_cpat env cp in
    let* (lp', env'') = resolve_lpat env' lp in
    let* (rp', env''') = resolve_rpat_res env'' rp in
    return (RPat.RCase (b, lp', l, cp', rp'), env''')
  | RPat.RIfTrue (b, rp) ->
    let* (rp', env') = resolve_rpat_res env rp in
    return (RPat.RIfTrue (b, rp'), env')
  | RPat.RIfFalse (b, rp) ->
    let* (rp', env') = resolve_rpat_res env rp in
    return (RPat.RIfFalse (b, rp'), env')
  | RPat.RUnfold (b, rp) ->
    let* (rp', env') = resolve_rpat_res env rp in
    return (RPat.RUnfold (b, rp'), env')
  | RPat.RAnnot (b, rp) ->
    let* (rp', env') = resolve_rpat_res env rp in
    return (RPat.RAnnot (b, rp'), env')

let resolve_qbase env (qb : (string, _) RPat.qbase)
  : ((Var.t, _) RPat.qbase * env) ElabM.t =
  match qb with
  | RPat.QCore cp ->
    let* (cp', env') = resolve_cpat env cp in
    return (RPat.QCore cp', env')
  | RPat.QLog lp ->
    let* (lp', env') = resolve_lpat env lp in
    return (RPat.QLog lp', env')
  | RPat.QRes rp ->
    let* (rp', env') = resolve_rpat_res env rp in
    return (RPat.QRes rp', env')
  | RPat.QDepRes (cp, rp) ->
    let* (cp', env') = resolve_cpat env cp in
    let* (rp', env'') = resolve_rpat_res env' rp in
    return (RPat.QDepRes (cp', rp'), env'')

let resolve_rpat env (rp : (string, _) RPat.t)
  : ((Var.t, _) RPat.t * env) ElabM.t =
  let rec go env = function
    | [] -> return ([], env)
    | qb :: rest ->
      let* (qb', env') = resolve_qbase env qb in
      let* (rest', env'') = go env' rest in
      return (qb' :: rest', env'')
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
  | RefinedExpr.CLetLog (lp, lpf, body) ->
    (* Resolve the lpf in the OUTER env, then resolve the lpat and
       bind its variables for the body. *)
    let* lpf' = resolve_lpf env lpf in
    let* (lp', env') = resolve_lpat env lp in
    let* body' = resolve_crt env' body in
    return (RefinedExpr.mk_crt b (RefinedExpr.CLetLog (lp', lpf', body')))
  | RefinedExpr.CLetRes (rp, rpf, body) ->
    let* rpf' = resolve_rpf env rpf in
    let* (rp', env') = resolve_rpat_res env rp in
    let* body' = resolve_crt env' body in
    return (RefinedExpr.mk_crt b (RefinedExpr.CLetRes (rp', rpf', body')))
  | RefinedExpr.CLetCore (lp, cp, ce, body) ->
    (* The core expression [ce] is a SurfExpr — resolve it in the
       outer env (none of the binders are in scope yet). Then resolve
       cpat and lpat to bind their variables for the body. *)
    let* ce' = resolve_expr env ce in
    let* (cp', env_cp) = resolve_cpat env cp in
    let* (lp', env') = resolve_lpat env_cp lp in
    let* body' = resolve_crt env' body in
    return (RefinedExpr.mk_crt b (RefinedExpr.CLetCore (lp', cp', ce', body')))
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
  | (l, b, name, body) :: rest ->
    let* v = mk_var name SourcePos.dummy in
    let env' = (name, v) :: env in
    let* body' = resolve_crt env' body in
    let* rest' = resolve_crt_branches env rest in
    return ((l, b, v, body') :: rest')

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

let resolve_rdecl env (d : (SurfExpr.parsed_se, _, string) RProg.decl)
  : ((SurfExpr.se, _, Var.t) RProg.decl * env) ElabM.t =
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
