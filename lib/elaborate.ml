let ( let* ) = ElabM.( let* )

let mk_ce pos s = CoreExpr.In (s, object method loc = pos end)
let mk_sort pos s = Sort.In (s, object method loc = pos end)

let fail_at pos msg =
  ElabM.fail (Format.asprintf "@[%a:@ %s@]" SourcePos.print pos msg)

let fail_at_f pos fmt =
  Format.kasprintf (fun msg -> fail_at pos msg) fmt

let sort_equal a b = Sort.compare a b = 0

(** {1 Coverage types} *)

type binding = Pat.pat * Sort.sort

type branch = {
  bindings : binding list;
  spec_bindings : (Var.t * Sort.sort) list;
  ectx : EvalCtx.t;
  body : SurfExpr.se;
}

(** {1 Coverage helpers} *)

let has_con branches =
  List.exists (fun br ->
    match br.bindings with
    | (Pat.In (Pat.Con _, _), _) :: _ -> true
    | _ -> false) branches

let has_tup branches =
  List.exists (fun br ->
    match br.bindings with
    | (Pat.In (Pat.Tuple _, _), _) :: _ -> true
    | _ -> false) branches

(** strip_var: all leading patterns are variables.
    Each [x : tau] gets moved to spec_bindings, and [let x = y] to ectx. *)
let strip_var y branches =
  List.map (fun br ->
    match br.bindings with
    | (Pat.In (Pat.Var x, _), tau) :: rest ->
      let y_ce = mk_ce (Var.binding_site y) (CoreExpr.Var y) in
      { bindings = rest;
        spec_bindings = br.spec_bindings @ [(x, tau)];
        ectx = EvalCtx.extend br.ectx x y_ce;
        body = br.body }
    | _ -> br
  ) branches

(** spec_con: filter branches for a given constructor label. *)
let rec spec_con label ctor_sort y branches =
  match branches with
  | [] -> ElabM.return []
  | br :: rest ->
    match br.bindings with
    | (Pat.In (Pat.Con (l, subpat), _), _tau) :: binds
      when Label.compare l label = 0 ->
      let* rest' = spec_con label ctor_sort y rest in
      ElabM.return ({ br with bindings = (subpat, ctor_sort) :: binds } :: rest')
    | (Pat.In (Pat.Con _, _), _) :: _ ->
      (* Different constructor — skip *)
      spec_con label ctor_sort y rest
    | (Pat.In (Pat.Var x, _), tau) :: binds ->
      let* z = ElabM.fresh (Var.binding_site x) in
      let y_ce = mk_ce (Var.binding_site y) (CoreExpr.Var y) in
      let z_pat = Pat.In (Pat.Var z, object method loc = Var.binding_site z end) in
      let* rest' = spec_con label ctor_sort y rest in
      ElabM.return ({ bindings = (z_pat, ctor_sort) :: binds;
                      spec_bindings = br.spec_bindings @ [(x, tau)];
                      ectx = EvalCtx.extend br.ectx x y_ce;
                      body = br.body } :: rest')
    | _ -> spec_con label ctor_sort y rest

(** expand_tup: expand tuple patterns in the leading position. *)
let rec expand_tup sorts y branches =
  match branches with
  | [] -> ElabM.return []
  | br :: rest ->
    match br.bindings with
    | (Pat.In (Pat.Tuple pats, _), _tau) :: binds ->
      if List.compare_lengths pats sorts <> 0 then
        ElabM.fail "tuple pattern length mismatch"
      else
        let new_bindings = List.combine pats sorts in
        let* rest' = expand_tup sorts y rest in
        ElabM.return ({ br with bindings = new_bindings @ binds } :: rest')
    | (Pat.In (Pat.Var x, _), tau) :: binds ->
      let* fresh_zs = fresh_vars_for_sorts sorts (Var.binding_site x) in
      let z_pats = List.map (fun (z, s) ->
        (Pat.In (Pat.Var z, object method loc = Var.binding_site z end), s)
      ) fresh_zs in
      let y_ce = mk_ce (Var.binding_site y) (CoreExpr.Var y) in
      let* rest' = expand_tup sorts y rest in
      ElabM.return ({ bindings = z_pats @ binds;
                      spec_bindings = br.spec_bindings @ [(x, tau)];
                      ectx = EvalCtx.extend br.ectx x y_ce;
                      body = br.body } :: rest')
    | _ ->
      let* rest' = expand_tup sorts y rest in
      ElabM.return rest'

and fresh_vars_for_sorts sorts pos =
  match sorts with
  | [] -> ElabM.return []
  | s :: rest ->
    let* z = ElabM.fresh pos in
    let* zs = fresh_vars_for_sorts rest pos in
    ElabM.return ((z, s) :: zs)

(** {1 Elaboration} *)

let rec synth sig_ ctx (SurfExpr.In (shape, info)) =
  let pos = info#loc in
  match shape with
  | SurfExpr.Var x ->
    (match Context.lookup_spec x ctx with
     | Some s ->
       ElabM.return (mk_ce pos (CoreExpr.Var x), s)
     | None -> fail_at_f pos "unbound spec variable %a" Var.print x)

  | SurfExpr.IntLit n ->
    ElabM.return (mk_ce pos (CoreExpr.IntLit n), mk_sort pos Sort.Int)

  | SurfExpr.BoolLit b ->
    ElabM.return (mk_ce pos (CoreExpr.BoolLit b), mk_sort pos Sort.Bool)

  | SurfExpr.Eq (se1, se2) ->
    let* (ce1, s) = synth sig_ ctx se1 in
    if not (Sort.is_spec_type s) then
      fail_at pos "equality requires spec type (no pred)"
    else
      let* ce2 = check sig_ ctx se2 s in
      ElabM.return (mk_ce pos (CoreExpr.Eq (ce1, ce2)), mk_sort pos Sort.Bool)

  | SurfExpr.And (se1, se2) ->
    let bool_sort = mk_sort pos Sort.Bool in
    let* ce1 = check sig_ ctx se1 bool_sort in
    let* ce2 = check sig_ ctx se2 bool_sort in
    ElabM.return (mk_ce pos (CoreExpr.And (ce1, ce2)), mk_sort pos Sort.Bool)

  | SurfExpr.Not se ->
    let bool_sort = mk_sort pos Sort.Bool in
    let* ce = check sig_ ctx se bool_sort in
    ElabM.return (mk_ce pos (CoreExpr.Not ce), mk_sort pos Sort.Bool)

  | SurfExpr.Call (name, arg) ->
    (match Sig.lookup_spec_fun name sig_ with
     | Some (arg_sort, ret_sort) ->
       let* ce_arg = check sig_ ctx arg arg_sort in
       ElabM.return (mk_ce pos (CoreExpr.Call (name, ce_arg)), ret_sort)
     | None ->
       match Sig.lookup_pure_fun name sig_ with
       | Some (arg_sort, ret_sort) ->
         let* ce_arg = check sig_ ctx arg arg_sort in
         ElabM.return (mk_ce pos (CoreExpr.Call (name, ce_arg)), ret_sort)
       | None ->
         fail_at_f pos "unknown spec function %a" Var.print name)

  | SurfExpr.Const name ->
    (match Sig.lookup_spec_val name sig_ with
     | Some s -> ElabM.return (mk_ce pos (CoreExpr.Const name), s)
     | None -> fail_at_f pos "unknown spec constant %a" Var.print name)

  | SurfExpr.Annot (se, s) ->
    let* ce = check sig_ ctx se s in
    ElabM.return (mk_ce pos (CoreExpr.Annot (ce, s)), s)

  | SurfExpr.Prim (p, arg) ->
    (match Prim.spec_sort p with
     | Some (arg_sort, ret_sort) ->
       let* ce_arg = check sig_ ctx arg arg_sort in
       ElabM.return (mk_ce pos (CoreExpr.Prim (p, ce_arg)), ret_sort)
     | None ->
       fail_at_f pos "primitive %a is not available in spec expressions"
         Prim.print p)

  | SurfExpr.Return _ | SurfExpr.Take _ | SurfExpr.Let _
  | SurfExpr.Tuple _ | SurfExpr.Inject _ | SurfExpr.Case _
  | SurfExpr.If _ ->
    fail_at pos "cannot synthesize sort; add a type annotation"

and check sig_ ctx (SurfExpr.In (shape, info) as se) sort =
  let pos = info#loc in
  match shape with
  | SurfExpr.Return inner ->
    (match Sort.shape sort with
     | Sort.Pred tau ->
       let* ce = check sig_ ctx inner tau in
       ElabM.return (mk_ce pos (CoreExpr.Return ce))
     | _ -> fail_at pos "return requires pred sort")

  | SurfExpr.Take (pat, se1, se2) ->
    (match Sort.shape sort with
     | Sort.Pred _ ->
       let* (ce1, s1) = synth sig_ ctx se1 in
       (match Sort.shape s1 with
        | Sort.Pred tau ->
          let* y = ElabM.fresh pos in
          let branch = {
            bindings = [(pat, tau)];
            spec_bindings = [];
            ectx = EvalCtx.Hole;
            body = se2;
          } in
          let* ce2 = coverage_check sig_ ctx [y] [branch] sort in
          ElabM.return (mk_ce pos (CoreExpr.Take (y, ce1, ce2)))
        | _ -> fail_at pos "take scrutinee must have pred sort")
     | _ -> fail_at pos "take requires pred sort as target")

  | SurfExpr.Let (pat, se1, se2) ->
    let* (ce1, tau) = synth sig_ ctx se1 in
    let* y = ElabM.fresh pos in
    let branch = {
      bindings = [(pat, tau)];
      spec_bindings = [];
      ectx = EvalCtx.Hole;
      body = se2;
    } in
    let* ce2 = coverage_check sig_ ctx [y] [branch] sort in
    ElabM.return (mk_ce pos (CoreExpr.Let (y, ce1, ce2)))

  | SurfExpr.Tuple ses ->
    (match Sort.shape sort with
     | Sort.Record taus ->
       if List.compare_lengths ses taus <> 0 then
         fail_at_f pos "tuple: expected %d components, got %d"
           (List.length taus) (List.length ses)
       else
         let* ces = check_list sig_ ctx ses taus in
         ElabM.return (mk_ce pos (CoreExpr.Tuple ces))
     | _ -> fail_at pos "tuple: expected record sort")

  | SurfExpr.Inject (l, inner) ->
    (match Sort.shape sort with
     | Sort.App (_d, args) ->
       (match CtorLookup.lookup sig_ l args with
        | Ok ctor_sort ->
          let* ce = check sig_ ctx inner ctor_sort in
          ElabM.return (mk_ce pos (CoreExpr.Con (l, ce)))
        | Error msg -> fail_at_f pos "%s" msg)
     | _ -> fail_at_f pos "constructor %a requires datasort type" Label.print l)

  | SurfExpr.Case (scrut, surf_branches) ->
    let* (ce_scrut, scrut_sort) = synth sig_ ctx scrut in
    let* y = ElabM.fresh pos in
    let branches = List.map (fun (pat, body) ->
      { bindings = [(pat, scrut_sort)];
        spec_bindings = [];
        ectx = EvalCtx.Hole;
        body }
    ) surf_branches in
    let* ce_body = coverage_check sig_ ctx [y] branches sort in
    ElabM.return (mk_ce pos (CoreExpr.Let (y, ce_scrut, ce_body)))

  | SurfExpr.If (se1, se2, se3) ->
    let bool_sort = mk_sort pos Sort.Bool in
    let* ce1 = check sig_ ctx se1 bool_sort in
    let* ce2 = check sig_ ctx se2 sort in
    let* ce3 = check sig_ ctx se3 sort in
    ElabM.return (mk_ce pos (CoreExpr.If (ce1, ce2, ce3)))

  | _ ->
    let* (ce, syn_sort) = synth sig_ ctx se in
    if sort_equal syn_sort sort then ElabM.return ce
    else
      fail_at_f pos "expected sort %a, got %a" Sort.print sort Sort.print syn_sort

and check_list sig_ ctx ses sorts =
  match ses, sorts with
  | [], [] -> ElabM.return []
  | se :: ses', s :: ss' ->
    let* ce = check sig_ ctx se s in
    let* rest = check_list sig_ ctx ses' ss' in
    ElabM.return (ce :: rest)
  | _ -> ElabM.fail "tuple length mismatch"

(** {1 Coverage} *)

and coverage_check sig_ ctx scrutinees branches sort =
  match scrutinees, branches with
  (* Cov_done: no scrutinees, at least one branch *)
  | [], br :: _ ->
    (match br.bindings with
     | [] ->
       let ctx' = Context.extend_spec_list br.spec_bindings ctx in
       let* ce' = check sig_ ctx' br.body sort in
       ElabM.return (EvalCtx.fill br.ectx ce')
     | _ -> ElabM.fail "coverage: bindings remain but no scrutinees")

  | [], [] ->
    ElabM.fail "non-exhaustive pattern match"

  (* Cov_var: all leading patterns are variables *)
  | y :: scrs, _ when not (has_con branches) && not (has_tup branches) ->
    let branches' = strip_var y branches in
    coverage_check sig_ ctx scrs branches' sort

  (* Cov_con: some leading patterns are constructors *)
  | y :: scrs, _ when has_con branches ->
    let lead_sort = find_lead_sort branches in
    (match Sort.shape lead_sort with
     | Sort.App (dsort, args) ->
       (match Sig.lookup_sort dsort sig_ with
        | None ->
          ElabM.fail (Format.asprintf "unknown datasort %a" Dsort.print dsort)
        | Some decl ->
          let labels = DsortDecl.ctor_labels decl in
          let* case_branches =
            build_con_branches sig_ ctx y scrs branches sort labels args decl in
          let y_ce = mk_ce (Var.binding_site y) (CoreExpr.Var y) in
          ElabM.return (mk_ce (Var.binding_site y)
            (CoreExpr.Case (y_ce, case_branches))))
     | _ -> ElabM.fail "coverage: constructor pattern on non-datasort type")

  (* Cov_tup: some leading patterns are tuples *)
  | y :: scrs, _ when has_tup branches ->
    let lead_sort = find_lead_sort branches in
    (match Sort.shape lead_sort with
     | Sort.Record sorts ->
       let* fresh_zs = fresh_vars_for_sorts sorts (Var.binding_site y) in
       let fresh_vars = List.map fst fresh_zs in
       let* branches' = expand_tup sorts y branches in
       let* ce = coverage_check sig_ ctx (fresh_vars @ scrs) branches' sort in
       let y_ce = mk_ce (Var.binding_site y) (CoreExpr.Var y) in
       ElabM.return (mk_ce (Var.binding_site y)
         (CoreExpr.LetTuple (fresh_vars, y_ce, ce)))
     | _ -> ElabM.fail "coverage: tuple pattern on non-record sort")

  | _ :: _, _ ->
    ElabM.fail "coverage: unexpected pattern form"

and build_con_branches sig_ ctx y scrs branches sort labels args decl =
  match labels with
  | [] -> ElabM.return []
  | label :: rest_labels ->
    let ctor_sort = match DsortDecl.lookup_ctor label decl with
      | Some s ->
        (match Subst.of_lists decl.DsortDecl.params args with
         | Ok sub -> Subst.apply sub s
         | Error _ -> s)
      | None -> failwith "impossible: label from ctor_labels not found"
    in
    let* filtered = spec_con label ctor_sort y branches in
    let* xi = ElabM.fresh (Var.binding_site y) in
    let* ce_i = coverage_check sig_ ctx (xi :: scrs) filtered sort in
    let* rest = build_con_branches sig_ ctx y scrs branches sort
      rest_labels args decl in
    ElabM.return ((label, xi, ce_i) :: rest)

and find_lead_sort branches =
  match branches with
  | br :: _ ->
    (match br.bindings with
     | (_, sort) :: _ -> sort
     | [] -> mk_sort SourcePos.dummy Sort.Int)
  | [] -> mk_sort SourcePos.dummy Sort.Int
