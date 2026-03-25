let ( let* ) = ElabM.( let* )

let mk_ce pos s = CoreExpr.mk (object method loc = pos end) s
let mk_sort pos s = Sort.mk (object method loc = pos end) s

let fail_at pos msg =
  ElabM.fail (Format.asprintf "@[%a:@ %s@]" SourcePos.print pos msg)

let fail_at_f pos fmt =
  Format.kasprintf (fun msg -> fail_at pos msg) fmt

let sort_equal a b = Sort.compare a b = 0

(** {1 Prim signature (sort-level)} *)

let prim_signature (p : Prim.t) =
  let dummy_info = object method loc = SourcePos.dummy end in
  let mk s = Sort.mk dummy_info s in
  let int_sort = mk Sort.Int in
  let bool_sort = mk Sort.Bool in
  let pair_int = mk (Sort.Record [int_sort; int_sort]) in
  let pair_bool = mk (Sort.Record [bool_sort; bool_sort]) in
  let unit_sort = mk (Sort.Record []) in
  match p with
  | Prim.Add -> (pair_int, int_sort, Effect.Pure)
  | Prim.Sub -> (pair_int, int_sort, Effect.Pure)
  | Prim.Mul -> (pair_int, int_sort, Effect.Pure)
  | Prim.Div -> (pair_int, int_sort, Effect.Impure)
  | Prim.Lt -> (pair_int, bool_sort, Effect.Pure)
  | Prim.Le -> (pair_int, bool_sort, Effect.Pure)
  | Prim.Gt -> (pair_int, bool_sort, Effect.Pure)
  | Prim.Ge -> (pair_int, bool_sort, Effect.Pure)
  | Prim.And -> (pair_bool, bool_sort, Effect.Pure)
  | Prim.Or -> (pair_bool, bool_sort, Effect.Pure)
  | Prim.Not -> (bool_sort, bool_sort, Effect.Pure)
  | Prim.Eq a ->
    let sa = Sort.typ_to_sort a in
    (mk (Sort.Record [sa; sa]), bool_sort, Effect.Pure)
  | Prim.New a ->
    let sa = Sort.typ_to_sort a in
    (sa, mk (Sort.Ptr sa), Effect.Impure)
  | Prim.Del a ->
    let sa = Sort.typ_to_sort a in
    (mk (Sort.Ptr sa), unit_sort, Effect.Impure)
  | Prim.Get a ->
    let sa = Sort.typ_to_sort a in
    (mk (Sort.Ptr sa), sa, Effect.Impure)
  | Prim.Set a ->
    let sa = Sort.typ_to_sort a in
    (mk (Sort.Record [mk (Sort.Ptr sa); sa]), unit_sort, Effect.Impure)
  | Prim.Own a ->
    let sa = Sort.typ_to_sort a in
    (mk (Sort.Ptr sa), mk (Sort.Pred sa), Effect.Spec)

(** {1 Coverage types} *)

type binding = Pat.pat * Sort.sort

type branch = {
  bindings : binding list;
  ctx_bindings : (Var.t * Sort.sort * Effect.t) list;
  ectx : < loc : SourcePos.t > EvalCtx.t;
  body : SurfExpr.se;
}

(** {1 Coverage helpers} *)

let has_con branches =
  List.exists (fun br ->
    match br.bindings with
    | (p, _) :: _ -> (match Pat.shape p with Pat.Con _ -> true | _ -> false)
    | _ -> false) branches

let has_tup branches =
  List.exists (fun br ->
    match br.bindings with
    | (p, _) :: _ -> (match Pat.shape p with Pat.Tuple _ -> true | _ -> false)
    | _ -> false) branches

(** strip_var: all leading patterns are variables.
    Each [x : sort] gets moved to ctx_bindings with [eff_b],
    and [let x = y] to ectx. *)
let strip_var y eff_b branches =
  List.map (fun br ->
    match br.bindings with
    | (p, sort) :: rest ->
      (match Pat.shape p with
       | Pat.Var x ->
         let y_ce = mk_ce (Var.binding_site y) (CoreExpr.Var y) in
         { bindings = rest;
           ctx_bindings = br.ctx_bindings @ [(x, sort, eff_b)];
           ectx = EvalCtx.extend br.ectx x (object method loc = Var.binding_site x end) y_ce;
           body = br.body }
       | _ -> br)
    | _ -> br
  ) branches

(** spec_con: filter branches for a given constructor label. *)
let rec spec_con label ctor_sort y eff_b branches =
  match branches with
  | [] -> ElabM.return []
  | br :: rest ->
    match br.bindings with
    | (p, sort) :: binds ->
      (match Pat.shape p with
       | Pat.Con (l, subpat) when Label.compare l label = 0 ->
         let* rest' = spec_con label ctor_sort y eff_b rest in
         ElabM.return ({ br with bindings = (subpat, ctor_sort) :: binds } :: rest')
       | Pat.Con _ ->
         spec_con label ctor_sort y eff_b rest
       | Pat.Var x ->
         let* z = ElabM.fresh (Var.binding_site x) in
         let y_ce = mk_ce (Var.binding_site y) (CoreExpr.Var y) in
         let z_pat = Pat.mk (object method loc = Var.binding_site z end) (Pat.Var z) in
         let* rest' = spec_con label ctor_sort y eff_b rest in
         ElabM.return ({ bindings = (z_pat, ctor_sort) :: binds;
                         ctx_bindings = br.ctx_bindings @ [(x, sort, eff_b)];
                         ectx = EvalCtx.extend br.ectx x (object method loc = Var.binding_site x end) y_ce;
                         body = br.body } :: rest')
       | _ -> spec_con label ctor_sort y eff_b rest)
    | _ -> spec_con label ctor_sort y eff_b rest

(** expand_tup: expand tuple patterns in the leading position. *)
let rec expand_tup sorts y eff_b branches =
  match branches with
  | [] -> ElabM.return []
  | br :: rest ->
    match br.bindings with
    | (p, sort) :: binds ->
      (match Pat.shape p with
       | Pat.Tuple pats ->
         if List.compare_lengths pats sorts <> 0 then
           ElabM.fail "tuple pattern length mismatch"
         else
           let new_bindings = List.combine pats sorts in
           let* rest' = expand_tup sorts y eff_b rest in
           ElabM.return ({ br with bindings = new_bindings @ binds } :: rest')
       | Pat.Var x ->
         let* fresh_zs = fresh_vars_for_sorts sorts (Var.binding_site x) in
         let z_pats = List.map (fun (z, s) ->
           (Pat.mk (object method loc = Var.binding_site z end) (Pat.Var z), s)
         ) fresh_zs in
         let y_ce = mk_ce (Var.binding_site y) (CoreExpr.Var y) in
         let* rest' = expand_tup sorts y eff_b rest in
         ElabM.return ({ bindings = z_pats @ binds;
                         ctx_bindings = br.ctx_bindings @ [(x, sort, eff_b)];
                         ectx = EvalCtx.extend br.ectx x (object method loc = Var.binding_site x end) y_ce;
                         body = br.body } :: rest')
       | _ ->
         let* rest' = expand_tup sorts y eff_b rest in
         ElabM.return rest')
    | _ ->
      let* rest' = expand_tup sorts y eff_b rest in
      ElabM.return rest'

and fresh_vars_for_sorts sorts pos =
  match sorts with
  | [] -> ElabM.return []
  | s :: rest ->
    let* z = ElabM.fresh pos in
    let* zs = fresh_vars_for_sorts rest pos in
    ElabM.return ((z, s) :: zs)

and fresh_vars_with_positions sorts positions =
  match sorts, positions with
  | [], _ -> ElabM.return []
  | s :: rest_s, pos :: rest_p ->
    let* z = ElabM.fresh pos in
    let* zs = fresh_vars_with_positions rest_s rest_p in
    ElabM.return ((z, s) :: zs)
  | s :: rest_s, [] ->
    let* z = ElabM.fresh SourcePos.dummy in
    let* zs = fresh_vars_with_positions rest_s [] in
    ElabM.return ((z, s) :: zs)

(** {1 Pattern position helpers} *)

(** Position of the first Con subpattern matching [label]. *)
let find_con_subpat_pos label branches fallback =
  let rec go = function
    | [] -> fallback
    | br :: rest ->
      match br.bindings with
      | (p, _) :: _ ->
        (match Pat.shape p with
         | Pat.Con (l, subpat) when Label.compare l label = 0 ->
           (Pat.info subpat)#loc
         | _ -> go rest)
      | _ -> go rest
  in
  go branches

(** Positions of sub-patterns from the first Tuple pattern in branches. *)
let find_tup_subpat_positions branches n fallback_pos =
  let rec go = function
    | [] -> List.init n (fun _ -> fallback_pos)
    | br :: rest ->
      match br.bindings with
      | (p, _) :: _ ->
        (match Pat.shape p with
         | Pat.Tuple pats -> List.map (fun p -> (Pat.info p)#loc) pats
         | _ -> go rest)
      | _ -> go rest
  in
  go branches

(** {1 Elaboration} *)

let rec synth sig_ ctx eff0 se =
  let pos = (SurfExpr.info se)#loc in
  match SurfExpr.shape se with
  | SurfExpr.Var x ->
    (match Context.lookup x ctx with
     | Some (s, var_eff) ->
       if Effect.sub var_eff eff0 then
         ElabM.return (mk_ce pos (CoreExpr.Var x), s)
       else
         fail_at_f pos "variable %a has effect %a, not usable at effect %a"
           Var.print x Effect.print var_eff Effect.print eff0
     | None -> fail_at_f pos "unbound variable %a" Var.print x)

  | SurfExpr.IntLit n ->
    ElabM.return (mk_ce pos (CoreExpr.IntLit n), mk_sort pos Sort.Int)

  | SurfExpr.BoolLit b ->
    ElabM.return (mk_ce pos (CoreExpr.BoolLit b), mk_sort pos Sort.Bool)

  | SurfExpr.Eq (se1, se2) ->
    let eff0' = Effect.purify eff0 in
    let* (ce1, s) = synth sig_ ctx eff0' se1 in
    if not (Sort.is_spec_type s) then
      fail_at pos "equality requires spec type (no pred)"
    else
      let* ce2 = check sig_ ctx se2 s eff0' in
      ElabM.return (mk_ce pos (CoreExpr.Eq (ce1, ce2)), mk_sort pos Sort.Bool)

  | SurfExpr.And (se1, se2) ->
    let bool_sort = mk_sort pos Sort.Bool in
    let* ce1 = check sig_ ctx se1 bool_sort eff0 in
    let* ce2 = check sig_ ctx se2 bool_sort eff0 in
    ElabM.return (mk_ce pos (CoreExpr.And (ce1, ce2)), mk_sort pos Sort.Bool)

  | SurfExpr.Not se ->
    let bool_sort = mk_sort pos Sort.Bool in
    let* ce = check sig_ ctx se bool_sort eff0 in
    ElabM.return (mk_ce pos (CoreExpr.Not ce), mk_sort pos Sort.Bool)

  | SurfExpr.App (p, arg) ->
    let* () = match p with
      | Prim.Eq a ->
        if Typ.is_eqtype a then ElabM.return ()
        else fail_at_f pos "Eq requires an equality type, got %a" Typ.print a
      | _ -> ElabM.return ()
    in
    let (arg_sort, ret_sort, prim_eff) = prim_signature p in
    if not (Effect.sub prim_eff eff0) then
      fail_at_f pos "primitive %a requires effect %a, but context allows %a"
        Prim.print p Effect.print prim_eff Effect.print eff0
    else
      let eff0' = Effect.purify eff0 in
      let* ce_arg = check sig_ ctx arg arg_sort eff0' in
      ElabM.return (mk_ce pos (CoreExpr.App (p, ce_arg)), ret_sort)

  | SurfExpr.Call (name, arg) ->
    (match Sig.lookup_fun name sig_ with
     | Some (arg_sort, ret_sort, fun_eff) ->
       if not (Effect.sub fun_eff eff0) then
         fail_at_f pos "function %a has effect %a, not usable at effect %a"
           Var.print name Effect.print fun_eff Effect.print eff0
       else
         let eff0' = Effect.purify eff0 in
         let* ce_arg = check sig_ ctx arg arg_sort eff0' in
         ElabM.return (mk_ce pos (CoreExpr.Call (name, ce_arg)), ret_sort)
     | None ->
       fail_at_f pos "unknown function %a" Var.print name)

  | SurfExpr.Annot (se, s, ann_eff) ->
    let* ce = check sig_ ctx se s ann_eff in
    ElabM.return (mk_ce pos (CoreExpr.Annot (ce, s, ann_eff)), s)

  | SurfExpr.Return _ | SurfExpr.Take _ | SurfExpr.Let _
  | SurfExpr.Tuple _ | SurfExpr.Inject _ | SurfExpr.Case _
  | SurfExpr.Iter _ | SurfExpr.If _ ->
    fail_at pos "cannot synthesize sort; add a type annotation"

and check sig_ ctx se sort eff0 =
  let pos = (SurfExpr.info se)#loc in
  match SurfExpr.shape se with
  | SurfExpr.Return inner ->
    if not (Effect.sub Effect.Spec eff0) then
      fail_at pos "return requires spec context"
    else
    (match Sort.shape sort with
     | Sort.Pred tau ->
       let* ce = check sig_ ctx inner tau eff0 in
       ElabM.return (mk_ce pos (CoreExpr.Return ce))
     | _ -> fail_at pos "return requires pred sort")

  | SurfExpr.Take (pat, se1, se2) ->
    if not (Effect.sub Effect.Spec eff0) then
      fail_at pos "take requires spec context"
    else
    (match Sort.shape sort with
     | Sort.Pred _ ->
       let* (ce1, s1) = synth sig_ ctx eff0 se1 in
       (match Sort.shape s1 with
        | Sort.Pred tau ->
          let eff_b = Effect.purify eff0 in
          let* y = ElabM.fresh (Pat.info pat)#loc in
          let branch = {
            bindings = [(pat, tau)];
            ctx_bindings = [];
            ectx = EvalCtx.Hole;
            body = se2;
          } in
          let* ce2 = coverage_check sig_ ctx [y] [branch] eff_b sort eff0 in
          let yb = (y, object method loc = Var.binding_site y end) in
          ElabM.return (mk_ce pos (CoreExpr.Take (yb, ce1, ce2)))
        | _ -> fail_at pos "take scrutinee must have pred sort")
     | _ -> fail_at pos "take requires pred sort as target")

  | SurfExpr.Let (pat, se1, se2) ->
    let* (ce1, tau) = synth sig_ ctx eff0 se1 in
    let eff_b = Effect.purify eff0 in
    let* y = ElabM.fresh (Pat.info pat)#loc in
    let branch = {
      bindings = [(pat, tau)];
      ctx_bindings = [];
      ectx = EvalCtx.Hole;
      body = se2;
    } in
    let* ce2 = coverage_check sig_ ctx [y] [branch] eff_b sort eff0 in
    let yb = (y, object method loc = Var.binding_site y end) in
    ElabM.return (mk_ce pos (CoreExpr.Let (yb, ce1, ce2)))

  | SurfExpr.Tuple ses ->
    (match Sort.shape sort with
     | Sort.Record sorts ->
       if List.compare_lengths ses sorts <> 0 then
         fail_at_f pos "tuple: expected %d components, got %d"
           (List.length sorts) (List.length ses)
       else
         let* ces = check_list sig_ ctx ses sorts eff0 in
         ElabM.return (mk_ce pos (CoreExpr.Tuple ces))
     | _ -> fail_at pos "tuple: expected record sort")

  | SurfExpr.Inject (l, inner) ->
    (match Sort.shape sort with
     | Sort.App (_d, args) ->
       (match CtorLookup.lookup sig_ l args with
        | Ok ctor_sort ->
          let eff0' = Effect.purify eff0 in
          let* ce = check sig_ ctx inner ctor_sort eff0' in
          ElabM.return (mk_ce pos (CoreExpr.Inject (l, ce)))
        | Error msg -> fail_at_f pos "%s" msg)
     | _ -> fail_at_f pos "constructor %a requires datasort/datatype" Label.print l)

  | SurfExpr.Case (scrut, surf_branches) ->
    let eff0' = Effect.purify eff0 in
    let* (ce_scrut, scrut_sort) = synth sig_ ctx eff0' scrut in
    let* y = ElabM.fresh (SurfExpr.info scrut)#loc in
    let branches = List.map (fun (pat, body, _) ->
      { bindings = [(pat, scrut_sort)];
        ctx_bindings = [];
        ectx = EvalCtx.Hole;
        body }
    ) surf_branches in
    let* ce_body = coverage_check sig_ ctx [y] branches eff0' sort eff0 in
    let yb = (y, object method loc = Var.binding_site y end) in
    ElabM.return (mk_ce pos (CoreExpr.Let (yb, ce_scrut, ce_body)))

  | SurfExpr.Iter (pat, se1, se2) ->
    if not (Effect.sub Effect.Impure eff0) then
      fail_at pos "iter requires impure context"
    else
    let* (ce1, init_sort) = synth sig_ ctx Effect.Pure se1 in
    let step_dsort = match Dsort.of_string "step" with Ok d -> d | Error _ -> failwith "impossible" in
    let iter_sort = mk_sort pos (Sort.App (step_dsort, [init_sort; sort])) in
    let* y = ElabM.fresh (Pat.info pat)#loc in
    let bind_eff = Effect.purify Effect.Impure in
    let branch = {
      bindings = [(pat, init_sort)];
      ctx_bindings = [];
      ectx = EvalCtx.Hole;
      body = se2;
    } in
    let* ce_body = coverage_check sig_ ctx [y] [branch] bind_eff iter_sort Effect.Impure in
    ElabM.return (mk_ce pos (CoreExpr.Iter (y, ce1, ce_body)))

  | SurfExpr.If (se1, se2, se3) ->
    let bool_sort = mk_sort pos Sort.Bool in
    let eff0' = Effect.purify eff0 in
    let* ce1 = check sig_ ctx se1 bool_sort eff0' in
    let* ce2 = check sig_ ctx se2 sort eff0 in
    let* ce3 = check sig_ ctx se3 sort eff0 in
    ElabM.return (mk_ce pos (CoreExpr.If (ce1, ce2, ce3)))

  | _ ->
    let* (ce, syn_sort) = synth sig_ ctx eff0 se in
    if not (sort_equal syn_sort sort) then
      fail_at_f pos "expected sort %a, got %a" Sort.print sort Sort.print syn_sort
    else ElabM.return ce

and check_list sig_ ctx ses sorts eff0 =
  match ses, sorts with
  | [], [] -> ElabM.return []
  | se :: ses', s :: ss' ->
    let* ce = check sig_ ctx se s eff0 in
    let* rest = check_list sig_ ctx ses' ss' eff0 in
    ElabM.return (ce :: rest)
  | _ -> ElabM.fail "tuple length mismatch"

(** {1 Coverage}

    The [eff_b] parameter is the binding effect for scrutinee variables —
    per the spec, this is [purify eff0] (the purification of the ambient effect). *)

and coverage_check sig_ ctx scrutinees branches eff_b sort eff0 =
  match scrutinees, branches with
  (* Cov_done: no scrutinees, at least one branch *)
  | [], br :: _ ->
    (match br.bindings with
     | [] ->
       let ctx' = Context.extend_list br.ctx_bindings ctx in
       let* ce' = check sig_ ctx' br.body sort eff0 in
       ElabM.return (EvalCtx.fill br.ectx ce')
     | _ -> ElabM.fail "coverage: bindings remain but no scrutinees")

  | [], [] ->
    ElabM.fail "non-exhaustive pattern match"

  (* Cov_var: all leading patterns are variables *)
  | y :: scrs, _ when not (has_con branches) && not (has_tup branches) ->
    let branches' = strip_var y eff_b branches in
    coverage_check sig_ ctx scrs branches' eff_b sort eff0

  (* Cov_con: some leading patterns are constructors *)
  | y :: scrs, _ when has_con branches ->
    let lead_sort = find_lead_sort branches in
    (match Sort.shape lead_sort with
     | Sort.App (dsort_name, args) ->
       (* Try datasort first, then datatype *)
       (match Sig.lookup_sort dsort_name sig_ with
        | Some decl ->
          let labels = DsortDecl.ctor_labels decl in
          let* case_branches =
            build_sort_con_branches sig_ ctx y scrs branches eff_b sort eff0 labels args decl in
          let y_ce = mk_ce (Var.binding_site y) (CoreExpr.Var y) in
          ElabM.return (mk_ce (Var.binding_site y)
            (CoreExpr.Case (y_ce, case_branches)))
        | None ->
          match Sig.lookup_type dsort_name sig_ with
          | Some decl ->
            let labels = DtypeDecl.ctor_labels decl in
            let* case_branches =
              build_type_con_branches sig_ ctx y scrs branches eff_b sort eff0 labels args decl in
            let y_ce = mk_ce (Var.binding_site y) (CoreExpr.Var y) in
            ElabM.return (mk_ce (Var.binding_site y)
              (CoreExpr.Case (y_ce, case_branches)))
          | None ->
            ElabM.fail (Format.asprintf "unknown sort/type %a" Dsort.print dsort_name))
     | _ -> ElabM.fail "coverage: constructor pattern on non-datasort/datatype")

  (* Cov_tup: some leading patterns are tuples *)
  | y :: scrs, _ when has_tup branches ->
    let lead_sort = find_lead_sort branches in
    (match Sort.shape lead_sort with
     | Sort.Record sorts ->
       let positions = find_tup_subpat_positions branches (List.length sorts) (Var.binding_site y) in
       let* fresh_zs = fresh_vars_with_positions sorts positions in
       let fresh_vars = List.map fst fresh_zs in
       let* branches' = expand_tup sorts y eff_b branches in
       let* ce = coverage_check sig_ ctx (fresh_vars @ scrs) branches' eff_b sort eff0 in
       let y_ce = mk_ce (Var.binding_site y) (CoreExpr.Var y) in
       let annotated_vars = List.map (fun z ->
         (z, object method loc = Var.binding_site z end)
       ) fresh_vars in
       ElabM.return (mk_ce (Var.binding_site y)
         (CoreExpr.LetTuple (annotated_vars, y_ce, ce)))
     | _ -> ElabM.fail "coverage: tuple pattern on non-record sort")

  | _ :: _, _ ->
    ElabM.fail "coverage: unexpected pattern form"

and build_sort_con_branches sig_ ctx y scrs branches eff_b sort eff0 labels args decl =
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
    let* filtered = spec_con label ctor_sort y eff_b branches in
    let xi_pos = find_con_subpat_pos label branches (Var.binding_site y) in
    let* xi = ElabM.fresh xi_pos in
    let* ce_i = coverage_check sig_ ctx (xi :: scrs) filtered eff_b sort eff0 in
    let* rest = build_sort_con_branches sig_ ctx y scrs branches eff_b sort eff0
      rest_labels args decl in
    let branch_info = object method loc = Var.binding_site xi end in
    ElabM.return ((label, xi, ce_i, branch_info) :: rest)

and build_type_con_branches sig_ ctx y scrs branches eff_b sort eff0 labels args decl =
  match labels with
  | [] -> ElabM.return []
  | label :: rest_labels ->
    let ctor_sort = match DtypeDecl.lookup_ctor label decl with
      | Some raw_ty ->
        (* Convert sort args to type args, substitute, convert back *)
        let ty_args = List.filter_map (fun s ->
          match Sort.sort_to_typ s with Ok t -> Some t | Error _ -> None
        ) args in
        let result_ty =
          match TypSubst.of_lists decl.DtypeDecl.params ty_args with
          | Ok sub -> TypSubst.apply sub raw_ty
          | Error _ -> raw_ty
        in
        Sort.typ_to_sort result_ty
      | None -> failwith "impossible: label from ctor_labels not found"
    in
    let* filtered = spec_con label ctor_sort y eff_b branches in
    let xi_pos = find_con_subpat_pos label branches (Var.binding_site y) in
    let* xi = ElabM.fresh xi_pos in
    let* ce_i = coverage_check sig_ ctx (xi :: scrs) filtered eff_b sort eff0 in
    let* rest = build_type_con_branches sig_ ctx y scrs branches eff_b sort eff0
      rest_labels args decl in
    let branch_info = object method loc = Var.binding_site xi end in
    ElabM.return ((label, xi, ce_i, branch_info) :: rest)

and find_lead_sort branches =
  match branches with
  | br :: _ ->
    (match br.bindings with
     | (_, sort) :: _ -> sort
     | [] -> mk_sort SourcePos.dummy Sort.Int)
  | [] -> mk_sort SourcePos.dummy Sort.Int
