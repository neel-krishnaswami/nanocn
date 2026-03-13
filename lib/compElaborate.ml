let ( let* ) = ElabM.( let* )

let mk_expr pos s = Expr.In (s, object method loc = pos end)
let mk_ty pos s = Typ.In (s, object method loc = pos end)

let fail_at pos msg =
  ElabM.fail (Format.asprintf "@[%a:@ %s@]" SourcePos.print pos msg)

let fail_at_f pos fmt =
  Format.kasprintf (fun msg -> fail_at pos msg) fmt

let typ_equal a b = Typ.compare a b = 0

(** Signature of a primitive. Returns (arg_type, ret_type, effect). *)
let prim_signature (p : Prim.t) =
  let dummy_info = object method loc = SourcePos.dummy end in
  let mk s = Typ.In (s, dummy_info) in
  let int_ty = mk Typ.Int in
  let bool_ty = mk Typ.Bool in
  let pair_int = mk (Typ.Record [int_ty; int_ty]) in
  let pair_bool = mk (Typ.Record [bool_ty; bool_ty]) in
  let unit_ty = mk (Typ.Record []) in
  match p with
  | Prim.Add -> (pair_int, int_ty, Effect.Pure)
  | Prim.Sub -> (pair_int, int_ty, Effect.Pure)
  | Prim.Mul -> (pair_int, int_ty, Effect.Pure)
  | Prim.Div -> (pair_int, int_ty, Effect.Impure)
  | Prim.And -> (pair_bool, bool_ty, Effect.Pure)
  | Prim.Or -> (pair_bool, bool_ty, Effect.Pure)
  | Prim.Not -> (bool_ty, bool_ty, Effect.Pure)
  | Prim.Eq a -> (mk (Typ.Record [a; a]), bool_ty, Effect.Pure)
  | Prim.New a -> (a, mk (Typ.Ptr a), Effect.Impure)
  | Prim.Del a -> (mk (Typ.Ptr a), unit_ty, Effect.Impure)
  | Prim.Get a -> (mk (Typ.Ptr a), a, Effect.Impure)
  | Prim.Set a -> (mk (Typ.Record [mk (Typ.Ptr a); a]), unit_ty, Effect.Impure)

(** {1 Coverage types} *)

type binding = Pat.pat * Typ.ty

type branch = {
  bindings : binding list;
  comp_bindings : (Var.t * Typ.ty) list;
  ectx : CompEvalCtx.t;
  body : SurfComp.se;
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
    Each [x : A] gets moved to comp_bindings, and [let x = y] to ectx. *)
let strip_var y branches =
  List.map (fun br ->
    match br.bindings with
    | (Pat.In (Pat.Var x, _), ty) :: rest ->
      let y_expr = mk_expr (Var.binding_site y) (Expr.Var y) in
      { bindings = rest;
        comp_bindings = br.comp_bindings @ [(x, ty)];
        ectx = CompEvalCtx.extend br.ectx x y_expr;
        body = br.body }
    | _ -> br
  ) branches

(** type_con: filter branches for a given constructor label. *)
let rec type_con label ctor_ty y branches =
  match branches with
  | [] -> ElabM.return []
  | br :: rest ->
    match br.bindings with
    | (Pat.In (Pat.Con (l, subpat), _), _ty) :: binds
      when Label.compare l label = 0 ->
      let* rest' = type_con label ctor_ty y rest in
      ElabM.return ({ br with bindings = (subpat, ctor_ty) :: binds } :: rest')
    | (Pat.In (Pat.Con _, _), _) :: _ ->
      (* Different constructor -- skip *)
      type_con label ctor_ty y rest
    | (Pat.In (Pat.Var x, _), ty) :: binds ->
      let* z = ElabM.fresh (Var.binding_site x) in
      let y_expr = mk_expr (Var.binding_site y) (Expr.Var y) in
      let z_pat = Pat.In (Pat.Var z, object method loc = Var.binding_site z end) in
      let* rest' = type_con label ctor_ty y rest in
      ElabM.return ({ bindings = (z_pat, ctor_ty) :: binds;
                      comp_bindings = br.comp_bindings @ [(x, ty)];
                      ectx = CompEvalCtx.extend br.ectx x y_expr;
                      body = br.body } :: rest')
    | _ -> type_con label ctor_ty y rest

(** expand_tup: expand tuple patterns in the leading position. *)
let rec expand_tup tys y branches =
  match branches with
  | [] -> ElabM.return []
  | br :: rest ->
    match br.bindings with
    | (Pat.In (Pat.Tuple pats, _), _ty) :: binds ->
      if List.compare_lengths pats tys <> 0 then
        ElabM.fail "tuple pattern length mismatch"
      else
        let new_bindings = List.combine pats tys in
        let* rest' = expand_tup tys y rest in
        ElabM.return ({ br with bindings = new_bindings @ binds } :: rest')
    | (Pat.In (Pat.Var x, _), ty) :: binds ->
      let* fresh_zs = fresh_vars_for_types tys (Var.binding_site x) in
      let z_pats = List.map (fun (z, t) ->
        (Pat.In (Pat.Var z, object method loc = Var.binding_site z end), t)
      ) fresh_zs in
      let y_expr = mk_expr (Var.binding_site y) (Expr.Var y) in
      let* rest' = expand_tup tys y rest in
      ElabM.return ({ bindings = z_pats @ binds;
                      comp_bindings = br.comp_bindings @ [(x, ty)];
                      ectx = CompEvalCtx.extend br.ectx x y_expr;
                      body = br.body } :: rest')
    | _ ->
      let* rest' = expand_tup tys y rest in
      ElabM.return rest'

and fresh_vars_for_types tys pos =
  match tys with
  | [] -> ElabM.return []
  | t :: rest ->
    let* z = ElabM.fresh pos in
    let* zs = fresh_vars_for_types rest pos in
    ElabM.return ((z, t) :: zs)

(** {1 Elaboration} *)

let rec synth sig_ ctx (SurfComp.In (shape, info)) =
  let pos = info#loc in
  match shape with
  | SurfComp.Var x ->
    (match Context.lookup_comp x ctx with
     | Some a ->
       ElabM.return (mk_expr pos (Expr.Var x), a, Effect.Pure)
     | None -> fail_at_f pos "unbound variable %a" Var.print x)

  | SurfComp.IntLit n ->
    ElabM.return (mk_expr pos (Expr.IntLit n), mk_ty pos Typ.Int, Effect.Pure)

  | SurfComp.BoolLit b ->
    ElabM.return (mk_expr pos (Expr.BoolLit b), mk_ty pos Typ.Bool, Effect.Pure)

  | SurfComp.App (p, arg) ->
    let* () = match p with
      | Prim.Eq a ->
        if Typ.is_eqtype a then ElabM.return ()
        else fail_at_f pos "Eq requires an equality type, got %a" Typ.print a
      | _ -> ElabM.return ()
    in
    let (arg_ty, ret_ty, eff) = prim_signature p in
    let* ce_arg = check sig_ ctx arg arg_ty Effect.Pure in
    ElabM.return (mk_expr pos (Expr.App (p, ce_arg)), ret_ty, eff)

  | SurfComp.Call (name, arg) ->
    (match Sig.lookup_fun name sig_ with
     | Some (arg_ty, ret_ty, eff) ->
       let* ce_arg = check sig_ ctx arg arg_ty Effect.Pure in
       ElabM.return (mk_expr pos (Expr.Call (name, ce_arg)), ret_ty, eff)
     | None -> fail_at_f pos "unknown function %a" Var.print name)

  | SurfComp.Annot (se, ty, eff) ->
    let* ce = check sig_ ctx se ty eff in
    ElabM.return (mk_expr pos (Expr.Annot (ce, ty, eff)), ty, eff)

  | SurfComp.Let _ | SurfComp.Tuple _ | SurfComp.Inject _
  | SurfComp.Case _ | SurfComp.Iter _ | SurfComp.If _ ->
    fail_at pos "cannot synthesize type; add a type annotation"

and check sig_ ctx (SurfComp.In (shape, info) as se) ty eff =
  let pos = info#loc in
  match shape with
  | SurfComp.Let (pat, se1, se2) ->
    let* (ce1, ty1, eff1) = synth sig_ ctx se1 in
    if not (Effect.sub eff1 eff) then
      fail_at pos "effect of let binding exceeds allowed effect"
    else
      let* y = ElabM.fresh pos in
      let branch = {
        bindings = [(pat, ty1)];
        comp_bindings = [];
        ectx = CompEvalCtx.Hole;
        body = se2;
      } in
      let* ce2 = coverage_check sig_ ctx [y] [branch] ty eff in
      ElabM.return (mk_expr pos (Expr.Let (y, ce1, ce2)))

  | SurfComp.Tuple ses ->
    (match Typ.shape ty with
     | Typ.Record tys ->
       if List.compare_lengths ses tys <> 0 then
         fail_at_f pos "tuple: expected %d components, got %d"
           (List.length tys) (List.length ses)
       else
         let* ces = check_list sig_ ctx ses tys eff in
         ElabM.return (mk_expr pos (Expr.Tuple ces))
     | _ -> fail_at pos "tuple: expected record type")

  | SurfComp.Inject (l, inner) ->
    (match Typ.shape ty with
     | Typ.App (_d, args) ->
       (match TypCtorLookup.lookup sig_ l args with
        | Ok ctor_ty ->
          let* ce = check sig_ ctx inner ctor_ty eff in
          ElabM.return (mk_expr pos (Expr.Inject (l, ce)))
        | Error msg -> fail_at_f pos "%s" msg)
     | _ -> fail_at_f pos "constructor %a requires datatype application" Label.print l)

  | SurfComp.Case (scrut, surf_branches) ->
    let* (ce_scrut, scrut_ty, scrut_eff) = synth sig_ ctx scrut in
    if not (Effect.sub scrut_eff eff) then
      fail_at pos "effect of case scrutinee exceeds allowed effect"
    else
      let* y = ElabM.fresh pos in
      let branches = List.map (fun (pat, body) ->
        { bindings = [(pat, scrut_ty)];
          comp_bindings = [];
          ectx = CompEvalCtx.Hole;
          body }
      ) surf_branches in
      let* ce_body = coverage_check sig_ ctx [y] branches ty eff in
      ElabM.return (mk_expr pos (Expr.Let (y, ce_scrut, ce_body)))

  | SurfComp.Iter (pat, se1, se2) ->
    if not (Effect.sub Effect.Impure eff) then
      fail_at pos "iter requires effectful context"
    else
      let* (ce1, init_ty, _init_eff) = synth sig_ ctx se1 in
      let step_dsort = match Dsort.of_string "step" with Ok d -> d | Error _ -> failwith "impossible" in
      let iter_ty = mk_ty pos (Typ.App (step_dsort, [init_ty; ty])) in
      let* y = ElabM.fresh pos in
      let branch = {
        bindings = [(pat, init_ty)];
        comp_bindings = [];
        ectx = CompEvalCtx.Hole;
        body = se2;
      } in
      let* ce_body = coverage_check sig_ ctx [y] [branch] iter_ty Effect.Impure in
      ElabM.return (mk_expr pos (Expr.Iter (y, ce1, ce_body)))

  | SurfComp.If (se1, se2, se3) ->
    let bool_ty = mk_ty pos Typ.Bool in
    let* ce1 = check sig_ ctx se1 bool_ty eff in
    let* ce2 = check sig_ ctx se2 ty eff in
    let* ce3 = check sig_ ctx se3 ty eff in
    ElabM.return (mk_expr pos (Expr.If (ce1, ce2, ce3)))

  | _ ->
    let* (ce, syn_ty, syn_eff) = synth sig_ ctx se in
    if not (typ_equal syn_ty ty) then
      fail_at_f pos "expected type %a, got %a" Typ.print ty Typ.print syn_ty
    else if not (Effect.sub syn_eff eff) then
      fail_at pos "effect mismatch"
    else ElabM.return ce

and check_list sig_ ctx ses tys eff =
  match ses, tys with
  | [], [] -> ElabM.return []
  | se :: ses', t :: ts' ->
    let* ce = check sig_ ctx se t eff in
    let* rest = check_list sig_ ctx ses' ts' eff in
    ElabM.return (ce :: rest)
  | _ -> ElabM.fail "tuple length mismatch"

(** {1 Coverage} *)

and coverage_check sig_ ctx scrutinees branches ty eff =
  match scrutinees, branches with
  (* Cov_done: no scrutinees, at least one branch *)
  | [], br :: _ ->
    (match br.bindings with
     | [] ->
       let ctx' = Context.extend_comp_list br.comp_bindings ctx in
       let* ce' = check sig_ ctx' br.body ty eff in
       ElabM.return (CompEvalCtx.fill br.ectx ce')
     | _ -> ElabM.fail "coverage: bindings remain but no scrutinees")

  | [], [] ->
    ElabM.fail "non-exhaustive pattern match"

  (* Cov_var: all leading patterns are variables *)
  | y :: scrs, _ when not (has_con branches) && not (has_tup branches) ->
    let branches' = strip_var y branches in
    coverage_check sig_ ctx scrs branches' ty eff

  (* Cov_con: some leading patterns are constructors *)
  | y :: scrs, _ when has_con branches ->
    let lead_ty = find_lead_ty branches in
    (match Typ.shape lead_ty with
     | Typ.App (dtype, args) ->
       (match Sig.lookup_type dtype sig_ with
        | None ->
          ElabM.fail (Format.asprintf "unknown datatype %a" Dsort.print dtype)
        | Some decl ->
          let labels = DtypeDecl.ctor_labels decl in
          let* case_branches =
            build_con_branches sig_ ctx y scrs branches ty eff labels args decl in
          let y_expr = mk_expr (Var.binding_site y) (Expr.Var y) in
          ElabM.return (mk_expr (Var.binding_site y)
            (Expr.Case (y_expr, case_branches))))
     | _ -> ElabM.fail "coverage: constructor pattern on non-datatype")

  (* Cov_tup: some leading patterns are tuples *)
  | y :: scrs, _ when has_tup branches ->
    let lead_ty = find_lead_ty branches in
    (match Typ.shape lead_ty with
     | Typ.Record tys ->
       let* fresh_zs = fresh_vars_for_types tys (Var.binding_site y) in
       let fresh_vars = List.map fst fresh_zs in
       let* branches' = expand_tup tys y branches in
       let* ce = coverage_check sig_ ctx (fresh_vars @ scrs) branches' ty eff in
       let y_expr = mk_expr (Var.binding_site y) (Expr.Var y) in
       ElabM.return (mk_expr (Var.binding_site y)
         (Expr.LetTuple (fresh_vars, y_expr, ce)))
     | _ -> ElabM.fail "coverage: tuple pattern on non-record type")

  | _ :: _, _ ->
    ElabM.fail "coverage: unexpected pattern form"

and build_con_branches sig_ ctx y scrs branches ty eff labels args decl =
  match labels with
  | [] -> ElabM.return []
  | label :: rest_labels ->
    let ctor_ty = match DtypeDecl.lookup_ctor label decl with
      | Some t ->
        (match TypSubst.of_lists decl.DtypeDecl.params args with
         | Ok sub -> TypSubst.apply sub t
         | Error _ -> t)
      | None -> failwith "impossible: label from ctor_labels not found"
    in
    let* filtered = type_con label ctor_ty y branches in
    let* xi = ElabM.fresh (Var.binding_site y) in
    let* ce_i = coverage_check sig_ ctx (xi :: scrs) filtered ty eff in
    let* rest = build_con_branches sig_ ctx y scrs branches ty eff
      rest_labels args decl in
    ElabM.return ((label, xi, ce_i) :: rest)

and find_lead_ty branches =
  match branches with
  | br :: _ ->
    (match br.bindings with
     | (_, ty) :: _ -> ty
     | [] -> mk_ty SourcePos.dummy Typ.Int)
  | [] -> mk_ty SourcePos.dummy Typ.Int
