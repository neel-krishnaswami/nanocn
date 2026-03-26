(* Refined typechecker — implements all refined typing judgements.
   Delegates core expression checking to Typecheck via RSig.comp and RCtx.erase.
   Elaboration of SurfExpr.se to CoreExpr.ce is done inline during typechecking,
   so each surface expression is elaborated with full context available. *)

(* ---------- helpers ---------- *)

let loc_dummy = object method loc = SourcePos.dummy end

(* Strip typed info down to located info *)
let strip_info (te : Typecheck.typed_ce) : CoreExpr.ce =
  CoreExpr.map (fun (b : Typecheck.typed_info) ->
    object method loc = b#loc end) te

(* Elaborate a surface expression to core, synthesizing its sort *)
let elab_se (rs : RSig.t) (gamma : Context.t) (eff : Effect.t) (se : SurfExpr.se) : (CoreExpr.ce * Sort.sort, string) result =
  let cs = RSig.comp rs in
  match ElabM.run (Elaborate.synth cs gamma eff se) with
  | Error msg -> Error msg
  | Ok (ce, _sort) ->
    let ( let* ) = Result.bind in
    let* te = Typecheck.synth cs gamma eff ce in
    Ok (strip_info te, (CoreExpr.info te)#sort)

(* Elaborate a surface expression to core, checking against a sort *)
let elab_se_check (rs : RSig.t) (gamma : Context.t) (se : SurfExpr.se) (sort : Sort.sort) (eff : Effect.t) : (CoreExpr.ce, string) result =
  let cs = RSig.comp rs in
  match ElabM.run (Elaborate.check cs gamma se sort eff) with
  | Error msg -> Error msg
  | Ok ce ->
    let ( let* ) = Result.bind in
    let* te = Typecheck.check cs gamma ce sort eff in
    Ok (strip_info te)

(* Elaborate a surface expression to core using a refined context *)
let elab_and_synth rs delta eff se =
  let gamma = RCtx.erase delta in
  elab_se rs gamma eff se

open ElabM

(* Elaborate a ProofSort from parsed (SurfExpr.se) to checked (CoreExpr.ce) *)
let elab_pf_entry (rs : RSig.t) (gamma : Context.t) (_eff : Effect.t) (entry : SurfExpr.se ProofSort.entry) : CoreExpr.ce ProofSort.entry ElabM.t =
  match entry with
  | ProofSort.Comp c -> return (ProofSort.Comp c)
  | ProofSort.Log { var; prop } ->
    let loc = object method loc = SourcePos.dummy end in
    let bool_sort = Sort.mk loc Sort.Bool in
    let* ce = lift (elab_se_check rs gamma prop bool_sort Effect.Spec) in
    return (ProofSort.Log { var; prop = ce })
  | ProofSort.Res { var; pred; value } ->
    let* (ce_pred, pred_sort) = lift (elab_se rs gamma Effect.Spec pred) in
    (match Sort.shape pred_sort with
     | Sort.Pred inner_sort ->
       let* ce_value = lift (elab_se_check rs gamma value inner_sort Effect.Spec) in
       return (ProofSort.Res { var; pred = ce_pred; value = ce_value })
     | _ -> fail (Format.asprintf "resource predicate must have pred sort, got %a" Sort.print pred_sort))
  | ProofSort.DepRes { var; bound_var; bound_sort; pred } ->
    let gamma' = Context.extend bound_var bound_sort Effect.Spec gamma in
    let* (ce_pred, pred_sort) = lift (elab_se rs gamma' Effect.Spec pred) in
    (match Sort.shape pred_sort with
     | Sort.Pred inner_sort ->
       if Sort.compare inner_sort bound_sort <> 0 then
         fail "dep-res: predicate sort doesn't match bound sort"
       else
         return (ProofSort.DepRes { var; bound_var; bound_sort; pred = ce_pred })
     | _ -> fail "dep-res: must have pred sort")

let elab_pf (rs : RSig.t) (gamma : Context.t) (eff : Effect.t) (pf : SurfExpr.se ProofSort.t) : CoreExpr.ce ProofSort.t ElabM.t =
  let rec go gamma = function
    | [] -> return []
    | entry :: rest ->
      let* entry' = elab_pf_entry rs gamma eff entry in
      let gamma' = match entry' with
        | ProofSort.Comp { var; sort; eff } -> Context.extend var sort eff gamma
        | ProofSort.Log _ | ProofSort.Res _ -> gamma
        | ProofSort.DepRes { bound_var; bound_sort; _ } ->
          Context.extend bound_var bound_sort Effect.Spec gamma
      in
      let* rest' = go gamma' rest in
      return (entry' :: rest')
  in
  go gamma pf

(* Make a core bool literal *)
let mk_bool b = CoreExpr.mk loc_dummy (CoreExpr.BoolLit b)
let mk_true = mk_bool true
let mk_false = mk_bool false

(* Make ce1 == ce2 as a core expression *)
let mk_eq ce1 ce2 = CoreExpr.mk loc_dummy (CoreExpr.Eq (ce1, ce2))

(* ---------- refined primitive signatures ---------- *)

(* Helper constructors for building proof sorts *)
let mk_var s = Var.of_string s SourcePos.dummy
let ce_var s = CoreExpr.mk loc_dummy (CoreExpr.Var (mk_var s))
let mk_prim_app p args = CoreExpr.mk loc_dummy (CoreExpr.App (p, CoreExpr.mk loc_dummy (CoreExpr.Tuple args)))
let mk_not ce = CoreExpr.mk loc_dummy (CoreExpr.Not ce)

let int_sort = Sort.mk loc_dummy Sort.Int
let bool_sort = Sort.mk loc_dummy Sort.Bool

(** Refined function type for a primitive, following the spec in refinement-types.md. *)
let rprim_signature (p : Prim.t) : CoreExpr.ce RFunType.t =
  match p with
  (* Arithmetic: (x:int, y:int) ⊸ (z:int, prop: z == prim(x,y) [log]) [pure] *)
  | Prim.Add | Prim.Sub | Prim.Mul ->
    let x = mk_var "x" and y = mk_var "y" in
    let z = mk_var "z" and prop = mk_var "prop" in
    let result_expr = mk_prim_app p [ce_var "x"; ce_var "y"] in
    { domain = [ProofSort.Comp { var = x; sort = int_sort; eff = Effect.Pure };
                ProofSort.Comp { var = y; sort = int_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = z; sort = int_sort; eff = Effect.Pure };
                  ProofSort.Log { var = prop; prop = mk_eq (ce_var "z") result_expr }];
      eff = Effect.Pure }

  (* Div: (x:int, y:int, pre: not(y == 0) [log]) ⊸ (z:int, prop: z == x/y [log]) [pure] *)
  | Prim.Div ->
    let x = mk_var "x" and y = mk_var "y" in
    let pre = mk_var "pre" in
    let z = mk_var "z" and prop = mk_var "prop" in
    let zero = CoreExpr.mk loc_dummy (CoreExpr.IntLit 0) in
    { domain = [ProofSort.Comp { var = x; sort = int_sort; eff = Effect.Pure };
                ProofSort.Comp { var = y; sort = int_sort; eff = Effect.Pure };
                ProofSort.Log { var = pre; prop = mk_not (mk_eq (ce_var "y") zero) }];
      codomain = [ProofSort.Comp { var = z; sort = int_sort; eff = Effect.Pure };
                  ProofSort.Log { var = prop; prop = mk_eq (ce_var "z") (mk_prim_app Prim.Div [ce_var "x"; ce_var "y"]) }];
      eff = Effect.Pure }

  (* Comparisons: (x:int, y:int) ⊸ (z:bool, prop: z == x cmp y [log]) [pure] *)
  | Prim.Lt | Prim.Le | Prim.Gt | Prim.Ge ->
    let x = mk_var "x" and y = mk_var "y" in
    let z = mk_var "z" and prop = mk_var "prop" in
    let result_expr = mk_prim_app p [ce_var "x"; ce_var "y"] in
    { domain = [ProofSort.Comp { var = x; sort = int_sort; eff = Effect.Pure };
                ProofSort.Comp { var = y; sort = int_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { var = prop; prop = mk_eq (ce_var "z") result_expr }];
      eff = Effect.Pure }

  (* Logic: same pattern as comparisons *)
  | Prim.And ->
    let x = mk_var "x" and y = mk_var "y" in
    let z = mk_var "z" and prop = mk_var "prop" in
    { domain = [ProofSort.Comp { var = x; sort = bool_sort; eff = Effect.Pure };
                ProofSort.Comp { var = y; sort = bool_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { var = prop; prop = mk_eq (ce_var "z") (CoreExpr.mk loc_dummy (CoreExpr.And (ce_var "x", ce_var "y"))) }];
      eff = Effect.Pure }

  | Prim.Or ->
    let x = mk_var "x" and y = mk_var "y" in
    let z = mk_var "z" and prop = mk_var "prop" in
    { domain = [ProofSort.Comp { var = x; sort = bool_sort; eff = Effect.Pure };
                ProofSort.Comp { var = y; sort = bool_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { var = prop; prop = mk_eq (ce_var "z") (mk_prim_app Prim.Or [ce_var "x"; ce_var "y"]) }];
      eff = Effect.Pure }

  | Prim.Not ->
    let x = mk_var "x" in
    let z = mk_var "z" and prop = mk_var "prop" in
    { domain = [ProofSort.Comp { var = x; sort = bool_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { var = prop; prop = mk_eq (ce_var "z") (mk_not (ce_var "x")) }];
      eff = Effect.Pure }

  (* Eq[A]: (x:A, y:A) ⊸ (z:bool, prop: z == (x == y) [log]) [pure] *)
  | Prim.Eq ty ->
    let a_sort = Sort.typ_to_sort ty in
    let x = mk_var "x" and y = mk_var "y" in
    let z = mk_var "z" and prop = mk_var "prop" in
    { domain = [ProofSort.Comp { var = x; sort = a_sort; eff = Effect.Pure };
                ProofSort.Comp { var = y; sort = a_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { var = prop; prop = mk_eq (ce_var "z") (mk_eq (ce_var "x") (ce_var "y")) }];
      eff = Effect.Pure }

  (* New[A]: (x:A) ⊸ (p:ptr A, r:Own[A](p) @ x [res]) [impure] *)
  | Prim.New ty ->
    let a_sort = Sort.typ_to_sort ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let x = mk_var "x" and p = mk_var "p" and r = mk_var "r" in
    let own_p = CoreExpr.mk loc_dummy (CoreExpr.App (Prim.Own ty, ce_var "p")) in
    { domain = [ProofSort.Comp { var = x; sort = a_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = p; sort = ptr_sort; eff = Effect.Pure };
                  ProofSort.Res { var = r; pred = own_p; value = ce_var "x" }];
      eff = Effect.Impure }

  (* Del[A]: (p:ptr A, x:A [spec], r:Own[A](p) @ x [res]) ⊸ () [impure] *)
  | Prim.Del ty ->
    let a_sort = Sort.typ_to_sort ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let p = mk_var "p" and x = mk_var "x" and r = mk_var "r" in
    let own_p = CoreExpr.mk loc_dummy (CoreExpr.App (Prim.Own ty, ce_var "p")) in
    { domain = [ProofSort.Comp { var = p; sort = ptr_sort; eff = Effect.Pure };
                ProofSort.Comp { var = x; sort = a_sort; eff = Effect.Spec };
                ProofSort.Res { var = r; pred = own_p; value = ce_var "x" }];
      codomain = [];
      eff = Effect.Impure }

  (* Get[A]: (p:ptr A, r:(x:A).Own[A](p) [res]) ⊸ (v:A, pf: v = x [log], r':Own[A](p) @ x [res]) [impure] *)
  | Prim.Get ty ->
    let a_sort = Sort.typ_to_sort ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let p = mk_var "p" and x = mk_var "x" and r = mk_var "r" in
    let v = mk_var "v" and pf = mk_var "pf" and r' = mk_var "r'" in
    let own_p = CoreExpr.mk loc_dummy (CoreExpr.App (Prim.Own ty, ce_var "p")) in
    { domain = [ProofSort.Comp { var = p; sort = ptr_sort; eff = Effect.Pure };
                ProofSort.DepRes { var = r; bound_var = x; bound_sort = a_sort; pred = own_p }];
      codomain = [ProofSort.Comp { var = v; sort = a_sort; eff = Effect.Pure };
                  ProofSort.Log { var = pf; prop = mk_eq (ce_var "v") (ce_var "x") };
                  ProofSort.Res { var = r'; pred = own_p; value = ce_var "x" }];
      eff = Effect.Impure }

  (* Set[A]: (p:ptr A, v:A, r:(x:A).Own[A](p) [res]) ⊸ (r':Own[A](p) @ v [res]) [impure] *)
  | Prim.Set ty ->
    let a_sort = Sort.typ_to_sort ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let p = mk_var "p" and v = mk_var "v" and r = mk_var "r" in
    let x = mk_var "x" and r' = mk_var "r'" in
    let own_p = CoreExpr.mk loc_dummy (CoreExpr.App (Prim.Own ty, ce_var "p")) in
    { domain = [ProofSort.Comp { var = p; sort = ptr_sort; eff = Effect.Pure };
                ProofSort.Comp { var = v; sort = a_sort; eff = Effect.Pure };
                ProofSort.DepRes { var = r; bound_var = x; bound_sort = a_sort; pred = own_p }];
      codomain = [ProofSort.Res { var = r'; pred = own_p; value = ce_var "v" }];
      eff = Effect.Impure }

  (* Own[A]: (p:ptr A) ⊸ (r:pred A) [spec] — same as core signature *)
  | Prim.Own ty ->
    let a_sort = Sort.typ_to_sort ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let pred_sort = Sort.mk loc_dummy (Sort.Pred a_sort) in
    let p = mk_var "p" and r = mk_var "r" in
    { domain = [ProofSort.Comp { var = p; sort = ptr_sort; eff = Effect.Spec }];
      codomain = [ProofSort.Comp { var = r; sort = pred_sort; eff = Effect.Spec }];
      eff = Effect.Spec }

(* ---------- typing judgements ---------- *)

(* Logical fact synthesis: RS; Delta |- lpf => ce -| Delta' ~> Ct *)
let rec synth_lpf (rs : RSig.t) (delta : RCtx.t) (lpf : RefinedExpr.parsed_lpf) : (CoreExpr.ce * RCtx.t * Constraint.t) ElabM.t =
  match RefinedExpr.lpf_shape lpf with
  | RefinedExpr.LVar x ->
    (match RCtx.lookup_log x delta with
     | Some ce -> return (ce, delta, Constraint.top)
     | None -> fail (Format.asprintf "logical variable %a not found" Var.print x))

  | RefinedExpr.LAuto ->
    fail "auto cannot synthesize; use in checking mode"

  | RefinedExpr.LUnfold (f, se_arg) ->
    let* (ce_arg, _sort) = lift (elab_and_synth rs delta Effect.Spec se_arg) in
    let cs = RSig.comp rs in
    (match Sig.lookup_fundef f cs with
     | Some (param, _arg_sort, _ret_sort, eff, body) ->
       if not (Effect.sub eff Effect.Spec) then
         fail (Format.asprintf "unfold: function %a must be spec" Var.print f)
       else
         let call_result = CoreExpr.mk loc_dummy (CoreExpr.Call (f, ce_arg)) in
         let subst_body = CoreExpr.subst param ce_arg body in
         let prop = mk_eq call_result subst_body in
         return (prop, delta, Constraint.top)
     | None -> fail (Format.asprintf "unfold: function %a not found or not a FunDef" Var.print f))

  | RefinedExpr.LAnnot (lpf', se) ->
    let gamma = RCtx.erase delta in
    let loc = object method loc = SourcePos.dummy end in
    let bool_sort = Sort.mk loc Sort.Bool in
    let* ce = lift (elab_se_check rs gamma se bool_sort Effect.Spec) in
    let* (delta', ct) = check_lpf rs delta lpf' ce in
    return (ce, delta', ct)

  | RefinedExpr.LOpenRet rpf ->
    let* (ce_pred, ce_val, delta', ct) = synth_rpf rs delta rpf in
    (match CoreExpr.shape ce_pred with
     | CoreExpr.Return ce1 ->
       return (mk_eq ce1 ce_val, delta', ct)
     | _ -> fail "open-ret: synthesized predicate is not a return")

(* Logical fact checking: RS; Delta |- lpf <= ce -| Delta' ~> Ct *)
and check_lpf (rs : RSig.t) (delta : RCtx.t) (lpf : RefinedExpr.parsed_lpf) (ce : CoreExpr.ce) : (RCtx.t * Constraint.t) ElabM.t =
  match RefinedExpr.lpf_shape lpf with
  | RefinedExpr.LAuto ->
    return (delta, Constraint.atom ce)

  | _ ->
    let* (ce_synth, delta', ct) = synth_lpf rs delta lpf in
    return (delta', Constraint.conj ct (Constraint.impl ce_synth (Constraint.atom ce)))

(* Resource fact synthesis: RS; Delta |- rpf => ce @ ce' -| Delta' ~> Ct *)
and synth_rpf (rs : RSig.t) (delta : RCtx.t) (rpf : RefinedExpr.parsed_rpf) : (CoreExpr.ce * CoreExpr.ce * RCtx.t * Constraint.t) ElabM.t =
  match RefinedExpr.rpf_shape rpf with
  | RefinedExpr.RVar x ->
    let* (pred, value, delta') = lift (RCtx.use_resource x delta) in
    return (pred, value, delta', Constraint.top)

  | RefinedExpr.RAnnot (rpf', se1, se2) ->
    let gamma = RCtx.erase delta in
    let* (ce1, _sort1) = lift (elab_se rs gamma Effect.Spec se1) in
    let* (ce2, _sort2) = lift (elab_se rs gamma Effect.Spec se2) in
    let* (delta', ct) = check_rpf rs delta rpf' ce1 ce2 in
    return (ce1, ce2, delta', ct)

  | RefinedExpr.RMakeRet _ | RefinedExpr.RMakeTake _ ->
    fail "make-ret/make-take cannot synthesize; use in checking mode"

(* Resource fact checking: RS; Delta |- rpf <= ce @ ce' -| Delta' ~> Ct *)
and check_rpf (rs : RSig.t) (delta : RCtx.t) (rpf : RefinedExpr.parsed_rpf) (ce1 : CoreExpr.ce) (ce2 : CoreExpr.ce) : (RCtx.t * Constraint.t) ElabM.t =
  match RefinedExpr.rpf_shape rpf with
  | RefinedExpr.RMakeRet lpf' ->
    (match CoreExpr.shape ce1 with
     | CoreExpr.Return ce_a ->
       check_lpf rs delta lpf' (mk_eq ce_a ce2)
     | _ -> fail "make-ret: expected return predicate")

  | RefinedExpr.RMakeTake crt ->
    (match CoreExpr.shape ce1 with
     | CoreExpr.Take ((x, _), pred_expr, pred_body) ->
       let loc = object method loc = SourcePos.dummy end in
       let sort = (Typecheck.prim_signature (Prim.New (Typ.mk loc Typ.Int))|> fun (_, _, _) ->
         Sort.mk loc Sort.Int) in
       let* r1 = fresh SourcePos.dummy in
       let* r2 = fresh SourcePos.dummy in
       let pf = [
         ProofSort.Comp { var = x; sort; eff = Effect.Spec };
         ProofSort.Res { var = r1; pred = pred_expr; value = CoreExpr.mk loc (CoreExpr.Var x) };
         ProofSort.Res { var = r2; pred = pred_body; value = ce2 };
       ] in
       let eff = Effect.Spec in
       check_crt rs delta eff crt pf
     | _ -> fail "make-take: expected take predicate")

  | _ ->
    let* (ce1_synth, ce2_synth, delta', ct) = synth_rpf rs delta rpf in
    let eq_ct = Constraint.conj (Constraint.atom (mk_eq ce1_synth ce1))
                                (Constraint.atom (mk_eq ce2_synth ce2)) in
    return (delta', Constraint.conj ct eq_ct)

(* Core refined term synthesis: RS; Delta |-[eff] crt => Pf -| Delta' ~> Ct *)
and synth_crt (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (crt : RefinedExpr.parsed_crt) : (CoreExpr.ce ProofSort.t * RCtx.t * Constraint.t) ElabM.t =
  match RefinedExpr.crt_shape crt with
  | RefinedExpr.CAnnot (crt', se_pf) ->
    let gamma = RCtx.erase delta in
    let* pf = elab_pf rs gamma eff se_pf in
    let* (delta', ct) = check_crt rs delta eff crt' pf in
    return (pf, delta', ct)

  | RefinedExpr.CCall (f, spine) ->
    (match RSig.lookup_rf f rs with
     | Some rf ->
       if not (Effect.sub rf.eff eff) then
         fail (Format.asprintf "call %a: effect %a not allowed at %a"
           Var.print f Effect.print rf.eff Effect.print eff)
       else
         let eff'' = Effect.purify eff in
         check_spine rs delta eff'' spine rf
     | None -> fail (Format.asprintf "function %a not found" Var.print f))

  | RefinedExpr.CPrimApp (prim, spine) ->
    let rf = rprim_signature prim in
    if not (Effect.sub rf.eff eff) then
      fail (Format.asprintf "prim %a effect %a not allowed at %a"
        Prim.print prim Effect.print rf.eff Effect.print eff)
    else
      let eff'' = Effect.purify eff in
      check_spine rs delta eff'' spine rf

  | RefinedExpr.COpenTake rpf ->
    let* (ce_pred, ce_val, delta', ct) = synth_rpf rs delta rpf in
    (match CoreExpr.shape ce_pred with
     | CoreExpr.Take ((x, _), ce1, ce2) ->
       let loc = object method loc = SourcePos.dummy end in
       let sort = Sort.mk loc Sort.Int in (* TODO: properly infer *)
       let* r1 = fresh SourcePos.dummy in
       let* r2 = fresh SourcePos.dummy in
       let pf = [
         ProofSort.Comp { var = x; sort; eff = Effect.Spec };
         ProofSort.Res { var = r1; pred = ce1; value = CoreExpr.mk loc (CoreExpr.Var x) };
         ProofSort.Res { var = r2; pred = ce2; value = ce_val };
       ] in
       return (pf, delta', ct)
     | _ -> fail "open-take: expected take predicate")

  | RefinedExpr.CTuple spine ->
    let* (delta', ct) = _check_tuple rs delta eff spine [] in
    return ([], delta', ct)

  | _ -> fail "cannot synthesize sort for this refined term"

(* Core refined term checking: RS; Delta |-[eff] crt <= Pf -| Delta' ~> Ct *)
and check_crt (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (crt : RefinedExpr.parsed_crt) (pf : CoreExpr.ce ProofSort.t) : (RCtx.t * Constraint.t) ElabM.t =
  match RefinedExpr.crt_shape crt with
  | RefinedExpr.CLet (pat, crt1, crt2) ->
    let* (pf', delta', ct1) = synth_crt rs delta eff crt1 in
    let gamma = RCtx.erase delta' in
    let delta_pat = rpat_match gamma pat pf' in
    let delta_ext = RCtx.concat delta' delta_pat in
    let* (delta'', ct2) = check_crt rs delta_ext eff crt2 pf in
    let n = RCtx.length delta' in
    let n_pat = RCtx.length delta_pat in
    let (delta_out, delta_pat_out) = RCtx.split n delta'' in
    let _ = n_pat in
    if not (RCtx.zero delta_pat_out) then
      fail "let: pattern resources not fully consumed"
    else
      return (delta_out, Constraint.conj ct1 ct2)

  | RefinedExpr.CIf (_x, se, crt1, crt2) ->
    let eff' = Effect.purify eff in
    let gamma = RCtx.erase delta in
    let loc = object method loc = SourcePos.dummy end in
    let bool_sort = Sort.mk loc Sort.Bool in
    let* ce = lift (elab_se_check rs gamma se bool_sort eff') in
    let* x_true = fresh SourcePos.dummy in
    let* x_false = fresh SourcePos.dummy in
    let delta_true = RCtx.extend_log x_true (mk_eq ce mk_true) delta in
    let delta_false = RCtx.extend_log x_false (mk_eq ce mk_false) delta in
    let* (delta1_ext, ct1) = check_crt rs delta_true eff crt1 pf in
    let* (delta2_ext, ct2) = check_crt rs delta_false eff crt2 pf in
    let n = RCtx.length delta in
    let (delta1, _) = RCtx.split n delta1_ext in
    let (delta2, _) = RCtx.split n delta2_ext in
    let* delta_merged = lift (RCtx.merge delta1 delta2) in
    let ct = Constraint.conj
      (Constraint.impl (mk_eq ce mk_true) ct1)
      (Constraint.impl (mk_eq ce mk_false) ct2) in
    return (delta_merged, ct)

  | RefinedExpr.CCase (_y, se, branches) ->
    let eff' = Effect.purify eff in
    let gamma = RCtx.erase delta in
    let cs = RSig.comp rs in
    let* (ce, ce_sort) = lift (elab_se rs gamma eff' se) in
    (match Sort.shape ce_sort with
     | Sort.App (dsort_name, _args) ->
       let decl_opt = Sig.lookup_sort dsort_name cs in
       let type_decl_opt = Sig.lookup_type dsort_name cs in
       (match decl_opt, type_decl_opt with
        | Some decl, _ ->
          let ctors = decl.DsortDecl.ctors in
          check_case_branches rs delta eff' ce ce_sort ctors branches pf
        | _, Some decl ->
          let ctors = List.map (fun (l, ty) -> (l, Sort.typ_to_sort ty)) decl.DtypeDecl.ctors in
          check_case_branches rs delta eff' ce ce_sort ctors branches pf
        | None, None ->
          fail (Format.asprintf "case: type %a not found" Dsort.print dsort_name))
     | _ -> fail "case: scrutinee must have datasort type")

  | RefinedExpr.CTuple spine ->
    let* (delta', ct) = _check_tuple rs delta eff spine pf in
    return (delta', ct)

  | RefinedExpr.CExfalso ->
    let delta' = RCtx.affinize delta in
    return (delta', Constraint.bot)

  | RefinedExpr.CIter _ ->
    fail "iter not yet implemented in refined checker"

  | _ ->
    let* (pf', delta', ct) = synth_crt rs delta eff crt in
    let* ct' = pf_eq rs delta' pf' pf in
    return (delta', Constraint.conj ct ct')

(* Spine checking: RS; Delta |-[eff] rsp : Pf1 -o Pf2 >> Pf -| Delta' ~> Ct *)
and check_spine (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (spine : RefinedExpr.parsed_spine) (rf : CoreExpr.ce RFunType.t) : (CoreExpr.ce ProofSort.t * RCtx.t * Constraint.t) ElabM.t =
  check_spine_inner rs delta eff spine rf.domain rf.codomain

and check_spine_inner rs delta eff spine domain codomain =
  match RefinedExpr.spine_shape spine, domain with
  | RefinedExpr.SNil, [] ->
    return (codomain, delta, Constraint.top)

  | RefinedExpr.SCore (se, rest), (ProofSort.Comp { var; sort; eff = Effect.Pure } :: pf_rest) ->
    let gamma = RCtx.erase delta in
    let* ce = lift (elab_se_check rs gamma se sort eff) in
    let pf_rest' = ProofSort.subst var ce pf_rest in
    let codomain' = ProofSort.subst var ce codomain in
    check_spine_inner rs delta eff rest pf_rest' codomain'

  | RefinedExpr.SCore (se, rest), (ProofSort.Comp { var; sort; eff = Effect.Spec } :: pf_rest) ->
    let gamma = RCtx.erase delta in
    let* ce = lift (elab_se_check rs gamma se sort Effect.Spec) in
    let pf_rest' = ProofSort.subst var ce pf_rest in
    let codomain' = ProofSort.subst var ce codomain in
    check_spine_inner rs delta eff rest pf_rest' codomain'

  | RefinedExpr.SCore (_, _), (ProofSort.Comp { eff = Effect.Impure; _ } :: _) ->
    fail "spine: impure comp entry not allowed in proof sort"

  | RefinedExpr.SLog (lpf, rest), (ProofSort.Log { prop; _ } :: pf_rest) ->
    let* (delta', ct) = check_lpf rs delta lpf prop in
    let* (result_pf, delta'', ct') = check_spine_inner rs delta' eff rest pf_rest codomain in
    return (result_pf, delta'', Constraint.conj ct ct')

  | RefinedExpr.SRes (rpf, rest), (ProofSort.Res { pred; value; _ } :: pf_rest) ->
    let* (delta', ct) = check_rpf rs delta rpf pred value in
    let* (result_pf, delta'', ct') = check_spine_inner rs delta' eff rest pf_rest codomain in
    return (result_pf, delta'', Constraint.conj ct ct')

  | RefinedExpr.SRes (rpf, rest), (ProofSort.DepRes { bound_var; pred; _ } :: pf_rest) ->
    let* (ce_pred_synth, ce_value, delta', ct) = synth_rpf rs delta rpf in
    let eq_ct = Constraint.atom (mk_eq pred ce_pred_synth) in
    let pf_rest' = ProofSort.subst bound_var ce_value pf_rest in
    let codomain' = ProofSort.subst bound_var ce_value codomain in
    let* (result_pf, delta'', ct') = check_spine_inner rs delta' eff rest pf_rest' codomain' in
    return (result_pf, delta'', Constraint.conj (Constraint.conj ct eq_ct) ct')

  | _ -> fail "spine: argument/parameter mismatch"

(* Tuple checking: RS; Delta |-[eff] rsp : Pf -| Delta' ~> Ct *)
and _check_tuple rs delta eff spine pf =
  match RefinedExpr.spine_shape spine, pf with
  | RefinedExpr.SNil, [] ->
    return (delta, Constraint.top)

  | RefinedExpr.SCore (se, rest), (ProofSort.Comp { var; sort; _ } :: pf_rest) ->
    let gamma = RCtx.erase delta in
    let* ce = lift (elab_se_check rs gamma se sort eff) in
    let pf_rest' = ProofSort.subst var ce pf_rest in
    _check_tuple rs delta eff rest pf_rest'

  | RefinedExpr.SLog (lpf, rest), (ProofSort.Log { prop; _ } :: pf_rest) ->
    let* (delta', ct) = check_lpf rs delta lpf prop in
    let* (delta'', ct') = _check_tuple rs delta' eff rest pf_rest in
    return (delta'', Constraint.conj ct ct')

  | RefinedExpr.SRes (rpf, rest), (ProofSort.Res { pred; value; _ } :: pf_rest) ->
    let* (delta', ct) = check_rpf rs delta rpf pred value in
    let* (delta'', ct') = _check_tuple rs delta' eff rest pf_rest in
    return (delta'', Constraint.conj ct ct')

  | RefinedExpr.SRes (rpf, rest), (ProofSort.DepRes { bound_var; pred; _ } :: pf_rest) ->
    let* (ce_pred_synth, ce_value, delta', ct) = synth_rpf rs delta rpf in
    let eq_ct = Constraint.atom (mk_eq pred ce_pred_synth) in
    let pf_rest' = ProofSort.subst bound_var ce_value pf_rest in
    let* (delta'', ct') = _check_tuple rs delta' eff rest pf_rest' in
    return (delta'', Constraint.conj (Constraint.conj ct eq_ct) ct')

  | _ -> fail "tuple: argument/entry mismatch"

(* Case branch checking *)
and check_case_branches rs delta eff ce ce_sort ctors branches pf =
  let cs = RSig.comp rs in
  let* branch_results = check_branches_list rs delta eff ce ce_sort ctors branches pf cs in
  let (deltas, cts) = List.split branch_results in
  let* delta_merged = lift (RCtx.merge_n deltas) in
  let ct = List.fold_left Constraint.conj Constraint.top cts in
  return (delta_merged, ct)

and check_branches_list rs delta eff ce _ce_sort ctors branches pf cs =
  let rec go = function
    | [] -> return []
    | (label, ctor_sort) :: rest_ctors ->
      let branch = List.find_opt (fun (l, _, _) -> Label.compare l label = 0) branches in
      (match branch with
       | None -> fail (Format.asprintf "case: missing branch for %a" Label.print label)
       | Some (_, x, body) ->
         let ctor_sort' = ctor_sort in
         let _ = cs in
         let* x_log = fresh SourcePos.dummy in
         let eq_prop = mk_eq ce (CoreExpr.mk loc_dummy (CoreExpr.Inject (label, CoreExpr.mk loc_dummy (CoreExpr.Var x)))) in
         let delta_ext = RCtx.extend_comp x ctor_sort' eff
                           (RCtx.extend_log x_log eq_prop delta) in
         let* (delta_out, ct) = check_crt rs delta_ext eff body pf in
         let n = RCtx.length delta in
         let (delta_base, _) = RCtx.split n delta_out in
         let ct' = Constraint.forall_ x ctor_sort' (Constraint.impl eq_prop ct) in
         let* rest = go rest_ctors in
         return ((delta_base, ct') :: rest))
  in
  go ctors

(* Proof sort equality: RS; Delta |- Pf1 = Pf2 ~> Ct *)
and pf_eq (_rs : RSig.t) (_delta : RCtx.t) (pf1 : CoreExpr.ce ProofSort.t) (pf2 : CoreExpr.ce ProofSort.t) : Constraint.t ElabM.t =
  let rec go pf1 pf2 =
    match pf1, pf2 with
    | [], [] -> return Constraint.top
    | _, [] -> return Constraint.top

    | ProofSort.Comp { var = x; sort; eff } :: rest1,
      ProofSort.Comp { var = y; sort = sort2; eff = eff2 } :: rest2 ->
      if Sort.compare sort sort2 <> 0 then
        fail "pf_eq: sort mismatch"
      else if Effect.compare eff eff2 <> 0 then
        fail "pf_eq: effect mismatch"
      else
        let rest2' = ProofSort.subst y (CoreExpr.mk loc_dummy (CoreExpr.Var x)) rest2 in
        let* ct = go rest1 rest2' in
        return (Constraint.forall_ x sort ct)

    | ProofSort.Log { prop = ce1; _ } :: rest1,
      ProofSort.Log { prop = ce2; _ } :: rest2 ->
      let* ct = go rest1 rest2 in
      return (Constraint.conj (Constraint.atom (mk_eq ce1 ce2)) ct)

    | ProofSort.Res { pred = p1; value = v1; _ } :: rest1,
      ProofSort.Res { pred = p2; value = v2; _ } :: rest2 ->
      let* ct = go rest1 rest2 in
      let eq_ct = Constraint.conj (Constraint.atom (mk_eq p1 p2))
                                  (Constraint.atom (mk_eq v1 v2)) in
      return (Constraint.conj eq_ct ct)

    | ProofSort.DepRes { bound_var = y1; bound_sort = bs1; pred = ce1; _ } :: rest1,
      ProofSort.DepRes { bound_var = y2; bound_sort = bs2; pred = ce2; _ } :: rest2 ->
      if Sort.compare bs1 bs2 <> 0 then fail "pf_eq: dep-res sort mismatch"
      else
        let* z = fresh SourcePos.dummy in
        let ce_z = CoreExpr.mk loc_dummy (CoreExpr.Var z) in
        let rest1' = ProofSort.subst y1 ce_z rest1 in
        let rest2' = ProofSort.subst y2 ce_z rest2 in
        let* ct = go rest1' rest2' in
        return (Constraint.conj (Constraint.atom (mk_eq ce1 ce2))
                                (Constraint.forall_ z bs1 ct))

    | _ -> fail "pf_eq: proof sort structure mismatch"
  in
  go pf1 pf2

(* Refined pattern matching: RS; Gamma |- q : Pf -| Delta *)
and rpat_match (_gamma : Context.t) (pat : RPat.t) (pf : CoreExpr.ce ProofSort.t) : RCtx.t =
  let rec go elems entries =
    match elems, entries with
    | [], [] -> RCtx.empty
    | RPat.Single x :: rest_elems, ProofSort.Comp { var = y; sort; eff } :: rest_pf ->
      let ce_x = CoreExpr.mk loc_dummy (CoreExpr.Var x) in
      let rest_pf' = ProofSort.subst y ce_x rest_pf in
      let delta = go rest_elems rest_pf' in
      RCtx.extend_comp x sort eff delta
    | RPat.Single x :: rest_elems, ProofSort.Log { var = y; prop } :: rest_pf ->
      let ce_x = CoreExpr.mk loc_dummy (CoreExpr.Var x) in
      let prop' = CoreExpr.subst y ce_x prop in
      let rest_pf' = ProofSort.subst y ce_x rest_pf in
      let delta = go rest_elems rest_pf' in
      RCtx.extend_log x prop' delta
    | RPat.Single x :: rest_elems, ProofSort.Res { var = y; pred; value } :: rest_pf ->
      let ce_x = CoreExpr.mk loc_dummy (CoreExpr.Var x) in
      let pred' = CoreExpr.subst y ce_x pred in
      let value' = CoreExpr.subst y ce_x value in
      let rest_pf' = ProofSort.subst y ce_x rest_pf in
      let delta = go rest_elems rest_pf' in
      RCtx.extend_res x pred' value' Usage.Avail delta
    | RPat.Pair (x, w) :: rest_elems, ProofSort.DepRes { var = y; bound_var = z; bound_sort; pred } :: rest_pf ->
      let ce_x = CoreExpr.mk loc_dummy (CoreExpr.Var x) in
      let pred' = CoreExpr.subst z ce_x pred in
      let rest_pf' = ProofSort.subst y ce_x (ProofSort.subst z ce_x rest_pf) in
      let delta = go rest_elems rest_pf' in
      let delta = RCtx.extend_res w pred' ce_x Usage.Avail delta in
      RCtx.extend_comp x bound_sort Effect.Spec delta
    | _ -> RCtx.empty (* mismatch — will be caught during type checking *)
  in
  go pat pf

(* ---------- Program checking ---------- *)

let elab_fundecl_body rs param arg_sort ret_sort eff body_se =
  let cs = RSig.comp rs in
  let gamma = Context.extend param arg_sort (Effect.purify eff) Context.empty in
  let ( let* ) = Result.bind in
  match ElabM.run (Elaborate.check cs gamma body_se ret_sort eff) with
  | Error msg -> Error msg
  | Ok ce ->
    let* _te = Typecheck.check cs gamma ce ret_sort eff in
    Ok (strip_info _te)

let check_rprog (prog : RProg.parsed) : (RSig.t * Constraint.t, string) result =
  ElabM.run (
    let rec check_rprog_decls rs = function
      | [] -> return rs
      | decl :: rest ->
        let* rs' = check_rprog_decl rs decl in
        check_rprog_decls rs' rest

    and check_rprog_decl rs = function
      | RProg.SortDecl d ->
        return (RSig.extend_sort rs d)
      | RProg.TypeDecl d ->
        return (RSig.extend_type rs d)
      | RProg.FunDecl { name; param; arg_sort; ret_sort; eff; body; _ } ->
        let* ce = lift (elab_fundecl_body rs param arg_sort ret_sort eff body) in
        let entry = match eff with
          | Effect.Pure ->
            RSig.FunDef { param; arg = arg_sort; ret = ret_sort; eff; body = ce }
          | _ ->
            RSig.FunSig { arg = arg_sort; ret = ret_sort; eff }
        in
        return (RSig.extend name entry rs)
      | RProg.RFunDecl { name; domain = se_domain; codomain = se_codomain; eff; body; _ } ->
        let gamma = Context.empty in
        let* domain = elab_pf rs gamma eff se_domain in
        let* codomain = elab_pf rs (ProofSort.bind gamma domain) eff se_codomain in
        let rf = RFunType.{ domain; codomain; eff } in
        let delta = ProofSort.pf_to_ctx RCtx.empty domain in
        let rs' = match eff with
          | Effect.Pure -> rs
          | _ -> RSig.extend name (RSig.RFunSig rf) rs
        in
        let* (_delta', _ct) = check_crt rs' delta eff body codomain in
        let entry = RSig.RFunSig rf in
        return (RSig.extend name entry rs)
    in

    let* rs = check_rprog_decls RSig.empty prog.decls in
    let gamma = Context.empty in
    let* main_pf = elab_pf rs gamma prog.main_eff prog.main_pf in
    let* (_delta, ct) = check_crt rs RCtx.empty prog.main_eff prog.main_body main_pf in
    return (rs, ct)
  )

module Test = struct
  let test = []
end
