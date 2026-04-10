(* Refined typechecker — implements all refined typing judgements.
   Elaboration of SurfExpr.se to CoreExpr.typed_ce is done inline during
   typechecking via Elaborate.synth/check, preserving sort/ctx/eff info
   from elaboration throughout the entire refinement pipeline. *)

(* ---------- helpers ---------- *)

open ElabM

let loc_dummy = object method loc = SourcePos.dummy end

let int_sort = Sort.mk loc_dummy Sort.Int
let bool_sort = Sort.mk loc_dummy Sort.Bool

(* Typed info constructor for manually-built expressions *)
let mk_info sort =
  (object method loc = SourcePos.dummy method ctx = Context.empty
          method sort = sort method eff = Effect.Spec end : CoreExpr.typed_info)

(* Elaborate a surface expression to typed core, synthesizing its sort *)
let elab_se (rs : RSig.t) (gamma : Context.t) (eff : Effect.t) (se : SurfExpr.se) : (CoreExpr.typed_ce * Sort.sort) ElabM.t =
  let cs = RSig.comp rs in
  Elaborate.synth cs gamma eff se

(* Elaborate a surface expression to typed core, checking against a sort *)
let elab_se_check (rs : RSig.t) (gamma : Context.t) (se : SurfExpr.se) (sort : Sort.sort) (eff : Effect.t) : CoreExpr.typed_ce ElabM.t =
  let cs = RSig.comp rs in
  Elaborate.check cs gamma se sort eff

(* Elaborate a surface expression to typed core using a refined context *)
let elab_and_synth rs delta eff se =
  let gamma = RCtx.erase delta in
  elab_se rs gamma eff se

(* Elaborate a ProofSort from parsed (SurfExpr.se) to checked (CoreExpr.typed_ce) *)
let elab_pf_entry (rs : RSig.t) (gamma : Context.t) (_eff : Effect.t) (entry : (SurfExpr.se, Var.t) ProofSort.entry) : (CoreExpr.typed_ce, Var.t) ProofSort.entry ElabM.t =
  match entry with
  | ProofSort.Comp c -> return (ProofSort.Comp c)
  | ProofSort.Log { prop } ->
    let* ce = elab_se_check rs gamma prop bool_sort Effect.Spec in
    return (ProofSort.Log { prop = ce })
  | ProofSort.Res { pred; value } ->
    let* (ce_pred, pred_sort) = elab_se rs gamma Effect.Spec pred in
    (match Sort.shape pred_sort with
     | Sort.Pred inner_sort ->
       let* ce_value = elab_se_check rs gamma value inner_sort Effect.Spec in
       return (ProofSort.Res { pred = ce_pred; value = ce_value })
     | _ -> fail (Format.asprintf "resource predicate must have pred sort, got %a" Sort.print pred_sort))
  | ProofSort.DepRes { bound_var; pred } ->
    let* (ce_pred, pred_sort) = elab_se rs gamma Effect.Spec pred in
    (match Sort.shape pred_sort with
     | Sort.Pred _inner_sort ->
       return (ProofSort.DepRes { bound_var; pred = ce_pred })
     | _ -> fail "dep-res: must have pred sort")

let elab_pf (rs : RSig.t) (gamma : Context.t) (eff : Effect.t) (pf : (SurfExpr.se, Var.t) ProofSort.t) : (CoreExpr.typed_ce, Var.t) ProofSort.t ElabM.t =
  let rec go gamma = function
    | [] -> return []
    | entry :: rest ->
      let* entry' = elab_pf_entry rs gamma eff entry in
      let* gamma' = match entry' with
        | ProofSort.Comp { var; sort; eff } -> return (Context.extend var sort eff gamma)
        | ProofSort.Log _ | ProofSort.Res _ -> return gamma
        | ProofSort.DepRes { pred; bound_var; _ } ->
          let pred_sort = (CoreExpr.info pred)#sort in
          (match Sort.shape pred_sort with
           | Sort.Pred inner -> return (Context.extend bound_var inner Effect.Spec gamma)
           | _ -> fail "dep-res: expected pred sort in elab_pf")
      in
      let* rest' = go gamma' rest in
      return (entry' :: rest')
  in
  go gamma pf

(* Make a typed core bool literal *)
let mk_bool b = CoreExpr.mk (mk_info bool_sort) (CoreExpr.BoolLit b)
let mk_true = mk_bool true
let mk_false = mk_bool false

(* Make ce1 == ce2 as a typed core expression *)
let mk_eq ce1 ce2 = CoreExpr.mk (mk_info bool_sort) (CoreExpr.Eq (ce1, ce2))

(* ---------- refined primitive signatures ---------- *)

(* Helper constructors for building proof sorts *)
let ce_of_var v sort = CoreExpr.mk (mk_info sort) (CoreExpr.Var v)
let mk_prim_app p args =
  let (arg_sort, ret_sort, _eff) = Typecheck.prim_signature p in
  CoreExpr.mk (mk_info ret_sort) (CoreExpr.App (p, CoreExpr.mk (mk_info arg_sort) (CoreExpr.Tuple args)))
let mk_not ce = CoreExpr.mk (mk_info bool_sort) (CoreExpr.Not ce)

(* Lift a plain FunSig/FunDef to an RF, creating fresh variables *)
let lift_to_rf arg ret eff =
  let* x = fresh SourcePos.dummy in
  let* y = fresh SourcePos.dummy in
  return RFunType.{
    domain = [ProofSort.Comp { var = x; sort = arg; eff }];
    codomain = [ProofSort.Comp { var = y; sort = ret; eff }];
    eff;
  }

(* Monadic lookup for refined function types, lifting plain entries *)
let lookup_rf_m (rs : RSig.t) (f : string) : (CoreExpr.typed_ce, Var.t) RFunType.t option ElabM.t =
  match RSig.lookup_rf f rs with
  | Some rf -> return (Some rf)
  | None ->
    match RSig.lookup_fun f rs with
    | Some (arg, ret, eff) ->
      let* rf = lift_to_rf arg ret eff in
      return (Some rf)
    | None -> return None

(** Refined function type for a primitive, following the spec in refinement-types.md. *)
let rprim_signature (p : Prim.t) : (CoreExpr.typed_ce, Var.t) RFunType.t ElabM.t =
  match p with
  (* Arithmetic: (x:int, y:int) ⊸ (z:int, prop: z == prim(x,y) [log]) [pure] *)
  | Prim.Add | Prim.Sub | Prim.Mul ->
    let* x = fresh SourcePos.dummy in
    let* y = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    let result_expr = mk_prim_app p [ce_of_var x int_sort; ce_of_var y int_sort] in
    return RFunType.{ domain = [ProofSort.Comp { var = x; sort = int_sort; eff = Effect.Pure };
                ProofSort.Comp { var = y; sort = int_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = z; sort = int_sort; eff = Effect.Pure };
                  ProofSort.Log { prop =mk_eq (ce_of_var z int_sort) result_expr }];
      eff = Effect.Pure }

  (* Div: (x:int, y:int, pre: not(y == 0) [log]) ⊸ (z:int, prop: z == x/y [log]) [pure] *)
  | Prim.Div ->
    let* x = fresh SourcePos.dummy in
    let* y = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    let zero = CoreExpr.mk (mk_info int_sort) (CoreExpr.IntLit 0) in
    return RFunType.{ domain = [ProofSort.Comp { var = x; sort = int_sort; eff = Effect.Pure };
                ProofSort.Comp { var = y; sort = int_sort; eff = Effect.Pure };
                ProofSort.Log { prop = mk_not (mk_eq (ce_of_var y int_sort) zero) }];
      codomain = [ProofSort.Comp { var = z; sort = int_sort; eff = Effect.Pure };
                  ProofSort.Log { prop =mk_eq (ce_of_var z int_sort) (mk_prim_app Prim.Div [ce_of_var x int_sort; ce_of_var y int_sort]) }];
      eff = Effect.Pure }

  (* Comparisons: (x:int, y:int) ⊸ (z:bool, prop: z == x cmp y [log]) [pure] *)
  | Prim.Lt | Prim.Le | Prim.Gt | Prim.Ge ->
    let* x = fresh SourcePos.dummy in
    let* y = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    let result_expr = mk_prim_app p [ce_of_var x int_sort; ce_of_var y int_sort] in
    return RFunType.{ domain = [ProofSort.Comp { var = x; sort = int_sort; eff = Effect.Pure };
                ProofSort.Comp { var = y; sort = int_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { prop =mk_eq (ce_of_var z bool_sort) result_expr }];
      eff = Effect.Pure }

  (* Logic: same pattern as comparisons *)
  | Prim.And ->
    let* x = fresh SourcePos.dummy in
    let* y = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    return RFunType.{ domain = [ProofSort.Comp { var = x; sort = bool_sort; eff = Effect.Pure };
                ProofSort.Comp { var = y; sort = bool_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { prop =mk_eq (ce_of_var z bool_sort) (CoreExpr.mk (mk_info bool_sort) (CoreExpr.And (ce_of_var x bool_sort, ce_of_var y bool_sort))) }];
      eff = Effect.Pure }

  | Prim.Or ->
    let* x = fresh SourcePos.dummy in
    let* y = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    return RFunType.{ domain = [ProofSort.Comp { var = x; sort = bool_sort; eff = Effect.Pure };
                ProofSort.Comp { var = y; sort = bool_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { prop =mk_eq (ce_of_var z bool_sort) (mk_prim_app Prim.Or [ce_of_var x bool_sort; ce_of_var y bool_sort]) }];
      eff = Effect.Pure }

  | Prim.Not ->
    let* x = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    return RFunType.{ domain = [ProofSort.Comp { var = x; sort = bool_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { prop =mk_eq (ce_of_var z bool_sort) (mk_not (ce_of_var x bool_sort)) }];
      eff = Effect.Pure }

  (* Eq[A]: (x:A, y:A) ⊸ (z:bool, prop: z == (x == y) [log]) [pure] *)
  | Prim.Eq ty ->
    let a_sort = ty in
    let* x = fresh SourcePos.dummy in
    let* y = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    return RFunType.{ domain = [ProofSort.Comp { var = x; sort = a_sort; eff = Effect.Pure };
                ProofSort.Comp { var = y; sort = a_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { prop =mk_eq (ce_of_var z bool_sort) (mk_eq (ce_of_var x a_sort) (ce_of_var y a_sort)) }];
      eff = Effect.Pure }

  (* New[A]: (x:A) ⊸ (p:ptr A, r:Own[A](p) @ x [res]) [impure] *)
  | Prim.New ty ->
    let a_sort = ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let pred_sort = Sort.mk loc_dummy (Sort.Pred a_sort) in
    let* x = fresh SourcePos.dummy in
    let* p = fresh SourcePos.dummy in
    let own_p = CoreExpr.mk (mk_info pred_sort) (CoreExpr.App (Prim.Own ty, ce_of_var p ptr_sort)) in
    return RFunType.{ domain = [ProofSort.Comp { var = x; sort = a_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { var = p; sort = ptr_sort; eff = Effect.Pure };
                  ProofSort.Res { pred = own_p; value = ce_of_var x a_sort }];
      eff = Effect.Impure }

  (* Del[A]: (p:ptr A, x:A [spec], r:Own[A](p) @ x [res]) ⊸ () [impure] *)
  | Prim.Del ty ->
    let a_sort = ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let pred_sort = Sort.mk loc_dummy (Sort.Pred a_sort) in
    let* p = fresh SourcePos.dummy in
    let* x = fresh SourcePos.dummy in
    let own_p = CoreExpr.mk (mk_info pred_sort) (CoreExpr.App (Prim.Own ty, ce_of_var p ptr_sort)) in
    return RFunType.{ domain = [ProofSort.Comp { var = p; sort = ptr_sort; eff = Effect.Pure };
                ProofSort.Comp { var = x; sort = a_sort; eff = Effect.Spec };
                ProofSort.Res { pred = own_p; value = ce_of_var x a_sort }];
      codomain = [];
      eff = Effect.Impure }

  (* Get[A]: (p:ptr A, r:(x:A).Own[A](p) [res]) ⊸ (v:A, pf: v = x [log], r':Own[A](p) @ x [res]) [impure] *)
  | Prim.Get ty ->
    let a_sort = ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let pred_sort = Sort.mk loc_dummy (Sort.Pred a_sort) in
    let* p = fresh SourcePos.dummy in
    let* x = fresh SourcePos.dummy in
    let* v = fresh SourcePos.dummy in
    let own_p = CoreExpr.mk (mk_info pred_sort) (CoreExpr.App (Prim.Own ty, ce_of_var p ptr_sort)) in
    return RFunType.{ domain = [ProofSort.Comp { var = p; sort = ptr_sort; eff = Effect.Pure };
                ProofSort.DepRes { bound_var = x; pred = own_p }];
      codomain = [ProofSort.Comp { var = v; sort = a_sort; eff = Effect.Pure };
                  ProofSort.Log { prop = mk_eq (ce_of_var v a_sort) (ce_of_var x a_sort) };
                  ProofSort.Res { pred = own_p; value = ce_of_var x a_sort }];
      eff = Effect.Impure }

  (* Set[A]: (p:ptr A, v:A, r:(x:A).Own[A](p) [res]) ⊸ (r':Own[A](p) @ v [res]) [impure] *)
  | Prim.Set ty ->
    let a_sort = ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let pred_sort = Sort.mk loc_dummy (Sort.Pred a_sort) in
    let* p = fresh SourcePos.dummy in
    let* v = fresh SourcePos.dummy in
    let* x = fresh SourcePos.dummy in
    let own_p = CoreExpr.mk (mk_info pred_sort) (CoreExpr.App (Prim.Own ty, ce_of_var p ptr_sort)) in
    return RFunType.{ domain = [ProofSort.Comp { var = p; sort = ptr_sort; eff = Effect.Pure };
                ProofSort.Comp { var = v; sort = a_sort; eff = Effect.Pure };
                ProofSort.DepRes { bound_var = x; pred = own_p }];
      codomain = [ProofSort.Res { pred = own_p; value = ce_of_var v a_sort }];
      eff = Effect.Impure }

  (* Own[A]: (p:ptr A) ⊸ (r:pred A) [spec] — same as core signature *)
  | Prim.Own ty ->
    let a_sort = ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let pred_sort = Sort.mk loc_dummy (Sort.Pred a_sort) in
    let* p = fresh SourcePos.dummy in
    let* r = fresh SourcePos.dummy in
    return RFunType.{ domain = [ProofSort.Comp { var = p; sort = ptr_sort; eff = Effect.Spec }];
      codomain = [ProofSort.Comp { var = r; sort = pred_sort; eff = Effect.Spec }];
      eff = Effect.Spec }

(* ---------- delta monotonicity check ---------- *)

(* Invariant: for every judgement RS; Delta |- ... -| Delta' ~> Ct,
   Delta ⊓ Delta' = Delta' (output only consumes resources, never creates them).
   Here ⊓ is the lattice meet in the total order Used ≤ Opt ≤ Avail. *)

let delta_check_enabled = ref false

let assert_delta_below delta delta' =
  if not !delta_check_enabled then return ()
  else
    match RCtx.lattice_merge delta delta' with
    | Error msg ->
      fail (Format.asprintf "delta invariant: %s" msg)
    | Ok merged ->
      if RCtx.usage_equal merged delta' then return ()
      else fail (Format.asprintf
        "delta invariant violated: output context not below input@.  input:  %a@.  output: %a"
        RCtx.print delta RCtx.print delta')

(* ---------- checked tree type aliases ---------- *)

type checked_crt = (CoreExpr.typed_ce, < loc : SourcePos.t >, Var.t) RefinedExpr.crt
type checked_lpf = (CoreExpr.typed_ce, < loc : SourcePos.t >, Var.t) RefinedExpr.lpf
type checked_rpf = (CoreExpr.typed_ce, < loc : SourcePos.t >, Var.t) RefinedExpr.rpf
type checked_spine = (CoreExpr.typed_ce, < loc : SourcePos.t >, Var.t) RefinedExpr.spine

(* ---------- typing judgements ---------- *)

(* Logical fact synthesis: RS; Delta |- lpf => ce -| Delta' ~> Ct *)
let rec synth_lpf (rs : RSig.t) (delta : RCtx.t) (lpf : RefinedExpr.parsed_lpf) : (checked_lpf * CoreExpr.typed_ce * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RefinedExpr.lpf_info lpf in
  let pos = binfo#loc in
  match RefinedExpr.lpf_shape lpf with
  | RefinedExpr.LVar x ->
    (match RCtx.lookup_log x delta with
     | Some ce ->
       let checked = RefinedExpr.mk_lpf binfo (RefinedExpr.LVar x) in
       return (checked, ce, delta, Constraint.top pos)
     | None -> fail (Format.asprintf "logical variable %a not found" Var.print x))

  | RefinedExpr.LAuto ->
    fail "auto cannot synthesize; use in checking mode"

  | RefinedExpr.LUnfold (f, se_arg) ->
    let* (ce_arg, _sort) = elab_and_synth rs delta Effect.Spec se_arg in
    let cs = RSig.comp rs in
    (match Sig.lookup_fundef f cs with
     | Some (param, _arg_sort, ret_sort, eff, body) ->
       if not (Effect.sub eff Effect.Spec) then
         fail (Format.asprintf "unfold: function %s must be spec" f)
       else
         let call_result = CoreExpr.mk (mk_info ret_sort) (CoreExpr.Call (f, ce_arg)) in
         let subst_body = Subst.apply_ce (Subst.extend_var param ce_arg Subst.empty) body in
         let prop = mk_eq call_result subst_body in
         let checked = RefinedExpr.mk_lpf binfo (RefinedExpr.LUnfold (f, ce_arg)) in
         return (checked, prop, delta, Constraint.top pos)
     | None -> fail (Format.asprintf "unfold: function %s not found or not a FunDef" f))

  | RefinedExpr.LAnnot (lpf', se) ->
    let gamma = RCtx.erase delta in
    let* ce = elab_se_check rs gamma se bool_sort Effect.Spec in
    let* (checked_lpf', delta', ct) = check_lpf rs delta lpf' ce in
    let checked = RefinedExpr.mk_lpf binfo (RefinedExpr.LAnnot (checked_lpf', ce)) in
    return (checked, ce, delta', ct)

  | RefinedExpr.LOpenRet rpf ->
    let* (checked_rpf, ce_pred, ce_val, delta', ct) = synth_rpf rs delta rpf in
    (match CoreExpr.shape ce_pred with
     | CoreExpr.Return ce1 ->
       let checked = RefinedExpr.mk_lpf binfo (RefinedExpr.LOpenRet checked_rpf) in
       return (checked, mk_eq ce1 ce_val, delta', ct)
     | _ -> fail "open-ret: synthesized predicate is not a return")

(* Logical fact checking: RS; Delta |- lpf <= ce -| Delta' ~> Ct *)
and check_lpf (rs : RSig.t) (delta : RCtx.t) (lpf : RefinedExpr.parsed_lpf) (ce : CoreExpr.typed_ce) : (checked_lpf * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RefinedExpr.lpf_info lpf in
  let pos = binfo#loc in
  match RefinedExpr.lpf_shape lpf with
  | RefinedExpr.LAuto ->
    let checked = RefinedExpr.mk_lpf binfo RefinedExpr.LAuto in
    return (checked, delta, Constraint.atom pos ce)

  | _ ->
    let* (checked_lpf, ce_synth, delta', ct) = synth_lpf rs delta lpf in
    return (checked_lpf, delta', Constraint.conj pos ct (Constraint.impl pos ce_synth (Constraint.atom pos ce)))

(* Resource fact synthesis: RS; Delta |- rpf => ce @ ce' -| Delta' ~> Ct *)
and synth_rpf (rs : RSig.t) (delta : RCtx.t) (rpf : RefinedExpr.parsed_rpf) : (checked_rpf * CoreExpr.typed_ce * CoreExpr.typed_ce * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RefinedExpr.rpf_info rpf in
  let pos = binfo#loc in
  match RefinedExpr.rpf_shape rpf with
  | RefinedExpr.RVar x ->
    let* (pred, value, delta') = lift (RCtx.use_resource x delta) in
    let checked = RefinedExpr.mk_rpf binfo (RefinedExpr.RVar x) in
    return (checked, pred, value, delta', Constraint.top pos)

  | RefinedExpr.RAnnot (rpf', se1, se2) ->
    let gamma = RCtx.erase delta in
    let* (ce1, _sort1) = elab_se rs gamma Effect.Spec se1 in
    let* (ce2, _sort2) = elab_se rs gamma Effect.Spec se2 in
    let* (checked_rpf', delta', ct) = check_rpf rs delta rpf' ce1 ce2 in
    let checked = RefinedExpr.mk_rpf binfo (RefinedExpr.RAnnot (checked_rpf', ce1, ce2)) in
    return (checked, ce1, ce2, delta', ct)

  | RefinedExpr.RMakeRet _ | RefinedExpr.RMakeTake _ ->
    fail "make-ret/make-take cannot synthesize; use in checking mode"

(* Resource fact checking: RS; Delta |- rpf <= ce @ ce' -| Delta' ~> Ct *)
and check_rpf (rs : RSig.t) (delta : RCtx.t) (rpf : RefinedExpr.parsed_rpf) (ce1 : CoreExpr.typed_ce) (ce2 : CoreExpr.typed_ce) : (checked_rpf * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RefinedExpr.rpf_info rpf in
  let pos = binfo#loc in
  match RefinedExpr.rpf_shape rpf with
  | RefinedExpr.RMakeRet lpf' ->
    (match CoreExpr.shape ce1 with
     | CoreExpr.Return ce_a ->
       let* (checked_lpf', delta', ct) = check_lpf rs delta lpf' (mk_eq ce_a ce2) in
       let checked = RefinedExpr.mk_rpf binfo (RefinedExpr.RMakeRet checked_lpf') in
       return (checked, delta', ct)
     | _ -> fail "make-ret: expected return predicate")

  | RefinedExpr.RMakeTake crt ->
    (match CoreExpr.shape ce1 with
     | CoreExpr.Take ((x, _), pred_expr, pred_body) ->
       let pred_sort = (CoreExpr.info pred_expr)#sort in
       (match Sort.shape pred_sort with
        | Sort.Pred inner_sort ->
          let pf = [
            ProofSort.Comp { var = x; sort = inner_sort; eff = Effect.Spec };
            ProofSort.Res { pred = pred_expr; value = ce_of_var x inner_sort };
            ProofSort.Res { pred = pred_body; value = ce2 };
          ] in
          let eff = Effect.Spec in
          let* (checked_crt, delta', ct) = check_crt rs delta eff crt pf in
          let checked = RefinedExpr.mk_rpf binfo (RefinedExpr.RMakeTake checked_crt) in
          return (checked, delta', ct)
        | _ -> fail "make-take: predicate must have pred sort")
     | _ -> fail "make-take: expected take predicate")

  | _ ->
    let* (checked_rpf, ce1_synth, ce2_synth, delta', ct) = synth_rpf rs delta rpf in
    let eq_ct = Constraint.conj pos (Constraint.atom pos (mk_eq ce1_synth ce1))
                                    (Constraint.atom pos (mk_eq ce2_synth ce2)) in
    return (checked_rpf, delta', Constraint.conj pos ct eq_ct)

(* Core refined term synthesis: RS; Delta |-[eff] crt => Pf -| Delta' ~> Ct *)
and synth_crt (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (crt : RefinedExpr.parsed_crt) : (checked_crt * (CoreExpr.typed_ce, Var.t) ProofSort.t * RCtx.t * Constraint.typed_ct) ElabM.t =
  let* (checked, pf, delta', ct) = synth_crt_impl rs delta eff crt in
  let* () = assert_delta_below delta delta' in
  return (checked, pf, delta', ct)

and synth_crt_impl (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (crt : RefinedExpr.parsed_crt) : (checked_crt * (CoreExpr.typed_ce, Var.t) ProofSort.t * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RefinedExpr.crt_info crt in
  match RefinedExpr.crt_shape crt with
  | RefinedExpr.CAnnot (crt', se_pf) ->
    let gamma = RCtx.erase delta in
    let* pf = elab_pf rs gamma eff se_pf in
    let* (checked_crt', delta', ct) = check_crt rs delta eff crt' pf in
    let checked = RefinedExpr.mk_crt binfo (RefinedExpr.CAnnot (checked_crt', pf)) in
    return (checked, pf, delta', ct)

  | RefinedExpr.CCall (f, spine) ->
    let* rf_opt = lookup_rf_m rs f in
    (match rf_opt with
     | Some rf ->
       if not (Effect.sub rf.eff eff) then
         fail (Format.asprintf "call %s: effect %a not allowed at %a"
           f Effect.print rf.eff Effect.print eff)
       else
         let eff'' = Effect.purify eff in
         let* (checked_spine, pf, delta', ct) = check_spine rs delta eff'' spine rf in
         let checked = RefinedExpr.mk_crt binfo (RefinedExpr.CCall (f, checked_spine)) in
         return (checked, pf, delta', ct)
     | None -> fail (Format.asprintf "function %s not found" f))

  | RefinedExpr.CPrimApp (prim, spine) ->
    let* rf = rprim_signature prim in
    if not (Effect.sub rf.eff eff) then
      fail (Format.asprintf "prim %a effect %a not allowed at %a"
        Prim.print prim Effect.print rf.eff Effect.print eff)
    else
      let eff'' = Effect.purify eff in
      let* (checked_spine, pf, delta', ct) = check_spine rs delta eff'' spine rf in
      let checked = RefinedExpr.mk_crt binfo (RefinedExpr.CPrimApp (prim, checked_spine)) in
      return (checked, pf, delta', ct)

  | RefinedExpr.COpenTake rpf ->
    let* (checked_rpf, ce_pred, ce_val, delta', ct) = synth_rpf rs delta rpf in
    (match CoreExpr.shape ce_pred with
     | CoreExpr.Take ((x, _), ce1, ce2) ->
       let pred_sort = (CoreExpr.info ce1)#sort in
       (match Sort.shape pred_sort with
        | Sort.Pred inner_sort ->
          let pf = [
            ProofSort.Comp { var = x; sort = inner_sort; eff = Effect.Spec };
            ProofSort.Res { pred = ce1; value = ce_of_var x inner_sort };
            ProofSort.Res { pred = ce2; value = ce_val };
          ] in
          let checked = RefinedExpr.mk_crt binfo (RefinedExpr.COpenTake checked_rpf) in
          return (checked, pf, delta', ct)
        | _ -> fail "open-take: predicate must have pred sort")
     | _ -> fail "open-take: expected take predicate")

  | RefinedExpr.CIter (se_pred, pat, crt1, crt2) ->
    (* iter requires impure effect *)
    if not (Effect.sub Effect.Impure eff) then
      fail "iter: requires impure effect"
    else
    (* Elaborate predicate at spec effect *)
    let* (ce_pred, pred_sort) = elab_and_synth rs delta Effect.Spec se_pred in
    (match Sort.shape pred_sort with
     | Sort.Pred inner_sort ->
       (match Sort.shape inner_sort with
        | Sort.App (_dsort_name, args) ->
          let cs = RSig.comp rs in
          let* next_label = lift (Label.of_string "Next") in
          let* done_label = lift (Label.of_string "Done") in
          (* Look up constructor sorts with type parameter substitution *)
          let* a_sort = lift (CtorLookup.lookup cs next_label args) in
          let* b_sort = lift (CtorLookup.lookup cs done_label args) in
          let step_sort = inner_sort in
          (* Build init proof sort: x:A [pure], y:ce @ Next(x) [res], pfnil *)
          let* x_var = fresh SourcePos.dummy in
          let ce_x = ce_of_var x_var a_sort in
          let ce_next_x = CoreExpr.mk (mk_info step_sort) (CoreExpr.Inject (next_label, ce_x)) in
          let init_pf = [
            ProofSort.Comp { var = x_var; sort = a_sort; eff = Effect.Pure };
            ProofSort.Res { pred = ce_pred; value = ce_next_x };
          ] in
          (* Check init (pure) *)
          let* (checked_crt1, delta', ct) = check_crt rs delta Effect.Pure crt1 init_pf in
          (* Build body input context: delta' + pattern bindings from init_pf *)
          let gamma = RCtx.erase delta' in
          let* delta_pat = lift (rpat_match cs gamma pat init_pf) in
          let delta_body = RCtx.concat delta' delta_pat in
          (* Build body proof sort: z:D(A,B) [pure], y2:ce @ z [res], pfnil *)
          let* z_var = fresh SourcePos.dummy in
          let ce_z = ce_of_var z_var step_sort in
          let body_pf = [
            ProofSort.Comp { var = z_var; sort = step_sort; eff = Effect.Pure };
            ProofSort.Res { pred = ce_pred; value = ce_z };
          ] in
          (* Check body (impure) *)
          let* (checked_crt2, delta_out, ct') = check_crt rs delta_body Effect.Impure crt2 body_pf in
          (* Validate output context: extension resources must be consumed *)
          let n = RCtx.length delta' in
          let (_delta_base, delta_pat_out) = RCtx.split n delta_out in
          if not (RCtx.zero delta_pat_out) then
            fail "iter: loop body did not consume pattern resources"
          else
          (* Build result proof sort: z:B [pure], y:ce @ Done(z) [res], pfnil *)
          let* zr_var = fresh SourcePos.dummy in
          let ce_zr = ce_of_var zr_var b_sort in
          let ce_done_z = CoreExpr.mk (mk_info step_sort) (CoreExpr.Inject (done_label, ce_zr)) in
          let result_pf = [
            ProofSort.Comp { var = zr_var; sort = b_sort; eff = Effect.Pure };
            ProofSort.Res { pred = ce_pred; value = ce_done_z };
          ] in
          (* Build constraint: Ct ∧ ∀x:A. Ct' *)
          (* x is the comp var bound by the pattern *)
          (match pat with
           | RPat.Single x_pat :: _ ->
             let pos = binfo#loc in
             let result_ct = Constraint.conj pos ct (Constraint.forall_ pos x_pat a_sort ct') in
             let checked = RefinedExpr.mk_crt binfo (RefinedExpr.CIter (ce_pred, pat, checked_crt1, checked_crt2)) in
             return (checked, result_pf, delta', result_ct)
           | _ -> fail "iter: pattern must start with a single variable")
        | _ -> fail "iter: predicate argument must be a datasort application")
     | _ -> fail "iter: expression must have pred sort")

  | RefinedExpr.CTuple spine ->
    let* (checked_spine, delta', ct) = _check_tuple rs delta eff spine [] in
    let checked = RefinedExpr.mk_crt binfo (RefinedExpr.CTuple checked_spine) in
    return (checked, [], delta', ct)

  | _ -> fail "cannot synthesize sort for this refined term"

(* Core refined term checking: RS; Delta |-[eff] crt <= Pf -| Delta' ~> Ct *)
and check_crt (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (crt : RefinedExpr.parsed_crt) (pf : (CoreExpr.typed_ce, Var.t) ProofSort.t) : (checked_crt * RCtx.t * Constraint.typed_ct) ElabM.t =
  let* (checked, delta', ct) = check_crt_impl rs delta eff crt pf in
  let* () = assert_delta_below delta delta' in
  return (checked, delta', ct)

and check_crt_impl (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (crt : RefinedExpr.parsed_crt) (pf : (CoreExpr.typed_ce, Var.t) ProofSort.t) : (checked_crt * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RefinedExpr.crt_info crt in
  let pos = binfo#loc in
  match RefinedExpr.crt_shape crt with
  | RefinedExpr.CLet (pat, crt1, crt2) ->
    let* (checked_crt1, pf', delta', ct1) = synth_crt rs delta eff crt1 in
    let gamma = RCtx.erase delta' in
    let cs = RSig.comp rs in
    let* delta_pat = lift (rpat_match cs gamma pat pf') in
    let delta_ext = RCtx.concat delta' delta_pat in
    let* (checked_crt2, delta'', ct2) = check_crt rs delta_ext eff crt2 pf in
    let n = RCtx.length delta' in
    let n_pat = RCtx.length delta_pat in
    let (delta_out, delta_pat_out) = RCtx.split n delta'' in
    let _ = n_pat in
    if not (RCtx.zero delta_pat_out) then
      fail "let: pattern resources not fully consumed"
    else
      let checked = RefinedExpr.mk_crt binfo (RefinedExpr.CLet (pat, checked_crt1, checked_crt2)) in
      return (checked, delta_out, Constraint.conj pos ct1 ct2)

  | RefinedExpr.CIf (_x, se, crt1, crt2) ->
    let eff' = Effect.purify eff in
    let gamma = RCtx.erase delta in
    let* ce = elab_se_check rs gamma se bool_sort eff' in
    let* x_true = fresh SourcePos.dummy in
    let* x_false = fresh SourcePos.dummy in
    let delta_true = RCtx.extend_log x_true (mk_eq ce mk_true) delta in
    let delta_false = RCtx.extend_log x_false (mk_eq ce mk_false) delta in
    let* (checked_crt1, delta1_ext, ct1) = check_crt rs delta_true eff crt1 pf in
    let* (checked_crt2, delta2_ext, ct2) = check_crt rs delta_false eff crt2 pf in
    let n = RCtx.length delta in
    let (delta1, _) = RCtx.split n delta1_ext in
    let (delta2, _) = RCtx.split n delta2_ext in
    let* delta_merged = lift (RCtx.merge delta1 delta2) in
    let ct = Constraint.conj pos
      (Constraint.impl pos (mk_eq ce mk_true) ct1)
      (Constraint.impl pos (mk_eq ce mk_false) ct2) in
    let checked = RefinedExpr.mk_crt binfo (RefinedExpr.CIf (_x, ce, checked_crt1, checked_crt2)) in
    return (checked, delta_merged, ct)

  | RefinedExpr.CCase (_y, se, branches) ->
    let eff' = Effect.purify eff in
    let gamma = RCtx.erase delta in
    let cs = RSig.comp rs in
    let* (ce, ce_sort) = elab_se rs gamma eff' se in
    (match Sort.shape ce_sort with
     | Sort.App (dsort_name, _args) ->
       let decl_opt = Sig.lookup_sort dsort_name cs in
       let type_decl_opt = Sig.lookup_type dsort_name cs in
       (match decl_opt, type_decl_opt with
        | Some decl, _ ->
          let ctors = decl.DsortDecl.ctors in
          let* (checked_branches, delta', ct) = check_case_branches pos rs delta eff' ce ce_sort ctors branches pf in
          let checked = RefinedExpr.mk_crt binfo (RefinedExpr.CCase (_y, ce, checked_branches)) in
          return (checked, delta', ct)
        | _, Some decl ->
          let ctors = decl.DtypeDecl.ctors in
          let* (checked_branches, delta', ct) = check_case_branches pos rs delta eff' ce ce_sort ctors branches pf in
          let checked = RefinedExpr.mk_crt binfo (RefinedExpr.CCase (_y, ce, checked_branches)) in
          return (checked, delta', ct)
        | None, None ->
          fail (Format.asprintf "case: type %a not found" Dsort.print dsort_name))
     | _ -> fail "case: scrutinee must have datasort type")

  | RefinedExpr.CTuple spine ->
    let* (checked_spine, delta', ct) = _check_tuple rs delta eff spine pf in
    let checked = RefinedExpr.mk_crt binfo (RefinedExpr.CTuple checked_spine) in
    return (checked, delta', ct)

  | RefinedExpr.CExfalso ->
    let delta' = RCtx.affinize delta in
    let checked = RefinedExpr.mk_crt binfo RefinedExpr.CExfalso in
    return (checked, delta', Constraint.bot pos)

  | _ ->
    let* (checked_crt, pf', delta', ct) = synth_crt rs delta eff crt in
    let* ct' = pf_eq pos rs delta' pf' pf in
    return (checked_crt, delta', Constraint.conj pos ct ct')

(* Spine checking: RS; Delta |-[eff] rsp : Pf1 -o Pf2 >> Pf -| Delta' ~> Ct *)
and check_spine (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (spine : RefinedExpr.parsed_spine) (rf : (CoreExpr.typed_ce, Var.t) RFunType.t) : (checked_spine * (CoreExpr.typed_ce, Var.t) ProofSort.t * RCtx.t * Constraint.typed_ct) ElabM.t =
  check_spine_inner rs delta eff spine rf.domain rf.codomain

and check_spine_inner rs delta eff spine domain codomain =
  let binfo = RefinedExpr.spine_info spine in
  let pos = binfo#loc in
  match RefinedExpr.spine_shape spine, domain with
  | RefinedExpr.SNil, [] ->
    let checked = RefinedExpr.mk_spine binfo RefinedExpr.SNil in
    return (checked, codomain, delta, Constraint.top pos)

  | RefinedExpr.SCore (se, rest), (ProofSort.Comp { var; sort; eff = Effect.Pure } :: pf_rest) ->
    let gamma = RCtx.erase delta in
    let* ce = elab_se_check rs gamma se sort eff in
    let pf_rest' = ProofSort.subst var ce pf_rest in
    let codomain' = ProofSort.subst var ce codomain in
    let* (checked_rest, result_pf, delta', ct) = check_spine_inner rs delta eff rest pf_rest' codomain' in
    let checked = RefinedExpr.mk_spine binfo (RefinedExpr.SCore (ce, checked_rest)) in
    return (checked, result_pf, delta', ct)

  | RefinedExpr.SCore (se, rest), (ProofSort.Comp { var; sort; eff = Effect.Spec } :: pf_rest) ->
    let gamma = RCtx.erase delta in
    let* ce = elab_se_check rs gamma se sort Effect.Spec in
    let pf_rest' = ProofSort.subst var ce pf_rest in
    let codomain' = ProofSort.subst var ce codomain in
    let* (checked_rest, result_pf, delta', ct) = check_spine_inner rs delta eff rest pf_rest' codomain' in
    let checked = RefinedExpr.mk_spine binfo (RefinedExpr.SCore (ce, checked_rest)) in
    return (checked, result_pf, delta', ct)

  | RefinedExpr.SCore (_, _), (ProofSort.Comp { eff = Effect.Impure; _ } :: _) ->
    fail "spine: impure comp entry not allowed in proof sort"

  | RefinedExpr.SLog (lpf, rest), (ProofSort.Log { prop; _ } :: pf_rest) ->
    let* (checked_lpf, delta', ct) = check_lpf rs delta lpf prop in
    let* (checked_rest, result_pf, delta'', ct') = check_spine_inner rs delta' eff rest pf_rest codomain in
    let checked = RefinedExpr.mk_spine binfo (RefinedExpr.SLog (checked_lpf, checked_rest)) in
    return (checked, result_pf, delta'', Constraint.conj pos ct ct')

  | RefinedExpr.SRes (rpf, rest), (ProofSort.Res { pred; value; _ } :: pf_rest) ->
    let* (checked_rpf, delta', ct) = check_rpf rs delta rpf pred value in
    let* (checked_rest, result_pf, delta'', ct') = check_spine_inner rs delta' eff rest pf_rest codomain in
    let checked = RefinedExpr.mk_spine binfo (RefinedExpr.SRes (checked_rpf, checked_rest)) in
    return (checked, result_pf, delta'', Constraint.conj pos ct ct')

  | RefinedExpr.SRes (rpf, rest), (ProofSort.DepRes { bound_var; pred; _ } :: pf_rest) ->
    let* (checked_rpf, ce_pred_synth, ce_value, delta', ct) = synth_rpf rs delta rpf in
    let eq_ct = Constraint.atom pos (mk_eq pred ce_pred_synth) in
    let pf_rest' = ProofSort.subst bound_var ce_value pf_rest in
    let codomain' = ProofSort.subst bound_var ce_value codomain in
    let* (checked_rest, result_pf, delta'', ct') = check_spine_inner rs delta' eff rest pf_rest' codomain' in
    let checked = RefinedExpr.mk_spine binfo (RefinedExpr.SRes (checked_rpf, checked_rest)) in
    return (checked, result_pf, delta'', Constraint.conj pos (Constraint.conj pos ct eq_ct) ct')

  | _ -> fail "spine: argument/parameter mismatch"

(* Tuple checking: RS; Delta |-[eff] rsp : Pf -| Delta' ~> Ct *)
and _check_tuple rs delta eff spine pf =
  let binfo = RefinedExpr.spine_info spine in
  let pos = binfo#loc in
  match RefinedExpr.spine_shape spine, pf with
  | RefinedExpr.SNil, [] ->
    let checked = RefinedExpr.mk_spine binfo RefinedExpr.SNil in
    return (checked, delta, Constraint.top pos)

  | RefinedExpr.SCore (se, rest), (ProofSort.Comp { var; sort; eff = entry_eff } :: pf_rest) ->
    let gamma = RCtx.erase delta in
    let check_eff = match entry_eff with Effect.Spec -> Effect.Spec | _ -> eff in
    let* ce = elab_se_check rs gamma se sort check_eff in
    let pf_rest' = ProofSort.subst var ce pf_rest in
    let* (checked_rest, delta', ct) = _check_tuple rs delta eff rest pf_rest' in
    let checked = RefinedExpr.mk_spine binfo (RefinedExpr.SCore (ce, checked_rest)) in
    return (checked, delta', ct)

  | RefinedExpr.SLog (lpf, rest), (ProofSort.Log { prop; _ } :: pf_rest) ->
    let* (checked_lpf, delta', ct) = check_lpf rs delta lpf prop in
    let* (checked_rest, delta'', ct') = _check_tuple rs delta' eff rest pf_rest in
    let checked = RefinedExpr.mk_spine binfo (RefinedExpr.SLog (checked_lpf, checked_rest)) in
    return (checked, delta'', Constraint.conj pos ct ct')

  | RefinedExpr.SRes (rpf, rest), (ProofSort.Res { pred; value; _ } :: pf_rest) ->
    let* (checked_rpf, delta', ct) = check_rpf rs delta rpf pred value in
    let* (checked_rest, delta'', ct') = _check_tuple rs delta' eff rest pf_rest in
    let checked = RefinedExpr.mk_spine binfo (RefinedExpr.SRes (checked_rpf, checked_rest)) in
    return (checked, delta'', Constraint.conj pos ct ct')

  | RefinedExpr.SRes (rpf, rest), (ProofSort.DepRes { bound_var; pred; _ } :: pf_rest) ->
    let* (checked_rpf, ce_pred_synth, ce_value, delta', ct) = synth_rpf rs delta rpf in
    let eq_ct = Constraint.atom pos (mk_eq pred ce_pred_synth) in
    let pf_rest' = ProofSort.subst bound_var ce_value pf_rest in
    let* (checked_rest, delta'', ct') = _check_tuple rs delta' eff rest pf_rest' in
    let checked = RefinedExpr.mk_spine binfo (RefinedExpr.SRes (checked_rpf, checked_rest)) in
    return (checked, delta'', Constraint.conj pos (Constraint.conj pos ct eq_ct) ct')

  | _ -> fail "tuple: argument/entry mismatch"

(* Case branch checking *)
and check_case_branches pos rs delta eff ce ce_sort ctors branches pf =
  let* (checked_branches, branch_results) = check_branches_list pos rs delta eff ce ce_sort ctors branches pf in
  let (deltas, cts) = List.split branch_results in
  let* delta_merged = lift (RCtx.merge_n deltas) in
  let ct = List.fold_left (Constraint.conj pos) (Constraint.top pos) cts in
  return (checked_branches, delta_merged, ct)

and check_branches_list pos rs delta eff ce _ce_sort ctors branches pf =
  let rec go = function
    | [] -> return ([], [])
    | (label, ctor_sort) :: rest_ctors ->
      let branch = List.find_opt (fun (l, _, _) -> Label.compare l label = 0) branches in
      (match branch with
       | None -> fail (Format.asprintf "case: missing branch for %a" Label.print label)
       | Some (_, x, body) ->
         let ctor_sort' = ctor_sort in
         let* x_log = fresh SourcePos.dummy in
         let eq_prop = mk_eq ce (CoreExpr.mk (mk_info (CoreExpr.info ce)#sort) (CoreExpr.Inject (label, ce_of_var x ctor_sort'))) in
         let delta_ext = RCtx.extend_comp x ctor_sort' eff
                           (RCtx.extend_log x_log eq_prop delta) in
         let* (checked_body, delta_out, ct) = check_crt rs delta_ext eff body pf in
         let n = RCtx.length delta in
         let (delta_base, _) = RCtx.split n delta_out in
         let ct' = Constraint.forall_ pos x ctor_sort' (Constraint.impl pos eq_prop ct) in
         let* (rest_checked, rest) = go rest_ctors in
         return ((label, x, checked_body) :: rest_checked, (delta_base, ct') :: rest))
  in
  go ctors

(* Proof sort equality: RS; Delta |- Pf1 = Pf2 ~> Ct *)
and pf_eq (pos : SourcePos.t) (rs : RSig.t) (delta : RCtx.t) (pf1 : (CoreExpr.typed_ce, Var.t) ProofSort.t) (pf2 : (CoreExpr.typed_ce, Var.t) ProofSort.t) : Constraint.typed_ct ElabM.t =
  let _cs = RSig.comp rs in
  let _gamma = RCtx.erase delta in
  let rec go pf1 pf2 =
    match pf1, pf2 with
    | [], [] -> return (Constraint.top pos)

    | ProofSort.Comp { var = x; sort; eff } :: rest1,
      ProofSort.Comp { var = y; sort = sort2; eff = eff2 } :: rest2 ->
      if Sort.compare sort sort2 <> 0 then
        fail "pf_eq: sort mismatch"
      else if Effect.compare eff eff2 <> 0 then
        fail "pf_eq: effect mismatch"
      else
        let rest2' = ProofSort.subst y (ce_of_var x sort) rest2 in
        let* ct = go rest1 rest2' in
        return (Constraint.forall_ pos x sort ct)

    | ProofSort.Log { prop = ce1; _ } :: rest1,
      ProofSort.Log { prop = ce2; _ } :: rest2 ->
      let* ct = go rest1 rest2 in
      return (Constraint.conj pos (Constraint.atom pos (mk_eq ce1 ce2)) ct)

    | ProofSort.Res { pred = p1; value = v1; _ } :: rest1,
      ProofSort.Res { pred = p2; value = v2; _ } :: rest2 ->
      let* ct = go rest1 rest2 in
      let eq_ct = Constraint.conj pos (Constraint.atom pos (mk_eq p1 p2))
                                      (Constraint.atom pos (mk_eq v1 v2)) in
      return (Constraint.conj pos eq_ct ct)

    | ProofSort.DepRes { bound_var = y1; pred = ce1; _ } :: rest1,
      ProofSort.DepRes { bound_var = y2; pred = ce2; _ } :: rest2 ->
      let pred_sort = (CoreExpr.info ce1)#sort in
      (match Sort.shape pred_sort with
       | Sort.Pred inner_sort ->
         let* z = fresh SourcePos.dummy in
         let ce_z = ce_of_var z inner_sort in
         let rest1' = ProofSort.subst y1 ce_z rest1 in
         let rest2' = ProofSort.subst y2 ce_z rest2 in
         let* ct = go rest1' rest2' in
         return (Constraint.conj pos (Constraint.atom pos (mk_eq ce1 ce2))
                                     (Constraint.forall_ pos z inner_sort ct))
       | _ -> fail "pf_eq: dep-res pred must have pred sort")

    | _ -> fail "pf_eq: proof sort structure mismatch"
  in
  go pf1 pf2

(* Refined pattern matching: RS; Gamma |- q : Pf -| Delta *)
and rpat_match (_cs : _ Sig.t) (_gamma : Context.t) (pat : Var.t RPat.t) (pf : (CoreExpr.typed_ce, Var.t) ProofSort.t) : (RCtx.t, string) result =
  let ( let* ) = Result.bind in
  let rec go elems entries =
    match elems, entries with
    | [], [] -> Ok RCtx.empty
    | RPat.Single x :: rest_elems, ProofSort.Comp { var = y; sort; eff } :: rest_pf ->
      let ce_x = ce_of_var x sort in
      let rest_pf' = ProofSort.subst y ce_x rest_pf in
      let* delta = go rest_elems rest_pf' in
      Ok (RCtx.extend_comp x sort eff delta)
    | RPat.Single x :: rest_elems, ProofSort.Log { prop } :: rest_pf ->
      let* delta = go rest_elems rest_pf in
      Ok (RCtx.extend_log x prop delta)
    | RPat.Single x :: rest_elems, ProofSort.Res { pred; value } :: rest_pf ->
      let* delta = go rest_elems rest_pf in
      Ok (RCtx.extend_res x pred value Usage.Avail delta)
    | RPat.Pair (x, w) :: rest_elems, ProofSort.DepRes { bound_var = z; pred } :: rest_pf ->
      let pred_sort = (CoreExpr.info pred)#sort in
      (match Sort.shape pred_sort with
       | Sort.Pred inner_sort ->
         let ce_x = ce_of_var x inner_sort in
         let sub = Subst.extend_var z ce_x Subst.empty in
         let pred' = Subst.apply_ce sub pred in
         let rest_pf' = ProofSort.subst z ce_x rest_pf in
         let* delta = go rest_elems rest_pf' in
         let delta = RCtx.extend_res w pred' ce_x Usage.Avail delta in
         Ok (RCtx.extend_comp x inner_sort Effect.Spec delta)
       | _ -> Error "rpat_match: dep-res pred must have pred sort")
    | _ -> Ok RCtx.empty (* mismatch — will be caught during type checking *)
  in
  go pat pf

(* ---------- Program checking ---------- *)

let elab_fundecl_body rs param arg_sort ret_sort eff body_se =
  let cs = RSig.comp rs in
  let gamma = Context.extend param arg_sort (Effect.purify eff) Context.empty in
  Elaborate.check cs gamma body_se ret_sort eff

let check_rprog (prog : RProg.parsed) : (RSig.t * Constraint.typed_ct) ElabM.t =
  let rec check_rprog_decls rs ct_acc = function
    | [] -> return (rs, ct_acc)
    | decl :: rest ->
      let* (rs', ct_acc') = check_rprog_decl rs ct_acc decl in
      check_rprog_decls rs' ct_acc' rest

  and check_rprog_decl rs ct_acc = function
    | RProg.SortDecl d ->
      return (RSig.extend_sort rs d, ct_acc)
    | RProg.TypeDecl d ->
      return (RSig.extend_type rs d, ct_acc)
    | RProg.FunDecl { name; param; arg_sort; ret_sort; eff; body; _ } ->
      let rs_for_body = match eff with
        | Effect.Spec ->
          RSig.extend name (RSig.FunSig { arg = arg_sort; ret = ret_sort; eff }) rs
        | _ -> rs
      in
      let* ce = elab_fundecl_body rs_for_body param arg_sort ret_sort eff body in
      let entry = match eff with
        | Effect.Pure ->
          RSig.FunDef { param; arg = arg_sort; ret = ret_sort; eff; body = ce }
        | _ ->
          RSig.FunSig { arg = arg_sort; ret = ret_sort; eff }
      in
      return (RSig.extend name entry rs, ct_acc)
    | RProg.RFunDecl { name; pat; domain = se_domain; codomain = se_codomain; eff; body; loc } ->
      let gamma = Context.empty in
      let* domain = elab_pf rs gamma eff se_domain in
      let* gamma' = lift (ProofSort.bind gamma domain) in
      let* codomain = elab_pf rs gamma' eff se_codomain in
      let rf = RFunType.{ domain; codomain; eff } in
      let cs = RSig.comp rs in
      let* delta = lift (rpat_match cs gamma pat domain) in
      let rs' = match eff with
        | Effect.Pure -> rs
        | _ -> RSig.extend name (RSig.RFunSig rf) rs
      in
      let* (_checked, _delta', ct) = check_crt rs' delta eff body codomain in
      let entry = RSig.RFunSig rf in
      return (RSig.extend name entry rs, Constraint.conj loc ct_acc ct)
  in

  let* (rs, ct_decls) = check_rprog_decls RSig.empty (Constraint.top prog.loc) prog.decls in
  let gamma = Context.empty in
  let* main_pf = elab_pf rs gamma prog.main_eff prog.main_pf in
  let* (_checked, _delta, ct_main) = check_crt rs RCtx.empty prog.main_eff prog.main_body main_pf in
  return (rs, Constraint.conj prog.loc ct_decls ct_main)

module Test = struct
  let with_delta_check f =
    delta_check_enabled := true;
    Fun.protect ~finally:(fun () -> delta_check_enabled := false) f

  let test =
    let check_program name src =
      QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
        with_delta_check (fun () ->
          match ElabM.run Var.empty_supply (
            let* prog = Parse.parse_rprog src ~file:"test" in
            check_rprog prog
          ) with
          | Error msg -> QCheck.Test.fail_reportf "check: %s" msg
          | Ok _ -> true))
    in
    [ check_program "delta monotonicity: incr (new/get/set/del)"
        {|
          rfun incr (p : Ptr Int, [res] r : (take x : Int = Own[Int](p)))
            -> ([res] (take x' : Int = Own[Int](p))) [impure] =
            let (v, pf, r2) = Get[Int](p, res r);
            let (r3) = Set[Int](p, v + 1, res r2);
            (res r3)
          main : () [impure] =
            let (p, r) = New[Int](0);
            let ((x', r')) = incr(p, res r);
            Del[Int](p, x', res r')
        |};

      check_program "delta monotonicity: if-then-else with resources"
        {|
          main : () [impure] =
            let (p, r) = New[Int](0);
            let (v, pf, r2) = Get[Int](p, res r);
            let (b, bpf) = Eq[Int](v, 0);
            if [w] b
              then let (r3) = Set[Int](p, 1, res r2);
                   Del[Int](p, 1, res r3)
              else Del[Int](p, v, res r2)
        |};

      check_program "delta monotonicity: pure computation (no resources)"
        {|
          main : () [pure] = ()
        |};

      check_program "delta monotonicity: iter with heap cell"
        {|
          type Step(a, b) = { Next : a | Done : b }
          main : () [impure] =
            let (p, r) = New[Step(Int, ())](Next 0);
            let (z_done, r_done) = iter [Own[Step(Int, ())](p)] ((x, r_loop) =
              (0, res r) : (x : Int, [res] Own[Step(Int, ())](p) @ Next x)
            ) {
              let (v, pf, r2) = Get[Step(Int, ())](p, res r_loop);
              let (r3) = Set[Step(Int, ())](p, Done (), res r2);
              (Done (), res r3) : (z : Step(Int, ()), [res] Own[Step(Int, ())](p) @ z)
            };
            Del[Step(Int, ())](p, Done z_done, res r_done)
        |};
    ]

  let pf_eq = pf_eq
end
