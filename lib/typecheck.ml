let err_at pos msg =
  Error (Format.asprintf "@[%a:@ %s@]" SourcePos.print pos msg)

let err_at_f pos fmt =
  Format.kasprintf (fun msg -> err_at pos msg) fmt

let ( let* ) = Result.bind

let sort_equal (a : Sort.sort) (b : Sort.sort) = Sort.compare a b = 0

let dummy_info = object method loc = SourcePos.dummy end

let mk_sort s = Sort.mk dummy_info s

type typed_info = < loc : SourcePos.t; ctx : Context.t; sort : Sort.sort; eff : Effect.t >
type typed_ce = typed_info CoreExpr.t

let mk ctx pos sort eff shape : typed_ce =
  CoreExpr.mk (object
    method loc = pos
    method ctx = ctx
    method sort = sort
    method eff = eff
  end) shape

let mk_bind_info x sort eff ctx : typed_info =
  object
    method loc = Var.binding_site x
    method ctx = ctx
    method sort = sort
    method eff = eff
  end

(** Lift a [Sort.sort] into [typed_info Sort.t] so it can be embedded
    in a typed core-expression shape.  The extra fields (ctx, sort, eff)
    on each sort node are fillers — no client inspects them. *)
let lift_sort (s : Sort.sort) : typed_info Sort.t =
  Sort.map (fun loc_info ->
    (object
      method loc = loc_info#loc
      method ctx = Context.empty
      method sort = s
      method eff = Effect.Pure
    end : typed_info)) s

(** Signature of a primitive (sort-level). *)
let prim_signature (p : Prim.t) =
  let int_sort = mk_sort Sort.Int in
  let bool_sort = mk_sort Sort.Bool in
  let pair_int = mk_sort (Sort.Record [int_sort; int_sort]) in
  let pair_bool = mk_sort (Sort.Record [bool_sort; bool_sort]) in
  let unit_sort = mk_sort (Sort.Record []) in
  match p with
  | Add -> (pair_int, int_sort, Effect.Pure)
  | Sub -> (pair_int, int_sort, Effect.Pure)
  | Mul -> (pair_int, int_sort, Effect.Pure)
  | Div -> (pair_int, int_sort, Effect.Impure)
  | Lt -> (pair_int, bool_sort, Effect.Pure)
  | Le -> (pair_int, bool_sort, Effect.Pure)
  | Gt -> (pair_int, bool_sort, Effect.Pure)
  | Ge -> (pair_int, bool_sort, Effect.Pure)
  | And -> (pair_bool, bool_sort, Effect.Pure)
  | Or -> (pair_bool, bool_sort, Effect.Pure)
  | Not -> (bool_sort, bool_sort, Effect.Pure)
  | Eq a ->
    let sa = Sort.typ_to_sort a in
    (mk_sort (Sort.Record [sa; sa]), bool_sort, Effect.Pure)
  | New a ->
    let sa = Sort.typ_to_sort a in
    (sa, mk_sort (Sort.Ptr sa), Effect.Impure)
  | Del a ->
    let sa = Sort.typ_to_sort a in
    (mk_sort (Sort.Ptr sa), unit_sort, Effect.Impure)
  | Get a ->
    let sa = Sort.typ_to_sort a in
    (mk_sort (Sort.Ptr sa), sa, Effect.Impure)
  | Set a ->
    let sa = Sort.typ_to_sort a in
    (mk_sort (Sort.Record [mk_sort (Sort.Ptr sa); sa]), unit_sort, Effect.Impure)

(** Synthesize: S; G |- [eff0] ce ==> tau *)
let rec synth sig_ ctx eff0 ce =
  let pos = (CoreExpr.info ce)#loc in
  match CoreExpr.shape ce with
  | CoreExpr.Var x ->
    (match Context.lookup x ctx with
     | Some (s, var_eff) ->
       if Effect.sub var_eff eff0 then
         Ok (mk ctx pos s eff0 (CoreExpr.Var x))
       else
         err_at_f pos "variable %a has effect %a, not usable at effect %a"
           Var.print x Effect.print var_eff Effect.print eff0
     | None -> err_at_f pos "unbound variable %a" Var.print x)

  | CoreExpr.IntLit n ->
    Ok (mk ctx pos (mk_sort Sort.Int) eff0 (CoreExpr.IntLit n))

  | CoreExpr.BoolLit b ->
    Ok (mk ctx pos (mk_sort Sort.Bool) eff0 (CoreExpr.BoolLit b))

  | CoreExpr.Eq (ce1, ce2) ->
    let eff0' = Effect.purify eff0 in
    let* ce1' = synth sig_ ctx eff0' ce1 in
    let s1 = (CoreExpr.info ce1')#sort in
    if not (Sort.is_spec_type s1) then
      err_at pos "equality requires spec type (no pred)"
    else
      let* ce2' = check sig_ ctx ce2 s1 eff0' in
      let bool_sort = mk_sort Sort.Bool in
      Ok (mk ctx pos bool_sort eff0 (CoreExpr.Eq (ce1', ce2')))

  | CoreExpr.And (ce1, ce2) ->
    let bool_sort = mk_sort Sort.Bool in
    let* ce1' = check sig_ ctx ce1 bool_sort eff0 in
    let* ce2' = check sig_ ctx ce2 bool_sort eff0 in
    Ok (mk ctx pos bool_sort eff0 (CoreExpr.And (ce1', ce2')))

  | CoreExpr.Not ce ->
    let bool_sort = mk_sort Sort.Bool in
    let* ce' = check sig_ ctx ce bool_sort eff0 in
    Ok (mk ctx pos bool_sort eff0 (CoreExpr.Not ce'))

  | CoreExpr.Own s ->
    let sort = mk_sort (Sort.Pred s) in
    Ok (mk ctx pos sort eff0 (CoreExpr.Own (lift_sort s)))

  | CoreExpr.App (p, arg) ->
    let* () = match p with
      | Prim.Eq a ->
        if Typ.is_eqtype a then Ok ()
        else err_at_f pos "Eq requires an equality type, got %a" Typ.print a
      | _ -> Ok ()
    in
    let (arg_sort, ret_sort, prim_eff) = prim_signature p in
    if not (Effect.sub prim_eff eff0) then
      err_at_f pos "primitive %a requires effect %a, not usable at %a"
        Prim.print p Effect.print prim_eff Effect.print eff0
    else
      let eff0' = Effect.purify eff0 in
      let* arg' = check sig_ ctx arg arg_sort eff0' in
      Ok (mk ctx pos ret_sort eff0 (CoreExpr.App (p, arg')))

  | CoreExpr.Call (name, arg) ->
    (match Sig.lookup_fun name sig_ with
     | Some (arg_sort, ret_sort, fun_eff) ->
       if not (Effect.sub fun_eff eff0) then
         err_at_f pos "function %a has effect %a, not usable at %a"
           Var.print name Effect.print fun_eff Effect.print eff0
       else
         let eff0' = Effect.purify eff0 in
         let* arg' = check sig_ ctx arg arg_sort eff0' in
         Ok (mk ctx pos ret_sort eff0 (CoreExpr.Call (name, arg')))
     | None -> err_at_f pos "unknown function %a" Var.print name)

  | CoreExpr.Annot (ce, s, ann_eff) ->
    let* ce' = check sig_ ctx ce s ann_eff in
    Ok (mk ctx pos s ann_eff (CoreExpr.Annot (ce', lift_sort s, ann_eff)))

  | CoreExpr.Return _ | CoreExpr.Take _ | CoreExpr.Let _
  | CoreExpr.Inject _ | CoreExpr.Case _ | CoreExpr.Tuple _
  | CoreExpr.LetTuple _ | CoreExpr.If _ | CoreExpr.Iter _ ->
    err_at pos "cannot synthesize sort; add a type annotation"

(** Check: S; G |- [eff0] ce <== tau *)
and check sig_ ctx ce sort eff0 =
  let pos = (CoreExpr.info ce)#loc in
  match CoreExpr.shape ce with
  | CoreExpr.Return inner ->
    if not (Effect.sub Effect.Spec eff0) then
      err_at pos "return requires spec context"
    else
    (match Sort.shape sort with
     | Sort.Pred tau ->
       let* inner' = check sig_ ctx inner tau eff0 in
       Ok (mk ctx pos sort eff0 (CoreExpr.Return inner'))
     | _ -> err_at pos "return requires pred sort")

  | CoreExpr.Take ((x, _), ce1, ce2) ->
    if not (Effect.sub Effect.Spec eff0) then
      err_at pos "take requires spec context"
    else
    (match Sort.shape sort with
     | Sort.Pred _ ->
       let* ce1' = synth sig_ ctx eff0 ce1 in
       let s1 = (CoreExpr.info ce1')#sort in
       (match Sort.shape s1 with
        | Sort.Pred tau ->
          let bind_eff = Effect.purify eff0 in
          let ctx' = Context.extend x tau bind_eff ctx in
          let* ce2' = check sig_ ctx' ce2 sort eff0 in
          let xb = (x, mk_bind_info x tau bind_eff ctx') in
          Ok (mk ctx pos sort eff0 (CoreExpr.Take (xb, ce1', ce2')))
        | _ -> err_at pos "take scrutinee must have pred sort")
     | _ -> err_at pos "take requires pred sort as target")

  | CoreExpr.Let ((x, _), ce1, ce2) ->
    let* ce1' = synth sig_ ctx eff0 ce1 in
    let tau = (CoreExpr.info ce1')#sort in
    let bind_eff = Effect.purify eff0 in
    let ctx' = Context.extend x tau bind_eff ctx in
    let* ce2' = check sig_ ctx' ce2 sort eff0 in
    let xb = (x, mk_bind_info x tau bind_eff ctx') in
    Ok (mk ctx pos sort eff0 (CoreExpr.Let (xb, ce1', ce2')))

  | CoreExpr.LetTuple (xs, ce1, ce2) ->
    let eff0' = Effect.purify eff0 in
    let* ce1' = synth sig_ ctx eff0' ce1 in
    let s1 = (CoreExpr.info ce1')#sort in
    (match Sort.shape s1 with
     | Sort.Record ts ->
       let vars = List.map fst xs in
       if List.compare_lengths vars ts <> 0 then
         err_at_f pos "let-tuple: expected %d components, got %d"
           (List.length ts) (List.length vars)
       else
         let bind_eff = Effect.purify eff0 in
         let bindings = List.map (fun (x, s) -> (x, s, bind_eff)) (List.combine vars ts) in
         let ctx' = Context.extend_list bindings ctx in
         let* ce2' = check sig_ ctx' ce2 sort eff0 in
         let typed_xs = List.map (fun ((x, _), s) ->
           (x, (object
             method loc = Var.binding_site x
             method ctx = ctx'
             method sort = s
             method eff = bind_eff
           end : typed_info))
         ) (List.combine xs ts) in
         Ok (mk ctx pos sort eff0 (CoreExpr.LetTuple (typed_xs, ce1', ce2')))
     | _ -> err_at pos "let-tuple: scrutinee must have record sort")

  | CoreExpr.Tuple es ->
    (match Sort.shape sort with
     | Sort.Record ts ->
       if List.compare_lengths es ts <> 0 then
         err_at_f pos "tuple: expected %d components, got %d"
           (List.length ts) (List.length es)
       else
         let* es' = check_list sig_ ctx es ts eff0 in
         Ok (mk ctx pos sort eff0 (CoreExpr.Tuple es'))
     | _ -> err_at pos "tuple: expected record sort")

  | CoreExpr.Inject (l, e_inner) ->
    (match Sort.shape sort with
     | Sort.App (_, args) ->
       (match CtorLookup.lookup sig_ l args with
        | Ok ctor_sort ->
          let eff0' = Effect.purify eff0 in
          let* e_inner' = check sig_ ctx e_inner ctor_sort eff0' in
          Ok (mk ctx pos sort eff0 (CoreExpr.Inject (l, e_inner')))
        | Error msg -> err_at pos msg)
     | _ -> err_at pos "injection: expected datasort/datatype application")

  | CoreExpr.Case (scrut, branches) ->
    let eff0' = Effect.purify eff0 in
    let* scrut' = synth sig_ ctx eff0' scrut in
    let scrut_sort = (CoreExpr.info scrut')#sort in
    let* branches' =
      check_case_branches sig_ ctx branches scrut_sort sort eff0 eff0' pos in
    Ok (mk ctx pos sort eff0 (CoreExpr.Case (scrut', branches')))

  | CoreExpr.Iter (x, e1, body) ->
    if not (Effect.sub Effect.Impure eff0) then
      err_at pos "iter requires impure context"
    else
      let* e1' = synth sig_ ctx Effect.Pure e1 in
      let a = (CoreExpr.info e1')#sort in
      let step_dsort = match Dsort.of_string "step" with Ok d -> d | Error _ -> failwith "impossible" in
      let iter_sort = mk_sort (Sort.App (step_dsort, [a; sort])) in
      let bind_eff = Effect.purify Effect.Impure in
      let ctx' = Context.extend x a bind_eff ctx in
      let* body' = check sig_ ctx' body iter_sort Effect.Impure in
      Ok (mk ctx pos sort eff0 (CoreExpr.Iter (x, e1', body')))

  | CoreExpr.If (cond, e_then, e_else) ->
    let bool_sort = mk_sort Sort.Bool in
    let eff0' = Effect.purify eff0 in
    let* cond' = check sig_ ctx cond bool_sort eff0' in
    let* then' = check sig_ ctx e_then sort eff0 in
    let* else' = check sig_ ctx e_else sort eff0 in
    Ok (mk ctx pos sort eff0 (CoreExpr.If (cond', then', else')))

  | CoreExpr.Annot (inner, ann_sort, ann_eff) ->
    let* inner' = check sig_ ctx inner ann_sort ann_eff in
    if not (sort_equal ann_sort sort) then
      err_at_f pos "annotation sort %a does not match expected sort %a"
        Sort.print ann_sort Sort.print sort
    else if not (Effect.sub ann_eff eff0) then
      err_at pos "annotation effect exceeds allowed effect"
    else
      Ok (mk ctx pos sort ann_eff (CoreExpr.Annot (inner', lift_sort ann_sort, ann_eff)))

  | _ ->
    let* e' = synth sig_ ctx eff0 ce in
    let syn_sort = (CoreExpr.info e')#sort in
    if not (sort_equal syn_sort sort) then
      err_at_f pos "expected sort %a, got %a" Sort.print sort Sort.print syn_sort
    else
      Ok e'

and check_list sig_ ctx es sorts eff0 =
  match es, sorts with
  | [], [] -> Ok []
  | e :: es', s :: ss' ->
    let* e' = check sig_ ctx e s eff0 in
    let* rest = check_list sig_ ctx es' ss' eff0 in
    Ok (e' :: rest)
  | _ -> Error "tuple length mismatch"

and check_case_branches sig_ ctx branches scrut_sort result_sort eff0 bind_eff pos =
  match Sort.shape scrut_sort with
  | Sort.App (_d, args) ->
    let rec go = function
      | [] -> Ok []
      | (l, x, body, _) :: rest ->
        (match CtorLookup.lookup sig_ l args with
         | Ok ctor_sort ->
           let ctx' = Context.extend x ctor_sort bind_eff ctx in
           let* body' = check sig_ ctx' body result_sort eff0 in
           let branch_info = (object
             method loc = pos
             method ctx = ctx'
             method sort = ctor_sort
             method eff = bind_eff
           end : typed_info) in
           let* rest' = go rest in
           Ok ((l, x, body', branch_info) :: rest')
         | Error msg -> err_at_f pos "%s" msg)
    in
    go branches
  | _ -> err_at pos "case scrutinee must have datasort/datatype"

(** Built-in step datatype: step(a, b) = { Next : a | Done : b } *)
let step_decl =
  let mk_ty s = Typ.mk dummy_info s in
  let next_label = match Label.of_string "Next" with Ok l -> l | Error _ -> failwith "impossible" in
  let done_label = match Label.of_string "Done" with Ok l -> l | Error _ -> failwith "impossible" in
  let a = Tvar.of_string "a" in
  let b = Tvar.of_string "b" in
  let step_dsort = match Dsort.of_string "step" with Ok d -> d | Error _ -> failwith "impossible" in
  DtypeDecl.{
    name = step_dsort;
    params = [a; b];
    ctors = [
      (next_label, mk_ty (Typ.TVar a));
      (done_label, mk_ty (Typ.TVar b));
    ];
    loc = SourcePos.dummy;
  }

let initial_sig : typed_ce Sig.t =
  Sig.extend_type Sig.empty step_decl

(** Check sort well-formedness: S ; Phi |- tau wf *)
let rec sort_wf sig_ params s =
  let pos = (Sort.info s)#loc in
  match Sort.shape s with
  | Sort.Int | Sort.Bool -> Ok ()
  | Sort.TVar a ->
    if List.exists (fun p -> Tvar.compare a p = 0) params then Ok ()
    else Error (Format.asprintf "%a: unbound type variable %a"
                  SourcePos.print pos Tvar.print a)
  | Sort.Ptr t -> sort_wf sig_ params t
  | Sort.Pred t -> sort_wf sig_ params t
  | Sort.Record ts -> sort_wf_list sig_ params ts
  | Sort.App (d, args) ->
    (match Sig.lookup_sort d sig_ with
     | Some decl ->
       if List.compare_lengths args decl.DsortDecl.params <> 0 then
         Error (Format.asprintf "%a: sort %a expects %d arguments, got %d"
                  SourcePos.print pos Dsort.print d
                  (List.length decl.DsortDecl.params) (List.length args))
       else
         sort_wf_list sig_ params args
     | None ->
       (* Also check type declarations *)
       match Sig.lookup_type d sig_ with
       | Some decl ->
         if List.compare_lengths args decl.DtypeDecl.params <> 0 then
           Error (Format.asprintf "%a: type %a expects %d arguments, got %d"
                    SourcePos.print pos Dsort.print d
                    (List.length decl.DtypeDecl.params) (List.length args))
         else
           sort_wf_list sig_ params args
       | None ->
         Error (Format.asprintf "%a: unknown sort/type %a"
                  SourcePos.print pos Dsort.print d))

and sort_wf_list sig_ params = function
  | [] -> Ok ()
  | s :: rest ->
    let* () = sort_wf sig_ params s in
    sort_wf_list sig_ params rest

(** Validate a datasort declaration *)
let validate_sort_decl sig_ (d : DsortDecl.t) =
  if List.length d.ctors = 0 then
    Error "sort declaration must have at least one constructor"
  else
    let labels = DsortDecl.ctor_labels d in
    let rec check_dups = function
      | [] -> Ok ()
      | l :: rest ->
        if List.exists (fun l' -> Label.compare l l' = 0) rest then
          Error (Format.asprintf "duplicate constructor %a in sort declaration"
                   Label.print l)
        else check_dups rest
    in
    let* () = check_dups labels in
    let sig_with_self = Sig.extend_sort sig_ d in
    sort_wf_list sig_with_self d.params
      (List.map snd d.ctors)

(** Check guarded well-formedness for datatype declarations *)
let rec type_guarded sig_ params guard s =
  let pos = (Sort.info s)#loc in
  match Sort.shape s with
  | Sort.Int | Sort.Bool -> Ok ()
  | Sort.TVar a ->
    if List.exists (fun p -> Tvar.compare a p = 0) params then Ok ()
    else Error (Format.asprintf "%a: unbound type variable %a"
                  SourcePos.print pos Tvar.print a)
  | Sort.Ptr t -> sort_wf sig_ params t
  | Sort.Pred _ ->
    Error (Format.asprintf "%a: pred not allowed in type declarations"
             SourcePos.print pos)
  | Sort.Record ts -> type_guarded_list sig_ params guard ts
  | Sort.App (d, args) ->
    if Dsort.compare d guard = 0 then
      Error (Format.asprintf
               "%a: recursive reference to %a must go through ptr"
               SourcePos.print pos Dsort.print d)
    else
      (match Sig.lookup_type d sig_ with
       | None ->
         Error (Format.asprintf "%a: unknown type %a"
                  SourcePos.print pos Dsort.print d)
       | Some decl ->
         if List.compare_lengths args decl.DtypeDecl.params <> 0 then
           Error (Format.asprintf "%a: type %a expects %d arguments, got %d"
                    SourcePos.print pos Dsort.print d
                    (List.length decl.DtypeDecl.params) (List.length args))
         else
           sort_wf_list sig_ params args)

and type_guarded_list sig_ params guard = function
  | [] -> Ok ()
  | t :: rest ->
    let* () = type_guarded sig_ params guard t in
    type_guarded_list sig_ params guard rest

(** Validate a datatype declaration *)
let validate_type_decl sig_ (d : DtypeDecl.t) =
  if List.length d.ctors = 0 then
    Error "type declaration must have at least one constructor"
  else
    let labels = DtypeDecl.ctor_labels d in
    let rec check_dups = function
      | [] -> Ok ()
      | l :: rest ->
        if List.exists (fun l' -> Label.compare l l' = 0) rest then
          Error (Format.asprintf "duplicate constructor %a in type declaration"
                   Label.print l)
        else check_dups rest
    in
    let* () = check_dups labels in
    let sig_with_self = Sig.extend_type sig_ d in
    (* Convert type constructors to sorts for guardedness check *)
    let ctor_sorts = List.map (fun (l, ty) -> (l, Sort.typ_to_sort ty)) d.ctors in
    type_guarded_list sig_with_self d.params d.name (List.map snd ctor_sorts)

(** Elaborate and typecheck a FunDecl *)
let elaborate_fun sig_ (d : (SurfExpr.se, _) Prog.decl) =
  match d with
  | Prog.FunDecl d ->
    let entry = Sig.FunSig { arg = d.arg_sort; ret = d.ret_sort; eff = d.eff } in
    (* Allow recursive calls for impure and spec functions *)
    let sig_for_body = match d.eff with
      | Effect.Pure -> sig_
      | Effect.Impure | Effect.Spec -> Sig.extend d.name entry sig_
    in
    let result = ElabM.run (
      let open ElabM in
      let param_pos = match d.branches with
        | (pat, _, _) :: _ -> (Pat.info pat)#loc
        | [] -> d.loc
      in
      let* y = fresh param_pos in
      let branches = List.map (fun (pat, body, _) ->
        Elaborate.({ bindings = [(pat, d.arg_sort)];
                     ctx_bindings = [];
                     ectx = EvalCtx.Hole;
                     body })
      ) d.branches in
      let param_eff_b = Effect.purify d.eff in
      let* core_body = Elaborate.coverage_check sig_for_body Context.empty
        [y] branches param_eff_b d.ret_sort d.eff in
      return (y, core_body)
    ) in
    let* (y, core_body) = result in
    let bind_eff = Effect.purify d.eff in
    let ctx = Context.extend y d.arg_sort bind_eff Context.empty in
    let* typed_body = check sig_for_body ctx core_body d.ret_sort d.eff in
    Ok (Prog.CoreFunDecl { name = d.name; param = y;
                            arg_sort = d.arg_sort; ret_sort = d.ret_sort;
                            eff = d.eff; body = typed_body; loc = d.loc })
  | _ -> Error "elaborate_fun: not a FunDecl"

let check_decl sig_ (d : (SurfExpr.se, _) Prog.decl) =
  match d with
  | Prog.FunDecl _ -> elaborate_fun sig_ d
  | Prog.SortDecl dd ->
    let* () = validate_sort_decl sig_ dd in
    Ok (Prog.CoreSortDecl dd)
  | Prog.TypeDecl dd ->
    let* () = validate_type_decl sig_ dd in
    Ok (Prog.CoreTypeDecl dd)

let check_spec_decl sig_ (d : (SurfExpr.se, _) Prog.decl) =
  let* d' = check_decl sig_ d in
  match d, d' with
  | Prog.SortDecl dd, _ ->
    Ok (Sig.extend_sort sig_ dd)
  | Prog.TypeDecl dd, _ ->
    Ok (Sig.extend_type sig_ dd)
  | Prog.FunDecl dd, Prog.CoreFunDecl cd ->
    let entry = match dd.eff with
      | Effect.Impure ->
        Sig.FunSig { arg = dd.arg_sort; ret = dd.ret_sort; eff = dd.eff }
      | Effect.Pure | Effect.Spec ->
        Sig.FunDef { param = cd.param; arg = dd.arg_sort; ret = dd.ret_sort;
                     eff = dd.eff; body = cd.body }
    in
    Ok (Sig.extend dd.name entry sig_)
  | _, _ -> Error "check_spec_decl: unexpected declaration combination"

(** Extend the core signature after elaborating a declaration.
    Pure/spec functions store their full definition ([FunDef]);
    impure functions store only a prototype ([FunSig]). *)
let extend_sig_with_decl sig_ (d : (SurfExpr.se, _) Prog.decl) (d' : typed_ce Prog.core_decl) =
  match d, d' with
  | Prog.FunDecl fd, Prog.CoreFunDecl cd ->
    let entry = match fd.eff with
      | Effect.Impure ->
        Sig.FunSig { arg = fd.arg_sort; ret = fd.ret_sort; eff = fd.eff }
      | Effect.Pure | Effect.Spec ->
        Sig.FunDef { param = cd.param; arg = fd.arg_sort; ret = fd.ret_sort;
                     eff = fd.eff; body = cd.body }
    in
    Sig.extend fd.name entry sig_
  | Prog.SortDecl dd, _ ->
    Sig.extend_sort sig_ dd
  | Prog.TypeDecl dd, _ ->
    Sig.extend_type sig_ dd
  | _, _ -> sig_

let check_prog (p : (SurfExpr.se, _) Prog.t) : (typed_ce Sig.t * typed_ce Prog.core_prog, string) result =
  let rec check_decls sig_ = function
    | [] -> Ok (sig_, [])
    | d :: rest ->
      let* d' = check_decl sig_ d in
      let sig' = extend_sig_with_decl sig_ d d' in
      let* (final_sig, rest') = check_decls sig' rest in
      Ok (final_sig, d' :: rest')
  in
  let* (final_sig, decls') = check_decls initial_sig p.decls in
  (* Elaborate main *)
  let result = ElabM.run (
    Elaborate.check final_sig Context.empty p.main p.main_sort p.main_eff
  ) in
  let* core_main = result in
  let* main' = check final_sig Context.empty core_main p.main_sort p.main_eff in
  Ok (final_sig,
      { Prog.core_decls = decls'; core_main = main';
        core_main_sort = p.main_sort; core_main_eff = p.main_eff;
        core_loc = p.loc })
