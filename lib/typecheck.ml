let err_at pos msg =
  Error (Format.asprintf "@[%a:@ %s@]" SourcePos.print pos msg)

let err_at_f pos fmt =
  Format.kasprintf (fun msg -> err_at pos msg) fmt

let ( let* ) = Result.bind

let sort_equal (a : Sort.sort) (b : Sort.sort) = Sort.compare a b = 0

let dummy_info = object method loc = SourcePos.dummy end

let mk_sort s = Sort.In (s, dummy_info)

type typed_info = < loc : SourcePos.t; ctx : Context.t; sort : Sort.sort; eff : Effect.t >
type typed_ce = typed_info CoreExpr.t

let mk ctx pos sort eff shape : typed_ce =
  CoreExpr.In (shape, object
    method loc = pos
    method ctx = ctx
    method sort = sort
    method eff = eff
  end)

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

(** Synthesize: S; G |- ce ==> tau [eff] *)
let rec synth sig_ ctx eff (CoreExpr.In (shape, info)) =
  let pos = info#loc in
  match shape with
  | CoreExpr.Var x ->
    (match Context.lookup x ctx with
     | Some (s, var_eff) ->
       if Effect.sub var_eff eff then
         Ok (mk ctx pos s var_eff (CoreExpr.Var x))
       else
         err_at_f pos "variable %a has effect %a, not usable at effect %a"
           Var.print x Effect.print var_eff Effect.print eff
     | None -> err_at_f pos "unbound variable %a" Var.print x)

  | CoreExpr.IntLit n ->
    Ok (mk ctx pos (mk_sort Sort.Int) Effect.Pure (CoreExpr.IntLit n))

  | CoreExpr.BoolLit b ->
    Ok (mk ctx pos (mk_sort Sort.Bool) Effect.Pure (CoreExpr.BoolLit b))

  | CoreExpr.Eq (ce1, ce2) ->
    let* ce1' = synth sig_ ctx eff ce1 in
    let s1 = (CoreExpr.extract ce1')#sort in
    if not (Sort.is_spec_type s1) then
      err_at pos "equality requires spec type (no pred)"
    else
      let* ce2' = check sig_ ctx ce2 s1 eff in
      let bool_sort = mk_sort Sort.Bool in
      Ok (mk ctx pos bool_sort eff (CoreExpr.Eq (ce1', ce2')))

  | CoreExpr.And (ce1, ce2) ->
    let bool_sort = mk_sort Sort.Bool in
    let* ce1' = check sig_ ctx ce1 bool_sort eff in
    let* ce2' = check sig_ ctx ce2 bool_sort eff in
    Ok (mk ctx pos bool_sort eff (CoreExpr.And (ce1', ce2')))

  | CoreExpr.Not ce ->
    let bool_sort = mk_sort Sort.Bool in
    let* ce' = check sig_ ctx ce bool_sort eff in
    Ok (mk ctx pos bool_sort eff (CoreExpr.Not ce'))

  | CoreExpr.Own s ->
    let sort = mk_sort (Sort.Pred s) in
    Ok (mk ctx pos sort Effect.Spec (CoreExpr.Own s))

  | CoreExpr.App (p, arg) ->
    let* () = match p with
      | Prim.Eq a ->
        if Typ.is_eqtype a then Ok ()
        else err_at_f pos "Eq requires an equality type, got %a" Typ.print a
      | _ -> Ok ()
    in
    let (arg_sort, ret_sort, prim_eff) = prim_signature p in
    if not (Effect.sub prim_eff eff) then
      err_at_f pos "primitive %a requires effect %a, not usable at %a"
        Prim.print p Effect.print prim_eff Effect.print eff
    else
      let* arg' = check sig_ ctx arg arg_sort eff in
      Ok (mk ctx pos ret_sort prim_eff (CoreExpr.App (p, arg')))

  | CoreExpr.Call (name, arg) ->
    (match Sig.lookup_fun name sig_ with
     | Some (arg_sort, ret_sort, fun_eff) ->
       if not (Effect.sub fun_eff eff) then
         err_at_f pos "function %a has effect %a, not usable at %a"
           Var.print name Effect.print fun_eff Effect.print eff
       else
         let* arg' = check sig_ ctx arg arg_sort eff in
         Ok (mk ctx pos ret_sort fun_eff (CoreExpr.Call (name, arg')))
     | None -> err_at_f pos "unknown function %a" Var.print name)

  | CoreExpr.Annot (ce, s, ann_eff) ->
    let* ce' = check sig_ ctx ce s ann_eff in
    Ok (mk ctx pos s ann_eff (CoreExpr.Annot (ce', s, ann_eff)))

  | CoreExpr.Return _ | CoreExpr.Take _ | CoreExpr.Let _
  | CoreExpr.Inject _ | CoreExpr.Case _ | CoreExpr.Tuple _
  | CoreExpr.LetTuple _ | CoreExpr.If _ | CoreExpr.Iter _ ->
    err_at pos "cannot synthesize sort; add a type annotation"

(** Check: S; G |- ce <== tau [eff] *)
and check sig_ ctx (CoreExpr.In (shape, info) as ce) sort eff =
  let pos = info#loc in
  match shape with
  | CoreExpr.Return inner ->
    (match Sort.shape sort with
     | Sort.Pred tau ->
       let* inner' = check sig_ ctx inner tau eff in
       Ok (mk ctx pos sort eff (CoreExpr.Return inner'))
     | _ -> err_at pos "return requires pred sort")

  | CoreExpr.Take (x, ce1, ce2) ->
    (match Sort.shape sort with
     | Sort.Pred _ ->
       let* ce1' = synth sig_ ctx eff ce1 in
       let s1 = (CoreExpr.extract ce1')#sort in
       (match Sort.shape s1 with
        | Sort.Pred tau ->
          let bind_eff = Effect.purify eff in
          let ctx' = Context.extend x tau bind_eff ctx in
          let* ce2' = check sig_ ctx' ce2 sort eff in
          Ok (mk ctx pos sort eff (CoreExpr.Take (x, ce1', ce2')))
        | _ -> err_at pos "take scrutinee must have pred sort")
     | _ -> err_at pos "take requires pred sort as target")

  | CoreExpr.Let (x, ce1, ce2) ->
    let* ce1' = synth sig_ ctx eff ce1 in
    let tau = (CoreExpr.extract ce1')#sort in
    let eff1 = (CoreExpr.extract ce1')#eff in
    if not (Effect.sub eff1 eff) then
      err_at pos "effect of let binding exceeds allowed effect"
    else
      let bind_eff = Effect.purify eff in
      let ctx' = Context.extend x tau bind_eff ctx in
      let* ce2' = check sig_ ctx' ce2 sort eff in
      Ok (mk ctx pos sort eff (CoreExpr.Let (x, ce1', ce2')))

  | CoreExpr.LetTuple (xs, ce1, ce2) ->
    let* ce1' = synth sig_ ctx eff ce1 in
    let s1 = (CoreExpr.extract ce1')#sort in
    let eff1 = (CoreExpr.extract ce1')#eff in
    if not (Effect.sub eff1 eff) then
      err_at pos "effect of let-tuple scrutinee exceeds allowed effect"
    else
      (match Sort.shape s1 with
       | Sort.Record ts ->
         if List.compare_lengths xs ts <> 0 then
           err_at_f pos "let-tuple: expected %d components, got %d"
             (List.length ts) (List.length xs)
         else
           let bind_eff = Effect.purify eff in
           let bindings = List.map (fun (x, s) -> (x, s, bind_eff)) (List.combine xs ts) in
           let ctx' = Context.extend_list bindings ctx in
           let* ce2' = check sig_ ctx' ce2 sort eff in
           Ok (mk ctx pos sort eff (CoreExpr.LetTuple (xs, ce1', ce2')))
       | _ -> err_at pos "let-tuple: scrutinee must have record sort")

  | CoreExpr.Tuple es ->
    (match Sort.shape sort with
     | Sort.Record ts ->
       if List.compare_lengths es ts <> 0 then
         err_at_f pos "tuple: expected %d components, got %d"
           (List.length ts) (List.length es)
       else
         let* es' = check_list sig_ ctx es ts eff in
         Ok (mk ctx pos sort eff (CoreExpr.Tuple es'))
     | _ -> err_at pos "tuple: expected record sort")

  | CoreExpr.Inject (l, e_inner) ->
    (match Sort.shape sort with
     | Sort.App (_, args) ->
       (match CtorLookup.lookup sig_ l args with
        | Ok ctor_sort ->
          let* e_inner' = check sig_ ctx e_inner ctor_sort eff in
          Ok (mk ctx pos sort eff (CoreExpr.Inject (l, e_inner')))
        | Error msg -> err_at pos msg)
     | _ -> err_at pos "injection: expected datasort/datatype application")

  | CoreExpr.Case (scrut, branches) ->
    let* scrut' = synth sig_ ctx eff scrut in
    let scrut_sort = (CoreExpr.extract scrut')#sort in
    let eff1 = (CoreExpr.extract scrut')#eff in
    if not (Effect.sub eff1 eff) then
      err_at pos "effect of case scrutinee exceeds allowed effect"
    else
      let* branches' = check_case_branches sig_ ctx branches scrut_sort sort eff pos in
      Ok (mk ctx pos sort eff (CoreExpr.Case (scrut', branches')))

  | CoreExpr.Iter (x, e1, body) ->
    let* e1' = synth sig_ ctx eff e1 in
    let a = (CoreExpr.extract e1')#sort in
    if not (Effect.sub Effect.Impure eff) then
      err_at pos "iter requires impure context"
    else
      let step_dsort = match Dsort.of_string "step" with Ok d -> d | Error _ -> failwith "impossible" in
      let iter_sort = mk_sort (Sort.App (step_dsort, [a; sort])) in
      let bind_eff = Effect.purify eff in
      let ctx' = Context.extend x a bind_eff ctx in
      let* body' = check sig_ ctx' body iter_sort Effect.Impure in
      Ok (mk ctx pos sort eff (CoreExpr.Iter (x, e1', body')))

  | CoreExpr.If (cond, e_then, e_else) ->
    let bool_sort = mk_sort Sort.Bool in
    let* cond' = check sig_ ctx cond bool_sort eff in
    let* then' = check sig_ ctx e_then sort eff in
    let* else' = check sig_ ctx e_else sort eff in
    Ok (mk ctx pos sort eff (CoreExpr.If (cond', then', else')))

  | CoreExpr.Annot (inner, ann_sort, ann_eff) ->
    let* inner' = check sig_ ctx inner ann_sort ann_eff in
    if not (sort_equal ann_sort sort) then
      err_at_f pos "annotation sort %a does not match expected sort %a"
        Sort.print ann_sort Sort.print sort
    else if not (Effect.sub ann_eff eff) then
      err_at pos "annotation effect exceeds allowed effect"
    else
      Ok (mk ctx pos sort eff (CoreExpr.Annot (inner', ann_sort, ann_eff)))

  | _ ->
    let* e' = synth sig_ ctx eff ce in
    let syn_sort = (CoreExpr.extract e')#sort in
    let syn_eff = (CoreExpr.extract e')#eff in
    if not (sort_equal syn_sort sort) then
      err_at_f pos "expected sort %a, got %a" Sort.print sort Sort.print syn_sort
    else if not (Effect.sub syn_eff eff) then
      err_at pos "effect mismatch"
    else Ok e'

and check_list sig_ ctx es sorts eff =
  match es, sorts with
  | [], [] -> Ok []
  | e :: es', s :: ss' ->
    let* e' = check sig_ ctx e s eff in
    let* rest = check_list sig_ ctx es' ss' eff in
    Ok (e' :: rest)
  | _ -> Error "tuple length mismatch"

and check_case_branches sig_ ctx branches scrut_sort result_sort eff pos =
  match Sort.shape scrut_sort with
  | Sort.App (_d, args) ->
    let rec go = function
      | [] -> Ok []
      | (l, x, body) :: rest ->
        (match CtorLookup.lookup sig_ l args with
         | Ok ctor_sort ->
           let bind_eff = Effect.purify eff in
           let ctx' = Context.extend x ctor_sort bind_eff ctx in
           let* body' = check sig_ ctx' body result_sort eff in
           let* rest' = go rest in
           Ok ((l, x, body') :: rest')
         | Error msg -> err_at_f pos "%s" msg)
    in
    go branches
  | _ -> err_at pos "case scrutinee must have datasort/datatype"

(** Built-in step datatype: step(a, b) = { Next : a | Done : b } *)
let step_decl =
  let mk_ty s = Typ.In (s, dummy_info) in
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

let initial_sig =
  Sig.extend_type Sig.empty step_decl

(** Check sort well-formedness: S ; Phi |- tau wf *)
let rec sort_wf sig_ params (Sort.In (sf, info)) =
  let pos = info#loc in
  match sf with
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
let rec type_guarded sig_ params guard (Sort.In (sf, info)) =
  let pos = info#loc in
  match sf with
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
           type_guarded_list sig_ params guard args)

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
let elaborate_fun sig_ (d : SurfExpr.se Prog.decl) =
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
      let* y = fresh d.loc in
      let branches = List.map (fun (pat, body) ->
        Elaborate.({ bindings = [(pat, d.arg_sort)];
                     ctx_bindings = [];
                     ectx = EvalCtx.Hole;
                     body })
      ) d.branches in
      let* core_body = Elaborate.coverage_check sig_for_body Context.empty
        [y] branches d.ret_sort d.eff in
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

let check_decl sig_ (d : SurfExpr.se Prog.decl) =
  match d with
  | Prog.FunDecl _ -> elaborate_fun sig_ d
  | Prog.SortDecl dd ->
    let* () = validate_sort_decl sig_ dd in
    Ok (Prog.CoreSortDecl dd)
  | Prog.TypeDecl dd ->
    let* () = validate_type_decl sig_ dd in
    Ok (Prog.CoreTypeDecl dd)

let check_spec_decl sig_ (d : SurfExpr.se Prog.decl) =
  let* _d' = check_decl sig_ d in
  match d with
  | Prog.SortDecl dd ->
    Ok (Sig.extend_sort sig_ dd)
  | Prog.TypeDecl dd ->
    Ok (Sig.extend_type sig_ dd)
  | Prog.FunDecl dd ->
    Ok (Sig.extend dd.name (Sig.FunSig { arg = dd.arg_sort; ret = dd.ret_sort; eff = dd.eff }) sig_)

let check_prog (p : SurfExpr.se Prog.t) : (typed_ce Prog.core_prog, string) result =
  let rec check_decls sig_ = function
    | [] -> Ok (sig_, [])
    | d :: rest ->
      let* d' = check_decl sig_ d in
      let sig' = match d with
        | Prog.FunDecl fd ->
          let entry = Sig.FunSig { arg = fd.arg_sort; ret = fd.ret_sort; eff = fd.eff } in
          Sig.extend fd.name entry sig_
        | Prog.SortDecl dd ->
          Sig.extend_sort sig_ dd
        | Prog.TypeDecl dd ->
          Sig.extend_type sig_ dd
      in
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
  Ok { Prog.core_decls = decls'; core_main = main';
       core_main_sort = p.main_sort; core_main_eff = p.main_eff;
       core_loc = p.loc }
