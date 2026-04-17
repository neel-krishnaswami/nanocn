(* Error helpers.

   [invariant_at pos ~rule msg]: "impossible-in-well-formed-code"
   check fired (the typechecker's own consistency guarantees should
   have ruled this out). Emits [K_internal_invariant]. *)
let invariant_at pos ~rule msg =
  Error (Error.internal_invariant ~loc:pos ~rule ~invariant:msg)

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
    (mk_sort (Sort.Record [a; a]), bool_sort, Effect.Pure)
  | New a ->
    (a, mk_sort (Sort.Ptr a), Effect.Impure)
  | Del a ->
    (mk_sort (Sort.Ptr a), unit_sort, Effect.Impure)
  | Get a ->
    (mk_sort (Sort.Ptr a), a, Effect.Impure)
  | Set a ->
    (mk_sort (Sort.Record [mk_sort (Sort.Ptr a); a]), unit_sort, Effect.Impure)
  | Own a ->
    (mk_sort (Sort.Ptr a), mk_sort (Sort.Pred a), Effect.Spec)

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
         Error (Error.var_effect_mismatch
                  ~loc:pos ~var:x
                  ~declared:var_eff ~required:eff0)
     | None ->
       Error (Error.unbound_var ~loc:pos x))

  | CoreExpr.IntLit n ->
    Ok (mk ctx pos (mk_sort Sort.Int) eff0 (CoreExpr.IntLit n))

  | CoreExpr.BoolLit b ->
    Ok (mk ctx pos (mk_sort Sort.Bool) eff0 (CoreExpr.BoolLit b))

  | CoreExpr.Eq (ce1, ce2) ->
    let eff0' = Effect.purify eff0 in
    let* ce1' = synth sig_ ctx eff0' ce1 in
    let s1 = (CoreExpr.info ce1')#sort in
    if not (Sort.is_spec_type s1) then
      Error (Error.not_spec_type ~loc:pos
               ~construct:"equality" ~got:s1)
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

  | CoreExpr.App (p, arg) ->
    let* () = match p with
      | Prim.Eq a ->
        if Sort.is_eqtype a then Ok ()
        else Error (Error.eq_not_equality_type ~loc:pos ~got:a)
      | _ -> Ok ()
    in
    let (arg_sort, ret_sort, prim_eff) = prim_signature p in
    if not (Effect.sub prim_eff eff0) then
      Error (Error.prim_effect_mismatch
               ~loc:pos ~prim:p ~declared:prim_eff ~required:eff0)
    else
      let eff0' = Effect.purify eff0 in
      let* arg' = check sig_ ctx arg arg_sort eff0' in
      Ok (mk ctx pos ret_sort eff0 (CoreExpr.App (p, arg')))

  | CoreExpr.Call (name, arg) ->
    (match Sig.lookup_fun name sig_ with
     | Some (arg_sort, ret_sort, fun_eff) ->
       if not (Effect.sub fun_eff eff0) then
         Error (Error.fun_effect_mismatch
                  ~loc:pos ~name
                  ~declared:fun_eff ~required:eff0)
       else
         let eff0' = Effect.purify eff0 in
         let* arg' = check sig_ ctx arg arg_sort eff0' in
         Ok (mk ctx pos ret_sort eff0 (CoreExpr.Call (name, arg')))
     | None ->
       Error (Error.unknown_function ~loc:pos ~name))

  | CoreExpr.Annot (ce, s) ->
    let* ce' = check sig_ ctx ce s eff0 in
    Ok (mk ctx pos s eff0 (CoreExpr.Annot (ce', lift_sort s)))

  | CoreExpr.Return _ | CoreExpr.Take _ | CoreExpr.Fail | CoreExpr.Let _
  | CoreExpr.Inject _ | CoreExpr.Case _ | CoreExpr.Tuple _
  | CoreExpr.LetTuple _ | CoreExpr.If _ | CoreExpr.Iter _ ->
    Error (Error.cannot_synthesize ~loc:pos ~construct:"sort")

(** Check: S; G |- [eff0] ce <== tau *)
and check sig_ ctx ce sort eff0 =
  let pos = (CoreExpr.info ce)#loc in
  match CoreExpr.shape ce with
  | CoreExpr.Return inner ->
    if not (Effect.sub Effect.Spec eff0) then
      Error (Error.spec_context_required ~loc:pos ~construct:"return")
    else
    (match Sort.shape sort with
     | Sort.Pred tau ->
       let* inner' = check sig_ ctx inner tau eff0 in
       Ok (mk ctx pos sort eff0 (CoreExpr.Return inner'))
     | _ ->
       Error (Error.construct_sort_mismatch ~loc:pos
                ~construct:"return" ~expected_shape:"Pred _"
                ~got:sort))

  | CoreExpr.Fail ->
    if not (Effect.sub Effect.Spec eff0) then
      Error (Error.spec_context_required ~loc:pos ~construct:"fail")
    else
    (match Sort.shape sort with
     | Sort.Pred _ ->
       Ok (mk ctx pos sort eff0 CoreExpr.Fail)
     | _ ->
       Error (Error.construct_sort_mismatch ~loc:pos
                ~construct:"fail" ~expected_shape:"Pred _"
                ~got:sort))

  | CoreExpr.Take ((x, _), ce1, ce2) ->
    if not (Effect.sub Effect.Spec eff0) then
      Error (Error.spec_context_required ~loc:pos ~construct:"take")
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
        | _ ->
          Error (Error.construct_sort_mismatch ~loc:pos
                   ~construct:"take scrutinee"
                   ~expected_shape:"Pred _" ~got:s1))
     | _ ->
       Error (Error.construct_sort_mismatch ~loc:pos
                ~construct:"take target"
                ~expected_shape:"Pred _" ~got:sort))

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
         Error (Error.tuple_arity_mismatch ~loc:pos
                  ~construct:"let-tuple"
                  ~expected:(List.length ts)
                  ~actual:(List.length vars))
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
     | _ ->
       Error (Error.construct_sort_mismatch ~loc:pos
                ~construct:"let-tuple scrutinee"
                ~expected_shape:"Record _" ~got:s1))

  | CoreExpr.Tuple es ->
    (match Sort.shape sort with
     | Sort.Record ts ->
       if List.compare_lengths es ts <> 0 then
         Error (Error.tuple_arity_mismatch ~loc:pos
                  ~construct:"tuple"
                  ~expected:(List.length ts)
                  ~actual:(List.length es))
       else
         let* es' = check_list sig_ ctx es ts eff0 in
         Ok (mk ctx pos sort eff0 (CoreExpr.Tuple es'))
     | _ ->
       Error (Error.construct_sort_mismatch ~loc:pos
                ~construct:"tuple"
                ~expected_shape:"Record _" ~got:sort))

  | CoreExpr.Inject (l, e_inner) ->
    (match Sort.shape sort with
     | Sort.App (_, args) ->
       (match CtorLookup.lookup sig_ l args with
        | Ok ctor_sort ->
          let eff0' = Effect.purify eff0 in
          let* e_inner' = check sig_ ctx e_inner ctor_sort eff0' in
          Ok (mk ctx pos sort eff0 (CoreExpr.Inject (l, e_inner')))
        | Error k ->
          Error (Error.structured ~loc:pos k))
     | _ ->
       Error (Error.construct_sort_mismatch ~loc:pos
                ~construct:"injection"
                ~expected_shape:"datasort/datatype application"
                ~got:sort))

  | CoreExpr.Case (scrut, branches) ->
    let eff0' = Effect.purify eff0 in
    let* scrut' = synth sig_ ctx eff0' scrut in
    let scrut_sort = (CoreExpr.info scrut')#sort in
    let* branches' =
      check_case_branches sig_ ctx branches scrut_sort sort eff0 eff0' pos in
    Ok (mk ctx pos sort eff0 (CoreExpr.Case (scrut', branches')))

  | CoreExpr.Iter (x, e1, body) ->
    if not (Effect.sub Effect.Impure eff0) then
      Error (Error.iter_requires_impure ~loc:pos ~actual:eff0)
    else
      let* e1' = synth sig_ ctx Effect.Pure e1 in
      let a = (CoreExpr.info e1')#sort in
      let step_dsort = match Dsort.of_string "Step" with Ok d -> d | Error _ -> failwith "impossible" in
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

  | CoreExpr.Annot (inner, ann_sort) ->
    let* inner' = check sig_ ctx inner ann_sort eff0 in
    if not (sort_equal ann_sort sort) then
      Error (Error.annotation_disagrees
               ~loc:pos ~inner:sort ~annot:ann_sort)
    else
      Ok (mk ctx pos sort eff0 (CoreExpr.Annot (inner', lift_sort ann_sort)))

  | _ ->
    let* e' = synth sig_ ctx eff0 ce in
    let syn_sort = (CoreExpr.info e')#sort in
    if not (sort_equal syn_sort sort) then
      Error (Error.sort_mismatch
               ~loc:pos ~expected:sort ~actual:syn_sort)
    else
      Ok e'

and check_list sig_ ctx es sorts eff0 =
  match es, sorts with
  | [], [] -> Ok []
  | e :: es', s :: ss' ->
    let* e' = check sig_ ctx e s eff0 in
    let* rest = check_list sig_ ctx es' ss' eff0 in
    Ok (e' :: rest)
  | es, sorts ->
    let pos = match es, sorts with
      | e :: _, _ -> (CoreExpr.info e)#loc
      | [], s :: _ -> (Sort.info s)#loc
      | [], [] -> SourcePos.dummy
    in
    invariant_at pos ~rule:"check_list"
      "tuple expression and record sort have different arities \
       (should have been caught earlier)"

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
         | Error k ->
           Error (Error.structured ~loc:pos k))
    in
    go branches
  | _ -> Error (Error.scrutinee_not_data ~loc:pos ~got:scrut_sort)

(** Built-in step datatype: step(a, b) = { Next : a | Done : b } *)
let step_decl =
  let mk_sort s = Sort.mk dummy_info s in
  let next_label = match Label.of_string "Next" with Ok l -> l | Error _ -> failwith "impossible" in
  let done_label = match Label.of_string "Done" with Ok l -> l | Error _ -> failwith "impossible" in
  let a = Tvar.of_string "a" in
  let b = Tvar.of_string "b" in
  let step_dsort = match Dsort.of_string "Step" with Ok d -> d | Error _ -> failwith "impossible" in
  DtypeDecl.{
    name = step_dsort;
    params = [a; b];
    ctors = [
      (next_label, mk_sort (Sort.TVar a));
      (done_label, mk_sort (Sort.TVar b));
    ];
    loc = SourcePos.dummy;
  }

let initial_sig : typed_ce Sig.t =
  Sig.extend_type Sig.empty step_decl

(** Check kind well-formedness: CS ; G |- tau : kind *)
let rec kind_wf sig_ ctx s kind =
  let pos = (Sort.info s)#loc in
  match Sort.shape s with
  | Sort.Int | Sort.Bool -> Ok ()
  | Sort.TVar a ->
    (match Context.lookup_tvar a ctx with
     | Some k ->
       if Kind.subkind k kind then Ok ()
       else
         Error (Error.tvar_kind_mismatch
                  ~loc:pos ~tvar:a ~got:k ~expected:kind)
     | None ->
       Error (Error.unbound_tvar ~loc:pos a))
  | Sort.Ptr t -> kind_wf sig_ ctx t Kind.Type
  | Sort.Pred t ->
    if Kind.compare kind Kind.Sort <> 0 then
      Error (Error.pred_misuse ~loc:pos
               ~context:(Format.asprintf "kind %a" Kind.print kind))
    else
      kind_wf sig_ ctx t Kind.Sort
  | Sort.Record ts -> kind_wf_list sig_ ctx ts kind
  | Sort.App (d, args) ->
    (match Sig.lookup_sort d sig_ with
     | Some decl ->
       if List.compare_lengths args decl.DsortDecl.params <> 0 then
         Error (Error.dsort_arity_mismatch ~loc:pos ~dsort:d
                  ~expected:(List.length decl.DsortDecl.params)
                  ~actual:(List.length args))
       else
         kind_wf_list sig_ ctx args Kind.Sort
     | None ->
       match Sig.lookup_type d sig_ with
       | Some decl ->
         if List.compare_lengths args decl.DtypeDecl.params <> 0 then
           Error (Error.dsort_arity_mismatch ~loc:pos ~dsort:d
                    ~expected:(List.length decl.DtypeDecl.params)
                    ~actual:(List.length args))
         else
           kind_wf_list sig_ ctx args Kind.Type
       | None ->
         Error (Error.unbound_sort ~loc:pos d))

and kind_wf_list sig_ ctx ss kind =
  match ss with
  | [] -> Ok ()
  | s :: rest ->
    let* () = kind_wf sig_ ctx s kind in
    kind_wf_list sig_ ctx rest kind

(** Validate a datasort declaration *)
let validate_sort_decl sig_ (d : DsortDecl.t) =
  let decl_name = Format.asprintf "%a" Dsort.print d.name in
  if List.length d.ctors = 0 then
    Error (Error.empty_decl ~loc:d.loc
             ~name:decl_name ~is_type:false)
  else
    let labels = DsortDecl.ctor_labels d in
    let rec check_dups = function
      | [] -> Ok ()
      | l :: rest ->
        if List.exists (fun l' -> Label.compare l l' = 0) rest then
          Error (Error.duplicate_ctor_in_decl ~loc:d.loc
                   ~label:l ~decl_name ~is_type:false)
        else check_dups rest
    in
    let* () = check_dups labels in
    (* Add D with empty ctors to sig (spec: SDWf_Ok) *)
    let empty_decl = DsortDecl.{ name = d.name; params = d.params; ctors = []; loc = d.loc } in
    let sig_with_self = Sig.extend_sort sig_ empty_decl in
    (* Build context with type vars at kind sort *)
    let ctx = List.fold_left (fun acc a -> Context.extend_tvar a Kind.Sort acc)
                Context.empty d.params in
    kind_wf_list sig_with_self ctx (List.map snd d.ctors) Kind.Sort

(** Check guarded well-formedness for datatype declarations:
    CS ; G ; D'(a1,...,an) |- tau guarded *)
let rec type_guarded sig_ ctx (guard_name, guard_params) s =
  let pos = (Sort.info s)#loc in
  match Sort.shape s with
  | Sort.Int | Sort.Bool -> Ok ()
  | Sort.TVar a ->
    (match Context.lookup_tvar a ctx with
     | Some _ -> Ok ()
     | None -> Error (Error.unbound_tvar ~loc:pos a))
  | Sort.Ptr t ->
    (* Re-add D (with its actual params) to allow recursive reference under Ptr *)
    let stub_decl = DtypeDecl.{ name = guard_name; params = guard_params;
                                ctors = []; loc = SourcePos.dummy } in
    let sig_with_guard = Sig.extend_type sig_ stub_decl in
    kind_wf sig_with_guard ctx t Kind.Type
  | Sort.Pred _ ->
    Error (Error.pred_misuse ~loc:pos
             ~context:"a type declaration")
  | Sort.Record ts -> type_guarded_list sig_ ctx (guard_name, guard_params) ts
  | Sort.App (d, args) ->
    if Dsort.compare d guard_name = 0 then
      Error (Error.unguarded_recursion ~loc:pos ~dsort:d)
    else
      (match Sig.lookup_type d sig_ with
       | None -> Error (Error.unbound_sort ~loc:pos d)
       | Some decl ->
         if List.compare_lengths args decl.DtypeDecl.params <> 0 then
           Error (Error.dsort_arity_mismatch ~loc:pos ~dsort:d
                    ~expected:(List.length decl.DtypeDecl.params)
                    ~actual:(List.length args))
         else
           kind_wf_list sig_ ctx args Kind.Type)

and type_guarded_list sig_ ctx guard = function
  | [] -> Ok ()
  | t :: rest ->
    let* () = type_guarded sig_ ctx guard t in
    type_guarded_list sig_ ctx guard rest

(** Validate a datatype declaration *)
let validate_type_decl sig_ (d : DtypeDecl.t) =
  let decl_name = Format.asprintf "%a" Dsort.print d.name in
  if List.length d.ctors = 0 then
    Error (Error.empty_decl ~loc:d.loc
             ~name:decl_name ~is_type:true)
  else
    let labels = DtypeDecl.ctor_labels d in
    let rec check_dups = function
      | [] -> Ok ()
      | l :: rest ->
        if List.exists (fun l' -> Label.compare l l' = 0) rest then
          Error (Error.duplicate_ctor_in_decl ~loc:d.loc
                   ~label:l ~decl_name ~is_type:true)
        else check_dups rest
    in
    let* () = check_dups labels in
    (* Do NOT add D to sig (spec: TDWf_Ok) — guardedness handles recursive refs *)
    (* Build context with type vars at kind type *)
    let ctx = List.fold_left (fun acc a -> Context.extend_tvar a Kind.Type acc)
                Context.empty d.params in
    (* Check guardedness of constructor sorts *)
    type_guarded_list sig_ ctx (d.name, d.params) (List.map snd d.ctors)

(** Elaborate and typecheck a FunDecl *)
let elaborate_fun supply sig_ (d : (SurfExpr.se, _, Var.t) Prog.decl) =
  match d with
  | Prog.FunDecl d ->
    let entry = Sig.FunSig { arg = d.arg_sort; ret = d.ret_sort; eff = d.eff } in
    (* Allow recursive calls for impure and spec functions *)
    let sig_for_body = match d.eff with
      | Effect.Pure -> sig_
      | Effect.Impure | Effect.Spec -> Sig.extend d.name entry sig_
    in
    let result = ElabM.run supply (
      let open ElabM in
      let param_pos = match d.branches with
        | (pat, _, _) :: _ -> (Pat.info pat)#loc
        | [] -> d.loc
      in
      let* y = fresh param_pos in
      let bind_eff = Effect.purify d.eff in
      let ctx = Context.extend y d.arg_sort bind_eff Context.empty in
      let branches = List.map (fun (pat, body, _) ->
        Elaborate.({ bindings = [(pat, d.arg_sort)];
                     let_bindings = [];
                     body })
      ) d.branches in
      let param_eff_b = Effect.purify d.eff in
      let rebuilder = function
        | [w] -> w
        | _ -> PatWitness.Wild
      in
      let* typed_body =
        Elaborate.coverage_check sig_for_body ctx
          [y] branches param_eff_b d.ret_sort d.eff
          ~cov_loc:d.loc rebuilder in
      return (y, typed_body)
    ) in
    let* ((y, typed_body), supply') = result in
    Ok (supply', Prog.CoreFunDecl { name = d.name; param = y;
                            arg_sort = d.arg_sort; ret_sort = d.ret_sort;
                            eff = d.eff; body = typed_body; loc = d.loc })
  | d ->
    (* Caller (check_decl) only dispatches FunDecl here; the other
       arms are handled directly. Reaching this would be a caller bug. *)
    let loc = match d with
      | Prog.FunDecl dd -> dd.loc
      | Prog.SortDecl dd -> dd.DsortDecl.loc
      | Prog.TypeDecl dd -> dd.DtypeDecl.loc
    in
    invariant_at loc ~rule:"elaborate_fun"
      "called with a non-FunDecl declaration"

let check_decl supply sig_ (d : (SurfExpr.se, _, Var.t) Prog.decl) =
  match d with
  | Prog.FunDecl _ -> elaborate_fun supply sig_ d
  | Prog.SortDecl dd ->
    let* () = validate_sort_decl sig_ dd in
    Ok (supply, Prog.CoreSortDecl dd)
  | Prog.TypeDecl dd ->
    let* () = validate_type_decl sig_ dd in
    Ok (supply, Prog.CoreTypeDecl dd)

let check_spec_decl supply sig_ (d : (SurfExpr.se, _, Var.t) Prog.decl) =
  let* (supply', d') = check_decl supply sig_ d in
  match d, d' with
  | Prog.SortDecl dd, _ ->
    Ok (supply', Sig.extend_sort sig_ dd)
  | Prog.TypeDecl dd, _ ->
    Ok (supply', Sig.extend_type sig_ dd)
  | Prog.FunDecl dd, Prog.CoreFunDecl cd ->
    let entry = match dd.eff with
      | Effect.Impure ->
        Sig.FunSig { arg = dd.arg_sort; ret = dd.ret_sort; eff = dd.eff }
      | Effect.Pure | Effect.Spec ->
        Sig.FunDef { param = cd.param; arg = dd.arg_sort; ret = dd.ret_sort;
                     eff = dd.eff; body = cd.body }
    in
    Ok (supply', Sig.extend dd.name entry sig_)
  | d, _ ->
    let loc = match d with
      | Prog.FunDecl dd -> dd.loc
      | Prog.SortDecl dd -> dd.DsortDecl.loc
      | Prog.TypeDecl dd -> dd.DtypeDecl.loc
    in
    invariant_at loc ~rule:"check_spec_decl"
      "unexpected decl / core-decl pairing"

(** Extend the core signature after elaborating a declaration.
    Pure/spec functions store their full definition ([FunDef]);
    impure functions store only a prototype ([FunSig]). *)
let extend_sig_with_decl sig_ (d : (SurfExpr.se, _, Var.t) Prog.decl) (d' : typed_ce Prog.core_decl) =
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

let extend_sig_with_header sig_ (d : (SurfExpr.se, _, Var.t) Prog.decl) =
  match d with
  | Prog.FunDecl dd ->
    let entry = Sig.FunSig { arg = dd.arg_sort; ret = dd.ret_sort; eff = dd.eff } in
    Ok (Sig.extend dd.name entry sig_)
  | Prog.SortDecl dd ->
    let* () = validate_sort_decl sig_ dd in
    Ok (Sig.extend_sort sig_ dd)
  | Prog.TypeDecl dd ->
    let* () = validate_type_decl sig_ dd in
    Ok (Sig.extend_type sig_ dd)

let check_prog supply (p : (SurfExpr.se, _, Var.t) Prog.t) : (typed_ce Sig.t * typed_ce Prog.core_prog, Error.t) result =
  let rec check_decls supply sig_ = function
    | [] -> Ok (supply, sig_, [])
    | d :: rest ->
      let* (supply', d') = check_decl supply sig_ d in
      let sig' = extend_sig_with_decl sig_ d d' in
      let* (supply'', final_sig, rest') = check_decls supply' sig' rest in
      Ok (supply'', final_sig, d' :: rest')
  in
  let* (supply', final_sig, decls') = check_decls supply initial_sig p.decls in
  (* Elaborate main — produces typed_ce directly *)
  let result = ElabM.run supply' (
    Elaborate.check final_sig Context.empty p.main p.main_sort p.main_eff
  ) in
  let* (main', _supply'') = result in
  Ok (final_sig,
      { Prog.core_decls = decls'; core_main = main';
        core_main_sort = p.main_sort; core_main_eff = p.main_eff;
        core_loc = p.loc })
