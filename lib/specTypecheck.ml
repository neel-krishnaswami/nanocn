let err_at pos msg =
  Error (Format.asprintf "@[%a:@ %s@]" SourcePos.print pos msg)

let err_at_f pos fmt =
  Format.kasprintf (fun msg -> err_at pos msg) fmt

let ( let* ) = Result.bind

let sort_equal (a : Sort.sort) (b : Sort.sort) = Sort.compare a b = 0

let mk_sort s = Sort.In (s, object method loc = SourcePos.dummy end)

type typed_info = < loc : SourcePos.t; ctx : Context.t; sort : Sort.sort >
type typed_ce = typed_info CoreExpr.t

let mk ctx pos sort shape : typed_ce =
  CoreExpr.In (shape, object
    method loc = pos
    method ctx = ctx
    method sort = sort
  end)

(** Synthesize: S; G |- ce ==> tau *)
let rec synth sig_ ctx (CoreExpr.In (shape, info)) =
  let pos = info#loc in
  match shape with
  | CoreExpr.Var x ->
    (match Context.lookup_spec x ctx with
     | Some s -> Ok (mk ctx pos s (CoreExpr.Var x))
     | None -> err_at_f pos "unbound spec variable %a" Var.print x)

  | CoreExpr.IntLit n ->
    let s = mk_sort Sort.Int in
    Ok (mk ctx pos s (CoreExpr.IntLit n))

  | CoreExpr.BoolLit b ->
    let s = mk_sort Sort.Bool in
    Ok (mk ctx pos s (CoreExpr.BoolLit b))

  | CoreExpr.Eq (ce1, ce2) ->
    let* ce1' = synth sig_ ctx ce1 in
    let s1 = (CoreExpr.extract ce1')#sort in
    if not (Sort.is_spec_type s1) then
      err_at pos "equality requires spec type (no pred)"
    else
      let* ce2' = check sig_ ctx ce2 s1 in
      let bool_sort = mk_sort Sort.Bool in
      Ok (mk ctx pos bool_sort (CoreExpr.Eq (ce1', ce2')))

  | CoreExpr.And (ce1, ce2) ->
    let bool_sort = mk_sort Sort.Bool in
    let* ce1' = check sig_ ctx ce1 bool_sort in
    let* ce2' = check sig_ ctx ce2 bool_sort in
    Ok (mk ctx pos bool_sort (CoreExpr.And (ce1', ce2')))

  | CoreExpr.Not ce ->
    let bool_sort = mk_sort Sort.Bool in
    let* ce' = check sig_ ctx ce bool_sort in
    Ok (mk ctx pos bool_sort (CoreExpr.Not ce'))

  | CoreExpr.Own s ->
    let sort = mk_sort (Sort.Pred s) in
    Ok (mk ctx pos sort (CoreExpr.Own s))

  | CoreExpr.Call (name, arg) ->
    (match Sig.lookup_spec_fun name sig_ with
     | Some (arg_sort, ret_sort) ->
       let* arg' = check sig_ ctx arg arg_sort in
       Ok (mk ctx pos ret_sort (CoreExpr.Call (name, arg')))
     | None -> err_at_f pos "unknown spec function %a" Var.print name)

  | CoreExpr.Const name ->
    (match Sig.lookup_spec_val name sig_ with
     | Some s -> Ok (mk ctx pos s (CoreExpr.Const name))
     | None -> err_at_f pos "unknown spec constant %a" Var.print name)

  | CoreExpr.Annot (ce, s) ->
    let* ce' = check sig_ ctx ce s in
    Ok (mk ctx pos s (CoreExpr.Annot (ce', s)))

  | CoreExpr.Return _ | CoreExpr.Take _ | CoreExpr.Let _
  | CoreExpr.Con _ | CoreExpr.Case _ | CoreExpr.Tuple _
  | CoreExpr.LetTuple _ | CoreExpr.If _ ->
    err_at pos "cannot synthesize sort; add a type annotation"

(** Check: S; G |- ce <== tau *)
and check sig_ ctx (CoreExpr.In (shape, info) as ce) sort =
  let pos = info#loc in
  match shape with
  | CoreExpr.Return inner ->
    (match Sort.shape sort with
     | Sort.Pred tau ->
       let* inner' = check sig_ ctx inner tau in
       Ok (mk ctx pos sort (CoreExpr.Return inner'))
     | _ -> err_at pos "return requires pred sort")

  | CoreExpr.Take (x, ce1, ce2) ->
    (match Sort.shape sort with
     | Sort.Pred _tau' ->
       let* ce1' = synth sig_ ctx ce1 in
       let s1 = (CoreExpr.extract ce1')#sort in
       (match Sort.shape s1 with
        | Sort.Pred tau ->
          let ctx' = Context.extend_spec x tau ctx in
          let* ce2' = check sig_ ctx' ce2 sort in
          Ok (mk ctx pos sort (CoreExpr.Take (x, ce1', ce2')))
        | _ -> err_at pos "take scrutinee must have pred sort")
     | _ -> err_at pos "take requires pred sort as target")

  | CoreExpr.Let (x, ce1, ce2) ->
    let* ce1' = synth sig_ ctx ce1 in
    let tau = (CoreExpr.extract ce1')#sort in
    let ctx' = Context.extend_spec x tau ctx in
    let* ce2' = check sig_ ctx' ce2 sort in
    Ok (mk ctx pos sort (CoreExpr.Let (x, ce1', ce2')))

  | CoreExpr.Con (l, inner) ->
    (match Sort.shape sort with
     | Sort.App (_d, args) ->
       (match CtorLookup.lookup sig_ l args with
        | Ok ctor_sort ->
          let* inner' = check sig_ ctx inner ctor_sort in
          Ok (mk ctx pos sort (CoreExpr.Con (l, inner')))
        | Error msg -> err_at_f pos "%s" msg)
     | _ -> err_at_f pos "constructor %a requires datasort type" Label.print l)

  | CoreExpr.Tuple es ->
    (match Sort.shape sort with
     | Sort.Record ts ->
       if List.compare_lengths es ts <> 0 then
         err_at_f pos "tuple: expected %d components, got %d"
           (List.length ts) (List.length es)
       else
         let* es' = check_list sig_ ctx es ts in
         Ok (mk ctx pos sort (CoreExpr.Tuple es'))
     | _ -> err_at pos "tuple: expected record sort")

  | CoreExpr.LetTuple (xs, ce1, ce2) ->
    let* ce1' = synth sig_ ctx ce1 in
    let s1 = (CoreExpr.extract ce1')#sort in
    (match Sort.shape s1 with
     | Sort.Record ts ->
       if List.compare_lengths xs ts <> 0 then
         err_at_f pos "let-tuple: expected %d components, got %d"
           (List.length ts) (List.length xs)
       else
         let bindings = List.combine xs ts in
         let ctx' = Context.extend_spec_list bindings ctx in
         let* ce2' = check sig_ ctx' ce2 sort in
         Ok (mk ctx pos sort (CoreExpr.LetTuple (xs, ce1', ce2')))
     | _ -> err_at pos "let-tuple: scrutinee must have record sort")

  | CoreExpr.Case (scrut, branches) ->
    let* scrut' = synth sig_ ctx scrut in
    let scrut_sort = (CoreExpr.extract scrut')#sort in
    let* branches' = check_case_branches sig_ ctx branches scrut_sort sort pos in
    Ok (mk ctx pos sort (CoreExpr.Case (scrut', branches')))

  | CoreExpr.If (ce1, ce2, ce3) ->
    let bool_sort = mk_sort Sort.Bool in
    let* ce1' = check sig_ ctx ce1 bool_sort in
    let* ce2' = check sig_ ctx ce2 sort in
    let* ce3' = check sig_ ctx ce3 sort in
    Ok (mk ctx pos sort (CoreExpr.If (ce1', ce2', ce3')))

  | _ ->
    (* Fall through to synthesis (sub rule) *)
    let* ce' = synth sig_ ctx ce in
    let syn_sort = (CoreExpr.extract ce')#sort in
    if sort_equal syn_sort sort then Ok ce'
    else
      err_at_f pos "expected sort %a, got %a" Sort.print sort Sort.print syn_sort

and check_list sig_ ctx es sorts =
  match es, sorts with
  | [], [] -> Ok []
  | e :: es', s :: ss' ->
    let* e' = check sig_ ctx e s in
    let* rest = check_list sig_ ctx es' ss' in
    Ok (e' :: rest)
  | _ -> Error "tuple length mismatch"

(** Check case branches using core_branch judgement:
    S; G |- (L x) : tau' -> ce <== tau *)
and check_case_branches sig_ ctx branches scrut_sort result_sort pos =
  match Sort.shape scrut_sort with
  | Sort.App (_d, args) ->
    let rec go = function
      | [] -> Ok []
      | (l, x, body) :: rest ->
        (match CtorLookup.lookup sig_ l args with
         | Ok ctor_sort ->
           let ctx' = Context.extend_spec x ctor_sort ctx in
           let* body' = check sig_ ctx' body result_sort in
           let* rest' = go rest in
           Ok ((l, x, body') :: rest')
         | Error msg -> err_at_f pos "%s" msg)
    in
    go branches
  | _ -> err_at pos "case scrutinee must have datasort type"
