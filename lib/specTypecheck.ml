let err_at pos msg =
  Error (Format.asprintf "@[%a:@ %s@]" SourcePos.print pos msg)

let err_at_f pos fmt =
  Format.kasprintf (fun msg -> err_at pos msg) fmt

let ( let* ) = Result.bind

let sort_equal (a : Sort.sort) (b : Sort.sort) = Sort.compare a b = 0

let mk_sort s = Sort.In (s, object method loc = SourcePos.dummy end)

(** Synthesize: S; G |- ce ==> tau *)
let rec synth sig_ ctx (CoreExpr.In (shape, info)) =
  let pos = info#loc in
  match shape with
  | CoreExpr.Var x ->
    (match Context.lookup_spec x ctx with
     | Some s -> Ok s
     | None -> err_at_f pos "unbound spec variable %a" Var.print x)

  | CoreExpr.IntLit _ ->
    Ok (mk_sort Sort.Int)

  | CoreExpr.BoolLit _ ->
    Ok (mk_sort Sort.Bool)

  | CoreExpr.Eq (ce1, ce2) ->
    let* s = synth sig_ ctx ce1 in
    if not (Sort.is_spec_type s) then
      err_at pos "equality requires spec type (no pred)"
    else
      let* () = check sig_ ctx ce2 s in
      Ok (mk_sort Sort.Bool)

  | CoreExpr.And (ce1, ce2) ->
    let bool_sort = mk_sort Sort.Bool in
    let* () = check sig_ ctx ce1 bool_sort in
    let* () = check sig_ ctx ce2 bool_sort in
    Ok (mk_sort Sort.Bool)

  | CoreExpr.Not ce ->
    let bool_sort = mk_sort Sort.Bool in
    let* () = check sig_ ctx ce bool_sort in
    Ok (mk_sort Sort.Bool)

  | CoreExpr.Own s ->
    Ok (mk_sort (Sort.Pred s))

  | CoreExpr.Call (name, arg) ->
    (match Sig.lookup_spec_fun name sig_ with
     | Some (arg_sort, ret_sort) ->
       let* () = check sig_ ctx arg arg_sort in
       Ok ret_sort
     | None -> err_at_f pos "unknown spec function %a" Var.print name)

  | CoreExpr.Const name ->
    (match Sig.lookup_spec_val name sig_ with
     | Some s -> Ok s
     | None -> err_at_f pos "unknown spec constant %a" Var.print name)

  | CoreExpr.Annot (ce, s) ->
    let* () = check sig_ ctx ce s in
    Ok s

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
       check sig_ ctx inner tau
     | _ -> err_at pos "return requires pred sort")

  | CoreExpr.Take (x, ce1, ce2) ->
    (match Sort.shape sort with
     | Sort.Pred _tau' ->
       let* s1 = synth sig_ ctx ce1 in
       (match Sort.shape s1 with
        | Sort.Pred tau ->
          let ctx' = Context.extend_spec x tau ctx in
          check sig_ ctx' ce2 sort
        | _ -> err_at pos "take scrutinee must have pred sort")
     | _ -> err_at pos "take requires pred sort as target")

  | CoreExpr.Let (x, ce1, ce2) ->
    let* tau = synth sig_ ctx ce1 in
    let ctx' = Context.extend_spec x tau ctx in
    check sig_ ctx' ce2 sort

  | CoreExpr.Con (l, inner) ->
    (match Sort.shape sort with
     | Sort.App (_d, args) ->
       (match CtorLookup.lookup sig_ l args with
        | Ok ctor_sort ->
          check sig_ ctx inner ctor_sort
        | Error msg -> err_at_f pos "%s" msg)
     | _ -> err_at_f pos "constructor %a requires datasort type" Label.print l)

  | CoreExpr.Tuple es ->
    (match Sort.shape sort with
     | Sort.Record ts ->
       if List.compare_lengths es ts <> 0 then
         err_at_f pos "tuple: expected %d components, got %d"
           (List.length ts) (List.length es)
       else
         check_list sig_ ctx es ts
     | _ -> err_at pos "tuple: expected record sort")

  | CoreExpr.LetTuple (xs, ce1, ce2) ->
    let* s1 = synth sig_ ctx ce1 in
    (match Sort.shape s1 with
     | Sort.Record ts ->
       if List.compare_lengths xs ts <> 0 then
         err_at_f pos "let-tuple: expected %d components, got %d"
           (List.length ts) (List.length xs)
       else
         let bindings = List.combine xs ts in
         let ctx' = Context.extend_spec_list bindings ctx in
         check sig_ ctx' ce2 sort
     | _ -> err_at pos "let-tuple: scrutinee must have record sort")

  | CoreExpr.Case (scrut, branches) ->
    let* scrut_sort = synth sig_ ctx scrut in
    check_case_branches sig_ ctx branches scrut_sort sort pos

  | CoreExpr.If (ce1, ce2, ce3) ->
    let bool_sort = mk_sort Sort.Bool in
    let* () = check sig_ ctx ce1 bool_sort in
    let* () = check sig_ ctx ce2 sort in
    check sig_ ctx ce3 sort

  | _ ->
    (* Fall through to synthesis (sub rule) *)
    let* syn_sort = synth sig_ ctx ce in
    if sort_equal syn_sort sort then Ok ()
    else
      err_at_f pos "expected sort %a, got %a" Sort.print sort Sort.print syn_sort

and check_list sig_ ctx es sorts =
  match es, sorts with
  | [], [] -> Ok ()
  | e :: es', s :: ss' ->
    let* () = check sig_ ctx e s in
    check_list sig_ ctx es' ss'
  | _ -> Error "tuple length mismatch"

(** Check case branches using core_branch judgement:
    S; G |- (L x) : tau' -> ce <== tau *)
and check_case_branches sig_ ctx branches scrut_sort result_sort pos =
  match Sort.shape scrut_sort with
  | Sort.App (_d, args) ->
    let rec go = function
      | [] -> Ok ()
      | (l, x, body) :: rest ->
        (match CtorLookup.lookup sig_ l args with
         | Ok ctor_sort ->
           let ctx' = Context.extend_spec x ctor_sort ctx in
           let* () = check sig_ ctx' body result_sort in
           go rest
         | Error msg -> err_at_f pos "%s" msg)
    in
    go branches
  | _ -> err_at pos "case scrutinee must have datasort type"
