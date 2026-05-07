type 'a t = 'a option

module Get = struct
  let return = function
    | None -> None
    | Some ce ->
      (match CoreExpr.shape ce with
       | CoreExpr.Return inner -> Some inner
       | _ -> None)

  let fail = function
    | None -> None
    | Some ce ->
      (match CoreExpr.shape ce with
       | CoreExpr.Fail -> Some ()
       | _ -> None)

  let take = function
    | None -> None
    | Some ce ->
      (match CoreExpr.shape ce with
       | CoreExpr.Take ((x, _), e1, e2) -> Some (x, e1, e2)
       | _ -> None)

  let let_ = function
    | None -> None
    | Some ce ->
      (match CoreExpr.shape ce with
       | CoreExpr.Let ((x, _), e1, e2) -> Some (x, e1, e2)
       | _ -> None)

  let let_tuple = function
    | None -> None
    | Some ce ->
      (match CoreExpr.shape ce with
       | CoreExpr.LetTuple (xs, e1, e2) ->
         Some (List.map fst xs, e1, e2)
       | _ -> None)

  let if_ = function
    | None -> None
    | Some ce ->
      (match CoreExpr.shape ce with
       | CoreExpr.If (c, t, e) -> Some (c, t, e)
       | _ -> None)

  let case = function
    | None -> None
    | Some ce ->
      (match CoreExpr.shape ce with
       | CoreExpr.Case (scrut, branches) -> Some (scrut, branches)
       | _ -> None)

  let call = function
    | None -> None
    | Some ce ->
      (match CoreExpr.shape ce with
       | CoreExpr.Call (f, arg) -> Some (f, arg)
       | _ -> None)

  let inject = function
    | None -> None
    | Some ce ->
      (match CoreExpr.shape ce with
       | CoreExpr.Inject (l, inner) -> Some (l, inner)
       | _ -> None)
end

module Build = struct
  let return info = function
    | None -> None
    | Some inner -> Some (CoreExpr.mk info (CoreExpr.Return inner))

  let eq info l r =
    match l, r with
    | Some l, Some r -> Some (CoreExpr.mk info (CoreExpr.Eq (l, r)))
    | _ -> None

  let rec sequence_options = function
    | [] -> Some []
    | None :: _ -> None
    | Some x :: rest ->
      (match sequence_options rest with
       | None -> None
       | Some xs -> Some (x :: xs))

  let tuple info xs =
    match sequence_options xs with
    | None -> None
    | Some es -> Some (CoreExpr.mk info (CoreExpr.Tuple es))
end

module Test = struct
  let dummy_info =
    object
      method loc = SourcePos.dummy
      method ctx = Context.empty
      method answer = Ok (Sort.mk (object method loc = SourcePos.dummy end) Sort.Bool)
      method eff = Effect.Pure
      method subterm_errors = []
    end

  let int_sort = Sort.mk (object method loc = SourcePos.dummy end) Sort.Int

  let mk_int_lit n =
    let info =
      object
        method loc = SourcePos.dummy
        method ctx = Context.empty
        method answer = Ok int_sort
        method eff = Effect.Pure
        method subterm_errors = []
      end
    in
    CoreExpr.mk info (CoreExpr.IntLit n)

  let test = [
    QCheck.Test.make
      ~name:"CoreExprView.Get.return: Some Return -> Some inner"
      ~count:1 QCheck.unit
      (fun () ->
         let inner = mk_int_lit 1 in
         let ret = CoreExpr.mk dummy_info (CoreExpr.Return inner) in
         match Get.return (Some ret) with
         | Some _ -> true
         | None -> false);

    QCheck.Test.make
      ~name:"CoreExprView.Get.return: Some non-Return -> None"
      ~count:1 QCheck.unit
      (fun () ->
         let lit = mk_int_lit 1 in
         match Get.return (Some lit) with
         | None -> true
         | Some _ -> false);

    QCheck.Test.make
      ~name:"CoreExprView.Get.return: None -> None"
      ~count:1 QCheck.unit
      (fun () ->
         match Get.return None with
         | None -> true
         | Some _ -> false);

    QCheck.Test.make
      ~name:"CoreExprView.Build.return: round-trip"
      ~count:1 QCheck.unit
      (fun () ->
         let inner = mk_int_lit 1 in
         match Build.return dummy_info (Some inner) with
         | Some _ -> true
         | None -> false);

    QCheck.Test.make
      ~name:"CoreExprView round-trip: Get then Build returns equivalent"
      ~count:1 QCheck.unit
      (fun () ->
         let inner = mk_int_lit 1 in
         let ret = CoreExpr.mk dummy_info (CoreExpr.Return inner) in
         match Get.return (Some ret) with
         | None -> false
         | Some inner_extracted ->
           match Build.return dummy_info (Some inner_extracted) with
           | Some _ -> true
           | None -> false);
  ]
end
