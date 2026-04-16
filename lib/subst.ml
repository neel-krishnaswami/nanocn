type entry =
  | TV of Tvar.t * Sort.sort
  | TM of Var.t * CoreExpr.typed_ce

type t = entry list

let empty = []

let extend_tvar a s sub = TV (a, s) :: sub
let extend_var x e sub = TM (x, e) :: sub

let rec lookup_tvar a = function
  | [] -> None
  | TV (b, s) :: rest ->
    if Tvar.compare a b = 0 then Some s else lookup_tvar a rest
  | TM _ :: rest -> lookup_tvar a rest

let rec lookup_var x = function
  | [] -> None
  | TM (y, e) :: rest ->
    if Var.compare x y = 0 then Some e else lookup_var x rest
  | TV _ :: rest -> lookup_var x rest

(* Sort substitution: only type variables are affected.
   Polymorphic in the info type so it works on both Sort.sort and
   typed_info Sort.t (as in CoreExpr.Annot). *)
let rec apply_sort : 'b. t -> 'b Sort.t -> 'b Sort.t = fun sub s ->
  let b = Sort.info s in
  match Sort.shape s with
  | Sort.TVar a ->
    (match lookup_tvar a sub with
     | Some s' -> Sort.map (fun _ -> b) s'
     | None -> s)
  | sf -> Sort.mk b (Sort.map_shape (apply_sort sub) sf)

let apply sub s = apply_sort sub s

(* Core expression substitution: extend at binders *)
let rec apply_ce sub e =
  let b = CoreExpr.info e in
  match CoreExpr.shape e with
  | CoreExpr.Var x ->
    (match lookup_var x sub with
     | Some e' -> e'
     | None -> e)
  | CoreExpr.IntLit _ | CoreExpr.BoolLit _ -> e
  | CoreExpr.Let ((v, vb), e1, e2) ->
    (* Shadowing entries use the *binder's* info, not [b] — [b] is the
       enclosing expression's info, whose sort is generally the let's
       result sort, not the bound variable's. See regression uncovered
       by examples/listlength.rcn. *)
    let e1' = apply_ce sub e1 in
    let sub' = extend_var v (CoreExpr.mk vb (CoreExpr.Var v)) sub in
    CoreExpr.mk b (CoreExpr.Let ((v, vb), e1', apply_ce sub' e2))
  | CoreExpr.Tuple es ->
    CoreExpr.mk b (CoreExpr.Tuple (List.map (apply_ce sub) es))
  | CoreExpr.LetTuple (vs, e1, e2) ->
    let e1' = apply_ce sub e1 in
    let sub' = List.fold_left (fun acc (v, vi) ->
      extend_var v (CoreExpr.mk vi (CoreExpr.Var v)) acc) sub vs in
    CoreExpr.mk b (CoreExpr.LetTuple (vs, e1', apply_ce sub' e2))
  | CoreExpr.Inject (l, e1) ->
    CoreExpr.mk b (CoreExpr.Inject (l, apply_ce sub e1))
  | CoreExpr.Case (scrut, branches) ->
    let scrut' = apply_ce sub scrut in
    let branches' = List.map (fun (l, v, body, bi) ->
      let sub' = extend_var v (CoreExpr.mk bi (CoreExpr.Var v)) sub in
      (l, v, apply_ce sub' body, bi)) branches in
    CoreExpr.mk b (CoreExpr.Case (scrut', branches'))
  | CoreExpr.Iter (v, e1, e2) ->
    (* Iter has no explicit bind info for [v]; its sort matches the
       init expression [e1]'s sort. *)
    let e1' = apply_ce sub e1 in
    let sub' = extend_var v (CoreExpr.mk (CoreExpr.info e1) (CoreExpr.Var v)) sub in
    CoreExpr.mk b (CoreExpr.Iter (v, e1', apply_ce sub' e2))
  | CoreExpr.App (p, e1) ->
    CoreExpr.mk b (CoreExpr.App (p, apply_ce sub e1))
  | CoreExpr.Call (f, e1) ->
    CoreExpr.mk b (CoreExpr.Call (f, apply_ce sub e1))
  | CoreExpr.If (e1, e2, e3) ->
    CoreExpr.mk b (CoreExpr.If (apply_ce sub e1, apply_ce sub e2, apply_ce sub e3))
  | CoreExpr.Annot (e1, sort) ->
    CoreExpr.mk b (CoreExpr.Annot (apply_ce sub e1, apply_sort sub sort))
  | CoreExpr.Eq (e1, e2) ->
    CoreExpr.mk b (CoreExpr.Eq (apply_ce sub e1, apply_ce sub e2))
  | CoreExpr.And (e1, e2) ->
    CoreExpr.mk b (CoreExpr.And (apply_ce sub e1, apply_ce sub e2))
  | CoreExpr.Not e1 ->
    CoreExpr.mk b (CoreExpr.Not (apply_ce sub e1))
  | CoreExpr.Take ((v, vb), e1, e2) ->
    let e1' = apply_ce sub e1 in
    let sub' = extend_var v (CoreExpr.mk vb (CoreExpr.Var v)) sub in
    CoreExpr.mk b (CoreExpr.Take ((v, vb), e1', apply_ce sub' e2))
  | CoreExpr.Return e1 ->
    CoreExpr.mk b (CoreExpr.Return (apply_ce sub e1))
  | CoreExpr.Fail ->
    CoreExpr.mk b CoreExpr.Fail

let of_lists tvars sorts =
  let n = List.length tvars in
  let m = List.length sorts in
  if n <> m then
    Error (Error.K_subst_arity_mismatch { expected = n; actual = m })
  else
    Ok (List.fold_right2 (fun a s acc -> TV (a, s) :: acc) tvars sorts [])

let id ctx =
  let mk_ce_info sort =
    (object method loc = SourcePos.dummy method ctx = Context.empty
            method sort = sort method eff = Effect.Spec end
     : CoreExpr.typed_info) in
  let mk_sort_info =
    (object method loc = SourcePos.dummy end) in
  let rec go = function
    | [] -> []
    | Context.Term (x, sort, _eff) :: rest ->
      TM (x, CoreExpr.mk (mk_ce_info sort) (CoreExpr.Var x)) :: go rest
    | Context.TVar (a, _kind) :: rest ->
      TV (a, Sort.mk mk_sort_info (Sort.TVar a)) :: go rest
  in
  go (Context.to_list ctx)

let compose gamma0 gamma1 =
  List.map (fun entry ->
    match entry with
    | TV (a, s) -> TV (a, apply gamma0 s)
    | TM (x, e) -> TM (x, apply_ce gamma0 e))
    gamma1

let compare s1 s2 =
  let compare_entry e1 e2 =
    match e1, e2 with
    | TV (a1, s1), TV (a2, s2) ->
      let c = Tvar.compare a1 a2 in
      if c <> 0 then c else Sort.compare s1 s2
    | TV _, TM _ -> -1
    | TM _, TV _ -> 1
    | TM (x1, _), TM (x2, _) -> Var.compare x1 x2
  in
  List.compare compare_entry s1 s2

let print fmt sub =
  let pp_entry fmt = function
    | TV (a, s) ->
      Format.fprintf fmt "@[<hov 2>%a@ :=@ %a@]" Tvar.print a Sort.print s
    | TM (x, _) ->
      Format.fprintf fmt "@[<hov 2>%a@ :=@ <ce>@]" Var.print x
  in
  Format.fprintf fmt "@[<hov 2>[%a]@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_entry)
    sub

module Test = struct
  let gen =
    let open QCheck.Gen in
    let mk_s s = Sort.mk (object method loc = SourcePos.dummy end) s in
    let simple_sort = oneof [
      pure (mk_s Sort.Int);
      pure (mk_s Sort.Bool);
      pure (mk_s (Sort.Ptr (mk_s Sort.Int)));
    ] in
    let* n = 0 -- 3 in
    let* entries = list_size (pure n) (
      let* tv = Tvar.Test.gen in
      let* s = simple_sort in
      pure (TV (tv, s))
    ) in
    pure entries

  let test =
    let mk_s s = Sort.mk (object method loc = SourcePos.dummy end) s in
    [ QCheck.Test.make ~name:"subst compare is reflexive"
        ~count:100
        (QCheck.make gen)
        (fun s -> compare s s = 0);

      QCheck.Test.make ~name:"subst empty is identity"
        ~count:100
        (QCheck.make Sort.Test.gen)
        (fun s -> Sort.compare (apply empty s) s = 0);

      QCheck.Test.make ~name:"subst apply replaces tvar"
        ~count:100
        (QCheck.make Tvar.Test.gen)
        (fun a ->
           let target = mk_s Sort.Int in
           let sub = extend_tvar a target empty in
           let input = mk_s (Sort.TVar a) in
           Sort.compare (apply sub input) target = 0);

      QCheck.Test.make ~name:"subst of_lists succeeds on equal lengths"
        ~count:100
        (QCheck.make gen)
        (fun entries ->
           let tvars = List.filter_map (function TV (a, _) -> Some a | TM _ -> None) entries in
           let sorts = List.filter_map (function TV (_, s) -> Some s | TM _ -> None) entries in
           match of_lists tvars sorts with
           | Ok _ -> true
           | Error _ -> false);
    ]
end
