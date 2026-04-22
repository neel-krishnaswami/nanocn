let ( let* ) = ElabM.( let* )

(** {1 Typed node types} *)

type typed_info = < loc : SourcePos.t; ctx : Context.t; sort : Sort.sort; eff : Effect.t >
type typed_ce = typed_info CoreExpr.t

let mk_typed ctx pos sort eff shape : typed_ce =
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

let lift_sort (s : Sort.sort) : typed_info Sort.t =
  Sort.map (fun loc_info ->
    (object
      method loc = loc_info#loc
      method ctx = Context.empty
      method sort = s
      method eff = Effect.Pure
    end : typed_info)) s

let mk_sort pos s = Sort.mk (object method loc = pos end) s

(* [invariant_at pos ~rule msg]: an "impossible-in-well-formed-code"
   check fired (e.g. a pattern-matrix shape that an earlier
   elaboration pass was meant to rule out). Emits
   [Error.K_internal_invariant]. *)
let invariant_at pos ~rule msg =
  ElabM.fail (Error.internal_invariant ~loc:pos ~rule ~invariant:msg)

let sort_equal a b = Sort.compare a b = 0

(** Take the first [n] elements and the remaining tail. If the list
    is shorter than [n], the second component is empty and the first
    is the whole list. *)
let rec split_at_n n xs =
  if n <= 0 then ([], xs)
  else match xs with
    | [] -> ([], [])
    | x :: xs' ->
      let (front, back) = split_at_n (n - 1) xs' in
      (x :: front, back)


(** {1 Prim signature (sort-level)} *)

let prim_signature (p : Prim.t) =
  let dummy_info = object method loc = SourcePos.dummy end in
  let mk s = Sort.mk dummy_info s in
  let int_sort = mk Sort.Int in
  let bool_sort = mk Sort.Bool in
  let pair_int = mk (Sort.Record [int_sort; int_sort]) in
  let pair_bool = mk (Sort.Record [bool_sort; bool_sort]) in
  let unit_sort = mk (Sort.Record []) in
  match p with
  | Prim.Add -> (pair_int, int_sort, Effect.Pure)
  | Prim.Sub -> (pair_int, int_sort, Effect.Pure)
  | Prim.Mul -> (pair_int, int_sort, Effect.Pure)
  | Prim.Div -> (pair_int, int_sort, Effect.Impure)
  | Prim.Lt -> (pair_int, bool_sort, Effect.Pure)
  | Prim.Le -> (pair_int, bool_sort, Effect.Pure)
  | Prim.Gt -> (pair_int, bool_sort, Effect.Pure)
  | Prim.Ge -> (pair_int, bool_sort, Effect.Pure)
  | Prim.And -> (pair_bool, bool_sort, Effect.Pure)
  | Prim.Or -> (pair_bool, bool_sort, Effect.Pure)
  | Prim.Not -> (bool_sort, bool_sort, Effect.Pure)
  | Prim.Eq a ->
    (mk (Sort.Record [a; a]), bool_sort, Effect.Pure)
  | Prim.New a ->
    (a, mk (Sort.Ptr a), Effect.Impure)
  | Prim.Del a ->
    (mk (Sort.Ptr a), unit_sort, Effect.Impure)
  | Prim.Get a ->
    (mk (Sort.Ptr a), a, Effect.Impure)
  | Prim.Set a ->
    (mk (Sort.Record [mk (Sort.Ptr a); a]), unit_sort, Effect.Impure)
  | Prim.Own a ->
    (mk (Sort.Ptr a), mk (Sort.Pred a), Effect.Spec)

(** {1 Coverage types} *)

type binding = Pat.pat * Sort.sort

type let_binding = {
  var : Var.t;
  rhs : Var.t;
  sort : Sort.sort;
  eff : Effect.t;
  loc : SourcePos.t;
}

type branch = {
  bindings : binding list;
  let_bindings : let_binding list;
  body : SurfExpr.se;
}

(** {1 Coverage helpers} *)

let ctx_of_let_bindings lbs =
  List.map (fun lb -> (lb.var, lb.sort, lb.eff)) lbs

let rec wrap_lets lets outer_ctx result_sort eff0 body =
  match lets with
  | [] -> body
  | lb :: rest ->
    let inner_ctx = Context.extend lb.var lb.sort lb.eff outer_ctx in
    let inner = wrap_lets rest inner_ctx result_sort eff0 body in
    let rhs = mk_typed outer_ctx lb.loc lb.sort eff0 (CoreExpr.Var lb.rhs) in
    let bind_info = mk_bind_info lb.var lb.sort lb.eff inner_ctx in
    mk_typed outer_ctx lb.loc result_sort eff0
      (CoreExpr.Let ((lb.var, bind_info), rhs, inner))

let has_con branches =
  List.exists (fun br ->
    match br.bindings with
    | (p, _) :: _ -> (match Pat.shape p with Pat.Con _ -> true | _ -> false)
    | _ -> false) branches

let has_tup branches =
  List.exists (fun br ->
    match br.bindings with
    | (p, _) :: _ -> (match Pat.shape p with Pat.Tuple _ -> true | _ -> false)
    | _ -> false) branches

(** strip_var: all leading patterns are variables.
    Each [x : sort] gets a let-binding [let x = y]. *)
let strip_var y eff_b branches =
  List.map (fun br ->
    match br.bindings with
    | (p, sort) :: rest ->
      (match Pat.shape p with
       | Pat.Var x ->
         { bindings = rest;
           let_bindings = br.let_bindings @ [{ var = x; rhs = y; sort; eff = eff_b;
                                                loc = Var.binding_site x }];
           body = br.body }
       | _ -> br)
    | _ -> br
  ) branches

(** spec_con: filter branches for a given constructor label. *)
let rec spec_con label ctor_sort y eff_b branches =
  match branches with
  | [] -> ElabM.return []
  | br :: rest ->
    match br.bindings with
    | (p, sort) :: binds ->
      (match Pat.shape p with
       | Pat.Con (l, subpat) when Label.compare l label = 0 ->
         let* rest' = spec_con label ctor_sort y eff_b rest in
         ElabM.return ({ br with bindings = (subpat, ctor_sort) :: binds } :: rest')
       | Pat.Con _ ->
         spec_con label ctor_sort y eff_b rest
       | Pat.Var x ->
         let* z = ElabM.fresh (Var.binding_site x) in
         let z_pat = Pat.mk (object method loc = Var.binding_site z end) (Pat.Var z) in
         let* rest' = spec_con label ctor_sort y eff_b rest in
         ElabM.return ({ bindings = (z_pat, ctor_sort) :: binds;
                         let_bindings = br.let_bindings @ [{ var = x; rhs = y; sort; eff = eff_b;
                                                              loc = Var.binding_site x }];
                         body = br.body } :: rest')
       | _ -> spec_con label ctor_sort y eff_b rest)
    | _ -> spec_con label ctor_sort y eff_b rest

(** expand_tup: expand tuple patterns in the leading position. *)
let rec expand_tup sorts y eff_b branches =
  match branches with
  | [] -> ElabM.return []
  | br :: rest ->
    match br.bindings with
    | (p, sort) :: binds ->
      (match Pat.shape p with
       | Pat.Tuple pats ->
         if List.compare_lengths pats sorts <> 0 then
           invariant_at (Pat.info p)#loc ~rule:"expand_tup"
             "tuple pattern's arity does not match the sort's record \
              arity (should have been checked by coverage dispatch)"
         else
           let new_bindings = List.combine pats sorts in
           let* rest' = expand_tup sorts y eff_b rest in
           ElabM.return ({ br with bindings = new_bindings @ binds } :: rest')
       | Pat.Var x ->
         let* fresh_zs = fresh_vars_for_sorts sorts (Var.binding_site x) in
         let z_pats = List.map (fun (z, s) ->
           (Pat.mk (object method loc = Var.binding_site z end) (Pat.Var z), s)
         ) fresh_zs in
         let* rest' = expand_tup sorts y eff_b rest in
         ElabM.return ({ bindings = z_pats @ binds;
                         let_bindings = br.let_bindings @ [{ var = x; rhs = y; sort; eff = eff_b;
                                                              loc = Var.binding_site x }];
                         body = br.body } :: rest')
       | _ ->
         let* rest' = expand_tup sorts y eff_b rest in
         ElabM.return rest')
    | _ ->
      let* rest' = expand_tup sorts y eff_b rest in
      ElabM.return rest'

and fresh_vars_for_sorts sorts pos =
  match sorts with
  | [] -> ElabM.return []
  | s :: rest ->
    let* z = ElabM.fresh pos in
    let* zs = fresh_vars_for_sorts rest pos in
    ElabM.return ((z, s) :: zs)

and fresh_vars_with_positions sorts positions =
  match sorts, positions with
  | [], _ -> ElabM.return []
  | s :: rest_s, pos :: rest_p ->
    let* z = ElabM.fresh pos in
    let* zs = fresh_vars_with_positions rest_s rest_p in
    ElabM.return ((z, s) :: zs)
  | s :: rest_s, [] ->
    let* z = ElabM.fresh SourcePos.dummy in
    let* zs = fresh_vars_with_positions rest_s [] in
    ElabM.return ((z, s) :: zs)

(** {1 Pattern position helpers} *)

(** Position of the first Con subpattern matching [label]. *)
let find_con_subpat_pos label branches fallback =
  let rec go = function
    | [] -> fallback
    | br :: rest ->
      match br.bindings with
      | (p, _) :: _ ->
        (match Pat.shape p with
         | Pat.Con (l, subpat) when Label.compare l label = 0 ->
           (Pat.info subpat)#loc
         | _ -> go rest)
      | _ -> go rest
  in
  go branches

(** Positions of sub-patterns from the first Tuple pattern in branches. *)
let find_tup_subpat_positions branches n fallback_pos =
  let rec go = function
    | [] -> List.init n (fun _ -> fallback_pos)
    | br :: rest ->
      match br.bindings with
      | (p, _) :: _ ->
        (match Pat.shape p with
         | Pat.Tuple pats -> List.map (fun p -> (Pat.info p)#loc) pats
         | _ -> go rest)
      | _ -> go rest
  in
  go branches

(** {1 Elaboration} *)

let rec synth sig_ ctx eff0 se =
  let pos = (SurfExpr.info se)#loc in
  match SurfExpr.shape se with
  | SurfExpr.Var x ->
    (match Context.lookup x ctx with
     | Some (s, var_eff) ->
       if Effect.sub var_eff eff0 then
         ElabM.return (mk_typed ctx pos s eff0 (CoreExpr.Var x), s)
       else
         ElabM.fail
           (Error.var_effect_mismatch
              ~loc:pos ~var:x
              ~declared:var_eff ~required:eff0)
     | None ->
       ElabM.fail (Error.unbound_var ~loc:pos x))

  | SurfExpr.IntLit n ->
    let s = mk_sort pos Sort.Int in
    ElabM.return (mk_typed ctx pos s eff0 (CoreExpr.IntLit n), s)

  | SurfExpr.BoolLit b ->
    let s = mk_sort pos Sort.Bool in
    ElabM.return (mk_typed ctx pos s eff0 (CoreExpr.BoolLit b), s)

  | SurfExpr.Eq (se1, se2) ->
    let eff0' = Effect.purify eff0 in
    let* (ce1, s) = synth sig_ ctx eff0' se1 in
    if not (Sort.is_spec_type s) then
      ElabM.fail (Error.not_spec_type
                    ~loc:pos ~construct:"equality" ~got:s)
    else
      let* ce2 = check sig_ ctx se2 s eff0' in
      let bool_sort = mk_sort pos Sort.Bool in
      ElabM.return (mk_typed ctx pos bool_sort eff0 (CoreExpr.Eq (ce1, ce2)), bool_sort)

  | SurfExpr.And (se1, se2) ->
    let bool_sort = mk_sort pos Sort.Bool in
    let* ce1 = check sig_ ctx se1 bool_sort eff0 in
    let* ce2 = check sig_ ctx se2 bool_sort eff0 in
    ElabM.return (mk_typed ctx pos bool_sort eff0 (CoreExpr.And (ce1, ce2)), bool_sort)

  | SurfExpr.Not se ->
    let bool_sort = mk_sort pos Sort.Bool in
    let* ce = check sig_ ctx se bool_sort eff0 in
    ElabM.return (mk_typed ctx pos bool_sort eff0 (CoreExpr.Not ce), bool_sort)

  | SurfExpr.App (p, arg) ->
    let* () = match p with
      | Prim.Eq a ->
        if Sort.is_eqtype a then ElabM.return ()
        else
          ElabM.fail (Error.eq_not_equality_type ~loc:pos ~got:a)
      | _ -> ElabM.return ()
    in
    let (arg_sort, ret_sort, prim_eff) = prim_signature p in
    if not (Effect.sub prim_eff eff0) then
      ElabM.fail
        (Error.prim_effect_mismatch
           ~loc:pos ~prim:p ~declared:prim_eff ~required:eff0)
    else
      let eff0' = Effect.purify eff0 in
      let* ce_arg = check sig_ ctx arg arg_sort eff0' in
      ElabM.return (mk_typed ctx pos ret_sort eff0 (CoreExpr.App (p, ce_arg)), ret_sort)

  | SurfExpr.Call (name, arg) ->
    (match Sig.lookup_fun name sig_ with
     | Some (arg_sort, ret_sort, fun_eff) ->
       if not (Effect.sub fun_eff eff0) then
         ElabM.fail
           (Error.fun_effect_mismatch
              ~loc:pos ~name ~declared:fun_eff ~required:eff0)
       else
         let eff0' = Effect.purify eff0 in
         let* ce_arg = check sig_ ctx arg arg_sort eff0' in
         ElabM.return (mk_typed ctx pos ret_sort eff0 (CoreExpr.Call (name, ce_arg)), ret_sort)
     | None ->
       ElabM.fail (Error.unknown_function ~loc:pos ~name))

  | SurfExpr.Annot (se, s) ->
    let* ce = check sig_ ctx se s eff0 in
    ElabM.return (mk_typed ctx pos s eff0 (CoreExpr.Annot (ce, lift_sort s)), s)

  | SurfExpr.Return _ | SurfExpr.Take _ | SurfExpr.Fail | SurfExpr.Hole _
  | SurfExpr.Let _ | SurfExpr.Tuple _ | SurfExpr.Inject _ | SurfExpr.Case _
  | SurfExpr.Iter _ | SurfExpr.If _ ->
    ElabM.fail (Error.cannot_synthesize ~loc:pos ~construct:"sort")

and check sig_ ctx se sort eff0 =
  let pos = (SurfExpr.info se)#loc in
  match SurfExpr.shape se with
  | SurfExpr.Return inner ->
    if not (Effect.sub Effect.Spec eff0) then
      ElabM.fail (Error.spec_context_required ~loc:pos ~construct:"return")
    else
    (match Sort.shape sort with
     | Sort.Pred tau ->
       let* ce = check sig_ ctx inner tau eff0 in
       ElabM.return (mk_typed ctx pos sort eff0 (CoreExpr.Return ce))
     | _ ->
       ElabM.fail
         (Error.construct_sort_mismatch ~loc:pos
            ~construct:"return" ~expected_shape:"Pred _"
            ~got:sort))

  | SurfExpr.Fail ->
    if not (Effect.sub Effect.Spec eff0) then
      ElabM.fail (Error.spec_context_required ~loc:pos ~construct:"fail")
    else
    (match Sort.shape sort with
     | Sort.Pred _ ->
       ElabM.return (mk_typed ctx pos sort eff0 CoreExpr.Fail)
     | _ ->
       ElabM.fail
         (Error.construct_sort_mismatch ~loc:pos
            ~construct:"fail" ~expected_shape:"Pred _"
            ~got:sort))

  | SurfExpr.Take (pat, se1, se2) ->
    if not (Effect.sub Effect.Spec eff0) then
      ElabM.fail (Error.spec_context_required ~loc:pos ~construct:"take")
    else
    (match Sort.shape sort with
     | Sort.Pred _ ->
       let* (ce1, s1) = synth sig_ ctx eff0 se1 in
       (match Sort.shape s1 with
        | Sort.Pred tau ->
          let eff_b = Effect.purify eff0 in
          let* y = ElabM.fresh (Pat.info pat)#loc in
          let ctx_y = Context.extend y tau eff_b ctx in
          let branch = {
            bindings = [(pat, tau)];
            let_bindings = [];
            body = se2;
          } in
          let rebuilder_init = function
            | [w] -> w
            | _ -> PatWitness.Wild
          in
          let* ce2 =
            coverage_check sig_ ctx_y [y] [branch] eff_b sort eff0
              ~cov_loc:pos rebuilder_init in
          let yb = (y, mk_bind_info y tau eff_b ctx_y) in
          ElabM.return (mk_typed ctx pos sort eff0 (CoreExpr.Take (yb, ce1, ce2)))
        | _ ->
          ElabM.fail
            (Error.construct_sort_mismatch ~loc:pos
               ~construct:"take scrutinee"
               ~expected_shape:"Pred _" ~got:s1))
     | _ ->
       ElabM.fail
         (Error.construct_sort_mismatch ~loc:pos
            ~construct:"take target"
            ~expected_shape:"Pred _" ~got:sort))

  | SurfExpr.Let (pat, se1, se2) ->
    let* (ce1, tau) = synth sig_ ctx eff0 se1 in
    let eff_b = Effect.purify eff0 in
    let* y = ElabM.fresh (Pat.info pat)#loc in
    let ctx_y = Context.extend y tau eff_b ctx in
    let branch = {
      bindings = [(pat, tau)];
      let_bindings = [];
      body = se2;
    } in
    let rebuilder_init = function
      | [w] -> w
      | _ -> PatWitness.Wild
    in
    let* ce2 =
      coverage_check sig_ ctx_y [y] [branch] eff_b sort eff0
        ~cov_loc:pos rebuilder_init in
    let yb = (y, mk_bind_info y tau eff_b ctx_y) in
    ElabM.return (mk_typed ctx pos sort eff0 (CoreExpr.Let (yb, ce1, ce2)))

  | SurfExpr.Tuple ses ->
    (match Sort.shape sort with
     | Sort.Record sorts ->
       if List.compare_lengths ses sorts <> 0 then
         ElabM.fail
           (Error.tuple_arity_mismatch ~loc:pos
              ~construct:"tuple"
              ~expected:(List.length sorts)
              ~actual:(List.length ses))
       else
         let* ces = check_list sig_ ctx ses sorts eff0 in
         ElabM.return (mk_typed ctx pos sort eff0 (CoreExpr.Tuple ces))
     | _ ->
       ElabM.fail
         (Error.construct_sort_mismatch ~loc:pos
            ~construct:"tuple" ~expected_shape:"Record _"
            ~got:sort))

  | SurfExpr.Inject (l, inner) ->
    (match Sort.shape sort with
     | Sort.App (_d, args) ->
       (match CtorLookup.lookup sig_ l args with
        | Ok ctor_sort ->
          let eff0' = Effect.purify eff0 in
          let* ce = check sig_ ctx inner ctor_sort eff0' in
          ElabM.return (mk_typed ctx pos sort eff0 (CoreExpr.Inject (l, ce)))
        | Error k ->
          ElabM.fail (Error.structured ~loc:pos k))
     | _ ->
       ElabM.fail
         (Error.construct_sort_mismatch ~loc:pos
            ~construct:(Format.asprintf "constructor %a" Label.print l)
            ~expected_shape:"datasort/datatype application"
            ~got:sort))

  | SurfExpr.Case (scrut, surf_branches) ->
    let eff0' = Effect.purify eff0 in
    let* (ce_scrut, scrut_sort) = synth sig_ ctx eff0' scrut in
    let* y = ElabM.fresh (SurfExpr.info scrut)#loc in
    let ctx_y = Context.extend y scrut_sort eff0' ctx in
    let branches = List.map (fun (pat, body, _) ->
      { bindings = [(pat, scrut_sort)];
        let_bindings = [];
        body }
    ) surf_branches in
    let rebuilder_init = function
      | [w] -> w
      | _ -> PatWitness.Wild
    in
    let* ce_body =
      coverage_check sig_ ctx_y [y] branches eff0' sort eff0
        ~cov_loc:pos rebuilder_init in
    let yb = (y, mk_bind_info y scrut_sort eff0' ctx_y) in
    ElabM.return (mk_typed ctx pos sort eff0 (CoreExpr.Let (yb, ce_scrut, ce_body)))

  | SurfExpr.Iter (pat, se1, se2) ->
    if not (Effect.sub Effect.Impure eff0) then
      ElabM.fail (Error.iter_requires_impure ~loc:pos ~actual:eff0)
    else
    let* (ce1, init_sort) = synth sig_ ctx Effect.Pure se1 in
    let* step_dsort = match Dsort.of_string "Step" with
      | Ok d -> ElabM.return d
      | Error _ ->
        invariant_at pos ~rule:"iter"
          "Dsort.of_string \"Step\" failed — \"Step\" must always parse as a \
           well-formed sort name"
    in
    let iter_sort = mk_sort pos (Sort.App (step_dsort, [init_sort; sort])) in
    let* y = ElabM.fresh (Pat.info pat)#loc in
    let bind_eff = Effect.purify Effect.Impure in
    let ctx_y = Context.extend y init_sort bind_eff ctx in
    let branch = {
      bindings = [(pat, init_sort)];
      let_bindings = [];
      body = se2;
    } in
    let rebuilder_init = function
      | [w] -> w
      | _ -> PatWitness.Wild
    in
    let* ce_body =
      coverage_check sig_ ctx_y [y] [branch] bind_eff iter_sort Effect.Impure
        ~cov_loc:pos rebuilder_init in
    ElabM.return (mk_typed ctx pos sort eff0 (CoreExpr.Iter (y, ce1, ce_body)))

  | SurfExpr.If (se1, se2, se3) ->
    let bool_sort = mk_sort pos Sort.Bool in
    let eff0' = Effect.purify eff0 in
    let* ce1 = check sig_ ctx se1 bool_sort eff0' in
    let* ce2 = check sig_ ctx se2 sort eff0 in
    let* ce3 = check sig_ ctx se3 sort eff0 in
    ElabM.return (mk_typed ctx pos sort eff0 (CoreExpr.If (ce1, ce2, ce3)))

  | SurfExpr.Hole h ->
    ElabM.return (mk_typed ctx pos sort eff0 (CoreExpr.Hole h))

  | _ ->
    let* (ce, syn_sort) = synth sig_ ctx eff0 se in
    if not (sort_equal syn_sort sort) then
      ElabM.fail
        (Error.sort_mismatch ~loc:pos ~expected:sort ~actual:syn_sort)
    else ElabM.return ce

and check_list sig_ ctx ses sorts eff0 =
  match ses, sorts with
  | [], [] -> ElabM.return []
  | se :: ses', s :: ss' ->
    let* ce = check sig_ ctx se s eff0 in
    let* rest = check_list sig_ ctx ses' ss' eff0 in
    ElabM.return (ce :: rest)
  | ses, sorts ->
    let pos = match ses, sorts with
      | se :: _, _ -> (SurfExpr.info se)#loc
      | [], s :: _ -> (Sort.info s)#loc
      | [], [] -> SourcePos.dummy
    in
    invariant_at pos ~rule:"check_list"
      "tuple expression and record sort have different arities \
       (should have been caught by the Tuple case in check)"

(** {1 Coverage}

    The [eff_b] parameter is the binding effect for scrutinee variables —
    per the spec, this is [purify eff0] (the purification of the ambient effect).

    [ctx] includes the scrutinee variables; callers extend it before calling. *)

(* Coverage threads a [rebuilder : PatWitness.t list -> PatWitness.t]
   closure. At any recursion depth, [rebuilder] takes a list of
   witnesses whose length matches the current [scrutinees] length
   and lifts them back up to a witness for the original top-level
   scrutinee(s). On reaching `[], []` we call [rebuilder []] to
   obtain the missing-case witness for the surface [case] error. *)

and coverage_check sig_ ctx scrutinees branches eff_b sort eff0 ~cov_loc rebuilder =
  match scrutinees, branches with
  (* Cov_done: no scrutinees, at least one branch *)
  | [], br :: _ ->
    (match br.bindings with
     | [] ->
       let ctx' = Context.extend_list (ctx_of_let_bindings br.let_bindings) ctx in
       let* ce' = check sig_ ctx' br.body sort eff0 in
       ElabM.return (wrap_lets br.let_bindings ctx sort eff0 ce')
     | (p, _) :: _ ->
       invariant_at (Pat.info p)#loc ~rule:"coverage_check:Cov_done"
         "branch still has un-consumed pattern bindings after all \
          scrutinees have been dispatched")

  | [], [] ->
    let witness = rebuilder [] in
    ElabM.fail (Error.non_exhaustive ~loc:cov_loc ~witness)

  (* Cov_var: all leading patterns are variables — consume the
     scrutinee, its witness slot becomes [Wild]. *)
  | y :: scrs, _ when not (has_con branches) && not (has_tup branches) ->
    let branches' = strip_var y eff_b branches in
    let rebuilder' rest = rebuilder (PatWitness.Wild :: rest) in
    coverage_check sig_ ctx scrs branches' eff_b sort eff0 ~cov_loc rebuilder'

  (* Cov_con: some leading patterns are constructors *)
  | y :: scrs, _ when has_con branches ->
    let lead_sort = find_lead_sort branches in
    (match Sort.shape lead_sort with
     | Sort.App (dsort_name, args) ->
       (* Try datasort first, then datatype *)
       (match Sig.lookup_sort dsort_name sig_ with
        | Some decl ->
          let labels = DsortDecl.ctor_labels decl in
          let* case_branches =
            build_sort_con_branches sig_ ctx y scrs branches eff_b sort eff0
              labels args decl ~cov_loc rebuilder in
          let y_ce = mk_typed ctx (Var.binding_site y) lead_sort eff_b (CoreExpr.Var y) in
          ElabM.return (mk_typed ctx (Var.binding_site y) sort eff0
            (CoreExpr.Case (y_ce, case_branches)))
        | None ->
          match Sig.lookup_type dsort_name sig_ with
          | Some decl ->
            let labels = DtypeDecl.ctor_labels decl in
            let* case_branches =
              build_type_con_branches sig_ ctx y scrs branches eff_b sort eff0
                labels args decl ~cov_loc rebuilder in
            let y_ce = mk_typed ctx (Var.binding_site y) lead_sort eff_b (CoreExpr.Var y) in
            ElabM.return (mk_typed ctx (Var.binding_site y) sort eff0
              (CoreExpr.Case (y_ce, case_branches)))
          | None ->
            ElabM.fail
              (Error.unbound_sort ~loc:(Var.binding_site y) dsort_name))
     | _ ->
       invariant_at (Sort.info lead_sort)#loc ~rule:"coverage_check:Cov_con"
         "a leading pattern is a constructor but its sort is not a \
          datasort or datatype application")

  (* Cov_tup: some leading patterns are tuples — decompose the
     scrutinee into its k fields. The scrutinee's witness slot
     becomes [Tuple <first k of rest>]. *)
  | y :: scrs, _ when has_tup branches ->
    let lead_sort = find_lead_sort branches in
    (match Sort.shape lead_sort with
     | Sort.Record sorts ->
       let k = List.length sorts in
       let positions = find_tup_subpat_positions branches k (Var.binding_site y) in
       let* fresh_zs = fresh_vars_with_positions sorts positions in
       let fresh_vars = List.map fst fresh_zs in
       let ctx' = List.fold_left (fun c (z, s) ->
         Context.extend z s eff_b c) ctx fresh_zs in
       let* branches' = expand_tup sorts y eff_b branches in
       let rebuilder' rest =
         let (firsts, tail) = split_at_n k rest in
         rebuilder (PatWitness.Tuple firsts :: tail)
       in
       let* ce =
         coverage_check sig_ ctx' (fresh_vars @ scrs) branches'
           eff_b sort eff0 ~cov_loc rebuilder' in
       let y_ce = mk_typed ctx (Var.binding_site y) lead_sort eff_b (CoreExpr.Var y) in
       let annotated_vars = List.map (fun (z, s) ->
         (z, mk_bind_info z s eff_b ctx')
       ) fresh_zs in
       ElabM.return (mk_typed ctx (Var.binding_site y) sort eff0
         (CoreExpr.LetTuple (annotated_vars, y_ce, ce)))
     | _ ->
       invariant_at (Sort.info lead_sort)#loc ~rule:"coverage_check:Cov_tup"
         "a leading pattern is a tuple but its sort is not a record")

  | _ :: _, branches ->
    (* Dispatch couldn't classify as Cov_var, Cov_con, or Cov_tup.
       Use the first branch's first pattern as a location if any. *)
    let pos = match branches with
      | br :: _ ->
        (match br.bindings with
         | (p, _) :: _ -> (Pat.info p)#loc
         | [] -> SourcePos.dummy)
      | [] -> SourcePos.dummy
    in
    invariant_at pos ~rule:"coverage_check"
      "a leading pattern does not match any dispatch case \
       (Cov_var / Cov_con / Cov_tup)"

and build_sort_con_branches sig_ ctx y scrs branches eff_b sort eff0 labels args decl ~cov_loc rebuilder =
  match labels with
  | [] -> ElabM.return []
  | label :: rest_labels ->
    let* ctor_sort = match DsortDecl.lookup_ctor label decl with
      | Some s ->
        (match Subst.of_lists decl.DsortDecl.params args with
         | Ok sub -> ElabM.return (Subst.apply sub s)
         | Error _ -> ElabM.return s)
      | None ->
        invariant_at cov_loc ~rule:"build_sort_con_branches"
          (Format.asprintf
             "constructor %a is listed in ctor_labels but \
              DsortDecl.lookup_ctor returned None"
             Label.print label)
    in
    let* filtered = spec_con label ctor_sort y eff_b branches in
    let xi_pos = find_con_subpat_pos label branches (Var.binding_site y) in
    let* xi = ElabM.fresh xi_pos in
    let ctx_xi = Context.extend xi ctor_sort eff_b ctx in
    (* Sub-call recurses with xi in front of scrs. Its rebuilder
       wraps the head witness in [Ctor(label, _)]. *)
    let rebuilder_L rest =
      match rest with
      | head :: tail -> rebuilder (PatWitness.Ctor (label, head) :: tail)
      | [] -> (* impossible: recursion must consume xi *)
        rebuilder (PatWitness.Ctor (label, PatWitness.Wild) :: [])
    in
    let* ce_i =
      coverage_check sig_ ctx_xi (xi :: scrs) filtered eff_b sort eff0
        ~cov_loc rebuilder_L in
    let* rest =
      build_sort_con_branches sig_ ctx y scrs branches eff_b sort eff0
        rest_labels args decl ~cov_loc rebuilder in
    let branch_info = mk_bind_info xi ctor_sort eff_b ctx_xi in
    ElabM.return ((label, xi, ce_i, branch_info) :: rest)

and build_type_con_branches sig_ ctx y scrs branches eff_b sort eff0 labels args decl ~cov_loc rebuilder =
  match labels with
  | [] -> ElabM.return []
  | label :: rest_labels ->
    let* ctor_sort = match DtypeDecl.lookup_ctor label decl with
      | Some raw_sort ->
        (match Subst.of_lists decl.DtypeDecl.params args with
         | Ok sub -> ElabM.return (Subst.apply sub raw_sort)
         | Error _ -> ElabM.return raw_sort)
      | None ->
        invariant_at cov_loc ~rule:"build_type_con_branches"
          (Format.asprintf
             "constructor %a is listed in ctor_labels but \
              DtypeDecl.lookup_ctor returned None"
             Label.print label)
    in
    let* filtered = spec_con label ctor_sort y eff_b branches in
    let xi_pos = find_con_subpat_pos label branches (Var.binding_site y) in
    let* xi = ElabM.fresh xi_pos in
    let ctx_xi = Context.extend xi ctor_sort eff_b ctx in
    let rebuilder_L rest =
      match rest with
      | head :: tail -> rebuilder (PatWitness.Ctor (label, head) :: tail)
      | [] ->
        rebuilder (PatWitness.Ctor (label, PatWitness.Wild) :: [])
    in
    let* ce_i =
      coverage_check sig_ ctx_xi (xi :: scrs) filtered eff_b sort eff0
        ~cov_loc rebuilder_L in
    let* rest =
      build_type_con_branches sig_ ctx y scrs branches eff_b sort eff0
        rest_labels args decl ~cov_loc rebuilder in
    let branch_info = mk_bind_info xi ctor_sort eff_b ctx_xi in
    ElabM.return ((label, xi, ce_i, branch_info) :: rest)

and find_lead_sort branches =
  match branches with
  | br :: _ ->
    (match br.bindings with
     | (_, sort) :: _ -> sort
     | [] -> mk_sort SourcePos.dummy Sort.Int)
  | [] -> mk_sort SourcePos.dummy Sort.Int
