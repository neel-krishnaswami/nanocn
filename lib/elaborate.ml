let ( let* ) = ElabM.( let* )

(** {1 Typed node types} *)

type typed_info = CoreExpr.typed_info
type typed_ce = typed_info CoreExpr.t

let mk_typed ctx pos sort eff shape : typed_ce =
  CoreExpr.mk (object
    method loc = pos
    method ctx = ctx
    method answer = Ok sort
    method eff = eff
  end) shape

(** [mk ctx pos answer eff shape]: like [mk_typed] but takes the
    [answer] field directly so a clause can attach an [Error _] result
    when the typechecker chose to continue past an error. *)
let mk ctx pos answer eff shape : typed_ce =
  CoreExpr.mk (object
    method loc = pos
    method ctx = ctx
    method answer = answer
    method eff = eff
  end) shape

let mk_bind_info x sort eff ctx : typed_info =
  object
    method loc = Var.binding_site x
    method ctx = ctx
    method answer = Ok sort
    method eff = eff
  end

let lift_sort (s : Sort.sort) : typed_info Sort.t =
  Sort.map (fun loc_info ->
    (object
      method loc = loc_info#loc
      method ctx = Context.empty
      method answer = Ok s
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

(** [check_pred] and [( &&& )]: linearization helpers — same shape as
    [Typecheck]'s.  Turn a boolean predicate into a result-typed gate
    that threads through [SortView.Build]-style answer construction
    without branching. *)
let check_pred (b : bool) (err : Error.t) : (unit, Error.t) result =
  if b then Ok () else Error err

let ( &&& ) (gate : (unit, Error.t) result) (x : ('a, Error.t) result)
    : ('a, Error.t) result =
  match gate with Ok () -> x | Error e -> Error e

(** [unsynth ~construct r] — same as [Typecheck.unsynth].  Converts a
    typed-info answer ([(Sort.sort, Error.t) result]) into the
    [Error.kind] expected by [check]'s expected-sort argument when the
    typechecker can't synthesize a prior subterm.  Reserved for the
    upcoming clause-by-clause migration of synth's user-error
    [ElabM.fail] sites. *)
let[@warning "-32"] unsynth ~construct r =
  Result.map_error (fun _ -> Error.K_cannot_synthesize { construct }) r

(** [replace_answer ce a] re-wraps [ce]'s outer info with answer [a],
    preserving every other field and the inner shape.  Used by synth
    to attach a synth-side cannot_synthesize answer over a [check]-
    elaborated subterm. *)
let replace_answer (ce : typed_ce) answer : typed_ce =
  let info = CoreExpr.info ce in
  let new_info : typed_info =
    object
      method loc = info#loc
      method ctx = info#ctx
      method answer = answer
      method eff = info#eff
    end
  in
  CoreExpr.mk new_info (CoreExpr.shape ce)

(** Pattern-driven check clauses that delegate to [coverage_check]
    require a concrete [Sort.sort], not a result.  When the expected
    sort is [Error], we fail through the monad — pattern compilation
    can't proceed without a target sort, and continuing would just
    cascade nonsense.  Future work (B.3 / coverage_check Hole
    emission) will let pattern compilation continue with [Hole]
    placeholders even when the target sort is unknown. *)
let unwrap_sort_for_coverage pos sort =
  match sort with
  | Ok s -> ElabM.return s
  | Error k -> ElabM.fail (Error.structured ~loc:pos k)

(** [unwrap_synth_sort ce]: extract the synth-output sort of [ce] for
    use as the bound-var sort in pattern-driven clauses (Take, Let,
    Case, Iter).  When [ce]'s [info#answer] is [Error e] (meaning the
    RHS errored under multi-error elaboration), fail through the
    monad with [e] — coverage compilation needs a real sort to
    proceed.  Phase B.3 will let coverage_check fall back to a
    [Hole] placeholder so this can continue past errors too. *)
let unwrap_synth_sort (ce : typed_ce) : Sort.sort ElabM.t =
  match (CoreExpr.info ce)#answer with
  | Ok s -> ElabM.return s
  | Error e -> ElabM.fail e

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
    let answer =
      Result.bind
        (Context.lookup x ctx
         |> Result.map_error (Error.structured ~loc:pos))
        (fun (s, var_eff) ->
          check_pred (Effect.sub var_eff eff0)
            (Error.var_effect_mismatch
               ~loc:pos ~var:x ~declared:var_eff ~required:eff0)
          &&& Ok s)
    in
    ElabM.return (mk ctx pos answer eff0 (CoreExpr.Var x))

  | SurfExpr.IntLit n ->
    let s = mk_sort pos Sort.Int in
    ElabM.return (mk_typed ctx pos s eff0 (CoreExpr.IntLit n))

  | SurfExpr.BoolLit b ->
    let s = mk_sort pos Sort.Bool in
    ElabM.return (mk_typed ctx pos s eff0 (CoreExpr.BoolLit b))

  | SurfExpr.Eq (se1, se2) ->
    let eff0' = Effect.purify eff0 in
    let* ce1 = synth sig_ ctx eff0' se1 in
    let ce1_answer = (CoreExpr.info ce1)#answer in
    let ce2_expected = unsynth ~construct:"equality" ce1_answer in
    let* ce2 = check sig_ ctx se2 ce2_expected eff0' in
    let answer =
      Result.bind ce1_answer (fun s ->
        check_pred (Sort.is_spec_type s)
          (Error.not_spec_type ~loc:pos ~construct:"equality" ~got:s)
        &&& Ok (mk_sort pos Sort.Bool))
    in
    ElabM.return (mk ctx pos answer eff0 (CoreExpr.Eq (ce1, ce2)))

  | SurfExpr.And (se1, se2) ->
    let bool_sort = mk_sort pos Sort.Bool in
    let* ce1 = check sig_ ctx se1 (Ok bool_sort) eff0 in
    let* ce2 = check sig_ ctx se2 (Ok bool_sort) eff0 in
    ElabM.return (mk_typed ctx pos bool_sort eff0 (CoreExpr.And (ce1, ce2)))

  | SurfExpr.Not se ->
    let bool_sort = mk_sort pos Sort.Bool in
    let* ce = check sig_ ctx se (Ok bool_sort) eff0 in
    ElabM.return (mk_typed ctx pos bool_sort eff0 (CoreExpr.Not ce))

  | SurfExpr.App (p, arg) ->
    let eq_check = match p with
      | Prim.Eq a ->
        check_pred (Sort.is_eqtype a)
          (Error.eq_not_equality_type ~loc:pos ~got:a)
      | _ -> Ok ()
    in
    let (arg_sort, ret_sort, prim_eff) = prim_signature p in
    let eff_check = check_pred (Effect.sub prim_eff eff0)
      (Error.prim_effect_mismatch
         ~loc:pos ~prim:p ~declared:prim_eff ~required:eff0) in
    let eff0' = Effect.purify eff0 in
    let* ce_arg = check sig_ ctx arg (Ok arg_sort) eff0' in
    let answer = eq_check &&& eff_check &&& Ok ret_sort in
    ElabM.return (mk ctx pos answer eff0 (CoreExpr.App (p, ce_arg)))

  | SurfExpr.Call (name, arg) ->
    let lookup_result =
      Sig.lookup_fun name sig_
      |> Result.map_error (Error.structured ~loc:pos) in
    let arg_expected =
      Result.map (fun (a, _, _) -> a) lookup_result
      |> unsynth ~construct:"function call" in
    let eff0' = Effect.purify eff0 in
    let* ce_arg = check sig_ ctx arg arg_expected eff0' in
    let answer =
      Result.bind lookup_result (fun (_, ret_sort, fun_eff) ->
        check_pred (Effect.sub fun_eff eff0)
          (Error.fun_effect_mismatch
             ~loc:pos ~name ~declared:fun_eff ~required:eff0)
        &&& Ok ret_sort)
    in
    ElabM.return (mk ctx pos answer eff0 (CoreExpr.Call (name, ce_arg)))

  | SurfExpr.Annot (se, s) ->
    let* ce = check sig_ ctx se (Ok s) eff0 in
    ElabM.return (mk_typed ctx pos s eff0 (CoreExpr.Annot (ce, lift_sort s)))

  | SurfExpr.Return _ | SurfExpr.Take _ | SurfExpr.Fail | SurfExpr.Hole _
  | SurfExpr.Let _ | SurfExpr.Tuple _ | SurfExpr.Inject _ | SurfExpr.Case _
  | SurfExpr.Iter _ | SurfExpr.If _ ->
    let unsynth_kind = Error.K_cannot_synthesize { construct = "sort" } in
    let* inner = check sig_ ctx se (Error unsynth_kind) eff0 in
    let answer = Error (Error.cannot_synthesize ~loc:pos ~construct:"sort") in
    ElabM.return (replace_answer inner answer)

(** Check: S ; G |- [eff0] se <== sort

    The expected-sort argument is itself a result.  Pattern-driven
    clauses (Take / Let / Case / Iter) currently bail when the
    expected sort is [Error _], because [coverage_check] needs a
    concrete [Sort.sort].  Phase B.3 will lift this restriction by
    teaching coverage to emit Holes when the target sort is unknown.
    Other clauses thread the result through [SortView] and continue
    elaborating their subterms regardless. *)
and check sig_ ctx se sort eff0 =
  let pos = (SurfExpr.info se)#loc in
  match SurfExpr.shape se with
  | SurfExpr.Return inner ->
    let eff_check = check_pred (Effect.sub Effect.Spec eff0)
      (Error.spec_context_required ~loc:pos ~construct:"return") in
    let inner_expected = SortView.Get.pred ~construct:"return" sort in
    let* ce = check sig_ ctx inner inner_expected eff0 in
    let answer = eff_check &&& Error.at ~loc:pos sort in
    ElabM.return (mk ctx pos answer eff0 (CoreExpr.Return ce))

  | SurfExpr.Fail ->
    let eff_check = check_pred (Effect.sub Effect.Spec eff0)
      (Error.spec_context_required ~loc:pos ~construct:"fail") in
    let _ = SortView.Get.pred ~construct:"fail" sort in
    let answer = eff_check &&& Error.at ~loc:pos sort in
    ElabM.return (mk ctx pos answer eff0 CoreExpr.Fail)

  | SurfExpr.Take (pat, se1, se2) ->
    let* sort_concrete = unwrap_sort_for_coverage pos sort in
    if not (Effect.sub Effect.Spec eff0) then
      ElabM.fail (Error.spec_context_required ~loc:pos ~construct:"take")
    else
      let* _ = ElabM.lift_at pos (SortGet.get_pred ~construct:"take target" sort_concrete) in
      let* ce1 = synth sig_ ctx eff0 se1 in
      let* s1 = unwrap_synth_sort ce1 in
      let* tau = ElabM.lift_at pos (SortGet.get_pred ~construct:"take scrutinee" s1) in
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
        coverage_check sig_ ctx_y [y] [branch] eff_b sort_concrete eff0
          ~cov_loc:pos rebuilder_init in
      let yb = (y, mk_bind_info y tau eff_b ctx_y) in
      ElabM.return (mk_typed ctx pos sort_concrete eff0 (CoreExpr.Take (yb, ce1, ce2)))

  | SurfExpr.Let (pat, se1, se2) ->
    let* sort_concrete = unwrap_sort_for_coverage pos sort in
    let* ce1 = synth sig_ ctx eff0 se1 in
    let* tau = unwrap_synth_sort ce1 in
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
      coverage_check sig_ ctx_y [y] [branch] eff_b sort_concrete eff0
        ~cov_loc:pos rebuilder_init in
    let yb = (y, mk_bind_info y tau eff_b ctx_y) in
    ElabM.return (mk_typed ctx pos sort_concrete eff0 (CoreExpr.Let (yb, ce1, ce2)))

  | SurfExpr.Tuple ses ->
    let n = List.length ses in
    let ts = SortView.Get.record ~construct:"tuple" n sort in
    let* ces =
      ElabM.sequence
        (List.map (fun (e, s_result) -> check sig_ ctx e s_result eff0)
           (List.combine ses ts))
    in
    let answer = Error.at ~loc:pos sort in
    ElabM.return (mk ctx pos answer eff0 (CoreExpr.Tuple ces))

  | SurfExpr.Inject (l, inner) ->
    let construct = Format.asprintf "constructor %a" Label.print l in
    let (d_result, args_results) =
      SortView.Get.app ~construct sort in
    let ctor_kind =
      Result.bind d_result (fun d ->
        Result.bind (Util.result_list args_results) (fun args ->
          CtorLookup.lookup sig_ d l args))
    in
    let eff0' = Effect.purify eff0 in
    let* ce = check sig_ ctx inner ctor_kind eff0' in
    let answer = Error.at ~loc:pos sort in
    ElabM.return (mk ctx pos answer eff0 (CoreExpr.Inject (l, ce)))

  | SurfExpr.Case (scrut, surf_branches) ->
    let* sort_concrete = unwrap_sort_for_coverage pos sort in
    let eff0' = Effect.purify eff0 in
    let* ce_scrut = synth sig_ ctx eff0' scrut in
    let* scrut_sort = unwrap_synth_sort ce_scrut in
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
      coverage_check sig_ ctx_y [y] branches eff0' sort_concrete eff0
        ~cov_loc:pos rebuilder_init in
    let yb = (y, mk_bind_info y scrut_sort eff0' ctx_y) in
    ElabM.return (mk_typed ctx pos sort_concrete eff0 (CoreExpr.Let (yb, ce_scrut, ce_body)))

  | SurfExpr.Iter (pat, se1, se2) ->
    let* sort_concrete = unwrap_sort_for_coverage pos sort in
    if not (Effect.sub Effect.Impure eff0) then
      ElabM.fail (Error.iter_requires_impure ~loc:pos ~actual:eff0)
    else
    let* ce1 = synth sig_ ctx Effect.Pure se1 in
    let* init_sort = unwrap_synth_sort ce1 in
    let* step_dsort = match Dsort.of_string "Step" with
      | Ok d -> ElabM.return d
      | Error _ ->
        invariant_at pos ~rule:"iter"
          "Dsort.of_string \"Step\" failed — \"Step\" must always parse as a \
           well-formed sort name"
    in
    let iter_sort = mk_sort pos (Sort.App (step_dsort, [init_sort; sort_concrete])) in
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
    ElabM.return (mk_typed ctx pos sort_concrete eff0 (CoreExpr.Iter (y, ce1, ce_body)))

  | SurfExpr.If (se1, se2, se3) ->
    let bool_sort = mk_sort pos Sort.Bool in
    let eff0' = Effect.purify eff0 in
    let* ce1 = check sig_ ctx se1 (Ok bool_sort) eff0' in
    let* ce2 = check sig_ ctx se2 sort eff0 in
    let* ce3 = check sig_ ctx se3 sort eff0 in
    let answer = Error.at ~loc:pos sort in
    ElabM.return (mk ctx pos answer eff0 (CoreExpr.If (ce1, ce2, ce3)))

  | SurfExpr.Hole h ->
    let answer = Error.at ~loc:pos sort in
    ElabM.return (mk ctx pos answer eff0 (CoreExpr.Hole h))

  | _ ->
    let* ce = synth sig_ ctx eff0 se in
    let syn_answer = (CoreExpr.info ce)#answer in
    let answer =
      Result.bind (Error.at ~loc:pos sort) (fun expected ->
      Result.bind syn_answer (fun syn_sort ->
        check_pred (sort_equal syn_sort expected)
          (Error.sort_mismatch ~loc:pos ~expected ~actual:syn_sort)
        &&& Ok syn_sort))
    in
    ElabM.return (replace_answer ce answer)

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
       let* ce' = check sig_ ctx' br.body (Ok sort) eff0 in
       ElabM.return (wrap_lets br.let_bindings ctx sort eff0 ce')
     | (p, _) :: _ ->
       invariant_at (Pat.info p)#loc ~rule:"coverage_check:Cov_done"
         "branch still has un-consumed pattern bindings after all \
          scrutinees have been dispatched")

  | [], [] ->
    (* Non-exhaustive: synthesize a [Hole] node with the
       non-exhaustive error attached to its [info#answer].  Coverage
       compilation continues — its caller still gets a well-formed
       typed_ce, and [collect_errors] surfaces the diagnostic for
       LSP / drivers.  The witness preserves the missing-case shape
       so the error message stays informative. *)
    let witness = rebuilder [] in
    let err = Error.non_exhaustive ~loc:cov_loc ~witness in
    let info : typed_info =
      object
        method loc = cov_loc
        method ctx = ctx
        method answer = Error err
        method eff = eff0
      end
    in
    ElabM.return
      (CoreExpr.mk info (CoreExpr.Hole "non-exhaustive-coverage"))

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
       let* decl =
         ElabM.lift_at (Var.binding_site y)
           (Sig.lookup_dsort_or_type dsort_name sig_) in
       (match decl with
        | Sig.LSortDecl decl ->
          let labels = DsortDecl.ctor_labels decl in
          let* case_branches =
            build_sort_con_branches sig_ ctx y scrs branches eff_b sort eff0
              labels args decl ~cov_loc rebuilder in
          let y_ce = mk_typed ctx (Var.binding_site y) lead_sort eff_b (CoreExpr.Var y) in
          ElabM.return (mk_typed ctx (Var.binding_site y) sort eff0
            (CoreExpr.Case (y_ce, case_branches)))
        | Sig.LTypeDecl decl ->
          let labels = DtypeDecl.ctor_labels decl in
          let* case_branches =
            build_type_con_branches sig_ ctx y scrs branches eff_b sort eff0
              labels args decl ~cov_loc rebuilder in
          let y_ce = mk_typed ctx (Var.binding_site y) lead_sort eff_b (CoreExpr.Var y) in
          ElabM.return (mk_typed ctx (Var.binding_site y) sort eff0
            (CoreExpr.Case (y_ce, case_branches))))
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
