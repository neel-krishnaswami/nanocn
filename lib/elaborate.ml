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
   elaboration pass was meant to rule out).  Raises
   [Util.Invariant_failure] — caught by the top-level driver and
   reported as a compiler bug, not as a user-facing diagnostic. *)
let invariant_at pos ~rule msg =
  Util.raise_invariant ~loc:pos ~rule msg

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

(** {1 Coverage types}

    Pattern-matrix bindings carry result-typed sorts so that an
    upstream [Error _] (scrutinee sort missing, undeclared dsort,
    etc.) propagates uniformly through the matrix instead of forcing
    elaboration to halt. *)

type binding = Pat.pat * (Sort.sort, Error.kind) result

type let_binding = {
  var : Var.t;
  rhs : Var.t;
  sort : (Sort.sort, Error.kind) result;
  eff : Effect.t;
  loc : SourcePos.t;
}

type branch = {
  bindings : binding list;
  let_bindings : let_binding list;
  body : SurfExpr.se;
}

(** {1 Coverage helpers} *)

let mk_bind_info_r x sort_kind eff ctx : typed_info =
  let answer = Error.at ~loc:(Var.binding_site x) sort_kind in
  object
    method loc = Var.binding_site x
    method ctx = ctx
    method answer = answer
    method eff = eff
  end

let rec wrap_lets lets outer_ctx result_sort eff0 body =
  match lets with
  | [] -> body
  | lb :: rest ->
    let inner_ctx =
      Context.extend_or_unknown lb.var lb.sort lb.eff outer_ctx in
    let inner = wrap_lets rest inner_ctx result_sort eff0 body in
    let rhs_answer = Error.at ~loc:lb.loc lb.sort in
    let rhs = mk outer_ctx lb.loc rhs_answer eff0 (CoreExpr.Var lb.rhs) in
    let bind_info = mk_bind_info_r lb.var lb.sort lb.eff inner_ctx in
    let outer_answer = Error.at ~loc:lb.loc result_sort in
    mk outer_ctx lb.loc outer_answer eff0
      (CoreExpr.Let ((lb.var, bind_info), rhs, inner))

(** Discriminator for a pattern-matrix column's leading patterns.

    Variable patterns are universally compatible; the discriminator
    is determined by the non-var leading patterns.  When the column
    has both tuples (of one fixed arity) and ctors, or tuples of
    differing arities, the column is incompatible — the typechecker
    has no syntactic basis to dispatch and must abandon the column
    with a [K_incompatible_patterns] diagnostic. *)
type column_kind =
  | Col_all_vars
  | Col_tuple of int
  | Col_ctor
  | Col_incompatible
      of (Error.pattern_shape_descriptor * SourcePos.t) list

let shape_eq (a : Error.pattern_shape_descriptor)
             (b : Error.pattern_shape_descriptor) =
  match a, b with
  | Error.PS_Tuple n, Error.PS_Tuple m -> n = m
  | Error.PS_Ctor l, Error.PS_Ctor l' -> Label.compare l l' = 0
  | Error.PS_Var, Error.PS_Var -> true
  | _ -> false

let dedupe_shapes shapes =
  let seen = ref [] in
  List.filter (fun (s, _) ->
    if List.exists (shape_eq s) !seen then false
    else begin seen := s :: !seen; true end
  ) shapes

let rec take_n n xs =
  if n <= 0 then []
  else match xs with
    | [] -> []
    | x :: rest -> x :: take_n (n - 1) rest

(** [classify_column branches] inspects every branch's leading
    pattern.  Var patterns are skipped (universally compatible).
    Returns [Col_all_vars] when no non-var pattern is present;
    [Col_tuple n] when every non-var pattern is a tuple of arity [n];
    [Col_ctor] when every non-var pattern is a constructor (any
    label); [Col_incompatible] otherwise, carrying up to three
    distinct conflicting shapes for the diagnostic. *)
let classify_column branches : column_kind =
  let descrs =
    List.filter_map (fun br ->
      match br.bindings with
      | (p, _) :: _ ->
        (match Pat.shape p with
         | Pat.Var _ -> None
         | Pat.Tuple ps ->
           Some (Error.PS_Tuple (List.length ps), (Pat.info p)#loc)
         | Pat.Con (l, _) ->
           Some (Error.PS_Ctor l, (Pat.info p)#loc))
      | [] -> None
    ) branches
  in
  match descrs with
  | [] -> Col_all_vars
  | (Error.PS_Tuple n, _) :: _ ->
    let all_same_tuple =
      List.for_all
        (function (Error.PS_Tuple m, _) -> n = m | _ -> false)
        descrs
    in
    if all_same_tuple then Col_tuple n
    else Col_incompatible (take_n 3 (dedupe_shapes descrs))
  | (Error.PS_Ctor _, _) :: _ ->
    let all_ctors =
      List.for_all
        (function (Error.PS_Ctor _, _) -> true | _ -> false)
        descrs
    in
    if all_ctors then Col_ctor
    else Col_incompatible (take_n 3 (dedupe_shapes descrs))
  | (Error.PS_Var, loc) :: _ ->
    (* PS_Var is filtered out by the [filter_map] above; this arm is
       genuinely unreachable. *)
    Util.raise_invariant ~loc ~rule:"Elaborate.classify_column"
      "PS_Var leaked through filter_map"

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

(** spec_con: filter branches for a given constructor label.
    [ctor_sort] is the inner pattern's expected sort (a result so
    upstream errors propagate to the bound subpattern). *)
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

(** expand_tup: expand tuple patterns in the leading position.
    [sorts] are the result-typed sub-sorts produced by
    [SortView.Get.record n] on the column sort. *)
let rec expand_tup sorts y eff_b branches =
  match branches with
  | [] -> ElabM.return []
  | br :: rest ->
    match br.bindings with
    | (p, sort) :: binds ->
      (match Pat.shape p with
       | Pat.Tuple pats ->
         if List.compare_lengths pats sorts <> 0 then
           (* Tuple-arity mismatch: classify_column should already have
              routed this case to Col_incompatible.  Skip the row. *)
           let* rest' = expand_tup sorts y eff_b rest in
           ElabM.return rest'
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
    let eff_check = check_pred (Effect.sub Effect.Spec eff0)
      (Error.spec_context_required ~loc:pos ~construct:"take") in
    let _target_check = SortView.Get.pred ~construct:"take target" sort in
    let* ce1 = synth sig_ ctx eff0 se1 in
    let ce1_answer = (CoreExpr.info ce1)#answer in
    let bound_kind =
      ce1_answer
      |> unsynth ~construct:"take scrutinee"
      |> SortView.Get.pred ~construct:"take scrutinee"
    in
    let eff_b = Effect.purify eff0 in
    let* y = ElabM.fresh (Pat.info pat)#loc in
    let ctx_y = Context.extend_or_unknown y bound_kind eff_b ctx in
    let branch = {
      bindings = [(pat, bound_kind)];
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
    let yb = (y, mk_bind_info_r y bound_kind eff_b ctx_y) in
    let outer_answer = eff_check &&& Error.at ~loc:pos sort in
    ElabM.return (mk ctx pos outer_answer eff0 (CoreExpr.Take (yb, ce1, ce2)))

  | SurfExpr.Let (pat, se1, se2) ->
    let* ce1 = synth sig_ ctx eff0 se1 in
    let bound_kind =
      unsynth ~construct:"let binding" (CoreExpr.info ce1)#answer in
    let eff_b = Effect.purify eff0 in
    let* y = ElabM.fresh (Pat.info pat)#loc in
    let ctx_y = Context.extend_or_unknown y bound_kind eff_b ctx in
    let branch = {
      bindings = [(pat, bound_kind)];
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
    let yb = (y, mk_bind_info_r y bound_kind eff_b ctx_y) in
    let outer_answer = Error.at ~loc:pos sort in
    ElabM.return (mk ctx pos outer_answer eff0 (CoreExpr.Let (yb, ce1, ce2)))

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
    let eff0' = Effect.purify eff0 in
    let* ce_scrut = synth sig_ ctx eff0' scrut in
    let scrut_kind =
      unsynth ~construct:"case scrutinee" (CoreExpr.info ce_scrut)#answer in
    let* y = ElabM.fresh (SurfExpr.info scrut)#loc in
    let ctx_y = Context.extend_or_unknown y scrut_kind eff0' ctx in
    let branches = List.map (fun (pat, body, _) ->
      { bindings = [(pat, scrut_kind)];
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
    let yb = (y, mk_bind_info_r y scrut_kind eff0' ctx_y) in
    let outer_answer = Error.at ~loc:pos sort in
    ElabM.return (mk ctx pos outer_answer eff0 (CoreExpr.Let (yb, ce_scrut, ce_body)))

  | SurfExpr.Iter (pat, se1, se2) ->
    let eff_check = check_pred (Effect.sub Effect.Impure eff0)
      (Error.iter_requires_impure ~loc:pos ~actual:eff0) in
    let* ce1 = synth sig_ ctx Effect.Pure se1 in
    let init_kind =
      unsynth ~construct:"iter step source" (CoreExpr.info ce1)#answer in
    let* step_dsort = match Dsort.of_string "Step" with
      | Ok d -> ElabM.return d
      | Error _ ->
        invariant_at pos ~rule:"iter"
          "Dsort.of_string \"Step\" failed — \"Step\" must always parse as a \
           well-formed sort name"
    in
    let iter_sort_kind : (Sort.sort, Error.kind) result =
      let ( let* ) = Result.bind in
      let* a = init_kind in
      let* result = sort in
      Ok (mk_sort pos (Sort.App (step_dsort, [a; result])))
    in
    let* y = ElabM.fresh (Pat.info pat)#loc in
    let bind_eff = Effect.purify Effect.Impure in
    let ctx_y = Context.extend_or_unknown y init_kind bind_eff ctx in
    let branch = {
      bindings = [(pat, init_kind)];
      let_bindings = [];
      body = se2;
    } in
    let rebuilder_init = function
      | [w] -> w
      | _ -> PatWitness.Wild
    in
    let* ce_body =
      coverage_check sig_ ctx_y [y] [branch] bind_eff iter_sort_kind Effect.Impure
        ~cov_loc:pos rebuilder_init in
    let outer_answer = eff_check &&& Error.at ~loc:pos sort in
    ElabM.return (mk ctx pos outer_answer eff0 (CoreExpr.Iter (y, ce1, ce_body)))

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
  (* Cov_done: no scrutinees, at least one branch.  The first branch
     wins (its body is what we elaborate). *)
  | [], br :: _ ->
    (match br.bindings with
     | [] ->
       let ctx' =
         List.fold_left (fun c lb ->
           Context.extend_or_unknown lb.var lb.sort lb.eff c)
           ctx br.let_bindings in
       let* ce' = check sig_ ctx' br.body sort eff0 in
       ElabM.return (wrap_lets br.let_bindings ctx sort eff0 ce')
     | (p, _) :: _ ->
       invariant_at (Pat.info p)#loc ~rule:"coverage_check:Cov_done"
         "branch still has un-consumed pattern bindings after all \
          scrutinees have been dispatched")

  | [], [] ->
    (* Non-exhaustive at this leaf — emit a [Hole] with the
       non-exhaustive diagnostic on its [answer]. *)
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

  | y :: scrs, _ ->
    (match classify_column branches with
     | Col_all_vars ->
       let branches' = strip_var y eff_b branches in
       let rebuilder' rest = rebuilder (PatWitness.Wild :: rest) in
       coverage_check sig_ ctx scrs branches' eff_b sort eff0
         ~cov_loc rebuilder'

     | Col_tuple n ->
       let column_sort = column_sort_of branches in
       let sub_sorts =
         SortView.Get.record ~construct:"tuple pattern" n column_sort in
       let positions =
         find_tup_subpat_positions branches n (Var.binding_site y) in
       let* fresh_zs = fresh_vars_with_positions sub_sorts positions in
       let ctx' =
         List.fold_left (fun c (z, s) ->
           Context.extend_or_unknown z s eff_b c) ctx fresh_zs in
       let fresh_vars = List.map fst fresh_zs in
       let* branches' = expand_tup sub_sorts y eff_b branches in
       let rebuilder' rest =
         let (firsts, tail) = split_at_n n rest in
         rebuilder (PatWitness.Tuple firsts :: tail)
       in
       let* ce =
         coverage_check sig_ ctx' (fresh_vars @ scrs) branches'
           eff_b sort eff0 ~cov_loc rebuilder' in
       let y_answer = Error.at ~loc:(Var.binding_site y) column_sort in
       let y_ce =
         mk ctx (Var.binding_site y) y_answer eff_b (CoreExpr.Var y) in
       let annotated_vars = List.map (fun (z, s) ->
         (z, mk_bind_info_r z s eff_b ctx')
       ) fresh_zs in
       let outer_answer = Error.at ~loc:(Var.binding_site y) sort in
       ElabM.return (mk ctx (Var.binding_site y) outer_answer eff0
         (CoreExpr.LetTuple (annotated_vars, y_ce, ce)))

     | Col_ctor ->
       let column_sort = column_sort_of branches in
       let observed = collect_observed_ctors branches in
       let outcomes =
         CtorLookup.lookup_all_observed sig_ column_sort observed in
       let* case_branches =
         build_observed_branches sig_ ctx y scrs branches eff_b sort eff0
           outcomes column_sort ~cov_loc rebuilder in
       let y_answer = Error.at ~loc:(Var.binding_site y) column_sort in
       let y_ce =
         mk ctx (Var.binding_site y) y_answer eff_b (CoreExpr.Var y) in
       let outer_answer = Error.at ~loc:(Var.binding_site y) sort in
       ElabM.return (mk ctx (Var.binding_site y) outer_answer eff0
         (CoreExpr.Case (y_ce, case_branches)))

     | Col_incompatible shapes ->
       let err = Error.incompatible_patterns ~loc:cov_loc ~shapes in
       let info : typed_info = object
         method loc = cov_loc
         method ctx = ctx
         method answer = Error err
         method eff = eff0
       end in
       ElabM.return
         (CoreExpr.mk info (CoreExpr.Hole "incompatible-patterns")))

and column_sort_of branches : (Sort.sort, Error.kind) result =
  let rec go = function
    | [] ->
      Error
        (Error.K_internal_invariant
           { rule = "column_sort_of"; invariant = "empty matrix" })
    | br :: rest ->
      match br.bindings with
      | (_, s) :: _ -> s
      | [] -> go rest
  in
  go branches

and collect_observed_ctors branches =
  List.filter_map (fun br ->
    match br.bindings with
    | (p, _) :: _ ->
      (match Pat.shape p with
       | Pat.Con (l, _) -> Some l
       | _ -> None)
    | _ -> None
  ) branches

and build_observed_branches sig_ ctx y scrs branches eff_b sort eff0
    outcomes column_sort ~cov_loc rebuilder =
  let observed_set = Hashtbl.create 8 in
  List.iter (fun l -> Hashtbl.replace observed_set l ())
    (collect_observed_ctors branches);
  let dsort_for_missing =
    match column_sort with
    | Error _ -> None
    | Ok _ ->
      let (d_result, _) =
        SortView.Get.app ~construct:"case scrutinee" column_sort in
      Result.to_option d_result
  in
  let process_one (label, ctor_sort_result) =
    let xi_pos = find_con_subpat_pos label branches (Var.binding_site y) in
    let* xi = ElabM.fresh xi_pos in
    let ctx_xi =
      Context.extend_or_unknown xi ctor_sort_result eff_b ctx in
    let rebuilder_L rest =
      match rest with
      | head :: tail -> rebuilder (PatWitness.Ctor (label, head) :: tail)
      | [] -> rebuilder (PatWitness.Ctor (label, PatWitness.Wild) :: [])
    in
    if Hashtbl.mem observed_set label then begin
      let* filtered = spec_con label ctor_sort_result y eff_b branches in
      let* ce_i =
        coverage_check sig_ ctx_xi (xi :: scrs) filtered eff_b sort eff0
          ~cov_loc rebuilder_L in
      let branch_answer = Error.at ~loc:cov_loc ctor_sort_result in
      let branch_info : typed_info = object
        method loc = xi_pos
        method ctx = ctx_xi
        method answer = branch_answer
        method eff = eff_b
      end in
      ElabM.return (label, xi, ce_i, branch_info)
    end else begin
      let label_str = Format.asprintf "%a" Label.print label in
      let hole_answer = Error.at ~loc:cov_loc ctor_sort_result in
      let hole_info : typed_info = object
        method loc = cov_loc
        method ctx = ctx_xi
        method answer = hole_answer
        method eff = eff0
      end in
      let body =
        CoreExpr.mk hole_info
          (CoreExpr.Hole ("missing-case-" ^ label_str)) in
      let missing_err =
        match dsort_for_missing with
        | Some d ->
          Error (Error.missing_ctor ~loc:cov_loc ~label ~decl:d)
        | None ->
          (* Unreachable: missing entries are emitted only when the
             wrapper resolved the dsort. *)
          Error (Error.internal_invariant ~loc:cov_loc
                   ~rule:"build_observed_branches"
                   ~invariant:"missing branch but no dsort")
      in
      let branch_info : typed_info = object
        method loc = cov_loc
        method ctx = ctx_xi
        method answer = missing_err
        method eff = eff_b
      end in
      ElabM.return (label, xi, body, branch_info)
    end
  in
  ElabM.sequence (List.map process_one outcomes)
