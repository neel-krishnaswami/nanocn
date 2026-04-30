(* Error helpers.

   [invariant_at pos ~rule msg]: "impossible-in-well-formed-code"
   check fired (the typechecker's own consistency guarantees should
   have ruled this out). Emits [K_internal_invariant]. *)
let invariant_at pos ~rule msg =
  Error (Error.internal_invariant ~loc:pos ~rule ~invariant:msg)

let ( let* ) = Result.bind

(** [check_pred b err] turns a boolean test into a result-typed gate.
    Used with [( &&& )] to linearize predicate checks (effect
    subsumption, spec-context, arity) into the answer-builder
    pipeline so clauses don't branch on the test. *)
let[@warning "-32"] check_pred (b : bool) (err : Error.t)
    : (unit, Error.t) result =
  if b then Ok () else Error err

(** [gate &&& x] threads an [Error] gate through a result-producing
    expression: when [gate] is [Ok ()], pass [x] through unchanged;
    when [gate] is [Error e], replace the result with [Error e].
    Equivalent to [Result.bind gate (fun () -> x)] but reads more
    naturally at builder sites where [x] is itself a [SortView.Build]
    expression. *)
let[@warning "-32"] ( &&& )
    (gate : (unit, Error.t) result) (x : ('a, Error.t) result)
    : ('a, Error.t) result =
  match gate with Ok () -> x | Error e -> Error e

let sort_equal (a : Sort.sort) (b : Sort.sort) = Sort.compare a b = 0

let dummy_info = object method loc = SourcePos.dummy end

let mk_sort s = Sort.mk dummy_info s

type typed_info = < loc : SourcePos.t; ctx : Context.t; answer : (Sort.sort, Error.t) result; eff : Effect.t >
type typed_ce = typed_info CoreExpr.t

let mk ctx pos answer eff shape : typed_ce =
  CoreExpr.mk (object
    method loc = pos
    method ctx = ctx
    method answer = answer
    method eff = eff
  end) shape

let mk_bind_info x answer eff ctx : typed_info =
  object
    method loc = Var.binding_site x
    method ctx = ctx
    method answer = answer
    method eff = eff
  end

(** Lift a [Sort.sort] into [typed_info Sort.t] so it can be embedded
    in a typed core-expression shape.  The extra fields (ctx, answer, eff)
    on each sort node are fillers — no client inspects them. *)
let lift_sort (s : Sort.sort) : typed_info Sort.t =
  Sort.map (fun loc_info ->
    (object
      method loc = loc_info#loc
      method ctx = Context.empty
      method answer = Ok s
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

(** [unsynth ~construct r] converts an [info#answer] (a result over
    [Error.t]) into the [Error.kind] expected by [check]'s expected-sort
    argument when the typechecker couldn't synthesize the prior subterm.
    The original error stays attached to the prior node; this lets the
    inner [check] continue without inventing a fictitious sort. *)
let unsynth ~construct r =
  Result.map_error (fun _ -> Error.K_cannot_synthesize { construct }) r

(** Outcome of pairing each given case branch with the declared
    constructor list — used by [merge_branches] / [check_case_branches]
    to detect missing, redundant, and unknown ctors. *)
type merged_branch =
  | M_present of Label.t * Var.t * CoreExpr.ce * Sort.sort
  | M_missing of Label.t * Sort.sort
    (** A declared ctor that the user did not match.  The body is
        synthesized as [CoreExpr.Hole "missing-case-<label>"]. *)
  | M_redundant of Label.t * Var.t * CoreExpr.ce * Sort.sort
    (** Second or later occurrence of [label].  The known ctor sort
        is preserved so the body still typechecks. *)
  | M_unknown_label of Label.t * Var.t * CoreExpr.ce
    (** Either the label isn't declared at this dsort, or the
        declared list itself errored — body still elaborates with
        an unknown bound-var sort. *)

(** [merge_branches given declared] pairs the user's case branches
    with the full declared ctor list.  Returns merged entries in
    declared order, with extra [M_unknown_label] entries appended for
    given branches whose label isn't declared.  The caller constructs
    per-branch [Error.t]s from the [M_*] tags using its location and
    dsort context. *)
let merge_branches branches declared =
  match declared with
  | Error _ ->
    List.map (fun (l, x, body, _) -> M_unknown_label (l, x, body)) branches
  | Ok lts ->
    let label_eq l1 l2 = Label.compare l1 l2 = 0 in
    let seen = Hashtbl.create 8 in
    let classified =
      List.map (fun (l, x, body, _) ->
        match List.find_opt (fun (l', _) -> label_eq l l') lts with
        | None -> M_unknown_label (l, x, body)
        | Some (_, ctor_sort) ->
          if Hashtbl.mem seen l then
            M_redundant (l, x, body, ctor_sort)
          else begin
            Hashtbl.add seen l ();
            M_present (l, x, body, ctor_sort)
          end
      ) branches
    in
    let missing =
      List.filter_map (fun (l, ctor_sort) ->
        if Hashtbl.mem seen l then None
        else Some (M_missing (l, ctor_sort))
      ) lts
    in
    classified @ missing

(** [replace_answer ce answer] returns a copy of [ce] whose top-level
    [answer] field has been replaced.  Used by synth's unsynthesizable
    fallback to re-wrap a check-elaborated subterm with a synth-side
    cannot_synthesize error while preserving the inner elaboration. *)
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

(** Synthesize: S; G |- [eff0] ce ==> tau

    Returns a typed core expression whose [info#answer] is [Ok sort]
    on success and [Error e] on failure.  Errors are recorded on the
    offending node so siblings can still be elaborated. *)
let rec synth sig_ ctx eff0 ce : typed_ce =
  let pos = (CoreExpr.info ce)#loc in
  match CoreExpr.shape ce with
  | CoreExpr.Var x ->
    let answer =
      let* (s, var_eff) =
        Context.lookup x ctx
        |> Result.map_error (Error.structured ~loc:pos)
      in
      check_pred (Effect.sub var_eff eff0)
        (Error.var_effect_mismatch ~loc:pos ~var:x
           ~declared:var_eff ~required:eff0)
      &&& Ok s
    in
    mk ctx pos answer eff0 (CoreExpr.Var x)

  | CoreExpr.IntLit n ->
    mk ctx pos (Ok (mk_sort Sort.Int)) eff0 (CoreExpr.IntLit n)

  | CoreExpr.BoolLit b ->
    mk ctx pos (Ok (mk_sort Sort.Bool)) eff0 (CoreExpr.BoolLit b)

  | CoreExpr.Eq (ce1, ce2) ->
    let eff0' = Effect.purify eff0 in
    let ce1' = synth sig_ ctx eff0' ce1 in
    let ce1_answer = (CoreExpr.info ce1')#answer in
    let ce2' =
      check sig_ ctx ce2 (unsynth ~construct:"equality" ce1_answer) eff0' in
    let answer =
      let* s1 = ce1_answer in
      check_pred (Sort.is_spec_type s1)
        (Error.not_spec_type ~loc:pos ~construct:"equality" ~got:s1)
      &&& Ok (mk_sort Sort.Bool)
    in
    mk ctx pos answer eff0 (CoreExpr.Eq (ce1', ce2'))

  | CoreExpr.And (ce1, ce2) ->
    let bool_sort = mk_sort Sort.Bool in
    let ce1' = check sig_ ctx ce1 (Ok bool_sort) eff0 in
    let ce2' = check sig_ ctx ce2 (Ok bool_sort) eff0 in
    mk ctx pos (Ok bool_sort) eff0 (CoreExpr.And (ce1', ce2'))

  | CoreExpr.Not ce_inner ->
    let bool_sort = mk_sort Sort.Bool in
    let ce' = check sig_ ctx ce_inner (Ok bool_sort) eff0 in
    mk ctx pos (Ok bool_sort) eff0 (CoreExpr.Not ce')

  | CoreExpr.App (p, arg) ->
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
    let arg' = check sig_ ctx arg (Ok arg_sort) eff0' in
    let answer = eq_check &&& eff_check &&& Ok ret_sort in
    mk ctx pos answer eff0 (CoreExpr.App (p, arg'))

  | CoreExpr.Call (name, arg) ->
    let lookup_result =
      Sig.lookup_fun name sig_
      |> Result.map_error (Error.structured ~loc:pos) in
    (* Pass the argument's expected sort to check.  When lookup
       fails, the construct wasn't synthesizable as a function call —
       represent that with K_cannot_synthesize so check still
       elaborates [arg] with no expected type. *)
    let arg_expected =
      Result.map (fun (a, _, _) -> a) lookup_result
      |> unsynth ~construct:"function call"
    in
    let eff0' = Effect.purify eff0 in
    let arg' = check sig_ ctx arg arg_expected eff0' in
    let answer =
      let* (_, ret_sort, fun_eff) = lookup_result in
      check_pred (Effect.sub fun_eff eff0)
        (Error.fun_effect_mismatch
           ~loc:pos ~name ~declared:fun_eff ~required:eff0)
      &&& Ok ret_sort
    in
    mk ctx pos answer eff0 (CoreExpr.Call (name, arg'))

  | CoreExpr.Annot (ce_inner, s) ->
    let ce' = check sig_ ctx ce_inner (Ok s) eff0 in
    mk ctx pos (Ok s) eff0 (CoreExpr.Annot (ce', lift_sort s))

  | CoreExpr.Return _ | CoreExpr.Take _ | CoreExpr.Fail | CoreExpr.Hole _
  | CoreExpr.Let _ | CoreExpr.Inject _ | CoreExpr.Case _ | CoreExpr.Tuple _
  | CoreExpr.LetTuple _ | CoreExpr.If _ | CoreExpr.Iter _ ->
    let unsynth_kind = Error.K_cannot_synthesize { construct = "sort" } in
    let inner = check sig_ ctx ce (Error unsynth_kind) eff0 in
    let answer = Error (Error.cannot_synthesize ~loc:pos ~construct:"sort") in
    replace_answer inner answer

(** Check: S; G |- [eff0] ce <== tau

    The expected-sort argument is itself a result so callers don't
    have to branch when the parent's synth produced no sort.  An
    [Error] expected sort still elaborates the term (recursion is
    unconditional) — the term's own [answer] inherits the failure
    reason from the View calls that consume [sort]. *)
and check sig_ ctx ce sort eff0 : typed_ce =
  let pos = (CoreExpr.info ce)#loc in
  match CoreExpr.shape ce with
  | CoreExpr.Return inner ->
    let eff_check = check_pred (Effect.sub Effect.Spec eff0)
      (Error.spec_context_required ~loc:pos ~construct:"return") in
    let inner_expected =
      SortView.Get.pred ~construct:"return" sort in
    let inner' = check sig_ ctx inner inner_expected eff0 in
    let answer =
      eff_check &&& Error.at ~loc:pos sort
    in
    mk ctx pos answer eff0 (CoreExpr.Return inner')

  | CoreExpr.Fail ->
    let eff_check = check_pred (Effect.sub Effect.Spec eff0)
      (Error.spec_context_required ~loc:pos ~construct:"fail") in
    let _ = SortView.Get.pred ~construct:"fail" sort in
    let answer = eff_check &&& Error.at ~loc:pos sort in
    mk ctx pos answer eff0 CoreExpr.Fail

  | CoreExpr.Take ((x, _), ce1, ce2) ->
    let eff_check = check_pred (Effect.sub Effect.Spec eff0)
      (Error.spec_context_required ~loc:pos ~construct:"take") in
    let _target_check = SortView.Get.pred ~construct:"take target" sort in
    let ce1' = synth sig_ ctx eff0 ce1 in
    let ce1_answer = (CoreExpr.info ce1')#answer in
    let bound_kind =
      ce1_answer
      |> unsynth ~construct:"take scrutinee"
      |> SortView.Get.pred ~construct:"take scrutinee"
    in
    let bind_eff = Effect.purify eff0 in
    let ctx' = Context.extend_or_unknown x bound_kind bind_eff ctx in
    let bound_answer = Error.at ~loc:pos bound_kind in
    let ce2' = check sig_ ctx' ce2 sort eff0 in
    let xb = (x, mk_bind_info x bound_answer bind_eff ctx') in
    let answer = eff_check &&& Error.at ~loc:pos sort in
    mk ctx pos answer eff0 (CoreExpr.Take (xb, ce1', ce2'))

  | CoreExpr.Let ((x, _), ce1, ce2) ->
    let ce1' = synth sig_ ctx eff0 ce1 in
    let ce1_answer = (CoreExpr.info ce1')#answer in
    let bind_eff = Effect.purify eff0 in
    let bound_kind = unsynth ~construct:"let binding" ce1_answer in
    let ctx' = Context.extend_or_unknown x bound_kind bind_eff ctx in
    let ce2' = check sig_ ctx' ce2 sort eff0 in
    let xb = (x, mk_bind_info x ce1_answer bind_eff ctx') in
    let answer = Error.at ~loc:pos sort in
    mk ctx pos answer eff0 (CoreExpr.Let (xb, ce1', ce2'))

  | CoreExpr.LetTuple (xs, ce1, ce2) ->
    let eff0' = Effect.purify eff0 in
    let ce1' = synth sig_ ctx eff0' ce1 in
    let ce1_answer = (CoreExpr.info ce1')#answer in
    let n = List.length xs in
    let scrut_record = unsynth ~construct:"let-tuple scrutinee" ce1_answer in
    let ts =
      SortView.Get.record ~construct:"let-tuple scrutinee" n scrut_record in
    let bind_eff = Effect.purify eff0 in
    let ctx' =
      List.fold_left
        (fun acc ((x, _), s_result) ->
           Context.extend_or_unknown x s_result bind_eff acc)
        ctx (List.combine xs ts) in
    let ce2' = check sig_ ctx' ce2 sort eff0 in
    let typed_xs = List.map (fun ((x, _), s_result) ->
      let s_answer = Error.at ~loc:pos s_result in
      (x, (object
        method loc = Var.binding_site x
        method ctx = ctx'
        method answer = s_answer
        method eff = bind_eff
      end : typed_info))
    ) (List.combine xs ts) in
    let answer = Error.at ~loc:pos sort in
    mk ctx pos answer eff0 (CoreExpr.LetTuple (typed_xs, ce1', ce2'))

  | CoreExpr.Tuple es ->
    let n = List.length es in
    let ts = SortView.Get.record ~construct:"tuple" n sort in
    let es' =
      List.map (fun (e, s_result) -> check sig_ ctx e s_result eff0)
        (List.combine es ts) in
    let answer = Error.at ~loc:pos sort in
    mk ctx pos answer eff0 (CoreExpr.Tuple es')

  | CoreExpr.Inject (l, e_inner) ->
    let (d_result, args_results) =
      SortView.Get.app ~construct:"injection" sort in
    let ctor_kind =
      let* d = d_result in
      let* args = Util.result_list args_results in
      CtorLookup.lookup sig_ d l args
    in
    let eff0' = Effect.purify eff0 in
    let e_inner' = check sig_ ctx e_inner ctor_kind eff0' in
    let answer = Error.at ~loc:pos sort in
    mk ctx pos answer eff0 (CoreExpr.Inject (l, e_inner'))

  | CoreExpr.Case (scrut, branches) ->
    let eff0' = Effect.purify eff0 in
    let scrut' = synth sig_ ctx eff0' scrut in
    let scrut_answer = (CoreExpr.info scrut')#answer in
    let bind_eff = eff0' in
    let branches' =
      check_case_branches sig_ ctx branches scrut_answer sort eff0 bind_eff pos in
    let answer = Error.at ~loc:pos sort in
    mk ctx pos answer eff0 (CoreExpr.Case (scrut', branches'))

  | CoreExpr.Iter (x, e1, body) ->
    let eff_check = check_pred (Effect.sub Effect.Impure eff0)
      (Error.iter_requires_impure ~loc:pos ~actual:eff0) in
    let e1' = synth sig_ ctx Effect.Pure e1 in
    let e1_answer = (CoreExpr.info e1')#answer in
    let bind_eff = Effect.purify Effect.Impure in
    let bound_kind = unsynth ~construct:"iter step source" e1_answer in
    let ctx' = Context.extend_or_unknown x bound_kind bind_eff ctx in
    let step_dsort =
      match Dsort.of_string "Step" with
      | Ok d -> d
      | Error _ -> failwith "impossible" in
    let iter_sort_kind =
      let* a = bound_kind in
      let* result = sort in
      Ok (mk_sort (Sort.App (step_dsort, [a; result]))) in
    let body' = check sig_ ctx' body iter_sort_kind Effect.Impure in
    let answer = eff_check &&& Error.at ~loc:pos sort in
    mk ctx pos answer eff0 (CoreExpr.Iter (x, e1', body'))

  | CoreExpr.If (cond, e_then, e_else) ->
    let bool_sort = mk_sort Sort.Bool in
    let eff0' = Effect.purify eff0 in
    let cond' = check sig_ ctx cond (Ok bool_sort) eff0' in
    let then' = check sig_ ctx e_then sort eff0 in
    let else' = check sig_ ctx e_else sort eff0 in
    let answer = Error.at ~loc:pos sort in
    mk ctx pos answer eff0 (CoreExpr.If (cond', then', else'))

  | CoreExpr.Annot (inner, ann_sort) ->
    let inner' = check sig_ ctx inner (Ok ann_sort) eff0 in
    let agree =
      let* expected = Error.at ~loc:pos sort in
      check_pred (sort_equal ann_sort expected)
        (Error.annotation_disagrees
           ~loc:pos ~inner:expected ~annot:ann_sort)
      &&& Ok ann_sort
    in
    mk ctx pos agree eff0 (CoreExpr.Annot (inner', lift_sort ann_sort))

  | CoreExpr.Hole h ->
    let answer = Error.at ~loc:pos sort in
    mk ctx pos answer eff0 (CoreExpr.Hole h)

  | _ ->
    let e' = synth sig_ ctx eff0 ce in
    let syn_answer = (CoreExpr.info e')#answer in
    let answer =
      let* expected = Error.at ~loc:pos sort in
      let* syn_sort = syn_answer in
      check_pred (sort_equal syn_sort expected)
        (Error.sort_mismatch
           ~loc:pos ~expected ~actual:syn_sort)
      &&& Ok syn_sort
    in
    replace_answer e' answer

and check_case_branches sig_ ctx branches scrut_answer result_sort eff0 bind_eff pos =
  let scrut_kind = unsynth ~construct:"case scrutinee" scrut_answer in
  let (d_result, args_results) =
    SortView.Get.app ~construct:"case scrutinee" scrut_kind in
  let declared : ((Label.t * Sort.sort) list, Error.kind) result =
    let* d = d_result in
    let* args = Util.result_list args_results in
    CtorLookup.lookup_all sig_ d args
  in
  let merged = merge_branches branches declared in
  List.map (fun mb ->
    match mb with
    | M_present (l, x, body, ctor_sort) ->
      let ctx' = Context.extend x ctor_sort bind_eff ctx in
      let body' = check sig_ ctx' body result_sort eff0 in
      let branch_info = (object
        method loc = pos
        method ctx = ctx'
        method answer = Ok ctor_sort
        method eff = bind_eff
      end : typed_info) in
      (l, x, body', branch_info)

    | M_redundant (l, x, body, ctor_sort) ->
      (* Body still typechecks against the known ctor sort so its
         own diagnostics aren't cascading; the redundancy itself
         is recorded on the branch's [answer]. *)
      let ctx' = Context.extend x ctor_sort bind_eff ctx in
      let body' = check sig_ ctx' body result_sort eff0 in
      let branch_info = (object
        method loc = pos
        method ctx = ctx'
        method answer =
          Error (Error.redundant_ctor ~loc:pos ~label:l)
        method eff = bind_eff
      end : typed_info) in
      (l, x, body', branch_info)

    | M_unknown_label (l, x, body) ->
      (* Label isn't in the declaration, or the declaration itself
         couldn't be looked up.  Bind [x] as Unknown so the body can
         still be elaborated, and attribute the error appropriately. *)
      let ctx' = Context.extend_unknown x ctx in
      let body_expected : (Sort.sort, Error.kind) result =
        Error (Error.K_cannot_synthesize { construct = "case branch payload" }) in
      let body' = check sig_ ctx' body body_expected eff0 in
      let ans : (Sort.sort, Error.t) result =
        match d_result with
        | Ok d -> Error (Error.ctor_not_in_decl ~loc:pos ~label:l ~decl:d)
        | Error k -> Error (Error.structured ~loc:pos k)
      in
      let branch_info = (object
        method loc = pos
        method ctx = ctx'
        method answer = ans
        method eff = bind_eff
      end : typed_info) in
      (l, x, body', branch_info)

    | M_missing (l, ctor_sort) ->
      (* Synthesize a branch with a Hole body and a fresh dummy var. *)
      let (x, _) = Var.mk "<missing>" pos Var.empty_supply in
      let ctx' = Context.extend x ctor_sort bind_eff ctx in
      let hole_info : typed_info = object
        method loc = pos
        method ctx = ctx'
        method answer = Ok ctor_sort
        method eff = eff0
      end in
      let label_str = Format.asprintf "%a" Label.print l in
      let body' =
        CoreExpr.mk hole_info
          (CoreExpr.Hole ("missing-case-" ^ label_str))
      in
      let ans : (Sort.sort, Error.t) result =
        match d_result with
        | Ok d -> Error (Error.missing_ctor ~loc:pos ~label:l ~decl:d)
        | Error k -> Error (Error.structured ~loc:pos k)
      in
      let branch_info = (object
        method loc = pos
        method ctx = ctx'
        method answer = ans
        method eff = bind_eff
      end : typed_info) in
      (l, x, body', branch_info)
  ) merged

(** Collect every [Error _] recorded on [info#answer] anywhere in the
    typed tree.  Used by drivers to check whether the multi-error
    typechecker produced a clean tree.  Pre-order traversal so the
    first error is the leftmost / shallowest. *)
let rec collect_errors (ce : typed_ce) : Error.t list =
  let info = CoreExpr.info ce in
  let here = match info#answer with Ok _ -> [] | Error e -> [e] in
  let children =
    match CoreExpr.shape ce with
    | CoreExpr.Var _ | CoreExpr.IntLit _ | CoreExpr.BoolLit _
    | CoreExpr.Fail | CoreExpr.Hole _ -> []
    | CoreExpr.Let (_, e1, e2)
    | CoreExpr.LetTuple (_, e1, e2)
    | CoreExpr.Take (_, e1, e2)
    | CoreExpr.Iter (_, e1, e2)
    | CoreExpr.Eq (e1, e2)
    | CoreExpr.And (e1, e2) ->
      collect_errors e1 @ collect_errors e2
    | CoreExpr.If (e1, e2, e3) ->
      collect_errors e1 @ collect_errors e2 @ collect_errors e3
    | CoreExpr.Tuple es ->
      List.concat_map collect_errors es
    | CoreExpr.Inject (_, e1)
    | CoreExpr.App (_, e1)
    | CoreExpr.Call (_, e1)
    | CoreExpr.Not e1
    | CoreExpr.Return e1 -> collect_errors e1
    | CoreExpr.Annot (e1, _) -> collect_errors e1
    | CoreExpr.Case (scrut, branches) ->
      collect_errors scrut
      @ List.concat_map
          (fun (_, _, body, branch_info) ->
             let here =
               match branch_info#answer with
               | Ok _ -> []
               | Error e -> [e]
             in
             here @ collect_errors body)
          branches
  in
  here @ children

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
    let* decl = Error.at ~loc:pos (Sig.lookup_dsort_or_type d sig_) in
    (match decl with
     | Sig.LSortDecl decl ->
       if List.compare_lengths args decl.DsortDecl.params <> 0 then
         Error (Error.dsort_arity_mismatch ~loc:pos ~dsort:d
                  ~expected:(List.length decl.DsortDecl.params)
                  ~actual:(List.length args))
       else
         kind_wf_list sig_ ctx args Kind.Sort
     | Sig.LTypeDecl decl ->
       if List.compare_lengths args decl.DtypeDecl.params <> 0 then
         Error (Error.dsort_arity_mismatch ~loc:pos ~dsort:d
                  ~expected:(List.length decl.DtypeDecl.params)
                  ~actual:(List.length args))
       else
         kind_wf_list sig_ ctx args Kind.Type)

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
      let* decl = Error.at ~loc:pos (Sig.lookup_type d sig_) in
      if List.compare_lengths args decl.DtypeDecl.params <> 0 then
        Error (Error.dsort_arity_mismatch ~loc:pos ~dsort:d
                 ~expected:(List.length decl.DtypeDecl.params)
                 ~actual:(List.length args))
      else
        kind_wf_list sig_ ctx args Kind.Type

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
    (* Multi-error: surface the first error recorded on the typed
       body's tree as a structured failure, preserving the
       fail-fast contract for legacy callers.  Resilient drivers
       can call [collect_errors] directly to get every error. *)
    (match collect_errors typed_body with
     | e :: _ -> Error e
     | [] ->
       Ok (supply', Prog.CoreFunDecl { name = d.name; param = y;
                                       arg_sort = d.arg_sort; ret_sort = d.ret_sort;
                                       eff = d.eff; body = typed_body; loc = d.loc }))
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

(** Multi-error counterpart of [check_decl] for resilient drivers
    (LSP, [CompileFile.compile_file]).  Re-runs [elaborate_fun]'s
    work but skips the [collect_errors] short-circuit so the caller
    can iterate every error attached to the typed body. *)
let check_decl_multi supply sig_ (d : (SurfExpr.se, _, Var.t) Prog.decl) =
  match d with
  | Prog.FunDecl d ->
    let entry = Sig.FunSig { arg = d.arg_sort; ret = d.ret_sort; eff = d.eff } in
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
    let errs = collect_errors typed_body in
    Ok (supply',
        Prog.CoreFunDecl { name = d.name; param = y;
                           arg_sort = d.arg_sort; ret_sort = d.ret_sort;
                           eff = d.eff; body = typed_body; loc = d.loc },
        errs)
  | Prog.SortDecl dd ->
    let* () = validate_sort_decl sig_ dd in
    Ok (supply, Prog.CoreSortDecl dd, [])
  | Prog.TypeDecl dd ->
    let* () = validate_type_decl sig_ dd in
    Ok (supply, Prog.CoreTypeDecl dd, [])

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
    Elaborate.check final_sig Context.empty p.main (Ok p.main_sort) p.main_eff
  ) in
  let* (main', _supply'') = result in
  (* Multi-error: same fail-fast surface as elaborate_fun. *)
  match collect_errors main' with
  | e :: _ -> Error e
  | [] ->
    Ok (final_sig,
        { Prog.core_decls = decls'; core_main = main';
          core_main_sort = p.main_sort; core_main_eff = p.main_eff;
          core_loc = p.loc })
