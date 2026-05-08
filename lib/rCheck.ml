(* Refined typechecker — implements all refined typing judgements.
   Elaboration of SurfExpr.se to CoreExpr.typed_ce is done inline during
   typechecking via Elaborate.synth/check, preserving sort/ctx/eff info
   from elaboration throughout the entire refinement pipeline. *)

(* ---------- helpers ---------- *)

open ElabM

(* Error helpers for this module.

   - [invariant_at ~rule pos msg]: an "impossible-in-well-formed-code"
     check fired, typically because an earlier elaboration pass was
     supposed to rule this shape out. Emits a
     [Error.K_internal_invariant] carrying the rule identifier
     and the specific failed check.
   - [invariant ~rule msg]: same, but for sites with no source
     position in scope (uses [SourcePos.dummy]).
   - [ElabM.lift_at]: forwards a submodule-structured
     [(_, Error.kind) result] into the monad, attaching a [SourcePos.t]
     via [Error.at]. Used at the boundary between the refined checker
     and its helper modules ([CtorLookup], [Subst], [RCtx],
     [ProofSort], [rpat_match]). *)
let invariant_at pos ~rule msg =
  Util.raise_invariant ~loc:pos ~rule msg
let invariant ~rule msg =
  Util.raise_invariant ~loc:SourcePos.dummy ~rule msg

let loc_dummy = object method loc = SourcePos.dummy end

let int_sort = Sort.mk loc_dummy Sort.Int
let bool_sort = Sort.mk loc_dummy Sort.Bool

(* Typed info constructor for manually-built expressions *)
let mk_info sort =
  (object method loc = SourcePos.dummy method ctx = Context.empty
          method answer = Ok sort method eff = Effect.Spec
          method subterm_errors = [] end : CoreExpr.typed_info)

(** {2 SortView / CoreExprView wrappers — local option→result helpers}

    Lift [SortView.Get.*] / [CoreExprView.Get.*] from option-typed to
    [(_, Error.kind) result] using the call-site's [construct] string
    plus the appropriate [K_construct_sort_mismatch] /
    [K_wrong_pred_shape] error kind.  Errors from the input result
    propagate unchanged. *)
let mismatch_sort_kind ~construct ~expected_shape s =
  Error.K_construct_sort_mismatch
    { construct; expected_shape; got = SortView.project s }

let[@warning "-32"] view_get_pred_sort ~construct (sr : (Sort.sort, Error.kind) result)
    : (Sort.sort, Error.kind) result =
  match sr with
  | Error _ as e -> e
  | Ok s ->
    Option.to_result
      ~none:(mismatch_sort_kind ~construct ~expected_shape:"Pred _" s)
      (SortView.Get.pred (Some s))

let[@warning "-32"] view_get_record_sorts ~construct (n : int)
    (sr : (Sort.sort, Error.kind) result)
    : (Sort.sort, Error.kind) result list =
  let sub_options = SortView.Get.record n (Result.to_option sr) in
  let mismatch_for s =
    mismatch_sort_kind ~construct ~expected_shape:"Record _" s in
  List.mapi (fun _ opt ->
    match sr, opt with
    | Error e, _ -> Error e
    | Ok s, None -> Error (mismatch_for s)
    | Ok _, Some t -> Ok t)
    sub_options

let[@warning "-32"] view_get_app_sort ~construct (sr : (Sort.sort, Error.kind) result)
    : (Dsort.t, Error.kind) result * (Sort.sort, Error.kind) result list =
  match sr with
  | Error e -> Error e, []
  | Ok s ->
    let (d_opt, ts_opt) = SortView.Get.app (Some s) in
    let mismatch =
      mismatch_sort_kind ~construct
        ~expected_shape:"datasort/datatype application" s in
    let d_result = Option.to_result ~none:mismatch d_opt in
    let ts_result =
      List.map (fun t_opt -> Option.to_result ~none:mismatch t_opt)
        ts_opt in
    (d_result, ts_result)

let mismatch_ce_kind ~construct ~expected_shape ce =
  Error.K_wrong_pred_shape
    { construct; expected_shape;
      got = Format.asprintf "%a" CoreExpr.print ce }

(** [view_get_X ~construct ce] lifts [CoreExprView.Get.X] over
    [Some ce] to a result-typed value, attaching a
    [K_wrong_pred_shape] error if the shape doesn't match.  These
    are the result-domain analogues of the option-typed primitives
    in [CoreExprView]. *)
let[@warning "-32"] view_get_return_ce ~construct (ce : CoreExpr.typed_ce)
    : (CoreExpr.typed_ce, Error.kind) result =
  Option.to_result
    ~none:(mismatch_ce_kind ~construct ~expected_shape:"return _" ce)
    (CoreExprView.Get.return (Some ce))

let[@warning "-32"] view_get_take_ce ~construct (ce : CoreExpr.typed_ce)
    : (Var.t, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result =
  let mismatch =
    mismatch_ce_kind ~construct ~expected_shape:"take _ = _; _" ce in
  let (x, e1, e2) = CoreExprView.Get.take (Some ce) in
  (Option.to_result ~none:mismatch x,
   Option.to_result ~none:mismatch e1,
   Option.to_result ~none:mismatch e2)

let[@warning "-32"] view_get_let_ce ~construct (ce : CoreExpr.typed_ce)
    : (Var.t, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result =
  let mismatch =
    mismatch_ce_kind ~construct ~expected_shape:"let _ = _; _" ce in
  let (x, e1, e2) = CoreExprView.Get.let_ (Some ce) in
  (Option.to_result ~none:mismatch x,
   Option.to_result ~none:mismatch e1,
   Option.to_result ~none:mismatch e2)

let[@warning "-32"] view_get_let_tuple_ce ~construct (ce : CoreExpr.typed_ce)
    : (Var.t list, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result =
  let mismatch =
    mismatch_ce_kind ~construct
      ~expected_shape:"let (_, ..., _) = _; _" ce in
  let (xs, e1, e2) = CoreExprView.Get.let_tuple (Some ce) in
  (Option.to_result ~none:mismatch xs,
   Option.to_result ~none:mismatch e1,
   Option.to_result ~none:mismatch e2)

let[@warning "-32"] view_get_if_ce ~construct (ce : CoreExpr.typed_ce)
    : (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result =
  let mismatch =
    mismatch_ce_kind ~construct ~expected_shape:"if _ then _ else _" ce in
  let (c, t, e) = CoreExprView.Get.if_ (Some ce) in
  (Option.to_result ~none:mismatch c,
   Option.to_result ~none:mismatch t,
   Option.to_result ~none:mismatch e)

let[@warning "-32"] view_get_call_ce ~construct (ce : CoreExpr.typed_ce)
    : (string, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result =
  let mismatch =
    mismatch_ce_kind ~construct ~expected_shape:"f(_)" ce in
  let (f, arg) = CoreExprView.Get.call (Some ce) in
  (Option.to_result ~none:mismatch f,
   Option.to_result ~none:mismatch arg)

let[@warning "-32"] view_get_fail_ce ~construct (ce : CoreExpr.typed_ce)
    : (unit, Error.kind) result =
  Option.to_result
    ~none:(mismatch_ce_kind ~construct ~expected_shape:"fail" ce)
    (CoreExprView.Get.fail (Some ce))

let[@warning "-32"] view_get_case_ce ~construct (ce : CoreExpr.typed_ce)
    : (CoreExpr.typed_ce, Error.kind) result
    * ((Label.t * Var.t * CoreExpr.typed_ce * CoreExpr.typed_info) list,
       Error.kind) result =
  let mismatch =
    mismatch_ce_kind ~construct ~expected_shape:"case _ of { ... }" ce in
  let (scrut, branches) = CoreExprView.Get.case (Some ce) in
  (Option.to_result ~none:mismatch scrut,
   Option.to_result ~none:mismatch branches)

(** Re-bundle per-component errkind tuples into a single errkind tuple,
    short-circuiting on the first [Error].  Adapter for callers that
    haven't yet been refactored to consume per-component output. *)
let[@warning "-32"] zip2_kind (a, b) =
  match a, b with
  | Ok a, Ok b -> Ok (a, b)
  | Error e, _ | _, Error e -> Error e

let[@warning "-32"] zip3_kind (a, b, c) =
  match a, b, c with
  | Ok a, Ok b, Ok c -> Ok (a, b, c)
  | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e

(** {2 Errkind-input view wrappers}

    Same shape as [view_get_X_ce] but accept a [ce errkind] instead of
    a plain [ce] — when the input is [Error], every component is
    [Error] (same kind).  Used in typechecker rules that thread
    errkinds through; consumers stay match-free. *)

let[@warning "-32"] view_get_return_ce' ~construct
    (ce_r : (CoreExpr.typed_ce, Error.kind) result)
    : (CoreExpr.typed_ce, Error.kind) result =
  match ce_r with
  | Error e -> Error e
  | Ok ce -> view_get_return_ce ~construct ce

let[@warning "-32"] view_get_take_ce' ~construct
    (ce_r : (CoreExpr.typed_ce, Error.kind) result)
    : (Var.t, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result =
  match ce_r with
  | Error e -> (Error e, Error e, Error e)
  | Ok ce -> view_get_take_ce ~construct ce

let[@warning "-32"] view_get_let_ce' ~construct
    (ce_r : (CoreExpr.typed_ce, Error.kind) result)
    : (Var.t, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result =
  match ce_r with
  | Error e -> (Error e, Error e, Error e)
  | Ok ce -> view_get_let_ce ~construct ce

let[@warning "-32"] view_get_let_tuple_ce' ~construct
    (ce_r : (CoreExpr.typed_ce, Error.kind) result)
    : (Var.t list, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result =
  match ce_r with
  | Error e -> (Error e, Error e, Error e)
  | Ok ce -> view_get_let_tuple_ce ~construct ce

let[@warning "-32"] view_get_if_ce' ~construct
    (ce_r : (CoreExpr.typed_ce, Error.kind) result)
    : (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result =
  match ce_r with
  | Error e -> (Error e, Error e, Error e)
  | Ok ce -> view_get_if_ce ~construct ce

let[@warning "-32"] view_get_call_ce' ~construct
    (ce_r : (CoreExpr.typed_ce, Error.kind) result)
    : (string, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result =
  match ce_r with
  | Error e -> (Error e, Error e)
  | Ok ce -> view_get_call_ce ~construct ce

let[@warning "-32"] view_get_fail_ce' ~construct
    (ce_r : (CoreExpr.typed_ce, Error.kind) result)
    : (unit, Error.kind) result =
  match ce_r with
  | Error e -> Error e
  | Ok ce -> view_get_fail_ce ~construct ce

let[@warning "-32"] view_get_case_ce' ~construct
    (ce_r : (CoreExpr.typed_ce, Error.kind) result)
    : (CoreExpr.typed_ce, Error.kind) result
    * ((Label.t * Var.t * CoreExpr.typed_ce * CoreExpr.typed_info) list,
       Error.kind) result =
  match ce_r with
  | Error e -> (Error e, Error e)
  | Ok ce -> view_get_case_ce ~construct ce

(** {2 Errkind-input wrappers for the bundled lookup helpers}

    Each takes an errkind input and returns a tuple of errkinds (one
    per output component).  When any input is [Error], every output
    component is [Error] of the same kind.  Lets typechecker rule
    bodies thread errkinds through lookups without ever pattern-
    matching on a [(_, Error.kind) result]. *)

let[@warning "-32"] sig_lookup_fundef_e
    (cs : CoreExpr.typed_ce Sig.t)
    (f_r : (string, Error.kind) result)
    : (Var.t, Error.kind) result
    * (Sort.sort, Error.kind) result
    * (Sort.sort, Error.kind) result
    * (Effect.t, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result =
  match f_r with
  | Error e -> (Error e, Error e, Error e, Error e, Error e)
  | Ok f ->
    (match Sig.lookup_fundef f cs with
     | Error e -> (Error e, Error e, Error e, Error e, Error e)
     | Ok (param, arg, ret, eff, body) ->
       (Ok param, Ok arg, Ok ret, Ok eff, Ok body))

let[@warning "-32"] ctor_lookup_e
    (cs : CoreExpr.typed_ce Sig.t)
    (d_r : (Dsort.t, Error.kind) result)
    (label : Label.t)
    (args_r : (Sort.sort list, Error.kind) result)
    : (Sort.sort, Error.kind) result =
  match d_r, args_r with
  | Error e, _ | _, Error e -> Error e
  | Ok d, Ok args -> CtorLookup.lookup cs d label args

let[@warning "-32"] sig_lookup_dsort_or_type_e
    (cs : CoreExpr.typed_ce Sig.t)
    (d_r : (Dsort.t, Error.kind) result)
    : (Sig.sort_or_type, Error.kind) result =
  match d_r with
  | Error e -> Error e
  | Ok d -> Sig.lookup_dsort_or_type d cs

let[@warning "-32"] rctx_use_resource_e
    (x_r : (Var.t, Error.kind) result)
    (delta : RCtx.t)
    : (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result
    * RCtx.t =
  match x_r with
  | Error e -> (Error e, Error e, RCtx.affinize delta)
  | Ok x ->
    (match RCtx.use_resource x delta with
     | Error e -> (Error e, Error e, RCtx.affinize delta)
     | Ok (pred, value, delta') -> (Ok pred, Ok value, delta'))

(** {2 ProofSortView wrappers — local option→result helpers}

    Lift [ProofSortView.Get.*] from option-typed to result-typed, using
    [K_rpat_length_mismatch] for nil mismatches and
    [K_rpat_kind_mismatch] for head-shape mismatches.  The wrapper
    inspects [pf_opt] to compute the actual head kind for the
    diagnostic. *)

let pf_head_kind = function
  | None -> "(unknown)"
  | Some [] -> "(end)"
  | Some (ProofSort.Comp _ :: _) -> "comp"
  | Some (ProofSort.Log _ :: _) -> "log"
  | Some (ProofSort.Res _ :: _) -> "res"
  | Some (ProofSort.DepRes _ :: _) -> "depres"

let pf_remaining_len = function
  | None -> 0
  | Some pf -> List.length pf

let[@warning "-32"] view_get_pf_nil
    (pf_opt : (CoreExpr.typed_ce, _, Var.t) ProofSort.t option)
    : (unit, Error.kind) result =
  match ProofSortView.Get.nil pf_opt with
  | Some () -> Ok ()
  | None ->
    Error (Error.K_rpat_length_mismatch
             { pat_len = 0; pf_len = pf_remaining_len pf_opt })

let[@warning "-32"] view_get_pf_comp
    (pf_opt : (CoreExpr.typed_ce, _, Var.t) ProofSort.t option)
    : Var.t option
    * (Sort.sort, Error.kind) result
    * (Effect.t, Error.kind) result
    * (CoreExpr.typed_ce, _, Var.t) ProofSort.t option =
  let (var_o, sort_o, eff_o, tail_o) = ProofSortView.Get.comp pf_opt in
  let mismatch =
    Error.K_rpat_kind_mismatch
      { pat_kind = "core"; pf_kind = pf_head_kind pf_opt } in
  ( var_o,
    Option.to_result ~none:mismatch sort_o,
    Option.to_result ~none:mismatch eff_o,
    tail_o )

let[@warning "-32"] view_get_pf_log
    (pf_opt : (CoreExpr.typed_ce, _, Var.t) ProofSort.t option)
    : (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, _, Var.t) ProofSort.t option =
  let (prop_o, tail_o) = ProofSortView.Get.log pf_opt in
  let mismatch =
    Error.K_rpat_kind_mismatch
      { pat_kind = "log"; pf_kind = pf_head_kind pf_opt } in
  ( Option.to_result ~none:mismatch prop_o, tail_o )

let[@warning "-32"] view_get_pf_res
    (pf_opt : (CoreExpr.typed_ce, _, Var.t) ProofSort.t option)
    : (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, _, Var.t) ProofSort.t option =
  let (pred_o, value_o, tail_o) = ProofSortView.Get.res pf_opt in
  let mismatch =
    Error.K_rpat_kind_mismatch
      { pat_kind = "res"; pf_kind = pf_head_kind pf_opt } in
  ( Option.to_result ~none:mismatch pred_o,
    Option.to_result ~none:mismatch value_o,
    tail_o )

let[@warning "-32"] view_get_pf_depres
    (pf_opt : (CoreExpr.typed_ce, _, Var.t) ProofSort.t option)
    : Var.t option
    * (CoreExpr.typed_ce, Error.kind) result
    * (CoreExpr.typed_ce, _, Var.t) ProofSort.t option =
  let (bvar_o, pred_o, tail_o) = ProofSortView.Get.depres pf_opt in
  let mismatch =
    Error.K_rpat_kind_mismatch
      { pat_kind = "depres"; pf_kind = pf_head_kind pf_opt } in
  ( bvar_o,
    Option.to_result ~none:mismatch pred_o,
    tail_o )

(** {2 [extend_*_opt] helpers — wrap [RCtx.extend_*] for option-typed
    var and result-typed payloads.

    Three rules, applied in order:
    - If [var] is [None], the variable name itself is unknown — don't
      extend at all.
    - If [var] is [Some v] but any payload argument is [Error _],
      extend with [extend_unknown] (we know the variable but not its
      sort/predicate/etc.).
    - Otherwise extend with the matching [RCtx.extend_<kind>]. *)

let[@warning "-32"] extend_comp_opt (var : Var.t option)
    (sort : (Sort.sort, Error.kind) result)
    (eff : (Effect.t, Error.kind) result)
    (delta : RCtx.t) : RCtx.t =
  match var, sort, eff with
  | None, _, _ -> delta
  | Some v, Ok s, Ok e -> RCtx.extend_comp v s e delta
  | Some v, _, _ -> RCtx.extend_unknown v delta

let[@warning "-32"] extend_log_opt (var : Var.t option)
    (prop : (CoreExpr.typed_ce, Error.kind) result)
    (delta : RCtx.t) : RCtx.t =
  match var, prop with
  | None, _ -> delta
  | Some v, Ok p -> RCtx.extend_log v p delta
  | Some v, Error _ -> RCtx.extend_unknown v delta

let[@warning "-32"] extend_res_opt (var : Var.t option)
    (pred : (CoreExpr.typed_ce, Error.kind) result)
    (value : (CoreExpr.typed_ce, Error.kind) result)
    (usage : Usage.t) (delta : RCtx.t) : RCtx.t =
  match var, pred, value with
  | None, _, _ -> delta
  | Some v, Ok p, Ok va -> RCtx.extend_res v p va usage delta
  | Some v, _, _ -> RCtx.extend_unknown v delta

(* Elaborate a surface expression to typed core, synthesizing its
   sort.  Defers to surface elaboration; if the elaborator recorded
   any errors on the typed tree, fail-fast through ElabM with the
   first one — the per-decl driver in [compileFile.compile_rfile]
   captures these so later decls still get checked.  Slices C.2-C.5
   will replace this fail-fast with an attach-and-continue path. *)
(* Elaborate a surface expression to typed core, synthesizing its
   sort.  Returns the typed_ce together with a result-typed sort:
   [Ok sort] when elaboration succeeded, [Error _] when it recorded
   any failure on the typed AST.  Errors live on [info#answer] /
   [info#subterm_errors] of the returned [ce]; downstream callers
   thread the result-typed sort through [view_get_*_sort] wrappers
   without needing to fail the monad. *)
let elab_se (rs : RSig.t) (gamma : Context.t) (eff : Effect.t)
    (se : SurfExpr.se)
    : (CoreExpr.typed_ce * (Sort.sort, Error.kind) result) ElabM.t =
  let cs = RSig.comp rs in
  let* ce = Elaborate.synth cs gamma eff se in
  let sort_r =
    Result.map_error Error.kind (CoreExpr.info ce)#answer in
  return (ce, sort_r)

(* Elaborate a surface expression to typed core, checking against a
   sort.  Returns the typed_ce regardless of internal errors; errors
   ride along on [info#answer] / [info#subterm_errors] for downstream
   consumption. *)
let elab_se_check (rs : RSig.t) (gamma : Context.t) (se : SurfExpr.se)
    (sort : Sort.sort) (eff : Effect.t)
    : CoreExpr.typed_ce ElabM.t =
  let cs = RSig.comp rs in
  Elaborate.check cs gamma se (Ok sort) eff

(* Elaborate a surface expression to typed core using a refined context *)
let elab_and_synth rs delta eff se =
  let gamma = RCtx.erase delta in
  elab_se rs gamma eff se

(* Elaborate a ProofSort from parsed (SurfExpr.se) to checked (CoreExpr.typed_ce) *)
let elab_pf_entry (rs : RSig.t) (gamma : Context.t) (eff : Effect.t) (entry : (SurfExpr.se, < loc : SourcePos.t >, Var.t) ProofSort.entry) : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.entry ElabM.t =
  let loc = (ProofSort.entry_info entry)#loc in
  let mk_ri sort eff : RProg.typed_rinfo =
    (object
      method loc = loc
      method ctx = gamma
      method rctx = RCtx.empty
      method sort = sort
      method eff = eff
      method goal = RProg.NoGoal
      method answer = Ok sort
      method subterm_errors = []
    end)
  in
  let ri = mk_ri bool_sort eff in
  match entry with
  | ProofSort.Comp { info = _; var; sort; eff } ->
    let ri_comp = mk_ri sort eff in
    return (ProofSort.Comp { info = ri_comp; var; sort; eff })
  | ProofSort.Log { info = _; prop } ->
    let* ce = elab_se_check rs gamma prop bool_sort Effect.Spec in
    return (ProofSort.Log { info = ri; prop = ce })
  | ProofSort.Res { info = _; pred; value } ->
    let* (ce_pred, pred_sort_r) = elab_se rs gamma Effect.Spec pred in
    let inner_sort_r =
      view_get_pred_sort ~construct:"resource predicate" pred_sort_r in
    let inner_sort = Result.value inner_sort_r ~default:bool_sort in
    let* ce_value = elab_se_check rs gamma value inner_sort Effect.Spec in
    return (ProofSort.Res { info = ri; pred = ce_pred; value = ce_value })
  | ProofSort.DepRes { info = _; bound_var; pred } ->
    let* (ce_pred, pred_sort_r) = elab_se rs gamma Effect.Spec pred in
    let _ : (Sort.sort, Error.kind) result =
      view_get_pred_sort ~construct:"dep-res predicate" pred_sort_r in
    return (ProofSort.DepRes { info = ri; bound_var; pred = ce_pred })

let elab_pf (rs : RSig.t) (gamma : Context.t) (eff : Effect.t) (pf : (SurfExpr.se, < loc : SourcePos.t >, Var.t) ProofSort.t) : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t ElabM.t =
  let rec go gamma = function
    | [] -> return []
    | entry :: rest ->
      let* entry' = elab_pf_entry rs gamma eff entry in
      let* gamma' = match entry' with
        | ProofSort.Comp { info = _; var; sort; eff } -> return (Context.extend var sort eff gamma)
        | ProofSort.Log _ | ProofSort.Res _ -> return gamma
        | ProofSort.DepRes { info = _; pred; bound_var } ->
          let pred_sort = (CoreExpr.sort_of_info (CoreExpr.info pred)) in
          let inner_r =
            SortGet.get_pred ~construct:"dep-res predicate" pred_sort in
          return (Context.extend_or_unknown bound_var inner_r Effect.Spec gamma)
      in
      let* rest' = go gamma' rest in
      return (entry' :: rest')
  in
  go gamma pf

(* Make a typed core bool literal *)
let mk_bool b = CoreExpr.mk (mk_info bool_sort) (CoreExpr.BoolLit b)
let mk_true = mk_bool true
let mk_false = mk_bool false

(* Make ce1 == ce2 as a typed core expression *)
let mk_eq ce1 ce2 = CoreExpr.mk (mk_info bool_sort) (CoreExpr.Eq (ce1, ce2))

(* The [close] auxiliary from [doc/refinement-types.md:157-162]:

      ·                          ⇒ C  =  C
      (Δ, x : τ [eff])           ⇒ C  =  Δ ⇒ ∀x:τ. C
      (Δ, x : ϕ [log])           ⇒ C  =  Δ ⇒ ϕ ⇒ C
      (Δ, x : ce@ce' [res(u)])   ⇒ C  =  Δ ⇒ C            (no contribution)

   Entries arrive in source order (oldest first; [rpat_match] now
   adds bindings left-to-right). The earliest binding should become
   the OUTERMOST wrapper, so a [fold_right] over the entry list
   wraps from inside out: the rightmost (latest) entry wraps [ct]
   first, then each earlier entry wraps the accumulating result. *)
let close_ctx pos delta_close ct =
  List.fold_right (fun entry acc ->
    match entry with
    | RCtx.Comp { var; sort; eff = _ } ->
      Constraint.forall_ pos var sort acc
    | RCtx.Log { var = _; prop } ->
      Constraint.impl pos prop acc
    | RCtx.Res _ -> acc
    | RCtx.Unknown _ ->
      (* Unknown entries don't contribute a quantifier — there's no
         sort to bind, and any constraint that depended on the
         erroneous binder has been replaced by Top per the
         atom-guard. *)
      acc)
    (RCtx.entries delta_close) ct

(* Unwrap layers that obscure the monadic skeleton of a predicate
   expression, leaving the underlying [Return] / [Take] / [Fail]
   shape visible to pattern-matching code in [open-ret] / [open-take]
   / [make-ret] / [make-take]. Two kinds of wrapper arise from
   elaboration:

   1. [CoreExpr.Annot] from [SurfExpr.Annot] elaboration.
   2. [CoreExpr.Let (y, Var x, body)] introduced by [coverage_check]
      when compiling a [Var] pattern (e.g. [take xs_tl = _; body]
      elaborates to [Take(y, _, Let(xs_tl, Var y, body))]). When the
      let's RHS is itself a variable, the let is a pure alias and
      inlining it preserves semantics.

   Other [Let] forms are left intact — their RHS might have side
   effects or non-trivial structure we don't want to duplicate. *)
let rec strip_annots ce =
  match CoreExpr.shape ce with
  | CoreExpr.Annot (inner, _) -> strip_annots inner
  | CoreExpr.Let ((x, _), rhs, body) ->
    (match CoreExpr.shape rhs with
     | CoreExpr.Var v ->
       let subst = Subst.extend_var x (CoreExpr.mk (CoreExpr.info rhs) (CoreExpr.Var v)) Subst.empty in
       strip_annots (Subst.apply_ce subst body)
     | _ -> ce)
  | _ -> ce

(** [strip_annots_shallow ce] strips only [CoreExpr.Annot] wrappers,
    leaving [Let (y, Var x, body)] aliases intact.  Used by
    [rpat_match]: the user's rpat is written against the structural
    form they see in hover (which preserves coverage-introduced
    alias-lets), so rpat matching must see the same form.  Other
    refined operations (open-take, unfold, RReturn/RFail/RCase/etc.)
    use [strip_annots] which inlines aliases, since they want the
    semantic form. *)
let rec strip_annots_shallow ce =
  match CoreExpr.shape ce with
  | CoreExpr.Annot (inner, _) -> strip_annots_shallow inner
  | _ -> ce

(* ---------- refined primitive signatures ---------- *)

(* Dummy rinfo for manually-built proof sorts (e.g. rprim_signature)
   where no meaningful context exists. *)
let rinfo_dummy : RProg.typed_rinfo =
  let bool_dummy =
    Sort.mk (object method loc = SourcePos.dummy end) Sort.Bool in
  (object
    method loc = SourcePos.dummy
    method ctx = Context.empty
    method rctx = RCtx.empty
    method sort = bool_dummy
    method eff = Effect.Spec
    method goal = RProg.NoGoal
    method answer = Ok bool_dummy
    method subterm_errors = []
  end)

(* Helper constructors for building proof sorts *)
let ce_of_var v sort = CoreExpr.mk (mk_info sort) (CoreExpr.Var v)
let mk_prim_app p args =
  let (arg_sort, ret_sort, _eff) = Typecheck.prim_signature p in
  CoreExpr.mk (mk_info ret_sort) (CoreExpr.App (p, CoreExpr.mk (mk_info arg_sort) (CoreExpr.Tuple args)))
let mk_not ce = CoreExpr.mk (mk_info bool_sort) (CoreExpr.Not ce)

(* Lift a plain FunSig/FunDef to an RF, creating fresh variables *)
let lift_to_rf arg ret eff =
  let* x = fresh SourcePos.dummy in
  let* y = fresh SourcePos.dummy in
  return RFunType.{
    domain = [ProofSort.Comp { info = rinfo_dummy; var = x; sort = arg; eff }];
    codomain = [ProofSort.Comp { info = rinfo_dummy; var = y; sort = ret; eff }];
    eff;
  }

(* Monadic lookup for refined function types, lifting plain entries.
   Spec rule [:: call] (syntax.ott:1758-1761) demands that [f] have a
   refined function type; if no [RFunSig] is registered we fall back
   to lifting a plain [FunSig]/[FunDef], and only fail (with
   [K_unknown_function]) if neither is bound. *)
let lookup_rf_m ~loc (rs : RSig.t) (f : string)
    : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RFunType.t ElabM.t =
  match RSig.lookup_rf f rs with
  | Ok rf -> return rf
  | Error _ ->
    let* (arg, ret, eff) = ElabM.lift_at loc (RSig.lookup_fun f rs) in
    lift_to_rf arg ret eff

(** Refined function type for a primitive, following the spec in refinement-types.md. *)
let rprim_signature (p : Prim.t) : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RFunType.t ElabM.t =
  match p with
  (* Arithmetic: (x:int, y:int) ⊸ (z:int, prop: z == prim(x,y) [log]) [pure] *)
  | Prim.Add | Prim.Sub | Prim.Mul ->
    let* x = fresh SourcePos.dummy in
    let* y = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    let result_expr = mk_prim_app p [ce_of_var x int_sort; ce_of_var y int_sort] in
    return RFunType.{ domain = [ProofSort.Comp { info = rinfo_dummy; var = x; sort = int_sort; eff = Effect.Pure };
                ProofSort.Comp { info = rinfo_dummy; var = y; sort = int_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { info = rinfo_dummy; var = z; sort = int_sort; eff = Effect.Pure };
                  ProofSort.Log { info = rinfo_dummy; prop =mk_eq (ce_of_var z int_sort) result_expr }];
      eff = Effect.Pure }

  (* Div: (x:int, y:int, pre: not(y == 0) [log]) ⊸ (z:int, prop: z == x/y [log]) [pure] *)
  | Prim.Div ->
    let* x = fresh SourcePos.dummy in
    let* y = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    let zero = CoreExpr.mk (mk_info int_sort) (CoreExpr.IntLit 0) in
    return RFunType.{ domain = [ProofSort.Comp { info = rinfo_dummy; var = x; sort = int_sort; eff = Effect.Pure };
                ProofSort.Comp { info = rinfo_dummy; var = y; sort = int_sort; eff = Effect.Pure };
                ProofSort.Log { info = rinfo_dummy; prop = mk_not (mk_eq (ce_of_var y int_sort) zero) }];
      codomain = [ProofSort.Comp { info = rinfo_dummy; var = z; sort = int_sort; eff = Effect.Pure };
                  ProofSort.Log { info = rinfo_dummy; prop =mk_eq (ce_of_var z int_sort) (mk_prim_app Prim.Div [ce_of_var x int_sort; ce_of_var y int_sort]) }];
      eff = Effect.Pure }

  (* Comparisons: (x:int, y:int) ⊸ (z:bool, prop: z == x cmp y [log]) [pure] *)
  | Prim.Lt | Prim.Le | Prim.Gt | Prim.Ge ->
    let* x = fresh SourcePos.dummy in
    let* y = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    let result_expr = mk_prim_app p [ce_of_var x int_sort; ce_of_var y int_sort] in
    return RFunType.{ domain = [ProofSort.Comp { info = rinfo_dummy; var = x; sort = int_sort; eff = Effect.Pure };
                ProofSort.Comp { info = rinfo_dummy; var = y; sort = int_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { info = rinfo_dummy; var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { info = rinfo_dummy; prop =mk_eq (ce_of_var z bool_sort) result_expr }];
      eff = Effect.Pure }

  (* Logic: same pattern as comparisons *)
  | Prim.And ->
    let* x = fresh SourcePos.dummy in
    let* y = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    return RFunType.{ domain = [ProofSort.Comp { info = rinfo_dummy; var = x; sort = bool_sort; eff = Effect.Pure };
                ProofSort.Comp { info = rinfo_dummy; var = y; sort = bool_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { info = rinfo_dummy; var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { info = rinfo_dummy; prop =mk_eq (ce_of_var z bool_sort) (CoreExpr.mk (mk_info bool_sort) (CoreExpr.And (ce_of_var x bool_sort, ce_of_var y bool_sort))) }];
      eff = Effect.Pure }

  | Prim.Or ->
    let* x = fresh SourcePos.dummy in
    let* y = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    return RFunType.{ domain = [ProofSort.Comp { info = rinfo_dummy; var = x; sort = bool_sort; eff = Effect.Pure };
                ProofSort.Comp { info = rinfo_dummy; var = y; sort = bool_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { info = rinfo_dummy; var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { info = rinfo_dummy; prop =mk_eq (ce_of_var z bool_sort) (mk_prim_app Prim.Or [ce_of_var x bool_sort; ce_of_var y bool_sort]) }];
      eff = Effect.Pure }

  | Prim.Not ->
    let* x = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    return RFunType.{ domain = [ProofSort.Comp { info = rinfo_dummy; var = x; sort = bool_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { info = rinfo_dummy; var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { info = rinfo_dummy; prop =mk_eq (ce_of_var z bool_sort) (mk_not (ce_of_var x bool_sort)) }];
      eff = Effect.Pure }

  (* Eq[A]: (x:A, y:A) ⊸ (z:bool, prop: z == (x == y) [log]) [pure] *)
  | Prim.Eq ty ->
    let a_sort = ty in
    let* x = fresh SourcePos.dummy in
    let* y = fresh SourcePos.dummy in
    let* z = fresh SourcePos.dummy in
    return RFunType.{ domain = [ProofSort.Comp { info = rinfo_dummy; var = x; sort = a_sort; eff = Effect.Pure };
                ProofSort.Comp { info = rinfo_dummy; var = y; sort = a_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { info = rinfo_dummy; var = z; sort = bool_sort; eff = Effect.Pure };
                  ProofSort.Log { info = rinfo_dummy; prop =mk_eq (ce_of_var z bool_sort) (mk_eq (ce_of_var x a_sort) (ce_of_var y a_sort)) }];
      eff = Effect.Pure }

  (* New[A]: (x:A) ⊸ (p:ptr A, r:Own[A](p) @ x [res]) [impure] *)
  | Prim.New ty ->
    let a_sort = ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let pred_sort = Sort.mk loc_dummy (Sort.Pred a_sort) in
    let* x = fresh SourcePos.dummy in
    let* p = fresh SourcePos.dummy in
    let own_p = CoreExpr.mk (mk_info pred_sort) (CoreExpr.App (Prim.Own ty, ce_of_var p ptr_sort)) in
    return RFunType.{ domain = [ProofSort.Comp { info = rinfo_dummy; var = x; sort = a_sort; eff = Effect.Pure }];
      codomain = [ProofSort.Comp { info = rinfo_dummy; var = p; sort = ptr_sort; eff = Effect.Pure };
                  ProofSort.Res { info = rinfo_dummy; pred = own_p; value = ce_of_var x a_sort }];
      eff = Effect.Impure }

  (* Del[A]: (p:ptr A, x:A [spec], r:Own[A](p) @ x [res]) ⊸ () [impure] *)
  | Prim.Del ty ->
    let a_sort = ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let pred_sort = Sort.mk loc_dummy (Sort.Pred a_sort) in
    let* p = fresh SourcePos.dummy in
    let* x = fresh SourcePos.dummy in
    let own_p = CoreExpr.mk (mk_info pred_sort) (CoreExpr.App (Prim.Own ty, ce_of_var p ptr_sort)) in
    return RFunType.{ domain = [ProofSort.Comp { info = rinfo_dummy; var = p; sort = ptr_sort; eff = Effect.Pure };
                ProofSort.Comp { info = rinfo_dummy; var = x; sort = a_sort; eff = Effect.Spec };
                ProofSort.Res { info = rinfo_dummy; pred = own_p; value = ce_of_var x a_sort }];
      codomain = [];
      eff = Effect.Impure }

  (* Get[A]: (p:ptr A, r:(x:A).Own[A](p) [res]) ⊸ (v:A, pf: v = x [log], r':Own[A](p) @ x [res]) [impure] *)
  | Prim.Get ty ->
    let a_sort = ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let pred_sort = Sort.mk loc_dummy (Sort.Pred a_sort) in
    let* p = fresh SourcePos.dummy in
    let* x = fresh SourcePos.dummy in
    let* v = fresh SourcePos.dummy in
    let own_p = CoreExpr.mk (mk_info pred_sort) (CoreExpr.App (Prim.Own ty, ce_of_var p ptr_sort)) in
    return RFunType.{ domain = [ProofSort.Comp { info = rinfo_dummy; var = p; sort = ptr_sort; eff = Effect.Pure };
                ProofSort.DepRes { info = rinfo_dummy; bound_var = x; pred = own_p }];
      codomain = [ProofSort.Comp { info = rinfo_dummy; var = v; sort = a_sort; eff = Effect.Pure };
                  ProofSort.Log { info = rinfo_dummy; prop = mk_eq (ce_of_var v a_sort) (ce_of_var x a_sort) };
                  ProofSort.Res { info = rinfo_dummy; pred = own_p; value = ce_of_var x a_sort }];
      eff = Effect.Impure }

  (* Set[A]: (p:ptr A, v:A, r:(x:A).Own[A](p) [res]) ⊸ (r':Own[A](p) @ v [res]) [impure] *)
  | Prim.Set ty ->
    let a_sort = ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let pred_sort = Sort.mk loc_dummy (Sort.Pred a_sort) in
    let* p = fresh SourcePos.dummy in
    let* v = fresh SourcePos.dummy in
    let* x = fresh SourcePos.dummy in
    let own_p = CoreExpr.mk (mk_info pred_sort) (CoreExpr.App (Prim.Own ty, ce_of_var p ptr_sort)) in
    return RFunType.{ domain = [ProofSort.Comp { info = rinfo_dummy; var = p; sort = ptr_sort; eff = Effect.Pure };
                ProofSort.Comp { info = rinfo_dummy; var = v; sort = a_sort; eff = Effect.Pure };
                ProofSort.DepRes { info = rinfo_dummy; bound_var = x; pred = own_p }];
      codomain = [ProofSort.Res { info = rinfo_dummy; pred = own_p; value = ce_of_var v a_sort }];
      eff = Effect.Impure }

  (* Own[A]: (p:ptr A) ⊸ (r:pred A) [spec] — same as core signature *)
  | Prim.Own ty ->
    let a_sort = ty in
    let ptr_sort = Sort.mk loc_dummy (Sort.Ptr a_sort) in
    let pred_sort = Sort.mk loc_dummy (Sort.Pred a_sort) in
    let* p = fresh SourcePos.dummy in
    let* r = fresh SourcePos.dummy in
    return RFunType.{ domain = [ProofSort.Comp { info = rinfo_dummy; var = p; sort = ptr_sort; eff = Effect.Spec }];
      codomain = [ProofSort.Comp { info = rinfo_dummy; var = r; sort = pred_sort; eff = Effect.Spec }];
      eff = Effect.Spec }

(* ---------- delta monotonicity check ---------- *)

(* Invariant: for every judgement RS; Delta |- ... -| Delta' ~> Ct,
   Delta ⊓ Delta' = Delta' (output only consumes resources, never creates them).
   Here ⊓ is the lattice meet in the total order Used ≤ Opt ≤ Avail. *)

let delta_check_enabled = ref false

let assert_delta_below delta delta' =
  if not !delta_check_enabled then return ()
  else
    match RCtx.lattice_merge delta delta' with
    | Error _ ->
      invariant ~rule:"assert_delta_below"
        "delta lattice merge failed: contexts have inconsistent \
         shape at an internal checkpoint"
    | Ok merged ->
      if RCtx.usage_equal merged delta' then return ()
      else
        invariant ~rule:"assert_delta_below"
          (Format.asprintf
             "output context not below input@.  input:  %a@.  output: %a"
             RCtx.print delta RCtx.print delta')

(* ---------- checked tree type aliases ---------- *)

type checked_crt = (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RefinedExpr.crt
type checked_lpf = (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RefinedExpr.lpf
type checked_rpf = (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RefinedExpr.rpf
type checked_spine = (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RefinedExpr.spine

(* Build a typed_rinfo annotation for a refined expression node.
   [loc] = source position from the parsed node
   [delta] = refined context (carries both core and refined info)
   [sort] = computational sort at this node
   [eff] = effect under which the node is checked *)
let mk_rinfo ?(goal=RProg.NoGoal) loc delta sort eff : RProg.typed_rinfo =
  (object
    method loc = loc
    method ctx = RCtx.erase delta
    method rctx = delta
    method sort = sort
    method eff = eff
    method goal = goal
    method answer = Ok sort
    method subterm_errors = []
  end)

(** [mk_rinfo_err ?goal loc delta sort eff err] is like [mk_rinfo]
    but with [info#answer = Error err] for clauses that detected a
    user error and are continuing.  [sort] is kept as the
    "would-have-been" sort so hover / inspector consumers still see
    a placeholder; the truth-of-record lives on [answer]. *)
let[@warning "-32"] mk_rinfo_err ?(goal=RProg.NoGoal) loc delta sort eff err : RProg.typed_rinfo =
  (object
    method loc = loc
    method ctx = RCtx.erase delta
    method rctx = delta
    method sort = sort
    method eff = eff
    method goal = goal
    method answer = Error err
    method subterm_errors = []
  end)

(** [mk_rinfo_with_answer ?goal loc delta sort eff answer] takes an
    explicit answer (already a [(Sort.sort, Error.t) result]) so
    callers can lift [(_, Error.kind) result] inputs from the View
    wrappers via [Error.structured ~loc].  [sort] is the placeholder
    used when [answer = Error _]. *)
let[@warning "-32"] mk_rinfo_with_answer
    ?(goal=RProg.NoGoal) loc delta sort eff
    (answer : (Sort.sort, Error.t) result) : RProg.typed_rinfo =
  (object
    method loc = loc
    method ctx = RCtx.erase delta
    method rctx = delta
    method sort = sort
    method eff = eff
    method goal = goal
    method answer = answer
    method subterm_errors = []
  end)

(** [answer_of_sort_kind_r ~loc sort_r] lifts a
    [(Sort.sort, Error.kind) result] into a
    [(Sort.sort, Error.t) result] suitable for the [answer] field, by
    attaching [loc] to the error kind. *)
let[@warning "-32"] answer_of_sort_kind_r ~loc
    (sort_r : (Sort.sort, Error.kind) result)
    : (Sort.sort, Error.t) result =
  Result.map_error (Error.structured ~loc) sort_r

(** [collect_cpat_vars cp] collects every [Var.t] introduced by [cp]
    (CVar binders, recursively through CTuple). *)
let rec collect_cpat_vars (cp : (_, Var.t) RPat.cpat) : Var.t list =
  match RPat.cpat_shape cp with
  | RPat.CVar x -> [x]
  | RPat.CTuple cps -> List.concat_map collect_cpat_vars cps

(** [collect_lpat_vars lp] collects [lp]'s introduced [Var.t], if any. *)
let collect_lpat_vars (lp : (_, Var.t) RPat.lpat) : Var.t list =
  match RPat.lpat_shape lp with
  | RPat.LVar x -> [x]
  | RPat.LAuto -> []

(** [collect_rp_vars rp] collects every [Var.t] introduced anywhere in
    [rp]'s structure: cpat binders (Comp/Spec), lpat binders (Log),
    and the trailing RVar (Res).  Used at error-short-circuit sites in
    [rpat_match] so the rpat's variables stay in scope as
    [extend_unknown] entries even when the structural pattern check
    fails — without this, the user loses every variable they named in
    the pattern from the body's context. *)
let rec collect_rp_vars (rp : (_, Var.t) RPat.rpat) : Var.t list =
  match RPat.rpat_shape rp with
  | RPat.RVar x -> [x]
  | RPat.RReturn lp -> collect_lpat_vars lp
  | RPat.RTake (cp, rp1, rp2) ->
    collect_cpat_vars cp @ collect_rp_vars rp1 @ collect_rp_vars rp2
  | RPat.RFail lp -> collect_lpat_vars lp
  | RPat.RLet (lp, cp, rp') ->
    collect_lpat_vars lp @ collect_cpat_vars cp @ collect_rp_vars rp'
  | RPat.RCase (lp, _, cp, rp') ->
    collect_lpat_vars lp @ collect_cpat_vars cp @ collect_rp_vars rp'
  | RPat.RIfTrue rp' | RPat.RIfFalse rp'
  | RPat.RUnfold rp' | RPat.RAnnot rp' ->
    collect_rp_vars rp'

(** [extend_delta_with_rp_unknowns rp delta] extends [delta] with an
    [extend_unknown] entry for every [Var.t] introduced anywhere in
    [rp]'s structure.  Used at error-short-circuit sites so the user's
    pattern variables are still in scope downstream even when the
    structural match failed. *)
let extend_delta_with_rp_unknowns rp delta =
  List.fold_left
    (fun d v -> RCtx.extend_unknown v d)
    delta (collect_rp_vars rp)

(** [error_rp_blanket rp delta eff k] produces a typed rpat with the
    same shape as [rp] but with [Error _] on every internal node's
    answer, and an extended context with an [extend_unknown] entry for
    every variable the rpat introduces.  Used at structural-mismatch
    short-circuits in [rpat_match]: if a view extract fails, we still
    produce a typed AST and keep the user's pattern variables in scope
    so LSP context queries downstream of the rpat see [a1, x, xs,
    rest2 : ?] rather than dropping them entirely. *)
let[@warning "-32"] error_rp_blanket
    (rp : (_, Var.t) RPat.rpat)
    (delta : RCtx.t)
    (eff : Effect.t)
    (k : Error.kind)
    : (RProg.typed_rinfo, Var.t) RPat.rpat * RCtx.t =
  let delta' = extend_delta_with_rp_unknowns rp delta in
  let typed_rp =
    RPat.map_info_rpat
      (fun b ->
        mk_rinfo_with_answer b#loc delta' bool_sort eff
          (Error (Error.structured ~loc:b#loc k)))
      rp in
  (typed_rp, delta')

(** [mk_rinfo_full ?goal loc delta sort eff errors] is like
    [mk_rinfo] but takes an explicit [errors] list.  When non-empty,
    the first error rides on [info#answer] and the remainder rides
    on [info#subterm_errors] — so [rinfo_error] reads them all
    without duplication.  When empty, [info#answer = Ok sort] and
    [info#subterm_errors = []].  Used at clauses with multiple
    cross-cutting checks (e.g. CIter's effect / sort / leak / pattern
    checks) where every error should surface, not just the first. *)
let[@warning "-32"] mk_rinfo_full
    ?(goal=RProg.NoGoal) loc delta sort eff
    (errors : Error.t list) : RProg.typed_rinfo =
  let (answer, rest) = match errors with
    | [] -> (Ok sort, [])
    | e :: rest -> (Error e, rest) in
  (object
    method loc = loc
    method ctx = RCtx.erase delta
    method rctx = delta
    method sort = sort
    method eff = eff
    method goal = goal
    method answer = answer
    method subterm_errors = rest
  end)

(** [prepend_subterm_errors_crt errs ce] rebuilds [ce]'s outer
    rinfo with [errs] prepended to its [subterm_errors] list.  Used
    at decl-level boundaries where cross-cutting errors (resource
    leak, ProofSort.bind failure, pf_eq mismatches) need to attach
    to the body's typed AST without their own node. *)
let[@warning "-32"] prepend_subterm_errors_crt
    (errs : Error.t list)
    (ce : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RefinedExpr.crt)
    : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RefinedExpr.crt =
  if errs = [] then ce
  else
    let info = RefinedExpr.crt_info ce in
    let new_info : RProg.typed_rinfo = object
      method loc = info#loc
      method ctx = info#ctx
      method rctx = info#rctx
      method sort = info#sort
      method eff = info#eff
      method goal = info#goal
      method answer = info#answer
      method subterm_errors = errs @ info#subterm_errors
    end in
    RefinedExpr.mk_crt new_info (RefinedExpr.crt_shape ce)

(* ---------- typing judgements ---------- *)

(* Tag description helpers for error messages *)
let spine_tag_name = function
  | RefinedExpr.SCore _ -> "a core expression"
  | RefinedExpr.SLog _ -> "a logical proof fact (log ...)"
  | RefinedExpr.SRes _ -> "a resource proof fact (res ...)"
  | RefinedExpr.SNil -> "end of arguments"

let pf_entry_tag_name = function
  | ProofSort.Comp _ -> "core"
  | ProofSort.Log _ -> "logical"
  | ProofSort.Res _ -> "resource"
  | ProofSort.DepRes _ -> "dependent resource"

let pf_entry_to_string entry =
  Format.asprintf "%a" ProofSort.print_ce [entry]

(* Logical fact synthesis: RS; Delta |- lpf => ce -| Delta' ~> Ct *)
let rec synth_lpf (rs : RSig.t) (delta : RCtx.t) (lpf : RefinedExpr.parsed_lpf) : (checked_lpf * CoreExpr.typed_ce * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RefinedExpr.lpf_info lpf in
  let pos = binfo#loc in
  match RefinedExpr.lpf_shape lpf with
  | RefinedExpr.LVar x ->
    (match RCtx.lookup_log x delta with
     | Ok ce ->
       let rinfo = mk_rinfo ~goal:(RProg.LpfGoal ce) pos delta bool_sort Effect.Spec in
       let checked = RefinedExpr.mk_lpf rinfo (RefinedExpr.LVar x) in
       return (checked, ce, delta, Constraint.top pos)
     | Error _ ->
       let err = Error.log_var_not_found ~loc:pos ~name:x in
       let placeholder_ce =
         CoreExpr.mk (mk_info bool_sort)
           (CoreExpr.Hole "lpf-unbound-log-var") in
       let rinfo =
         mk_rinfo_err ~goal:(RProg.LpfGoal placeholder_ce)
           pos delta bool_sort Effect.Spec err in
       let checked = RefinedExpr.mk_lpf rinfo (RefinedExpr.LVar x) in
       return (checked, placeholder_ce, delta, Constraint.top pos))

  | RefinedExpr.LAuto ->
    let err = Error.cannot_synthesize ~loc:pos ~construct:"auto" in
    let placeholder_ce =
      CoreExpr.mk (mk_info bool_sort)
        (CoreExpr.Hole "lpf-auto-unsynth") in
    let rinfo =
      mk_rinfo_err ~goal:(RProg.LpfGoal placeholder_ce)
        pos delta bool_sort Effect.Spec err in
    let checked = RefinedExpr.mk_lpf rinfo RefinedExpr.LAuto in
    return (checked, placeholder_ce, delta, Constraint.top pos)

  | RefinedExpr.LHole h ->
    let err = Error.cannot_synthesize ~loc:pos ~construct:"hole" in
    let placeholder_ce =
      CoreExpr.mk (mk_info bool_sort)
        (CoreExpr.Hole "lpf-hole-unsynth") in
    let rinfo =
      mk_rinfo_err ~goal:(RProg.LpfGoal placeholder_ce)
        pos delta bool_sort Effect.Spec err in
    let checked = RefinedExpr.mk_lpf rinfo (RefinedExpr.LHole h) in
    return (checked, placeholder_ce, delta, Constraint.top pos)

  | RefinedExpr.LUnfold (f, se_arg) ->
    let* (ce_arg, _sort) = elab_and_synth rs delta Effect.Spec se_arg in
    let cs = RSig.comp rs in
    let* (param, arg_sort, ret_sort, eff, body) =
      ElabM.lift_at pos (Sig.lookup_fundef f cs) in
    if not (Effect.sub eff Effect.Spec) then begin
      let err = Error.unfold_not_spec ~loc:pos ~name:f in
      let placeholder_ce =
        CoreExpr.mk (mk_info bool_sort)
          (CoreExpr.Hole "lpf-unfold-not-spec") in
      let rinfo =
        mk_rinfo_err ~goal:(RProg.LpfGoal placeholder_ce)
          pos delta bool_sort Effect.Spec err in
      let checked = RefinedExpr.mk_lpf rinfo (RefinedExpr.LUnfold (f, ce_arg)) in
      return (checked, placeholder_ce, delta, Constraint.top pos)
    end else
      let call_result = CoreExpr.mk (mk_info ret_sort) (CoreExpr.Call (f, ce_arg)) in
      let arg_typed_sort = Elaborate.lift_sort arg_sort in
      let ce_arg_annot =
        CoreExpr.mk (mk_info arg_sort) (CoreExpr.Annot (ce_arg, arg_typed_sort)) in
      let subst_body = Subst.apply_ce (Subst.extend_var param ce_arg_annot Subst.empty) body in
      let prop = mk_eq call_result subst_body in
      let rinfo = mk_rinfo ~goal:(RProg.LpfGoal prop) pos delta bool_sort Effect.Spec in
      let checked = RefinedExpr.mk_lpf rinfo (RefinedExpr.LUnfold (f, ce_arg)) in
      return (checked, prop, delta, Constraint.top pos)

  | RefinedExpr.LAnnot (lpf', se) ->
    let gamma = RCtx.erase delta in
    let* ce = elab_se_check rs gamma se bool_sort Effect.Spec in
    let* (checked_lpf', delta', ct) = check_lpf rs delta lpf' ce in
    let rinfo = mk_rinfo ~goal:(RProg.LpfGoal ce) pos delta bool_sort Effect.Spec in
    let checked = RefinedExpr.mk_lpf rinfo (RefinedExpr.LAnnot (checked_lpf', ce)) in
    return (checked, ce, delta', ct)

(* Logical fact checking: RS; Delta |- lpf <= ce -| Delta' ~> Ct *)
and check_lpf (rs : RSig.t) (delta : RCtx.t) (lpf : RefinedExpr.parsed_lpf) (ce : CoreExpr.typed_ce) : (checked_lpf * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RefinedExpr.lpf_info lpf in
  let pos = binfo#loc in
  match RefinedExpr.lpf_shape lpf with
  | RefinedExpr.LAuto ->
    let rinfo = mk_rinfo ~goal:(RProg.LpfGoal ce) pos delta bool_sort Effect.Spec in
    let checked = RefinedExpr.mk_lpf rinfo RefinedExpr.LAuto in
    return (checked, delta, Constraint.atom pos ce)

  | RefinedExpr.LHole h ->
    let rinfo = mk_rinfo ~goal:(RProg.LpfGoal ce) pos delta bool_sort Effect.Spec in
    let checked = RefinedExpr.mk_lpf rinfo (RefinedExpr.LHole h) in
    return (checked, delta, Constraint.top pos)

  | _ ->
    let* (checked_lpf, ce_synth, delta', ct) = synth_lpf rs delta lpf in
    return (checked_lpf, delta', Constraint.conj pos ct (Constraint.impl pos ce_synth (Constraint.atom pos ce)))

(* Resource fact synthesis: RS; Delta |- rpf => ce @ ce' -| Delta' ~> Ct *)
and synth_rpf (rs : RSig.t) (delta : RCtx.t) (rpf : RefinedExpr.parsed_rpf) : (checked_rpf * CoreExpr.typed_ce * CoreExpr.typed_ce * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RefinedExpr.rpf_info rpf in
  let pos = binfo#loc in
  match RefinedExpr.rpf_shape rpf with
  | RefinedExpr.RVar x ->
    let* (pred, value, delta') = lift_at pos (RCtx.use_resource x delta) in
    let rinfo = mk_rinfo ~goal:(RProg.RpfGoal (pred, value)) pos delta bool_sort Effect.Spec in
    let checked = RefinedExpr.mk_rpf rinfo (RefinedExpr.RVar x) in
    return (checked, pred, value, delta', Constraint.top pos)

  | RefinedExpr.RAnnot (rpf', se1, se2) ->
    (* Per the [res] well-formedness rule (doc/syntax.ott:1325-1327):
       synth the value [ce2] to obtain τ, then check the predicate
       [ce1] against sort [Pred τ]. Checking (not synth) gives
       constructs like [take]/[case]/[return] enough context to
       elaborate inside the annotation. *)
    let gamma = RCtx.erase delta in
    let* (ce2, sort2_r) = elab_se rs gamma Effect.Spec se2 in
    let sort2 = Result.value sort2_r ~default:bool_sort in
    let pred_sort =
      Sort.mk (object method loc = SourcePos.dummy end) (Sort.Pred sort2)
    in
    let* ce1 = elab_se_check rs gamma se1 pred_sort Effect.Spec in
    let* (checked_rpf', delta', ct) = check_rpf rs delta rpf' ce1 ce2 in
    let rinfo = mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2)) pos delta bool_sort Effect.Spec in
    let checked = RefinedExpr.mk_rpf rinfo (RefinedExpr.RAnnot (checked_rpf', ce1, ce2)) in
    return (checked, ce1, ce2, delta', ct)

  | RefinedExpr.RReturn _
  | RefinedExpr.RTake _
  | RefinedExpr.RFail _
  | RefinedExpr.RLet _
  | RefinedExpr.RCase _
  | RefinedExpr.RIfTrue _
  | RefinedExpr.RIfFalse _
  | RefinedExpr.RUnfold _
  | RefinedExpr.RAnnotStrip _ ->
    (* All check-only rpf forms are unsynthesizable; ascribe via [: pred @ value]. *)
    let construct = match RefinedExpr.rpf_shape rpf with
      | RefinedExpr.RReturn _ -> "return"
      | RefinedExpr.RTake _ -> "take"
      | RefinedExpr.RFail _ -> "fail"
      | RefinedExpr.RLet _ -> "let"
      | RefinedExpr.RCase _ -> "case"
      | RefinedExpr.RIfTrue _ -> "iftrue"
      | RefinedExpr.RIfFalse _ -> "iffalse"
      | RefinedExpr.RUnfold _ -> "unfold;"
      | RefinedExpr.RAnnotStrip _ -> "annot;"
      | _ -> "rpf"
    in
    let err = Error.cannot_synthesize ~loc:pos
                ~construct:(construct ^ " (add a : pred @ value annotation)") in
    let placeholder_pred =
      CoreExpr.mk (mk_info bool_sort)
        (CoreExpr.Hole ("rpf-" ^ construct ^ "-unsynth-pred")) in
    let placeholder_value =
      CoreExpr.mk (mk_info bool_sort)
        (CoreExpr.Hole ("rpf-" ^ construct ^ "-unsynth-value")) in
    let rinfo = mk_rinfo_err
      ~goal:(RProg.RpfGoal (placeholder_pred, placeholder_value))
      pos delta bool_sort Effect.Spec err in
    let placeholder =
      RefinedExpr.mk_rpf rinfo
        (RefinedExpr.RHole (construct ^ "-unsynth")) in
    return (placeholder, placeholder_pred, placeholder_value, delta,
            Constraint.top pos)
  | RefinedExpr.RHole h ->
    let err = Error.cannot_synthesize ~loc:pos
                ~construct:"hole (add a : pred @ value annotation)" in
    let placeholder_pred =
      CoreExpr.mk (mk_info bool_sort)
        (CoreExpr.Hole "rpf-hole-unsynth-pred") in
    let placeholder_value =
      CoreExpr.mk (mk_info bool_sort)
        (CoreExpr.Hole "rpf-hole-unsynth-value") in
    let rinfo = mk_rinfo_err
      ~goal:(RProg.RpfGoal (placeholder_pred, placeholder_value))
      pos delta bool_sort Effect.Spec err in
    let checked =
      RefinedExpr.mk_rpf rinfo (RefinedExpr.RHole h) in
    return (checked, placeholder_pred, placeholder_value, delta,
            Constraint.top pos)

(* Resource fact checking: RS; Delta |- rpf <= ce @ ce' -| Delta' ~> Ct *)
and check_rpf (rs : RSig.t) (delta : RCtx.t) (rpf : RefinedExpr.parsed_rpf) (ce1 : CoreExpr.typed_ce) (ce2 : CoreExpr.typed_ce) : (checked_rpf * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RefinedExpr.rpf_info rpf in
  let pos = binfo#loc in
  match RefinedExpr.rpf_shape rpf with
  | RefinedExpr.RHole h ->
    let delta' = RCtx.affinize delta in
    let rinfo = mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2)) pos delta (CoreExpr.sort_of_info (CoreExpr.info ce1)) Effect.Spec in
    let checked = RefinedExpr.mk_rpf rinfo (RefinedExpr.RHole h) in
    return (checked, delta', Constraint.top pos)

  | RefinedExpr.RUnfold rpf' ->
    (* Old [unfold rpf] semantics, preserved for Phase A.
       Phase E will rewrite this rule to match the new spec, where
       [unfold ; rpf] strips a function-call predicate and recurses
       on the body's substitution.  The body's logic is identical;
       only the surface keyword shape changed.

       [view_get_call_ce] returns the function name and argument as
       per-component errkinds; both must be Ok to look up the body
       and substitute.  When the predicate isn't a [Call], we emit
       a placeholder RHole rather than aborting elaboration.  The
       [Sig.lookup_fundef] [lift_at] on the [Ok] path is a deferred
       Phase 5 site. *)
    (match view_get_call_ce ~construct:"unfold" (strip_annots ce1) with
     | (Error k, _) | (_, Error k) ->
       let err = Error.structured ~loc:pos k in
       let rinfo =
         mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
           pos delta bool_sort Effect.Spec err in
       let inner_rinfo =
         mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2))
           pos delta bool_sort Effect.Spec in
       let inner_rpf = RefinedExpr.mk_rpf inner_rinfo
         (RefinedExpr.RHole "unfold-shape-mismatch") in
       let _ = rpf' in
       let checked =
         RefinedExpr.mk_rpf rinfo (RefinedExpr.RUnfold inner_rpf) in
       return (checked, delta, Constraint.top pos)
     | (Ok f, Ok ce_arg) ->
       let cs = RSig.comp rs in
       let* (param, arg_sort, _ret_sort, eff, body) =
         ElabM.lift_at pos (Sig.lookup_fundef f cs) in
       if not (Effect.sub eff Effect.Spec) then begin
         let err = Error.unfold_not_spec ~loc:pos ~name:f in
         let rinfo =
           mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
             pos delta bool_sort Effect.Spec err in
         let inner_rinfo =
           mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2))
             pos delta bool_sort Effect.Spec in
         let inner_rpf = RefinedExpr.mk_rpf inner_rinfo
           (RefinedExpr.RHole "unfold-not-spec") in
         let checked =
           RefinedExpr.mk_rpf rinfo (RefinedExpr.RUnfold inner_rpf) in
         let _ = rpf' in
         return (checked, delta, Constraint.top pos)
       end else
         let arg_typed_sort = Elaborate.lift_sort arg_sort in
         let ce_arg_annot =
           CoreExpr.mk (mk_info arg_sort)
             (CoreExpr.Annot (ce_arg, arg_typed_sort)) in
         let subst_body =
           Subst.apply_ce
             (Subst.extend_var param ce_arg_annot Subst.empty) body in
         let* (checked_rpf', delta', ct) =
           check_rpf rs delta rpf' subst_body ce2 in
         let rinfo =
           mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2))
             pos delta bool_sort Effect.Spec in
         let checked =
           RefinedExpr.mk_rpf rinfo (RefinedExpr.RUnfold checked_rpf') in
         return (checked, delta', ct))

  | RefinedExpr.RReturn lpf ->
    (* :: return — RS;Δ |- return lpf r<== return ce1 @ ce2 ↝ Ct
       requires lpf l<== ce1 == ce2.

       When the predicate isn't a [Return _], substitute a placeholder
       [Hole] for [ret_ce] so [check_lpf] still runs against a
       syntactically well-formed [eq_prop]; the shape error rides on
       this node's [info#answer], and we drop the constraint on the
       Error path (the equation is meaningless when one side is a
       hole). *)
    let ce1' = strip_annots ce1 in
    let ret_ce_r = view_get_return_ce ~construct:"return rpf" ce1' in
    let ret_ce =
      Result.value ret_ce_r
        ~default:(CoreExpr.mk (CoreExpr.info ce1)
                    (CoreExpr.Hole "return-shape")) in
    let eq_prop =
      CoreExpr.mk (CoreExpr.info ce1) (CoreExpr.Eq (ret_ce, ce2)) in
    let* (checked_lpf, delta', ct) = check_lpf rs delta lpf eq_prop in
    let ct =
      match ret_ce_r with Ok _ -> ct | Error _ -> Constraint.top pos in
    let answer = answer_of_sort_kind_r ~loc:pos
                   (Result.map (fun _ -> bool_sort) ret_ce_r) in
    let rinfo =
      mk_rinfo_with_answer ~goal:(RProg.RpfGoal (ce1, ce2))
        pos delta bool_sort Effect.Spec answer in
    let checked =
      RefinedExpr.mk_rpf rinfo (RefinedExpr.RReturn checked_lpf) in
    return (checked, delta', ct)

  | RefinedExpr.RTake (rpf1, rpf2) ->
    (* :: take — RS;Δ1 |- take(rpf1, rpf2) r<== (take x = ce_a; ce_b) @ ce2 ↝ Ct
       rpf1 r==> ce_a' @ ce_w (synth);  rpf2 r<== ce_b[ce_w/x] @ ce2 (check).

       The three components ([x], [ce_a], [ce_b]) all flow into
       downstream consumers that need real values (substitution and
       recursive [check_rpf]); when the view doesn't match they share
       fate and we emit a placeholder. *)
    let ce1' = strip_annots ce1 in
    (match view_get_take_ce ~construct:"take rpf" ce1' with
     | (Error k, _, _) | (_, Error k, _) | (_, _, Error k) ->
       let err = Error.structured ~loc:pos k in
       let rinfo =
         mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
           pos delta bool_sort Effect.Spec err in
       let checked =
         RefinedExpr.mk_rpf rinfo (RefinedExpr.RHole "take-shape-mismatch") in
       let _ = rpf1 and _ = rpf2 in
       return (checked, delta, Constraint.top pos)
     | (Ok x, Ok ce_a, Ok ce_b) ->
       let* (checked_rpf1, ce_a_synth, ce_w, delta1, ct1) =
         synth_rpf rs delta rpf1 in
       let ct_a_eq =
         Constraint.atom pos (mk_eq ce_a_synth ce_a) in
       let ce_w_sort = (CoreExpr.sort_of_info (CoreExpr.info ce_w)) in
       let arg_typed_sort = Elaborate.lift_sort ce_w_sort in
       let ce_w_annot =
         CoreExpr.mk (mk_info ce_w_sort)
           (CoreExpr.Annot (ce_w, arg_typed_sort)) in
       let sub = Subst.extend_var x ce_w_annot Subst.empty in
       let ce_b_subst = Subst.apply_ce sub ce_b in
       let* (checked_rpf2, delta2, ct2) =
         check_rpf rs delta1 rpf2 ce_b_subst ce2 in
       let ct = Constraint.conj pos ct_a_eq (Constraint.conj pos ct1 ct2) in
       let rinfo =
         mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2))
           pos delta bool_sort Effect.Spec in
       let checked =
         RefinedExpr.mk_rpf rinfo
           (RefinedExpr.RTake (checked_rpf1, checked_rpf2)) in
       return (checked, delta2, ct))

  | RefinedExpr.RFail lpf ->
    (* :: fail — RS;Δ |- fail[lpf] r<== fail @ ce ↝ Ct
       requires lpf l<== false; output Δ'' = affinize Δ'.

       [check_lpf] takes a real [false_ce] regardless of the shape
       check, so we run it unconditionally and ride the predicate-
       not-Fail error on this node's [info#answer]. *)
    let ce1' = strip_annots ce1 in
    let fail_check = view_get_fail_ce ~construct:"fail rpf" ce1' in
    let false_ce =
      CoreExpr.mk (CoreExpr.info ce1) (CoreExpr.BoolLit false) in
    let* (checked_lpf, delta', ct) = check_lpf rs delta lpf false_ce in
    let delta'' = RCtx.affinize delta' in
    let answer = answer_of_sort_kind_r ~loc:pos
                   (Result.map (fun () -> bool_sort) fail_check) in
    let rinfo =
      mk_rinfo_with_answer ~goal:(RProg.RpfGoal (ce1, ce2))
        pos delta bool_sort Effect.Spec answer in
    let checked = RefinedExpr.mk_rpf rinfo (RefinedExpr.RFail checked_lpf) in
    return (checked, delta'', ct)

  | RefinedExpr.RLet (lpat, cpat, rpf') ->
    (* :: let / let_tuple — RS;Δ0 |- let[lpat] cpat; rpf r<== ... ↝ Ct.
       Like rpat_match's RLet, dispatch on the predicate's shape after
       a shallow strip (alias-lets preserved) so the user's pattern
       matches the structural form they read from hover.  Two cases:
         - Let x = ce_a; ce_b   → single-binder, RChk_let.
         - LetTuple xs = ce; ce' → tuple-destructure, RChk_let_tuple. *)
    let ce1' = strip_annots_shallow ce1 in
    (match CoreExpr.shape ce1' with
     | CoreExpr.Let _ ->
       (match zip3_kind (view_get_let_ce ~construct:"let rpf" ce1' )with
        | Error k ->
          let err = Error.structured ~loc:pos k in
          let rinfo = mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
                        pos delta bool_sort Effect.Spec err in
          let checked = RefinedExpr.mk_rpf rinfo
            (RefinedExpr.RHole "let-rpf-shape-mismatch") in
          return (checked, delta, Constraint.top pos)
        | Ok (x, ce_a, ce_b) ->
          let tau = (CoreExpr.sort_of_info (CoreExpr.info ce_a)) in
          let* (typed_cp, delta1, ce_w) =
            cpat_match rs delta (Ok Effect.Spec) cpat (Ok tau) in
          let eq_prop =
            CoreExpr.mk (CoreExpr.info ce1) (CoreExpr.Eq (ce_w, ce_a)) in
          let* (typed_lp, delta2, ct1) =
            lpat_match rs delta1 lpat (Ok eq_prop) in
          let arg_typed_sort = Elaborate.lift_sort tau in
          let ce_w_annot =
            CoreExpr.mk (mk_info tau)
              (CoreExpr.Annot (ce_w, arg_typed_sort)) in
          let sub = Subst.extend_var x ce_w_annot Subst.empty in
          let ce_b_subst = Subst.apply_ce sub ce_b in
          let* (checked_rpf', delta_full, ct2) =
            check_rpf rs delta2 rpf' ce_b_subst ce2 in
          let n0 = RCtx.length delta in
          let (delta3, delta_pop) = RCtx.split n0 delta_full in
          let ct_pat_body = Constraint.conj pos ct1 ct2 in
          let ct_closed = close_ctx pos delta_pop ct_pat_body in
          let leak = not (RCtx.zero delta_pop) in
          let rinfo =
            if leak then
              let leftovers =
                List.filter_map (function
                  | RCtx.Res { var; pred; value; usage } when not (Usage.is_zero usage) ->
                    Some (Format.asprintf "@[<hov 2>%a : %a @@ %a [%a]@]"
                            Var.print var CoreExpr.print pred
                            CoreExpr.print value Usage.print usage)
                  | _ -> None) (RCtx.entries delta_pop) in
              let err = Error.let_pattern_resource_leak ~loc:pos ~leftovers in
              mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
                pos delta bool_sort Effect.Spec err
            else
              mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2))
                pos delta bool_sort Effect.Spec
          in
          let checked = RefinedExpr.mk_rpf rinfo
            (RefinedExpr.RLet (typed_lp, typed_cp, checked_rpf')) in
          let final_ct = if leak then Constraint.top pos else ct_closed in
          return (checked, delta3, final_ct))
     | CoreExpr.LetTuple _ ->
       (match zip3_kind (view_get_let_tuple_ce ~construct:"let-tuple rpf" ce1' )with
        | Error k ->
          let err = Error.structured ~loc:pos k in
          let rinfo = mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
                        pos delta bool_sort Effect.Spec err in
          let checked = RefinedExpr.mk_rpf rinfo
            (RefinedExpr.RHole "let-tuple-rpf-shape-mismatch") in
          return (checked, delta, Constraint.top pos)
        | Ok (xs, ce_a, ce_b) ->
          let n = List.length xs in
          let ce_a_sort = (CoreExpr.sort_of_info (CoreExpr.info ce_a)) in
          let* (typed_cp, delta1, ce_w) =
            cpat_match rs delta (Ok Effect.Spec) cpat (Ok ce_a_sort) in
          (* Extract per-component witnesses + sorts.  cpat_match's
             CTuple branch produces ce_w = Tuple [ce1; ...; cen]; for
             other shapes (CVar against a record sort, or arity
             mismatch) fall back to placeholder Holes. *)
          let sub_ces =
            match CoreExpr.shape ce_w with
            | CoreExpr.Tuple ces when List.length ces = n -> ces
            | _ ->
              List.init n
                (fun _ -> CoreExpr.mk (mk_info bool_sort)
                            (CoreExpr.Hole "let-tuple-rpf-cpat-mismatch")) in
          let sub_taus =
            match Sort.shape ce_a_sort with
            | Sort.Record taus when List.compare_length_with taus n = 0 ->
              taus
            | _ -> List.init n (fun _ -> bool_sort) in
          let eq_prop =
            CoreExpr.mk (CoreExpr.info ce1) (CoreExpr.Eq (ce_w, ce_a)) in
          let* (typed_lp, delta2, ct1) =
            lpat_match rs delta1 lpat (Ok eq_prop) in
          let sub =
            try
              List.fold_left2
                (fun acc xi (cei, taui) ->
                  let arg_typed_sort = Elaborate.lift_sort taui in
                  let cei_annot =
                    CoreExpr.mk (mk_info taui)
                      (CoreExpr.Annot (cei, arg_typed_sort)) in
                  Subst.extend_var xi cei_annot acc)
                Subst.empty xs (List.combine sub_ces sub_taus)
            with Invalid_argument _ -> Subst.empty in
          let ce_b_subst = Subst.apply_ce sub ce_b in
          let* (checked_rpf', delta_full, ct2) =
            check_rpf rs delta2 rpf' ce_b_subst ce2 in
          let n0 = RCtx.length delta in
          let (delta3, delta_pop) = RCtx.split n0 delta_full in
          let ct_pat_body = Constraint.conj pos ct1 ct2 in
          let ct_closed = close_ctx pos delta_pop ct_pat_body in
          let leak = not (RCtx.zero delta_pop) in
          let rinfo =
            if leak then
              let leftovers =
                List.filter_map (function
                  | RCtx.Res { var; pred; value; usage } when not (Usage.is_zero usage) ->
                    Some (Format.asprintf "@[<hov 2>%a : %a @@ %a [%a]@]"
                            Var.print var CoreExpr.print pred
                            CoreExpr.print value Usage.print usage)
                  | _ -> None) (RCtx.entries delta_pop) in
              let err = Error.let_pattern_resource_leak ~loc:pos ~leftovers in
              mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
                pos delta bool_sort Effect.Spec err
            else
              mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2))
                pos delta bool_sort Effect.Spec
          in
          let checked = RefinedExpr.mk_rpf rinfo
            (RefinedExpr.RLet (typed_lp, typed_cp, checked_rpf')) in
          let final_ct = if leak then Constraint.top pos else ct_closed in
          return (checked, delta3, final_ct))
     | _ ->
       let k = mismatch_ce_kind ~construct:"let rpf"
                 ~expected_shape:"let _ = _; _ or let (_, ..., _) = _; _"
                 ce1' in
       let err = Error.structured ~loc:pos k in
       let rinfo = mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
                     pos delta bool_sort Effect.Spec err in
       let checked = RefinedExpr.mk_rpf rinfo
         (RefinedExpr.RHole "let-rpf-wrong-shape") in
       return (checked, delta, Constraint.top pos))

  | RefinedExpr.RCase (lpat, label, cpat, rpf') ->
    (* :: case — RS;Δ0 |- case[lpat] L cpat; rpf r<== ce''' @ ce2 ↝ (is ce L) cand Ct
       where ce''' = case ce of {Li xi -> cei}.  Looks up branch L,
       takes its body cek.  Pattern bindings as in :: let. *)
    let ce1' = strip_annots ce1 in
    (match CoreExpr.shape ce1' with
     | CoreExpr.Case (scrutinee, branches) ->
       (match List.find_opt (fun (l, _, _, _) -> Label.compare l label = 0) branches with
        | None ->
          let case_labels = List.map (fun (l, _, _, _) -> l) branches in
          let err = Error.rcase_label_not_in_branches
                      ~loc:pos ~label ~case_labels in
          let rinfo = mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
                        pos delta bool_sort Effect.Spec err in
          let checked = RefinedExpr.mk_rpf rinfo
            (RefinedExpr.RHole "case-label-not-in-branches") in
          let delta' = RCtx.affinize delta in
          return (checked, delta', Constraint.top pos)
        | Some (_l, x_br, ce_br, _bi) ->
          let scrut_sort = (CoreExpr.sort_of_info (CoreExpr.info scrutinee)) in
          let cs = RSig.comp rs in
          (* Compose the sort projection and ctor lookup as a single
             errkind chain.  [payload_sort] is a placeholder when the
             chain errs; [cpat_match] handles errkind sort directly so
             the binders stay in scope for the body.  [CtorLookup] is
             still bundled — Phase 5 will refactor it. *)
          let payload_sort_r =
            Result.bind
              (SortGet.get_app ~construct:"case rpf scrutinee" scrut_sort)
              (fun (d, args) -> CtorLookup.lookup cs d label args) in
          let payload_sort = Result.value payload_sort_r ~default:bool_sort in
          let* (typed_cp, delta1, ce_w) =
            cpat_match rs delta (Ok Effect.Spec) cpat payload_sort_r in
          let inject_ce =
            CoreExpr.mk (CoreExpr.info scrutinee)
              (CoreExpr.Inject (label, ce_w)) in
          let eq_prop =
            CoreExpr.mk (CoreExpr.info ce1)
              (CoreExpr.Eq (inject_ce, scrutinee)) in
          let* (typed_lp, delta2, ct1) =
            lpat_match rs delta1 lpat (Ok eq_prop) in
          let payload_typed_sort = Elaborate.lift_sort payload_sort in
          let ce_w_annot =
            CoreExpr.mk (mk_info payload_sort)
              (CoreExpr.Annot (ce_w, payload_typed_sort)) in
          let sub = Subst.extend_var x_br ce_w_annot Subst.empty in
          let ce_br_subst = Subst.apply_ce sub ce_br in
          let* (checked_rpf', delta_full, ct2) =
            check_rpf rs delta2 rpf' ce_br_subst ce2 in
          let n0 = RCtx.length delta in
          let (delta3, delta_pop) = RCtx.split n0 delta_full in
          let ct_pat_body = Constraint.conj pos ct1 ct2 in
          let ct_closed = close_ctx pos delta_pop ct_pat_body in
          let leak = not (RCtx.zero delta_pop) in
          let ct_with_disc =
            Constraint.conj pos
              (Constraint.is_ pos label scrutinee)
              (if leak then Constraint.top pos else ct_closed) in
          let rinfo =
            if leak then
              let leftovers =
                List.filter_map (function
                  | RCtx.Res { var; pred; value; usage } when not (Usage.is_zero usage) ->
                    Some (Format.asprintf "@[<hov 2>%a : %a @@ %a [%a]@]"
                            Var.print var CoreExpr.print pred
                            CoreExpr.print value Usage.print usage)
                  | _ -> None) (RCtx.entries delta_pop) in
              let err = Error.let_pattern_resource_leak ~loc:pos ~leftovers in
              mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
                pos delta bool_sort Effect.Spec err
            else
              mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2))
                pos delta bool_sort Effect.Spec
          in
          let checked = RefinedExpr.mk_rpf rinfo
            (RefinedExpr.RCase (typed_lp, label, typed_cp, checked_rpf')) in
          return (checked, delta3, ct_with_disc))
     | _ ->
       let err = Error.structured ~loc:pos
         (Error.K_wrong_pred_shape
            { construct = "case rpf";
              expected_shape = "case _ of { ... }";
              got = CoreExpr.to_string ce1' }) in
       let rinfo = mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
                     pos delta bool_sort Effect.Spec err in
       let checked = RefinedExpr.mk_rpf rinfo
         (RefinedExpr.RHole "case-pred-shape") in
       let delta' = RCtx.affinize delta in
       return (checked, delta', Constraint.top pos))

  | RefinedExpr.RIfTrue rpf' ->
    (* :: iftrue — RS;Δ |- iftrue; rpf r<== (if ce1 then ce2 else ce3) @ ce4
       ↝ Ct_ce ce1 cand Ct  where rpf' r<== ce2 @ ce4 ↝ Ct *)
    let ce1' = strip_annots ce1 in
    (match view_get_if_ce ~construct:"iftrue rpf" ce1' with
     | (Error k, _, _) | (_, Error k, _) | (_, _, Error k) ->
       let err = Error.structured ~loc:pos k in
       let rinfo =
         mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
           pos delta bool_sort Effect.Spec err in
       let checked =
         RefinedExpr.mk_rpf rinfo (RefinedExpr.RHole "iftrue-shape-mismatch") in
       let _ = rpf' in
       return (checked, delta, Constraint.top pos)
     | (Ok ce_cond, Ok ce_t, Ok _ce_e) ->
       let* (checked_rpf', delta', ct) =
         check_rpf rs delta rpf' ce_t ce2 in
       let ct_full =
         Constraint.conj pos (Constraint.atom pos ce_cond) ct in
       let rinfo = mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2))
                     pos delta bool_sort Effect.Spec in
       let checked = RefinedExpr.mk_rpf rinfo
         (RefinedExpr.RIfTrue checked_rpf') in
       return (checked, delta', ct_full))

  | RefinedExpr.RIfFalse rpf' ->
    (* :: iffalse — symmetric to iftrue *)
    let ce1' = strip_annots ce1 in
    (match view_get_if_ce ~construct:"iffalse rpf" ce1' with
     | (Error k, _, _) | (_, Error k, _) | (_, _, Error k) ->
       let err = Error.structured ~loc:pos k in
       let rinfo =
         mk_rinfo_err ~goal:(RProg.RpfGoal (ce1, ce2))
           pos delta bool_sort Effect.Spec err in
       let checked =
         RefinedExpr.mk_rpf rinfo (RefinedExpr.RHole "iffalse-shape-mismatch") in
       let _ = rpf' in
       return (checked, delta, Constraint.top pos)
     | (Ok ce_cond, Ok _ce_t, Ok ce_e) ->
       let* (checked_rpf', delta', ct) =
         check_rpf rs delta rpf' ce_e ce2 in
       let not_ce = CoreExpr.mk (CoreExpr.info ce_cond)
                      (CoreExpr.App (Prim.Not, ce_cond)) in
       let ct_full =
         Constraint.conj pos (Constraint.atom pos not_ce) ct in
       let rinfo = mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2))
                     pos delta bool_sort Effect.Spec in
       let checked = RefinedExpr.mk_rpf rinfo
         (RefinedExpr.RIfFalse checked_rpf') in
       return (checked, delta', ct_full))

  | RefinedExpr.RAnnotStrip rpf' ->
    (* :: annot — RS;Δ |- annot; rpf r<== (ce:τ) @ ce' ↝ Ct
       where rpf r<== ce @ ce' ↝ Ct *)
    let ce1' = strip_annots ce1 in
    let* (checked_rpf', delta', ct) =
      check_rpf rs delta rpf' ce1' ce2 in
    let rinfo = mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2))
                  pos delta bool_sort Effect.Spec in
    let checked = RefinedExpr.mk_rpf rinfo
      (RefinedExpr.RAnnotStrip checked_rpf') in
    return (checked, delta', ct)

  | _ ->
    let* (checked_rpf, ce1_synth, ce2_synth, delta', ct) = synth_rpf rs delta rpf in
    let eq_ct = Constraint.conj pos (Constraint.atom pos (mk_eq ce1_synth ce1))
                                    (Constraint.atom pos (mk_eq ce2_synth ce2)) in
    return (checked_rpf, delta', Constraint.conj pos ct eq_ct)

(* Core refined term synthesis: RS; Delta |-[eff] crt => Pf -| Delta' ~> Ct *)
and synth_crt (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (crt : RefinedExpr.parsed_crt) : (checked_crt * (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t * RCtx.t * Constraint.typed_ct) ElabM.t =
  let* (checked, pf, delta', ct) = synth_crt_impl rs delta eff crt in
  let* () = assert_delta_below delta delta' in
  return (checked, pf, delta', ct)

and synth_crt_impl (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (crt : RefinedExpr.parsed_crt) : (checked_crt * (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RefinedExpr.crt_info crt in
  let pos = binfo#loc in
  match RefinedExpr.crt_shape crt with
  | RefinedExpr.CAnnot (crt', se_pf) ->
    let gamma = RCtx.erase delta in
    let* pf = elab_pf rs gamma eff se_pf in
    let* (checked_crt', delta', ct) = check_crt rs delta eff crt' pf in
    let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
    let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CAnnot (checked_crt', pf)) in
    return (checked, pf, delta', ct)

  | RefinedExpr.CCall (f, spine) ->
    let* rf = lookup_rf_m ~loc:pos rs f in
    let eff'' = Effect.purify eff in
    let* (checked_spine, pf, delta', ct) = check_spine rs delta eff'' spine rf in
    if not (Effect.sub rf.eff eff) then begin
      let err = Error.fun_effect_mismatch
                  ~loc:pos ~name:f ~declared:rf.eff ~required:eff in
      let rinfo = mk_rinfo_err ~goal:(RProg.CrtGoal pf) pos delta
                    (ProofSort.comp pf) eff err in
      let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CCall (f, checked_spine)) in
      return (checked, pf, delta', Constraint.top pos)
    end else
      let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
      let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CCall (f, checked_spine)) in
      return (checked, pf, delta', ct)

  | RefinedExpr.CPrimApp (prim, spine) ->
    let* rf = rprim_signature prim in
    let eff'' = Effect.purify eff in
    let* (checked_spine, pf, delta', ct) = check_spine rs delta eff'' spine rf in
    if not (Effect.sub rf.eff eff) then begin
      let err = Error.prim_effect_mismatch
                  ~loc:pos ~prim ~declared:rf.eff ~required:eff in
      let rinfo = mk_rinfo_err ~goal:(RProg.CrtGoal pf) pos delta
                    (ProofSort.comp pf) eff err in
      let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CPrimApp (prim, checked_spine)) in
      return (checked, pf, delta', Constraint.top pos)
    end else
      let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
      let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CPrimApp (prim, checked_spine)) in
      return (checked, pf, delta', ct)

  | RefinedExpr.CIter (se_pred, pat, crt1, crt2) ->
    let iter_pos = binfo#loc in
    (* Each cross-cutting check produces a result; we accumulate
       errors and surface them on the outer rinfo's
       answer/subterm_errors via mk_rinfo_full. *)
    let eff_check =
      if Effect.sub Effect.Impure eff then Ok ()
      else Error (Error.iter_requires_impure ~loc:iter_pos ~actual:eff) in
    (* Elaborate predicate at spec effect *)
    let* (ce_pred, pred_sort_r) = elab_and_synth rs delta Effect.Spec se_pred in
    let inner_sort_r =
      view_get_pred_sort ~construct:"iter" pred_sort_r in
    let inner_sort = Result.value inner_sort_r ~default:bool_sort in
    let dsort_args_r =
      Result.bind inner_sort_r (fun inner ->
        SortGet.get_app ~construct:"iter" inner) in
    let cs = RSig.comp rs in
    let next_label =
      match Label.of_string "Next" with
      | Ok l -> l
      | Error _ ->
        invariant_at iter_pos ~rule:"CIter"
          "Label.of_string \"Next\" failed — the literal \"Next\" \
           is always a valid constructor name"
    in
    let done_label =
      match Label.of_string "Done" with
      | Ok l -> l
      | Error _ ->
        invariant_at iter_pos ~rule:"CIter"
          "Label.of_string \"Done\" failed — the literal \"Done\" \
           is always a valid constructor name"
    in
    let a_sort_r =
      Result.bind dsort_args_r (fun (d, args) ->
        CtorLookup.lookup cs d next_label args) in
    let b_sort_r =
      Result.bind dsort_args_r (fun (d, args) ->
        CtorLookup.lookup cs d done_label args) in
    let a_sort = Result.value a_sort_r ~default:bool_sort in
    let b_sort = Result.value b_sort_r ~default:bool_sort in
    let step_sort = inner_sort in
    (* Build init proof sort: x:A [pure], y:ce @ Next(x) [res], pfnil *)
    let* x_var = fresh SourcePos.dummy in
    let ce_x = ce_of_var x_var a_sort in
    let ce_next_x = CoreExpr.mk (mk_info step_sort) (CoreExpr.Inject (next_label, ce_x)) in
    let init_pf = [
      ProofSort.Comp { info = rinfo_dummy; var = x_var; sort = a_sort; eff = Effect.Pure };
      ProofSort.Res { info = rinfo_dummy; pred = ce_pred; value = ce_next_x };
    ] in
    (* Check init (pure) *)
    let* (checked_crt1, delta', ct) = check_crt rs delta Effect.Pure crt1 init_pf in
    (* Build body input context: delta' + pattern bindings from init_pf *)
    let* (typed_pat_init, delta_pat, _ct_pat) =
      q_match rs delta' (Effect.purify eff) pat init_pf in
    let delta_body = RCtx.concat delta' delta_pat in
    (* Build body proof sort: z:D(A,B) [pure], y2:ce @ z [res], pfnil *)
    let* z_var = fresh SourcePos.dummy in
    let ce_z = ce_of_var z_var step_sort in
    let body_pf = [
      ProofSort.Comp { info = rinfo_dummy; var = z_var; sort = step_sort; eff = Effect.Pure };
      ProofSort.Res { info = rinfo_dummy; pred = ce_pred; value = ce_z };
    ] in
    (* Check body (impure) *)
    let* (checked_crt2, delta_out, ct') = check_crt rs delta_body Effect.Impure crt2 body_pf in
    (* Validate output context: extension resources must be consumed *)
    let n = RCtx.length delta' in
    let (_delta_base, delta_pat_out) = RCtx.split n delta_out in
    let leak_check =
      if RCtx.zero delta_pat_out then Ok ()
      else Error (Error.resource_leak ~loc:iter_pos ~name:None) in
    (* Build result proof sort: z:B [pure], y:ce @ Done(z) [res], pfnil *)
    let* zr_var = fresh SourcePos.dummy in
    let ce_zr = ce_of_var zr_var b_sort in
    let ce_done_z = CoreExpr.mk (mk_info step_sort) (CoreExpr.Inject (done_label, ce_zr)) in
    let result_pf = [
      ProofSort.Comp { info = rinfo_dummy; var = zr_var; sort = b_sort; eff = Effect.Pure };
      ProofSort.Res { info = rinfo_dummy; pred = ce_pred; value = ce_done_z };
    ] in
    (* Extract iter binder.  Per the C4 plan: when the pattern doesn't
       start with a QCore CVar, drop the forall and just use ct'.
       The QLog/QRes/QDepRes cases collapse to one "non-core
       pattern" diagnostic. *)
    let (x_pat_o, pat_shape_err_o) =
      match RPat.shape pat with
      | RPat.QCore (cp, _rest) ->
        (match RPatGet.get_cvar ~construct:"iter" cp with
         | Ok x -> (Some x, None)
         | Error k -> (None, Some (Error.structured ~loc:iter_pos k)))
      | RPat.QLog _ | RPat.QRes _ | RPat.QDepRes _ ->
        (None,
         Some (Error.iter_pattern_shape ~loc:iter_pos
                 ~got:"a non-core pattern"))
      | RPat.QNil ->
        (None,
         Some (Error.iter_pattern_shape ~loc:iter_pos
                 ~got:"an empty pattern"))
    in
    let result_ct = match x_pat_o with
      | Some x_pat ->
        Constraint.conj pos ct (Constraint.forall_ pos x_pat a_sort ct')
      | None ->
        Constraint.conj pos ct ct' in
    (* Aggregate all cross-cutting errors in source order. *)
    let acc_t errs r = match r with Ok _ -> errs | Error e -> e :: errs in
    let acc_k errs r = match r with
      | Ok _ -> errs
      | Error k -> Error.structured ~loc:iter_pos k :: errs in
    let errors_rev =
      let errs = [] in
      let errs = acc_t errs eff_check in
      let errs = acc_k errs inner_sort_r in
      let errs = acc_k errs (Result.map (fun _ -> ()) dsort_args_r) in
      let errs = acc_k errs a_sort_r in
      let errs = acc_k errs b_sort_r in
      let errs = acc_t errs leak_check in
      let errs = match pat_shape_err_o with
        | None -> errs
        | Some e -> e :: errs in
      errs in
    let errors = List.rev errors_rev in
    let rinfo =
      mk_rinfo_full ~goal:(RProg.CrtGoal result_pf)
        pos delta (ProofSort.comp result_pf) eff errors in
    (* Use the typed_q from q_match so any kind/length errors q_match
       attached ride along to LSP collection. *)
    let typed_pat = typed_pat_init in
    let checked =
      RefinedExpr.mk_crt rinfo
        (RefinedExpr.CIter (ce_pred, typed_pat, checked_crt1, checked_crt2)) in
    return (checked, result_pf, delta', result_ct)

  | RefinedExpr.CTuple spine ->
    let* (checked_spine, delta', ct) = _check_tuple rs delta eff spine [] in
    let rinfo = mk_rinfo pos delta (ProofSort.comp []) eff in
    let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CTuple checked_spine) in
    return (checked, [], delta', ct)

  | _ ->
    let err = Error.cannot_synthesize ~loc:binfo#loc
                ~construct:"proof sort" in
    let placeholder_pf = [] in
    let placeholder_sort =
      Sort.mk (object method loc = binfo#loc end) Sort.Bool in
    let rinfo = mk_rinfo_err ~goal:(RProg.CrtGoal placeholder_pf)
                  pos delta placeholder_sort eff err in
    let checked = RefinedExpr.mk_crt rinfo
      (RefinedExpr.CHole "crt-cannot-synthesize") in
    return (checked, placeholder_pf, delta, Constraint.top pos)

(* Core refined term checking: RS; Delta |-[eff] crt <= Pf -| Delta' ~> Ct *)
and check_crt (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (crt : RefinedExpr.parsed_crt) (pf : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t) : (checked_crt * RCtx.t * Constraint.typed_ct) ElabM.t =
  let* (checked, delta', ct) = check_crt_impl rs delta eff crt pf in
  let* () = assert_delta_below delta delta' in
  return (checked, delta', ct)

and check_crt_impl (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (crt : RefinedExpr.parsed_crt) (pf : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t) : (checked_crt * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RefinedExpr.crt_info crt in
  let pos = binfo#loc in
  match RefinedExpr.crt_shape crt with
  | RefinedExpr.CLet (pat, crt1, crt2) ->
    (* Per the [let] rule in [doc/syntax.ott] / [doc/refinement-types.md]:
         RS;Δ₀ ⊢[eff] let q = crt1; crt2 ⇐ Pf ⊣ Δ₂ ↝ Ct1 ∧ Ct''
       where  Ct'' = close Δ'''' Ct2.
       The body's constraint is closed under the pattern's bindings
       (Δ'''') so that comp/log entries introduced by the pattern act
       as hypotheses for subsequent atoms. *)
    let* (checked_crt1, pf', delta1, ct) = synth_crt rs delta eff crt1 in
    let eff_pat = Effect.purify eff in
    let* (typed_pat, delta2, ct_pat) = q_match rs delta1 eff_pat pat pf' in
    let* (checked_crt2, delta3, ct2) = check_crt rs delta2 eff crt2 pf in
    let n = RCtx.length delta1 in
    let (delta_out, delta_pat_out) = RCtx.split n delta3 in
    let leak =
      if not (RCtx.zero delta_pat_out) then
        let leftovers =
          List.filter_map (function
            | RCtx.Res { var; pred; value; usage } when not (Usage.is_zero usage) ->
              Some (Format.asprintf "@[<hov 2>%a : %a @@ %a [%a]@]"
                      Var.print var
                      CoreExpr.print pred
                      CoreExpr.print value
                      Usage.print usage)
            | _ -> None)
            (RCtx.entries delta_pat_out)
        in
        Some (Error.let_pattern_resource_leak ~loc:pos ~leftovers)
      else None
    in
    let ct_closed = close_ctx pos delta_pat_out (Constraint.conj pos ct_pat ct2) in
    let rinfo = match leak with
      | None -> mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff
      | Some err -> mk_rinfo_err ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff err
    in
    let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CLet (typed_pat, checked_crt1, checked_crt2)) in
    let final_ct = match leak with
      | None -> Constraint.conj pos ct ct_closed
      | Some _ -> Constraint.top pos
    in
    return (checked, delta_out, final_ct)

  | RefinedExpr.CLetLog (lp, lpf, body) ->
    (* Per [doc/extended-resource-terms.md] let-log rule:
       Synthesize lpf to get ce, pattern-match lpat against ce[log],
       check body, close (Δ'' ⇒ (C_pat ∧ C_body)). *)
    let* (checked_lpf, ce, delta1, ct) = synth_lpf rs delta lpf in
    let eff_pat = Effect.purify eff in
    let pf_log = [ProofSort.Log { info = rinfo_dummy; prop = ce }] in
    let lp_b = RPat.lpat_info lp in
    let* (typed_q, delta2, ct_pat) =
      q_match rs delta1 eff_pat
        (RPat.mk lp_b (RPat.QLog (lp, RPat.mk lp_b RPat.QNil))) pf_log in
    let* (checked_body, delta3, ct_body) = check_crt rs delta2 eff body pf in
    let n = RCtx.length delta1 in
    let (delta_out, delta_close) = RCtx.split n delta3 in
    let ct_closed = close_ctx pos delta_close (Constraint.conj pos ct_pat ct_body) in
    let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
    (* Extract typed_lp from the singleton typed_q; q_match wrapped lp
       in QLog (lp, QNil), so the shape is QLog (typed_lp, _).  Errors
       attached by lpat_match ride along on typed_lp. *)
    let typed_lp = match RPat.shape typed_q with
      | RPat.QLog (typed_lp, _) -> typed_lp
      | _ ->
        RPat.map_info_lpat
          (fun b -> mk_rinfo b#loc RCtx.empty bool_sort eff) lp in
    let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CLetLog (typed_lp, checked_lpf, checked_body)) in
    return (checked, delta_out, Constraint.conj pos ct ct_closed)

  | RefinedExpr.CLetRes (rp, rpf, body) ->
    (* Per [doc/extended-resource-terms.md] let-res rule:
       Synthesize rpf to get ce@ce', pattern-match rpat against ce@ce'[res],
       check body, verify resource consumption, close constraints. *)
    let* (checked_rpf, ce_pred, ce_val, delta1, ct) = synth_rpf rs delta rpf in
    let eff_pat = Effect.purify eff in
    let pf_res = [ProofSort.Res { info = rinfo_dummy; pred = ce_pred; value = ce_val }] in
    let rp_b = RPat.rpat_info rp in
    let* (typed_q, delta2, ct_pat) =
      q_match rs delta1 eff_pat
        (RPat.mk rp_b (RPat.QRes (rp, RPat.mk rp_b RPat.QNil))) pf_res in
    let* (checked_body, delta3, ct_body) = check_crt rs delta2 eff body pf in
    let n = RCtx.length delta1 in
    let (delta_out, delta_close) = RCtx.split n delta3 in
    let leak =
      if not (RCtx.zero delta_close) then
        let leftovers =
          List.filter_map (function
            | RCtx.Res { var; pred; value; usage } when not (Usage.is_zero usage) ->
              Some (Format.asprintf "@[<hov 2>%a : %a @@ %a [%a]@]"
                      Var.print var CoreExpr.print pred
                      CoreExpr.print value Usage.print usage)
            | _ -> None) (RCtx.entries delta_close)
        in
        Some (Error.let_pattern_resource_leak ~loc:pos ~leftovers)
      else None
    in
    let ct_closed = close_ctx pos delta_close (Constraint.conj pos ct_pat ct_body) in
    let rinfo = match leak with
      | None -> mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff
      | Some err -> mk_rinfo_err ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff err
    in
    (* Extract typed_rp from the singleton typed_q; q_match wrapped rp
       in QRes (rp, QNil).  Errors attached by rpat_match ride along
       on typed_rp. *)
    let typed_rp = match RPat.shape typed_q with
      | RPat.QRes (typed_rp, _) -> typed_rp
      | _ ->
        RPat.map_info_rpat
          (fun b -> mk_rinfo b#loc RCtx.empty bool_sort eff) rp in
    let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CLetRes (typed_rp, checked_rpf, checked_body)) in
    let final_ct = match leak with
      | None -> Constraint.conj pos ct ct_closed
      | Some _ -> Constraint.top pos
    in
    return (checked, delta_out, final_ct)

  | RefinedExpr.CIf (eq_var, se, crt1, crt2) ->
    let eff' = Effect.purify eff in
    let gamma = RCtx.erase delta in
    let* ce = elab_se_check rs gamma se bool_sort eff' in
    let delta_true = RCtx.extend_log eq_var (mk_eq ce mk_true) delta in
    let delta_false = RCtx.extend_log eq_var (mk_eq ce mk_false) delta in
    let* (checked_crt1, delta1_ext, ct1) = check_crt rs delta_true eff crt1 pf in
    let* (checked_crt2, delta2_ext, ct2) = check_crt rs delta_false eff crt2 pf in
    let n = RCtx.length delta in
    let (delta1, _) = RCtx.split n delta1_ext in
    let (delta2, _) = RCtx.split n delta2_ext in
    (* Branch-context merge: when the two branches' deltas don't match
       (kind/length mismatch), fall back to the affinized input delta
       and attach the merge error to the typed CIf so downstream
       processing (and the LSP hover index) keeps working. *)
    let merge_r = RCtx.merge delta1 delta2 in
    let (delta_merged, merge_errs) = match merge_r with
      | Ok d -> (d, [])
      | Error k -> (RCtx.affinize delta, [Error.structured ~loc:pos k]) in
    let ct = Constraint.conj pos
      (Constraint.impl pos (mk_eq ce mk_true) ct1)
      (Constraint.impl pos (mk_eq ce mk_false) ct2) in
    let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
    let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CIf (eq_var, ce, checked_crt1, checked_crt2)) in
    let checked = prepend_subterm_errors_crt merge_errs checked in
    return (checked, delta_merged, ct)

  | RefinedExpr.CCase (_y, se, branches) ->
    (* Scrutinee runs at the purified effect (cf. the surface [Case]
       rule in [lib/elaborate.ml:410-422]); branch bodies inherit the
       outer [eff] so impure work inside a case branch is permitted
       when the enclosing refined term is impure. *)
    let eff_scrut = Effect.purify eff in
    let gamma = RCtx.erase delta in
    let cs = RSig.comp rs in
    let* (ce, ce_sort_r) = elab_se rs gamma eff_scrut se in
    let ce_sort = Result.value ce_sort_r ~default:bool_sort in
    (match Sort.shape ce_sort with
     | Sort.App (dsort_name, args) ->
       let* decl =
         ElabM.lift_at pos (Sig.lookup_dsort_or_type dsort_name cs) in
       (* Instantiate the declared ctor sorts with the scrutinee's type
          arguments so [Cons : (a * List(a))] on a [List(Int)] scrutinee
          binds its payload at [(Int * List(Int))], not the generic
          [(a * List(a))]. *)
       let instantiate_ctors params ctors =
         let* subst = ElabM.lift_at pos (Subst.of_lists params args) in
         return (List.map (fun (l, s) -> (l, Subst.apply subst s)) ctors)
       in
       let* ctors =
         match decl with
         | Sig.LSortDecl decl ->
           instantiate_ctors decl.DsortDecl.params decl.DsortDecl.ctors
         | Sig.LTypeDecl decl ->
           instantiate_ctors decl.DtypeDecl.params decl.DtypeDecl.ctors
       in
       let* (checked_branches, delta', ct, merge_errs) =
         check_case_branches pos rs delta eff _y ce ce_sort ctors branches pf in
       let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
       let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CCase (_y, ce, checked_branches)) in
       let checked = prepend_subterm_errors_crt merge_errs checked in
       return (checked, delta', ct)
     | _ ->
       let err = Error.scrutinee_not_data ~loc:pos ~got:ce_sort in
       let rinfo = mk_rinfo_err ~goal:(RProg.CrtGoal pf) pos delta
                     (ProofSort.comp pf) eff err in
       let checked = RefinedExpr.mk_crt rinfo
         (RefinedExpr.CHole "case-scrutinee-not-data") in
       return (checked, delta, Constraint.top pos))

  | RefinedExpr.CTuple spine ->
    let* (checked_spine, delta', ct) = _check_tuple rs delta eff spine pf in
    let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
    let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CTuple checked_spine) in
    return (checked, delta', ct)

  | RefinedExpr.CExfalso ->
    let delta' = RCtx.affinize delta in
    let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
    let checked = RefinedExpr.mk_crt rinfo RefinedExpr.CExfalso in
    return (checked, delta', Constraint.bot pos)

  | RefinedExpr.CHole h ->
    let delta' = RCtx.affinize delta in
    let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
    let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CHole h) in
    return (checked, delta', Constraint.top pos)

  | RefinedExpr.CLetCore (lp, cp, se_ce, body) ->
    (* Per [doc/extended-resource-terms.md] let-core rule:
       Elaborate ce, build proof sort (y:τ[⌊eff⌋], y=ce[log]),
       pattern-match (cpat, lpat) against it, check body, close. *)
    let eff_pure = Effect.purify eff in
    let gamma = RCtx.erase delta in
    let* (ce, sort_r) = elab_se rs gamma eff_pure se_ce in
    let sort = Result.value sort_r ~default:bool_sort in
    let* y = fresh pos in
    let ce_y = ce_of_var y sort in
    let prop = mk_eq ce_y ce in
    let pf_core = [
      ProofSort.Comp { info = rinfo_dummy; var = y; sort; eff = eff_pure };
      ProofSort.Log { info = rinfo_dummy; prop }
    ] in
    let cp_b = RPat.cpat_info cp in
    let lp_b = RPat.lpat_info lp in
    let pat =
      RPat.mk cp_b
        (RPat.QCore (cp,
          RPat.mk lp_b
            (RPat.QLog (lp, RPat.mk lp_b RPat.QNil)))) in
    let* (typed_q, delta1, ct_pat) = q_match rs delta eff_pure pat pf_core in
    let* (checked_body, delta2, ct_body) = check_crt rs delta1 eff body pf in
    let n = RCtx.length delta in
    let (delta_out, delta_close) = RCtx.split n delta2 in
    let ct_closed = close_ctx pos delta_close (Constraint.conj pos ct_pat ct_body) in
    let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
    (* Extract typed_cp and typed_lp from the typed_q whose shape is
       QCore (typed_cp, QLog (typed_lp, QNil)).  Falls back to vanilla
       map_info if the shape doesn't match (shouldn't happen — q_match
       preserves the input shape). *)
    let (typed_cp, typed_lp) = match RPat.shape typed_q with
      | RPat.QCore (typed_cp, rest) ->
        (match RPat.shape rest with
         | RPat.QLog (typed_lp, _) -> (typed_cp, typed_lp)
         | _ ->
           (typed_cp,
            RPat.map_info_lpat
              (fun b -> mk_rinfo b#loc RCtx.empty bool_sort eff) lp))
      | _ ->
        (RPat.map_info_cpat
           (fun b -> mk_rinfo b#loc RCtx.empty sort eff) cp,
         RPat.map_info_lpat
           (fun b -> mk_rinfo b#loc RCtx.empty bool_sort eff) lp) in
    let checked =
      RefinedExpr.mk_crt rinfo
        (RefinedExpr.CLetCore (
           typed_lp,
           typed_cp,
           ce, checked_body))
    in
    return (checked, delta_out, ct_closed)

  | _ ->
    let* (checked_crt, pf', delta', ct) = synth_crt rs delta eff crt in
    let* (ct', pf_errs) = pf_eq pos rs delta' pf' pf in
    let checked_crt = prepend_subterm_errors_crt pf_errs checked_crt in
    return (checked_crt, delta', Constraint.conj pos ct ct')

(* Spine checking: RS; Delta |-[eff] rsp : Pf1 -o Pf2 >> Pf -| Delta' ~> Ct *)
and check_spine (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t) (spine : RefinedExpr.parsed_spine) (rf : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RFunType.t) : (checked_spine * (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t * RCtx.t * Constraint.typed_ct) ElabM.t =
  check_spine_inner rs delta eff spine rf.domain rf.codomain

and check_spine_inner rs delta eff spine domain codomain =
  let binfo = RefinedExpr.spine_info spine in
  let pos = binfo#loc in
  let spine_rinfo = mk_rinfo ~goal:(RProg.CrtGoal codomain) pos delta (ProofSort.comp codomain) eff in
  match RefinedExpr.spine_shape spine, domain with
  | RefinedExpr.SNil, [] ->
    let checked = RefinedExpr.mk_spine spine_rinfo RefinedExpr.SNil in
    return (checked, codomain, delta, Constraint.top pos)

  | RefinedExpr.SCore (se, rest), (ProofSort.Comp { info = _; var; sort; eff = Effect.Pure } :: pf_rest) ->
    let gamma = RCtx.erase delta in
    let* ce = elab_se_check rs gamma se sort eff in
    let pf_rest' = ProofSort.subst var ce pf_rest in
    let codomain' = ProofSort.subst var ce codomain in
    let* (checked_rest, result_pf, delta', ct) = check_spine_inner rs delta eff rest pf_rest' codomain' in
    let checked = RefinedExpr.mk_spine spine_rinfo (RefinedExpr.SCore (ce, checked_rest)) in
    return (checked, result_pf, delta', ct)

  | RefinedExpr.SCore (se, rest), (ProofSort.Comp { info = _; var; sort; eff = Effect.Spec } :: pf_rest) ->
    let gamma = RCtx.erase delta in
    let* ce = elab_se_check rs gamma se sort Effect.Spec in
    let pf_rest' = ProofSort.subst var ce pf_rest in
    let codomain' = ProofSort.subst var ce codomain in
    let* (checked_rest, result_pf, delta', ct) = check_spine_inner rs delta eff rest pf_rest' codomain' in
    let checked = RefinedExpr.mk_spine spine_rinfo (RefinedExpr.SCore (ce, checked_rest)) in
    return (checked, result_pf, delta', ct)

  | RefinedExpr.SCore (_, _), (ProofSort.Comp { info = _; sort; eff = Effect.Impure; _ } :: _) ->
    let err = Error.construct_sort_mismatch ~loc:pos
                ~construct:"function argument"
                ~expected_shape:"pure or spec effect"
                ~got:sort in
    let rinfo = mk_rinfo_err pos delta bool_sort eff err in
    let checked = RefinedExpr.mk_spine rinfo RefinedExpr.SNil in
    return (checked, codomain, delta, Constraint.top pos)

  | RefinedExpr.SLog (lpf, rest), (ProofSort.Log { info = _; prop } :: pf_rest) ->
    let* (checked_lpf, delta', ct) = check_lpf rs delta lpf prop in
    let* (checked_rest, result_pf, delta'', ct') = check_spine_inner rs delta' eff rest pf_rest codomain in
    let checked = RefinedExpr.mk_spine spine_rinfo (RefinedExpr.SLog (checked_lpf, checked_rest)) in
    return (checked, result_pf, delta'', Constraint.conj pos ct ct')

  | RefinedExpr.SRes (rpf, rest), (ProofSort.Res { info = _; pred; value } :: pf_rest) ->
    let* (checked_rpf, delta', ct) = check_rpf rs delta rpf pred value in
    let* (checked_rest, result_pf, delta'', ct') = check_spine_inner rs delta' eff rest pf_rest codomain in
    let checked = RefinedExpr.mk_spine spine_rinfo (RefinedExpr.SRes (checked_rpf, checked_rest)) in
    return (checked, result_pf, delta'', Constraint.conj pos ct ct')

  | RefinedExpr.SRes (rpf, rest), (ProofSort.DepRes { info = _; bound_var; pred } :: pf_rest) ->
    let* (checked_rpf, ce_pred_synth, ce_value, delta', ct) = synth_rpf rs delta rpf in
    let eq_ct = Constraint.atom pos (mk_eq pred ce_pred_synth) in
    let pf_rest' = ProofSort.subst bound_var ce_value pf_rest in
    let codomain' = ProofSort.subst bound_var ce_value codomain in
    let* (checked_rest, result_pf, delta'', ct') = check_spine_inner rs delta' eff rest pf_rest' codomain' in
    let checked = RefinedExpr.mk_spine spine_rinfo (RefinedExpr.SRes (checked_rpf, checked_rest)) in
    return (checked, result_pf, delta'', Constraint.conj pos (Constraint.conj pos ct eq_ct) ct')

  | _, entry :: _ ->
    let err = Error.spine_tag_mismatch ~loc:pos
                ~expected_tag:(pf_entry_tag_name entry)
                ~expected_entry:(pf_entry_to_string entry)
                ~actual_tag:(spine_tag_name (RefinedExpr.spine_shape spine)) in
    let rinfo = mk_rinfo_err pos delta bool_sort eff err in
    let checked = RefinedExpr.mk_spine rinfo RefinedExpr.SNil in
    return (checked, codomain, delta, Constraint.top pos)
  | _, [] ->
    let err = Error.spine_tag_mismatch ~loc:pos
                ~expected_tag:"end of arguments"
                ~expected_entry:"(no more parameters expected)"
                ~actual_tag:(spine_tag_name (RefinedExpr.spine_shape spine)) in
    let rinfo = mk_rinfo_err pos delta bool_sort eff err in
    let checked = RefinedExpr.mk_spine rinfo RefinedExpr.SNil in
    return (checked, codomain, delta, Constraint.top pos)

(* Tuple checking: RS; Delta |-[eff] rsp : Pf -| Delta' ~> Ct *)
and _check_tuple rs delta eff spine pf =
  let binfo = RefinedExpr.spine_info spine in
  let pos = binfo#loc in
  let tuple_rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
  match RefinedExpr.spine_shape spine, pf with
  | RefinedExpr.SNil, [] ->
    let checked = RefinedExpr.mk_spine tuple_rinfo RefinedExpr.SNil in
    return (checked, delta, Constraint.top pos)

  | RefinedExpr.SCore (se, rest), (ProofSort.Comp { info = _; var; sort; eff = entry_eff } :: pf_rest) ->
    let gamma = RCtx.erase delta in
    let check_eff = match entry_eff with Effect.Spec -> Effect.Spec | _ -> eff in
    let* ce = elab_se_check rs gamma se sort check_eff in
    let pf_rest' = ProofSort.subst var ce pf_rest in
    let* (checked_rest, delta', ct) = _check_tuple rs delta eff rest pf_rest' in
    let checked = RefinedExpr.mk_spine tuple_rinfo (RefinedExpr.SCore (ce, checked_rest)) in
    return (checked, delta', ct)

  | RefinedExpr.SLog (lpf, rest), (ProofSort.Log { info = _; prop } :: pf_rest) ->
    let* (checked_lpf, delta', ct) = check_lpf rs delta lpf prop in
    let* (checked_rest, delta'', ct') = _check_tuple rs delta' eff rest pf_rest in
    let checked = RefinedExpr.mk_spine tuple_rinfo (RefinedExpr.SLog (checked_lpf, checked_rest)) in
    return (checked, delta'', Constraint.conj pos ct ct')

  | RefinedExpr.SRes (rpf, rest), (ProofSort.Res { info = _; pred; value } :: pf_rest) ->
    let* (checked_rpf, delta', ct) = check_rpf rs delta rpf pred value in
    let* (checked_rest, delta'', ct') = _check_tuple rs delta' eff rest pf_rest in
    let checked = RefinedExpr.mk_spine tuple_rinfo (RefinedExpr.SRes (checked_rpf, checked_rest)) in
    return (checked, delta'', Constraint.conj pos ct ct')

  | RefinedExpr.SRes (rpf, rest), (ProofSort.DepRes { info = _; bound_var; pred } :: pf_rest) ->
    let* (checked_rpf, ce_pred_synth, ce_value, delta', ct) = synth_rpf rs delta rpf in
    let eq_ct = Constraint.atom pos (mk_eq pred ce_pred_synth) in
    let pf_rest' = ProofSort.subst bound_var ce_value pf_rest in
    let* (checked_rest, delta'', ct') = _check_tuple rs delta' eff rest pf_rest' in
    let checked = RefinedExpr.mk_spine tuple_rinfo (RefinedExpr.SRes (checked_rpf, checked_rest)) in
    return (checked, delta'', Constraint.conj pos (Constraint.conj pos ct eq_ct) ct')

  | _, entry :: _ ->
    let err = Error.spine_tag_mismatch ~loc:pos
                ~expected_tag:(pf_entry_tag_name entry)
                ~expected_entry:(pf_entry_to_string entry)
                ~actual_tag:(spine_tag_name (RefinedExpr.spine_shape spine)) in
    let rinfo = mk_rinfo_err pos delta bool_sort eff err in
    let checked = RefinedExpr.mk_spine rinfo RefinedExpr.SNil in
    return (checked, delta, Constraint.top pos)
  | _, [] ->
    let err = Error.spine_tag_mismatch ~loc:pos
                ~expected_tag:"end of entries"
                ~expected_entry:"(no more entries expected)"
                ~actual_tag:(spine_tag_name (RefinedExpr.spine_shape spine)) in
    let rinfo = mk_rinfo_err pos delta bool_sort eff err in
    let checked = RefinedExpr.mk_spine rinfo RefinedExpr.SNil in
    return (checked, delta, Constraint.top pos)

(* Case branch checking.

   Returns [(checked_branches, delta_merged, ct, merge_errs)].
   On a successful branch-context merge, [merge_errs = []].  When the
   branches' deltas don't match (length / kind / usage mismatch),
   [merge_errs] carries the structural error and [delta_merged] falls
   back to the affinized input [delta] so downstream processing
   continues — the caller folds [merge_errs] into the typed CCase's
   subterm_errors via [prepend_subterm_errors_crt]. *)
and check_case_branches pos rs delta eff eq_var ce ce_sort ctors branches pf =
  let* (checked_branches, branch_results) = check_branches_list pos rs delta eff eq_var ce ce_sort ctors branches pf in
  let (deltas, cts) = List.split branch_results in
  let merge_r = RCtx.merge_n deltas in
  let (delta_merged, merge_errs) = match merge_r with
    | Ok d -> (d, [])
    | Error k -> (RCtx.affinize delta, [Error.structured ~loc:pos k]) in
  let ct = List.fold_left (Constraint.conj pos) (Constraint.top pos) cts in
  return (checked_branches, delta_merged, ct, merge_errs)

and check_branches_list pos rs delta eff eq_var ce _ce_sort ctors branches pf =
  (* Payload binders bind at the purified effect (they are
     decompositions of a pure scrutinee value); branch bodies run at
     the outer effect so they retain access to impure operations. *)
  let eff_binder = Effect.purify eff in
  let rec go = function
    | [] -> return ([], [])
    | (label, ctor_sort) :: rest_ctors ->
      let branch = List.find_opt (fun (l, _, _, _) -> Label.compare l label = 0) branches in
      (match branch with
       | None ->
         (* Multi-error: missing branch becomes a synthesized branch
            with a CHole body and the non_exhaustive diagnostic
            attached to its rinfo.  The constraint contribution is
            Top so SMT skips this branch's constraint contribution. *)
         let witness = PatWitness.Ctor (label, PatWitness.Wild) in
         let err = Error.non_exhaustive ~loc:pos ~witness in
         let (x, _) = Var.mk "<missing>" pos Var.empty_supply in
         let eff_binder = Effect.purify eff in
         let delta_ext = RCtx.extend_comp x ctor_sort eff_binder delta in
         let body_rinfo = mk_rinfo_err pos delta_ext bool_sort eff err in
         let body =
           RefinedExpr.mk_crt body_rinfo
             (RefinedExpr.CHole
                (Format.asprintf "missing-rcase-%a" Label.print label))
         in
         let branch_rinfo = mk_rinfo_err pos delta ctor_sort eff err in
         let n = RCtx.length delta in
         let (delta_base, _) = RCtx.split n delta in
         let* (rest_checked, rest) = go rest_ctors in
         return ((label, branch_rinfo, x, body) :: rest_checked,
                 (delta_base, Constraint.top pos) :: rest)
       | Some (_, _b, x, body) ->
         let ctor_sort' = ctor_sort in
         let eq_prop = mk_eq ce (CoreExpr.mk (mk_info (CoreExpr.sort_of_info (CoreExpr.info ce))) (CoreExpr.Inject (label, ce_of_var x ctor_sort'))) in
         let delta_ext = RCtx.extend_comp x ctor_sort' eff_binder
                           (RCtx.extend_log eq_var eq_prop delta) in
         let* (checked_body, delta_out, ct) = check_crt rs delta_ext eff body pf in
         let n = RCtx.length delta in
         let (delta_base, _) = RCtx.split n delta_out in
         let ct' = Constraint.forall_ pos x ctor_sort' (Constraint.impl pos eq_prop ct) in
         let branch_rinfo = mk_rinfo pos delta ctor_sort' eff in
         let* (rest_checked, rest) = go rest_ctors in
         return ((label, branch_rinfo, x, checked_body) :: rest_checked, (delta_base, ct') :: rest))
  in
  go ctors

(* Proof sort equality: RS; Delta |- Pf1 = Pf2 ~> Ct

   Returns the equality constraint together with a list of any
   structural-mismatch errors encountered; a non-empty list means the
   constraint is best-effort (typically Constraint.top contributions
   for the mismatched portion).  The single caller folds the errors
   into the synthesizing crt's outer rinfo via
   prepend_subterm_errors_crt. *)
and pf_eq (pos : SourcePos.t) (rs : RSig.t) (delta : RCtx.t)
    (pf1 : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t)
    (pf2 : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t)
  : (Constraint.typed_ct * Error.t list) ElabM.t =
  let _cs = RSig.comp rs in
  let _gamma = RCtx.erase delta in
  let rec go pf1 pf2 =
    match pf1, pf2 with
    | [], [] -> return (Constraint.top pos, [])

    | ProofSort.Comp { info = _; var = x; sort; eff } :: rest1,
      ProofSort.Comp { info = _; var = y; sort = sort2; eff = eff2 } :: rest2 ->
      let sort_check =
        if Sort.compare sort sort2 = 0 then []
        else [Error.sort_mismatch ~loc:pos ~expected:sort ~actual:sort2] in
      let eff_check =
        if Effect.compare eff eff2 = 0 then []
        else [Error.pf_effect_mismatch ~loc:pos ~sort
                ~synthesized_eff:eff ~expected_eff:eff2] in
      let rest2' = ProofSort.subst y (ce_of_var x sort) rest2 in
      let* (ct, errs_rest) = go rest1 rest2' in
      let here_errs = sort_check @ eff_check in
      return (Constraint.forall_ pos x sort ct, here_errs @ errs_rest)

    | ProofSort.Log { info = _; prop = ce1 } :: rest1,
      ProofSort.Log { info = _; prop = ce2 } :: rest2 ->
      let* (ct, errs_rest) = go rest1 rest2 in
      return (Constraint.conj pos (Constraint.atom pos (mk_eq ce1 ce2)) ct,
              errs_rest)

    | ProofSort.Res { info = _; pred = p1; value = v1 } :: rest1,
      ProofSort.Res { info = _; pred = p2; value = v2 } :: rest2 ->
      let* (ct, errs_rest) = go rest1 rest2 in
      let eq_ct = Constraint.conj pos (Constraint.atom pos (mk_eq p1 p2))
                                      (Constraint.atom pos (mk_eq v1 v2)) in
      return (Constraint.conj pos eq_ct ct, errs_rest)

    | ProofSort.DepRes { info = _; bound_var = y1; pred = ce1 } :: rest1,
      ProofSort.DepRes { info = _; bound_var = y2; pred = ce2 } :: rest2 ->
      let pred_sort = (CoreExpr.sort_of_info (CoreExpr.info ce1)) in
      let inner_sort_r =
        SortGet.get_pred ~construct:"dependent resource" pred_sort in
      let* z = fresh SourcePos.dummy in
      (match inner_sort_r with
       | Ok inner_sort ->
         let ce_z = ce_of_var z inner_sort in
         let rest1' = ProofSort.subst y1 ce_z rest1 in
         let rest2' = ProofSort.subst y2 ce_z rest2 in
         let* (ct, errs_rest) = go rest1' rest2' in
         return
           (Constraint.conj pos (Constraint.atom pos (mk_eq ce1 ce2))
              (Constraint.forall_ pos z inner_sort ct),
            errs_rest)
       | Error k ->
         (* Inner sort unknown: skip the substitution chain but still
            walk both tails so trailing mismatches surface. *)
         let* (ct, errs_rest) = go rest1 rest2 in
         return (ct, Error.structured ~loc:pos k :: errs_rest))

    | e1 :: rest1, e2 :: rest2 ->
      let err =
        Error.pf_structure_mismatch ~loc:pos
          ~synthesized_entry:(pf_entry_to_string e1)
          ~expected_entry:(pf_entry_to_string e2) in
      let* (ct, errs_rest) = go rest1 rest2 in
      return (ct, err :: errs_rest)

    | [], (e :: _ as rest2) ->
      let err =
        Error.pf_structure_mismatch ~loc:pos
          ~synthesized_entry:"(end)"
          ~expected_entry:(pf_entry_to_string e) in
      (* Trailing pf2 entries: no useful constraint contribution. *)
      let _ = rest2 in
      return (Constraint.top pos, [err])

    | (e :: _ as rest1), [] ->
      let err =
        Error.pf_structure_mismatch ~loc:pos
          ~synthesized_entry:(pf_entry_to_string e)
          ~expected_entry:"(end)" in
      let _ = rest1 in
      return (Constraint.top pos, [err])
  in
  go pf1 pf2

(* ===== Pattern matching judgements =====

   Four mutually-recursive functions implement the four pattern-
   matching judgements from [doc/syntax.ott]:

   - [cpat_match]  RS; Δ |-[eff] cpat : τ      -| Δ' ~~> ce
   - [lpat_match]  RS; Δ |-      lpat : φ      -| Δ' ~~> Ct
   - [rpat_match]  RS; Δ |-[eff] rpat : ce@ce' -| Δ' ~~> Ct
   - [q_match]     RS; Δ |-[eff] q    : Pf     -| Δ' ~~> Ct

   Each judgement pattern-matches on the shape of the pattern, then
   uses a View ([SortView] for sorts, [CoreExprView] for propositions
   /predicates) to inspect the expected shape.  Errors at the pattern
   level (sort mismatch, predicate-shape mismatch, length mismatch,
   kind mismatch) are attached to the appropriate pattern node's
   [info#answer] via attach-and-continue.  *)

and cpat_match (rs : RSig.t) (delta : RCtx.t)
    (eff : (Effect.t, Error.kind) result)
    (cp : (_, Var.t) RPat.cpat)
    (sort : (Sort.sort, Error.kind) result)
  : ((RProg.typed_rinfo, Var.t) RPat.cpat * RCtx.t * CoreExpr.typed_ce) ElabM.t =
  let binfo = RPat.cpat_info cp in
  let pos = binfo#loc in
  let _ = rs in
  let eff' = Result.map Effect.purify eff in
  let placeholder_sort = Result.value sort ~default:bool_sort in
  let placeholder_eff = Result.value eff' ~default:Effect.Pure in
  let answer = answer_of_sort_kind_r ~loc:pos sort in
  match RPat.cpat_shape cp with
  | RPat.CVar x ->
    let delta' = extend_comp_opt (Some x) sort eff' delta in
    let ce_x = ce_of_var x placeholder_sort in
    let info =
      mk_rinfo_with_answer pos delta' placeholder_sort placeholder_eff answer in
    let typed_cp = RPat.mk_cpat info (RPat.CVar x) in
    return (typed_cp, delta', ce_x)
  | RPat.CTuple cps ->
    let n = List.length cps in
    let component_sort_results =
      view_get_record_sorts ~construct:"tuple pattern" n sort in
    let pairs = List.combine cps component_sort_results in
    let rec loop pairs delta_acc typed_acc ce_acc =
      match pairs with
      | [] ->
        return (List.rev typed_acc, delta_acc, List.rev ce_acc)
      | (cp, sr) :: rest ->
        let* (typed_cp, delta', ce_w) =
          cpat_match rs delta_acc eff cp sr in
        loop rest delta' (typed_cp :: typed_acc) (ce_w :: ce_acc)
    in
    let* (typed_cps, delta', ces) = loop pairs delta [] [] in
    let tuple_ce =
      CoreExpr.mk (mk_info placeholder_sort) (CoreExpr.Tuple ces) in
    let info =
      mk_rinfo_with_answer pos delta' placeholder_sort placeholder_eff answer in
    let typed_tuple = RPat.mk_cpat info (RPat.CTuple typed_cps) in
    return (typed_tuple, delta', tuple_ce)

and lpat_match (rs : RSig.t) (delta : RCtx.t)
    (lp : (_, Var.t) RPat.lpat)
    (prop : (CoreExpr.typed_ce, Error.kind) result)
  : ((RProg.typed_rinfo, Var.t) RPat.lpat * RCtx.t * Constraint.typed_ct) ElabM.t =
  let _ = rs in
  let binfo = RPat.lpat_info lp in
  let pos = binfo#loc in
  match RPat.lpat_shape lp with
  | RPat.LVar x ->
    let delta' = extend_log_opt (Some x) prop delta in
    let info = mk_rinfo pos delta' bool_sort Effect.Spec in
    let typed_lp = RPat.mk_lpat info (RPat.LVar x) in
    return (typed_lp, delta', Constraint.top pos)
  | RPat.LAuto ->
    let info = mk_rinfo pos delta bool_sort Effect.Spec in
    let typed_lp = RPat.mk_lpat info RPat.LAuto in
    let ct = match prop with
      | Ok p -> Constraint.atom pos p
      | Error _ -> Constraint.top pos in
    return (typed_lp, delta, ct)

and rpat_match (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t)
    (rp : (_, Var.t) RPat.rpat)
    (pred : (CoreExpr.typed_ce, Error.kind) result)
    (value : (CoreExpr.typed_ce, Error.kind) result)
  : ((RProg.typed_rinfo, Var.t) RPat.rpat * RCtx.t * Constraint.typed_ct) ElabM.t =
  let binfo = RPat.rpat_info rp in
  let pos = binfo#loc in
  match pred, value with
  | (Error e, _) | (_, Error e) ->
    (* Input cascade: pred or value is fundamentally Error.  Build
       typed_rp with Error annotations throughout. *)
    let (typed_rp, delta') = error_rp_blanket rp delta eff e in
    return (typed_rp, delta', Constraint.top pos)
  | Ok pred, Ok value ->
  let cs = RSig.comp rs in
  let pred' = strip_annots pred in
  let pred_info = CoreExpr.info pred in
  match RPat.rpat_shape rp with
  | RPat.RVar x ->
    let delta' = RCtx.extend_res x pred value Usage.Avail delta in
    let info = mk_rinfo pos delta' bool_sort eff in
    let typed_rp = RPat.mk_rpat info (RPat.RVar x) in
    return (typed_rp, delta', Constraint.top pos)

  | RPat.RReturn lpat ->
    (match view_get_return_ce ~construct:"return pattern" pred' with
     | Error k ->
       let (typed_rp, delta') = error_rp_blanket rp delta eff k in
       return (typed_rp, delta', Constraint.top pos)
     | Ok ret_ce ->
       let eq_ce = CoreExpr.mk pred_info (CoreExpr.Eq (ret_ce, value)) in
       let* (typed_lp, delta', ct) = lpat_match rs delta lpat (Ok eq_ce) in
       let info = mk_rinfo pos delta' bool_sort eff in
       let typed_rp = RPat.mk_rpat info (RPat.RReturn typed_lp) in
       return (typed_rp, delta', ct))

  | RPat.RTake (cpat, rp1, rp2) ->
    (match zip3_kind (view_get_take_ce ~construct:"take pattern" pred' )with
     | Error k ->
       let (typed_rp, delta') = error_rp_blanket rp delta eff k in
       return (typed_rp, delta', Constraint.top pos)
     | Ok (x, ce1, ce2) ->
       let ce1_sort = (CoreExpr.sort_of_info (CoreExpr.info ce1)) in
       let inner_sort_r =
         SortGet.get_pred ~construct:"take pattern" ce1_sort in
       let inner_sort = Result.value inner_sort_r ~default:bool_sort in
       let ce_x = ce_of_var x inner_sort in
       let* (typed_cp, delta1, _) =
         cpat_match rs delta (Ok eff) cpat inner_sort_r in
       let* (typed_rp1, delta2, ct1) =
         rpat_match rs delta1 eff rp1 (Ok ce1) (Ok ce_x) in
       let* (typed_rp2, delta3, ct2) =
         rpat_match rs delta2 eff rp2 (Ok ce2) (Ok value) in
       let ct = Constraint.conj pos ct1 ct2 in
       let answer = answer_of_sort_kind_r ~loc:pos
                      (Result.map (fun _ -> bool_sort) inner_sort_r) in
       let info =
         mk_rinfo_with_answer pos delta3 bool_sort eff answer in
       let typed_rp =
         RPat.mk_rpat info (RPat.RTake (typed_cp, typed_rp1, typed_rp2)) in
       return (typed_rp, delta3, ct))

  | RPat.RFail lpat ->
    let fail_check = view_get_fail_ce ~construct:"fail pattern" pred' in
    let false_ce = CoreExpr.mk pred_info (CoreExpr.BoolLit false) in
    let* (typed_lp, delta', _ct) =
      lpat_match rs delta lpat (Ok false_ce) in
    let ct = match fail_check with
      | Ok () -> Constraint.bot pos
      | Error _ -> Constraint.top pos in
    let answer = answer_of_sort_kind_r ~loc:pos
                   (Result.map (fun () -> bool_sort) fail_check) in
    let info = mk_rinfo_with_answer pos delta' bool_sort eff answer in
    let typed_rp = RPat.mk_rpat info (RPat.RFail typed_lp) in
    return (typed_rp, delta', ct)

  | RPat.RLet (lpat, cpat, rp_inner) ->
    (* RLet matches the user's pattern against the structural form
       of the predicate, which the user reads from hover.  Hover
       displays the raw form (alias-lets preserved), so we use
       [strip_annots_shallow] here to keep [Let (y, Var x, body)]
       wrappers visible — [strip_annots] would inline them and the
       rpat would falsely fail to match.

       Two predicate shapes apply (matching [doc/syntax.ott]'s
       RPM_let and RPM_let_tuple):
         - [Let x = ce; ce']: single-binder let.
         - [LetTuple (x1,..,xn) = ce; ce']: tuple-destructuring let.
       Dispatch on the predicate shape; the cpat is whatever the
       user wrote (CVar or CTuple), and cpat_match handles both. *)
    let pred_for_let = strip_annots_shallow pred in
    (match CoreExpr.shape pred_for_let with
     | CoreExpr.Let _ ->
       (match zip3_kind (view_get_let_ce ~construct:"let pattern" pred_for_let )with
        | Error k ->
          let (typed_rp, delta') = error_rp_blanket rp delta eff k in
          return (typed_rp, delta', Constraint.top pos)
        | Ok (x, ce1, ce2) ->
          let sort = (CoreExpr.sort_of_info (CoreExpr.info ce1)) in
          let* (typed_cp, delta1, ce_w) =
            cpat_match rs delta (Ok eff) cpat (Ok sort) in
          let eq_ce = CoreExpr.mk pred_info (CoreExpr.Eq (ce_w, ce1)) in
          let* (typed_lp, delta2, ct1) =
            lpat_match rs delta1 lpat (Ok eq_ce) in
          (* Substitute (ce_w : sort) / x into ce2 *)
          let arg_typed_sort = Elaborate.lift_sort sort in
          let ce_w_annot =
            CoreExpr.mk (mk_info sort) (CoreExpr.Annot (ce_w, arg_typed_sort)) in
          let sub = Subst.extend_var x ce_w_annot Subst.empty in
          let ce2_subst = Subst.apply_ce sub ce2 in
          let* (typed_inner, delta3, ct2) =
            rpat_match rs delta2 eff rp_inner (Ok ce2_subst) (Ok value) in
          let ct = Constraint.conj pos ct1 ct2 in
          let info = mk_rinfo pos delta3 bool_sort eff in
          let typed_rp =
            RPat.mk_rpat info (RPat.RLet (typed_lp, typed_cp, typed_inner)) in
          return (typed_rp, delta3, ct))
     | CoreExpr.LetTuple _ ->
       (match zip3_kind (view_get_let_tuple_ce ~construct:"let-tuple pattern" pred_for_let )with
        | Error k ->
          let (typed_rp, delta') = error_rp_blanket rp delta eff k in
          return (typed_rp, delta', Constraint.top pos)
        | Ok (xs, ce, ce') ->
          let n = List.length xs in
          let ce_sort = (CoreExpr.sort_of_info (CoreExpr.info ce)) in
          (* Match cpat (typically CTuple of arity n) against ce's
             record sort.  cpat_match handles arity mismatch / CVar
             via its existing CTuple/CVar branches and the
             view_get_record_sorts contract. *)
          let* (typed_cp, delta1, ce_w) =
            cpat_match rs delta (Ok eff) cpat (Ok ce_sort) in
          (* Extract per-component witnesses and sorts.  cpat_match's
             CTuple branch returns ce_w = Tuple [ce1; ...; cen].  If
             cpat was a CVar (or arity-mismatched CTuple), fall back
             to placeholders. *)
          let sub_ces =
            match CoreExpr.shape ce_w with
            | CoreExpr.Tuple ces when List.length ces = n -> ces
            | _ ->
              List.init n
                (fun _ -> CoreExpr.mk (mk_info bool_sort)
                            (CoreExpr.Hole "let-tuple-cpat-mismatch")) in
          let sub_taus =
            match Sort.shape ce_sort with
            | Sort.Record taus when List.compare_length_with taus n = 0 ->
              taus
            | _ -> List.init n (fun _ -> bool_sort) in
          let eq_ce = CoreExpr.mk pred_info (CoreExpr.Eq (ce_w, ce)) in
          let* (typed_lp, delta2, ct1) =
            lpat_match rs delta1 lpat (Ok eq_ce) in
          (* Build parallel substitution: xi -> (cei : taui) *)
          let sub =
            try
              List.fold_left2
                (fun acc xi (cei, taui) ->
                  let arg_typed_sort = Elaborate.lift_sort taui in
                  let cei_annot =
                    CoreExpr.mk (mk_info taui)
                      (CoreExpr.Annot (cei, arg_typed_sort)) in
                  Subst.extend_var xi cei_annot acc)
                Subst.empty xs (List.combine sub_ces sub_taus)
            with Invalid_argument _ -> Subst.empty in
          let ce'_subst = Subst.apply_ce sub ce' in
          let* (typed_inner, delta3, ct2) =
            rpat_match rs delta2 eff rp_inner (Ok ce'_subst) (Ok value) in
          let ct = Constraint.conj pos ct1 ct2 in
          let info = mk_rinfo pos delta3 bool_sort eff in
          let typed_rp =
            RPat.mk_rpat info (RPat.RLet (typed_lp, typed_cp, typed_inner)) in
          return (typed_rp, delta3, ct))
     | _ ->
       let k = mismatch_ce_kind ~construct:"let pattern"
                 ~expected_shape:"let _ = _; _ or let (_, ..., _) = _; _"
                 pred_for_let in
       let (typed_rp, delta') = error_rp_blanket rp delta eff k in
       return (typed_rp, delta', Constraint.top pos))

  | RPat.RIfTrue rp_inner ->
    (match zip3_kind (view_get_if_ce ~construct:"iftrue pattern" pred' )with
     | Error k ->
       let (typed_rp, delta') = error_rp_blanket rp delta eff k in
       return (typed_rp, delta', Constraint.top pos)
     | Ok (ce_cond, ce_t, _ce_e) ->
       let* (typed_inner, delta', ct_inner) =
         rpat_match rs delta eff rp_inner (Ok ce_t) (Ok value) in
       let info = mk_rinfo pos delta' bool_sort eff in
       let typed_rp = RPat.mk_rpat info (RPat.RIfTrue typed_inner) in
       let ct = Constraint.conj pos (Constraint.atom pos ce_cond) ct_inner in
       return (typed_rp, delta', ct))

  | RPat.RIfFalse rp_inner ->
    (match zip3_kind (view_get_if_ce ~construct:"iffalse pattern" pred' )with
     | Error k ->
       let (typed_rp, delta') = error_rp_blanket rp delta eff k in
       return (typed_rp, delta', Constraint.top pos)
     | Ok (ce_cond, _ce_t, ce_e) ->
       let* (typed_inner, delta', ct_inner) =
         rpat_match rs delta eff rp_inner (Ok ce_e) (Ok value) in
       let info = mk_rinfo pos delta' bool_sort eff in
       let typed_rp = RPat.mk_rpat info (RPat.RIfFalse typed_inner) in
       let not_ce =
         CoreExpr.mk (CoreExpr.info ce_cond) (CoreExpr.App (Prim.Not, ce_cond)) in
       let ct = Constraint.conj pos (Constraint.atom pos not_ce) ct_inner in
       return (typed_rp, delta', ct))

  | RPat.RCase (lpat, label, cpat, rp_inner) ->
    (match zip2_kind (view_get_case_ce ~construct:"case pattern" pred' )with
     | Error k ->
       let (typed_rp, delta') = error_rp_blanket rp delta eff k in
       return (typed_rp, delta', Constraint.top pos)
     | Ok (scrutinee, branches) ->
       (match List.find_opt
                (fun (l, _, _, _) -> Label.compare l label = 0) branches with
        | None ->
          let case_labels = List.map (fun (l, _, _, _) -> l) branches in
          let err_t =
            Error.rcase_label_not_in_branches
              ~loc:pos ~label ~case_labels in
          let typed_rp =
            RPat.map_info_rpat
              (fun b -> mk_rinfo_with_answer b#loc delta bool_sort eff
                          (Error err_t))
              rp in
          return (typed_rp, delta, Constraint.top pos)
        | Some (_l, x_br, ce_br, _bi) ->
          let scrut_sort = (CoreExpr.sort_of_info (CoreExpr.info scrutinee)) in
          let d_args_r =
            SortGet.get_app ~construct:"case pattern scrutinee" scrut_sort in
          let payload_sort_r =
            Result.bind d_args_r (fun (d, args) ->
              CtorLookup.lookup cs d label args) in
          let payload_sort = Result.value payload_sort_r ~default:bool_sort in
          let* (typed_cp, delta1, ce_w) =
            cpat_match rs delta (Ok eff) cpat payload_sort_r in
          let inject_ce =
            CoreExpr.mk (CoreExpr.info scrutinee)
              (CoreExpr.Inject (label, ce_w)) in
          let eq_ce =
            CoreExpr.mk pred_info (CoreExpr.Eq (inject_ce, scrutinee)) in
          let* (typed_lp, delta2, ct1) =
            lpat_match rs delta1 lpat (Ok eq_ce) in
          (* Substitute (ce_w : payload_sort) / x_br into ce_br *)
          let payload_typed_sort = Elaborate.lift_sort payload_sort in
          let ce_w_annot =
            CoreExpr.mk (mk_info payload_sort)
              (CoreExpr.Annot (ce_w, payload_typed_sort)) in
          let sub = Subst.extend_var x_br ce_w_annot Subst.empty in
          let ce_br_subst = Subst.apply_ce sub ce_br in
          let* (typed_inner, delta3, ct2) =
            rpat_match rs delta2 eff rp_inner (Ok ce_br_subst) (Ok value) in
          let ct =
            Constraint.conj pos
              (Constraint.is_ pos label scrutinee)
              (Constraint.conj pos ct1 ct2) in
          let answer = answer_of_sort_kind_r ~loc:pos
                         (Result.map (fun _ -> bool_sort) payload_sort_r) in
          let info =
            mk_rinfo_with_answer pos delta3 bool_sort eff answer in
          let typed_rp =
            RPat.mk_rpat info
              (RPat.RCase (typed_lp, label, typed_cp, typed_inner)) in
          return (typed_rp, delta3, ct)))

  | RPat.RUnfold rp_inner ->
    (match zip2_kind (view_get_call_ce ~construct:"unfold pattern" pred' )with
     | Error k ->
       let (typed_rp, delta') = error_rp_blanket rp delta eff k in
       return (typed_rp, delta', Constraint.top pos)
     | Ok (f, ce_arg) ->
       (match Sig.lookup_fundef f cs with
        | Error k ->
          let (typed_rp, delta') = error_rp_blanket rp delta eff k in
          return (typed_rp, delta', Constraint.top pos)
        | Ok (param, arg_sort, _ret_sort, eff', body) ->
          if not (Effect.sub eff' Effect.Spec) then
            let err = Error.unfold_not_spec ~loc:pos ~name:f in
            let info = mk_rinfo_err pos delta bool_sort eff err in
            let typed_inner =
              RPat.map_info_rpat (fun b ->
                mk_rinfo b#loc delta bool_sort eff) rp_inner in
            let typed_rp = RPat.mk_rpat info (RPat.RUnfold typed_inner) in
            return (typed_rp, delta, Constraint.top pos)
          else
            let arg_typed_sort = Elaborate.lift_sort arg_sort in
            let ce_arg_annot =
              CoreExpr.mk (mk_info arg_sort)
                (CoreExpr.Annot (ce_arg, arg_typed_sort)) in
            let sub = Subst.extend_var param ce_arg_annot Subst.empty in
            let unfolded = Subst.apply_ce sub body in
            let* (typed_inner, delta', ct) =
              rpat_match rs delta eff rp_inner (Ok unfolded) (Ok value) in
            let info = mk_rinfo pos delta' bool_sort eff in
            let typed_rp = RPat.mk_rpat info (RPat.RUnfold typed_inner) in
            return (typed_rp, delta', ct)))

  | RPat.RAnnot rp_inner ->
    let* (typed_inner, delta', ct) =
      rpat_match rs delta eff rp_inner (Ok pred') (Ok value) in
    let info = mk_rinfo pos delta' bool_sort eff in
    let typed_rp = RPat.mk_rpat info (RPat.RAnnot typed_inner) in
    return (typed_rp, delta', ct)

(* Refined pattern matching: RS; Delta |- [eff] q : Pf -| Delta' ~~> Ct

   Walks RPat.shape and ProofSort.t in lockstep via ProofSortView.
   Mismatches between the pattern's qbase shape and the proof sort's
   head naturally produce Error-typed components (via the View
   wrappers), which propagate through cpat_match / lpat_match /
   rpat_match into the typed AST without halting. *)
and q_match (rs : RSig.t) (delta : RCtx.t) (eff : Effect.t)
    (pat : (_, Var.t) RPat.t)
    (pf : (CoreExpr.typed_ce, _, Var.t) ProofSort.t)
  : ((RProg.typed_rinfo, Var.t) RPat.t * RCtx.t * Constraint.typed_ct) ElabM.t =
  let pos = (RPat.info pat)#loc in
  let cs = RSig.comp rs in
  let _ = cs in
  let answer_ok = Ok bool_sort in
  let answer_of_kind ~loc kind_r : (Sort.sort, Error.t) result =
    Result.map_error (Error.structured ~loc) kind_r in
  let rec go pat_t pf_opt delta ct_acc =
    let b = RPat.info pat_t in
    match RPat.shape pat_t with
    | RPat.QNil ->
      let nil_check = view_get_pf_nil pf_opt in
      let answer = match nil_check with
        | Ok () -> answer_ok
        | Error k -> answer_of_kind ~loc:b#loc (Error k) in
      let info =
        mk_rinfo_with_answer b#loc delta bool_sort eff answer in
      return (RPat.mk info RPat.QNil, delta, ct_acc)

    | RPat.QCore (cp, rest_pat) ->
      let (var_o, sort_r, eff_r, tail_o) = view_get_pf_comp pf_opt in
      let* (typed_cp, delta', ce_w) =
        cpat_match rs delta eff_r cp sort_r in
      let tail_o' = match var_o with
        | Some v -> Option.map (ProofSort.subst v ce_w) tail_o
        | None -> tail_o in
      let* (typed_rest, delta'', ct) =
        go rest_pat tail_o' delta' ct_acc in
      let info = mk_rinfo b#loc delta'' bool_sort eff in
      return (RPat.mk info (RPat.QCore (typed_cp, typed_rest)), delta'', ct)

    | RPat.QLog (lp, rest_pat) ->
      let (prop_r, tail_o) = view_get_pf_log pf_opt in
      let* (typed_lp, delta', ct1) = lpat_match rs delta lp prop_r in
      let ct_acc' = Constraint.conj pos ct_acc ct1 in
      let* (typed_rest, delta'', ct) =
        go rest_pat tail_o delta' ct_acc' in
      let info = mk_rinfo b#loc delta'' bool_sort eff in
      return (RPat.mk info (RPat.QLog (typed_lp, typed_rest)), delta'', ct)

    | RPat.QRes (rp, rest_pat) ->
      let (pred_r, value_r, tail_o) = view_get_pf_res pf_opt in
      let* (typed_rp, delta', ct1) =
        rpat_match rs delta eff rp pred_r value_r in
      let ct_acc' = Constraint.conj pos ct_acc ct1 in
      let* (typed_rest, delta'', ct) =
        go rest_pat tail_o delta' ct_acc' in
      let info = mk_rinfo b#loc delta'' bool_sort eff in
      return (RPat.mk info (RPat.QRes (typed_rp, typed_rest)), delta'', ct)

    | RPat.QDepRes (cp, rp, rest_pat) ->
      let (bvar_o, pred_r, tail_o) = view_get_pf_depres pf_opt in
      (* Extract pred's inner sort: pred_r's sort must be Pred _.  If
         not, view_get_pred_sort returns Error which propagates into
         cpat_match's sort argument. *)
      let pred_sort_r =
        Result.map (fun pred -> CoreExpr.sort_of_info (CoreExpr.info pred))
          pred_r in
      let inner_sort_r =
        view_get_pred_sort ~construct:"dep res predicate" pred_sort_r in
      let* (typed_cp, delta1, ce_w) =
        cpat_match rs delta (Ok Effect.Spec) cp inner_sort_r in
      (* Substitute z with ce_w in pred (when possible) and tail. *)
      let pred_subst_r = match bvar_o, pred_r with
        | Some z, Ok pred ->
          let sub = Subst.extend_var z ce_w Subst.empty in
          Ok (Subst.apply_ce sub pred)
        | _, Ok p -> Ok p
        | _, (Error _ as e) -> e in
      let tail_o' = match bvar_o with
        | Some z -> Option.map (ProofSort.subst z ce_w) tail_o
        | None -> tail_o in
      let* (typed_rp, delta2, ct1) =
        rpat_match rs delta1 eff rp pred_subst_r (Ok ce_w) in
      let ct_acc' = Constraint.conj pos ct_acc ct1 in
      let* (typed_rest, delta3, ct) =
        go rest_pat tail_o' delta2 ct_acc' in
      let info = mk_rinfo b#loc delta3 bool_sort eff in
      return (RPat.mk info (RPat.QDepRes (typed_cp, typed_rp, typed_rest)),
              delta3, ct)
  in
  go pat (Some pf) delta (Constraint.top pos)
(* ---------- Program checking ---------- *)

let elab_fundecl_body rs param arg_sort ret_sort eff body_se =
  let cs = RSig.comp rs in
  let gamma = Context.extend param arg_sort (Effect.purify eff) Context.empty in
  Elaborate.check cs gamma body_se (Ok ret_sort) eff

let check_rdecl rs ct_acc = function
  | RProg.SortDecl d ->
    return (RProg.SortDecl d, RSig.extend_sort rs d, ct_acc)
  | RProg.TypeDecl d ->
    return (RProg.TypeDecl d, RSig.extend_type rs d, ct_acc)
  | RProg.FunDecl { name; param; arg_sort; ret_sort; eff; body; loc } ->
    let rs_for_body = match eff with
      | Effect.Spec ->
        RSig.extend name (RSig.FunSig { arg = arg_sort; ret = ret_sort; eff }) rs
      | _ -> rs
    in
    let* ce = elab_fundecl_body rs_for_body param arg_sort ret_sort eff body in
    (* Spec functions retain their body in the signature so [unfold]
       can generate the corresponding equation at proof time (see
       [lib/rCheck.ml] LUnfold handler, ~line 307). Impure functions
       have no spec-level meaning and get only a signature. *)
    let entry = match eff with
      | Effect.Pure | Effect.Spec ->
        RSig.FunDef { param; arg = arg_sort; ret = ret_sort; eff; body = ce }
      | Effect.Impure ->
        RSig.FunSig { arg = arg_sort; ret = ret_sort; eff }
    in
    let typed_decl = RProg.FunDecl { name; param; arg_sort; ret_sort; eff; body = ce; loc } in
    return (typed_decl, RSig.extend name entry rs, ct_acc)
  | RProg.RFunDecl { name; pat; domain = se_domain; codomain = se_codomain; eff; body; loc } ->
    let gamma = Context.empty in
    let* domain = elab_pf rs gamma eff se_domain in
    let bind_r =
      Result.map_error (Error.structured ~loc)
        (ProofSort.bind gamma domain) in
    let gamma' = Result.value bind_r ~default:gamma in
    let* codomain = elab_pf rs gamma' eff se_codomain in
    let rf = RFunType.{ domain; codomain; eff } in
    let pat_eff = match eff with Effect.Spec -> Effect.Spec | _ -> Effect.Pure in
    let* (typed_pat, delta, ct_pat) = q_match rs RCtx.empty pat_eff pat domain in
    let rs' = match eff with
      | Effect.Pure -> rs
      | _ -> RSig.extend name (RSig.RFunSig rf) rs
    in
    let* (checked, delta', ct_body) = check_crt rs' delta eff body codomain in
    let leak_check =
      if RCtx.zero delta' then Ok ()
      else
        let leftovers =
          List.filter_map (function
            | RCtx.Res { var; pred; value; usage } when not (Usage.is_zero usage) ->
              Some (Format.asprintf "@[<hov 2>%a : %a @@ %a [%a]@]"
                      Var.print var CoreExpr.print pred
                      CoreExpr.print value Usage.print usage)
            | _ -> None) (RCtx.entries delta') in
        Error (Error.let_pattern_resource_leak ~loc ~leftovers) in
    let decl_errors =
      let collect = function Ok _ -> [] | Error e -> [e] in
      collect bind_r @ collect leak_check in
    let checked = prepend_subterm_errors_crt decl_errors checked in
    let ct_closed = close_ctx loc delta' (Constraint.conj loc ct_pat ct_body) in
    let entry = RSig.RFunSig rf in
    let typed_decl = RProg.RFunDecl { name; pat = typed_pat; domain; codomain; eff; body = checked; loc } in
    return (typed_decl, RSig.extend name entry rs, Constraint.conj loc ct_acc ct_closed)

let check_rprog (prog : RProg.parsed) : (RProg.typed * RSig.t * Constraint.typed_ct) ElabM.t =
  let rec check_rprog_decls rs ct_acc = function
    | [] -> return ([], rs, ct_acc)
    | decl :: rest ->
      let* (typed_decl, rs', ct_acc') = check_rdecl rs ct_acc decl in
      let* (typed_rest, rs'', ct_acc'') = check_rprog_decls rs' ct_acc' rest in
      return (typed_decl :: typed_rest, rs'', ct_acc'')
  in
  let* (typed_decls, rs, ct_decls) = check_rprog_decls RSig.empty (Constraint.top prog.loc) prog.decls in
  let gamma = Context.empty in
  let* main_pf = elab_pf rs gamma prog.main_eff prog.main_pf in
  let* (checked_main, _delta, ct_main) = check_crt rs RCtx.empty prog.main_eff prog.main_body main_pf in
  let typed_prog : RProg.typed = {
    decls = typed_decls;
    main_pf;
    main_eff = prog.main_eff;
    main_body = checked_main;
    loc = prog.loc;
  } in
  return (typed_prog, rs, Constraint.conj prog.loc ct_decls ct_main)

(** {1 Error collection over typed refined programs}

    Each [collect_errors_*] walker traverses its argument, pulls
    every error recorded on [info#answer] (via [typed_rinfo]), and
    recurses into embedded [typed_ce] sub-trees through
    [Typecheck.collect_errors].  Errors are returned in source-
    pre-order.  Slices C.2-C.5 will populate the [answer] fields
    that this walker reads; until those slices land, refined
    judgements still fail-fast through ElabM and the walker
    returns an empty list on successful runs. *)

let rinfo_error (info : RProg.typed_rinfo) : Error.t list =
  let own = match info#answer with
    | Ok _ -> []
    | Error e -> [e] in
  (* [info#subterm_errors] holds cross-cutting errors prepended at
     this node (e.g. RFunDecl resource leak, pf_eq structural
     mismatches, CIter check accumulation).  Sub-tree errors are
     collected via the structural recursion in [collect_errors_rdecl
     / _crt / _rpf / _spine / _pf], not through this field, so we
     don't double-count. *)
  own @ info#subterm_errors

let collect_errors_pf (pf : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t)
    : Error.t list =
  List.concat_map (fun entry ->
    let here = rinfo_error (ProofSort.entry_info entry) in
    let inner =
      match entry with
      | ProofSort.Comp _ -> []
      | ProofSort.Log { prop; _ } -> Typecheck.collect_errors prop
      | ProofSort.Res { pred; value; _ } ->
        Typecheck.collect_errors pred @ Typecheck.collect_errors value
      | ProofSort.DepRes { pred; _ } -> Typecheck.collect_errors pred
    in
    here @ inner
  ) pf

let rec collect_errors_cpat (cpat : (RProg.typed_rinfo, Var.t) RPat.cpat)
    : Error.t list =
  let here = rinfo_error (RPat.cpat_info cpat) in
  let inner =
    match RPat.cpat_shape cpat with
    | RPat.CVar _ -> []
    | RPat.CTuple cps -> List.concat_map collect_errors_cpat cps
  in
  here @ inner

and collect_errors_lpat (lpat : (RProg.typed_rinfo, Var.t) RPat.lpat)
    : Error.t list =
  rinfo_error (RPat.lpat_info lpat)

and collect_errors_rpat_inner (rpat : (RProg.typed_rinfo, Var.t) RPat.rpat)
    : Error.t list =
  let here = rinfo_error (RPat.rpat_info rpat) in
  let inner =
    match RPat.rpat_shape rpat with
    | RPat.RVar _ -> []
    | RPat.RReturn lp -> collect_errors_lpat lp
    | RPat.RTake (cp, rp1, rp2) ->
      collect_errors_cpat cp
      @ collect_errors_rpat_inner rp1
      @ collect_errors_rpat_inner rp2
    | RPat.RFail lp -> collect_errors_lpat lp
    | RPat.RLet (lp, cp, rp) ->
      collect_errors_lpat lp
      @ collect_errors_cpat cp
      @ collect_errors_rpat_inner rp
    | RPat.RCase (lp, _, cp, rp) ->
      collect_errors_lpat lp
      @ collect_errors_cpat cp
      @ collect_errors_rpat_inner rp
    | RPat.RIfTrue rp | RPat.RIfFalse rp -> collect_errors_rpat_inner rp
    | RPat.RUnfold rp -> collect_errors_rpat_inner rp
    | RPat.RAnnot rp -> collect_errors_rpat_inner rp
  in
  here @ inner

let rec collect_errors_rpat (pat : (RProg.typed_rinfo, Var.t) RPat.t)
    : Error.t list =
  let here = rinfo_error (RPat.info pat) in
  let rest = match RPat.shape pat with
    | RPat.QNil -> []
    | RPat.QCore (cp, rest) ->
      collect_errors_cpat cp @ collect_errors_rpat rest
    | RPat.QLog (lp, rest) ->
      collect_errors_lpat lp @ collect_errors_rpat rest
    | RPat.QRes (rp, rest) ->
      collect_errors_rpat_inner rp @ collect_errors_rpat rest
    | RPat.QDepRes (cp, rp, rest) ->
      collect_errors_cpat cp
      @ collect_errors_rpat_inner rp
      @ collect_errors_rpat rest
  in
  here @ rest

let rec collect_errors_lpf (lpf : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RefinedExpr.lpf)
    : Error.t list =
  let here = rinfo_error (RefinedExpr.lpf_info lpf) in
  let inner =
    match RefinedExpr.lpf_shape lpf with
    | RefinedExpr.LVar _ -> []
    | RefinedExpr.LAuto -> []
    | RefinedExpr.LHole _ -> []
    | RefinedExpr.LUnfold (_, ce) -> Typecheck.collect_errors ce
    | RefinedExpr.LAnnot (lp, ce) ->
      collect_errors_lpf lp @ Typecheck.collect_errors ce
  in
  here @ inner

and collect_errors_rpf (rpf : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RefinedExpr.rpf)
    : Error.t list =
  let here = rinfo_error (RefinedExpr.rpf_info rpf) in
  let inner =
    match RefinedExpr.rpf_shape rpf with
    | RefinedExpr.RVar _ -> []
    | RefinedExpr.RHole _ -> []
    | RefinedExpr.RAnnot (rp, ce1, ce2) ->
      collect_errors_rpf rp
      @ Typecheck.collect_errors ce1
      @ Typecheck.collect_errors ce2
    | RefinedExpr.RReturn lp -> collect_errors_lpf lp
    | RefinedExpr.RTake (r1, r2) ->
      collect_errors_rpf r1 @ collect_errors_rpf r2
    | RefinedExpr.RFail lp -> collect_errors_lpf lp
    | RefinedExpr.RLet (lp, cp, rp) ->
      collect_errors_lpat lp
      @ collect_errors_cpat cp
      @ collect_errors_rpf rp
    | RefinedExpr.RCase (lp, _, cp, rp) ->
      collect_errors_lpat lp
      @ collect_errors_cpat cp
      @ collect_errors_rpf rp
    | RefinedExpr.RIfTrue rp -> collect_errors_rpf rp
    | RefinedExpr.RIfFalse rp -> collect_errors_rpf rp
    | RefinedExpr.RUnfold rp -> collect_errors_rpf rp
    | RefinedExpr.RAnnotStrip rp -> collect_errors_rpf rp
  in
  here @ inner

and collect_errors_spine (spine : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RefinedExpr.spine)
    : Error.t list =
  let here = rinfo_error (RefinedExpr.spine_info spine) in
  let inner =
    match RefinedExpr.spine_shape spine with
    | RefinedExpr.SNil -> []
    | RefinedExpr.SCore (ce, sp) ->
      Typecheck.collect_errors ce @ collect_errors_spine sp
    | RefinedExpr.SLog (lp, sp) ->
      collect_errors_lpf lp @ collect_errors_spine sp
    | RefinedExpr.SRes (rp, sp) ->
      collect_errors_rpf rp @ collect_errors_spine sp
  in
  here @ inner

and collect_errors_crt (crt : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RefinedExpr.crt)
    : Error.t list =
  let here = rinfo_error (RefinedExpr.crt_info crt) in
  let inner =
    match RefinedExpr.crt_shape crt with
    | RefinedExpr.CHole _ -> []
    | RefinedExpr.CExfalso -> []
    | RefinedExpr.CLet (pat, c1, c2) ->
      collect_errors_rpat pat
      @ collect_errors_crt c1
      @ collect_errors_crt c2
    | RefinedExpr.CLetLog (lp, lpf, crt') ->
      collect_errors_lpat lp
      @ collect_errors_lpf lpf
      @ collect_errors_crt crt'
    | RefinedExpr.CLetRes (rp, rpf, crt') ->
      collect_errors_rpat_inner rp
      @ collect_errors_rpf rpf
      @ collect_errors_crt crt'
    | RefinedExpr.CLetCore (lp, cp, ce, crt') ->
      collect_errors_lpat lp
      @ collect_errors_cpat cp
      @ Typecheck.collect_errors ce
      @ collect_errors_crt crt'
    | RefinedExpr.CAnnot (crt', pf) ->
      collect_errors_crt crt' @ collect_errors_pf pf
    | RefinedExpr.CPrimApp (_, sp) -> collect_errors_spine sp
    | RefinedExpr.CCall (_, sp) -> collect_errors_spine sp
    | RefinedExpr.CTuple sp -> collect_errors_spine sp
    | RefinedExpr.CIter (ce, pat, c1, c2) ->
      Typecheck.collect_errors ce
      @ collect_errors_rpat pat
      @ collect_errors_crt c1
      @ collect_errors_crt c2
    | RefinedExpr.CIf (_, ce, c1, c2) ->
      Typecheck.collect_errors ce
      @ collect_errors_crt c1
      @ collect_errors_crt c2
    | RefinedExpr.CCase (_, ce, branches) ->
      Typecheck.collect_errors ce
      @ List.concat_map (fun (_, info, _, body) ->
          rinfo_error info @ collect_errors_crt body
        ) branches
  in
  here @ inner

let collect_errors_rdecl
    (decl : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RProg.decl)
    : Error.t list =
  match decl with
  | RProg.SortDecl _ | RProg.TypeDecl _ -> []
  | RProg.FunDecl { body; _ } -> Typecheck.collect_errors body
  | RProg.RFunDecl { pat; domain; codomain; body; _ } ->
    collect_errors_rpat pat
    @ collect_errors_pf domain
    @ collect_errors_pf codomain
    @ collect_errors_crt body

let collect_errors_rprog (prog : RProg.typed) : Error.t list =
  List.concat_map collect_errors_rdecl prog.decls
  @ collect_errors_pf prog.main_pf
  @ collect_errors_crt prog.main_body

module Test = struct
  let with_delta_check f =
    delta_check_enabled := true;
    Fun.protect ~finally:(fun () -> delta_check_enabled := false) f

  let test =
    let check_program name src =
      QCheck.Test.make ~name ~count:1 QCheck.unit (fun () ->
        with_delta_check (fun () ->
          match ElabM.run Var.empty_supply (
            let* prog = Parse.parse_rprog src ~file:"test" in
            check_rprog prog
          ) with
          | Error msg -> QCheck.Test.fail_reportf "check: %s" (Error.to_string msg)
          | Ok _ -> true))
    in
    [ check_program "delta monotonicity: incr (new/get/set/del)"
        {|
          rfun incr (p : Ptr Int, [res] r : (do x : Int = Own[Int](p)))
            -> ([res] (do x' : Int = Own[Int](p))) [impure] =
            let (v, log pf, res r2) = Get[Int](p, res r);
            let (res r3) = Set[Int](p, v + 1, res r2);
            (res r3)
          main : () [impure] =
            let (p, res r) = New[Int](0);
            let (do x' = r') = incr(p, res r);
            Del[Int](p, x', res r')
        |};

      check_program "delta monotonicity: if-then-else with resources"
        {|
          main : () [impure] =
            let (p, res r) = New[Int](0);
            let (v, log pf, res r2) = Get[Int](p, res r);
            let (b, log bpf) = Eq[Int](v, 0);
            if [w] b
              then let (res r3) = Set[Int](p, 1, res r2);
                   Del[Int](p, 1, res r3)
              else Del[Int](p, v, res r2)
        |};

      check_program "delta monotonicity: pure computation (no resources)"
        {|
          main : () [pure] = ()
        |};

      check_program "delta monotonicity: iter with heap cell"
        {|
          type Step(a, b) = { Next : a | Done : b }
          main : () [impure] =
            let (p, res r) = New[Step(Int, ())](Next 0);
            let (z_done, res r_done) = iter [Own[Step(Int, ())](p)] ((x, res r_loop) =
              (0, res r) : (x : Int, [res] Own[Step(Int, ())](p) @ Next x)
            ) {
              let (v, log pf, res r2) = Get[Step(Int, ())](p, res r_loop);
              let (res r3) = Set[Step(Int, ())](p, Done (), res r2);
              (Done (), res r3) : (z : Step(Int, ()), [res] Own[Step(Int, ())](p) @ z)
            };
            Del[Step(Int, ())](p, Done z_done, res r_done)
        |};

      (* let log / let res named-binder syntax *)
      check_program "let log and let res with named binders"
        {|
          main : () [impure] =
            let (p, res r) = New[Int](0);
            let res r2 = r;
            let (v, log pf, res r3) = Get[Int](p, res r2);
            let res r4 = r3;
            Del[Int](p, v, res r4)
        |};

      (* let res x : ce@ce' = rpf; ... — annotation sugar *)
      check_program "let res with type annotation sugar"
        {|
          main : () [impure] =
            let (p, res r) = New[Int](0);
            let res r2 : Own[Int](p) @ 0 = r;
            let (v, log pf, res r3) = Get[Int](p, res r2);
            let res r4 = r3;
            Del[Int](p, v, res r4)
        |};
    ]

  let pf_eq = pf_eq
end
