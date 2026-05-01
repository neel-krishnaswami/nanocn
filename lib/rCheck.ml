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

(* Elaborate a surface expression to typed core, synthesizing its
   sort.  Defers to surface elaboration; if the elaborator recorded
   any errors on the typed tree, fail-fast through ElabM with the
   first one — the per-decl driver in [compileFile.compile_rfile]
   captures these so later decls still get checked.  Slices C.2-C.5
   will replace this fail-fast with an attach-and-continue path. *)
let elab_se (rs : RSig.t) (gamma : Context.t) (eff : Effect.t) (se : SurfExpr.se) : (CoreExpr.typed_ce * Sort.sort) ElabM.t =
  let cs = RSig.comp rs in
  let* ce = Elaborate.synth cs gamma eff se in
  let ce = Typecheck.annotate_subterm_errors ce in
  match (CoreExpr.info ce)#answer with
  | Error e -> ElabM.fail e
  | Ok sort ->
    (match (CoreExpr.info ce)#subterm_errors with
     | e :: _ -> ElabM.fail e
     | [] -> return (ce, sort))

(* Elaborate a surface expression to typed core, checking against
   a sort.  Same fail-fast contract as [elab_se]. *)
let elab_se_check (rs : RSig.t) (gamma : Context.t) (se : SurfExpr.se) (sort : Sort.sort) (eff : Effect.t) : CoreExpr.typed_ce ElabM.t =
  let cs = RSig.comp rs in
  let* ce = Elaborate.check cs gamma se (Ok sort) eff in
  let ce = Typecheck.annotate_subterm_errors ce in
  match (CoreExpr.info ce)#subterm_errors with
  | e :: _ -> ElabM.fail e
  | [] -> return ce

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
    let* (ce_pred, pred_sort) = elab_se rs gamma Effect.Spec pred in
    let* inner_sort =
      ElabM.lift_at loc
        (SortGet.get_pred ~construct:"resource predicate" pred_sort) in
    let* ce_value = elab_se_check rs gamma value inner_sort Effect.Spec in
    return (ProofSort.Res { info = ri; pred = ce_pred; value = ce_value })
  | ProofSort.DepRes { info = _; bound_var; pred } ->
    let* (ce_pred, pred_sort) = elab_se rs gamma Effect.Spec pred in
    let* _inner_sort =
      ElabM.lift_at loc
        (SortGet.get_pred ~construct:"dep-res predicate" pred_sort) in
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
          let pred_loc = (CoreExpr.info pred)#loc in
          let* inner =
            ElabM.lift_at pred_loc
              (SortGet.get_pred ~construct:"dep-res predicate" pred_sort) in
          return (Context.extend bound_var inner Effect.Spec gamma)
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

  | RefinedExpr.LOpenRet rpf ->
    let* (checked_rpf, ce_pred, ce_val, delta', ct) = synth_rpf rs delta rpf in
    let* ce1 =
      ElabM.lift_at pos
        (CoreExprGet.get_return ~construct:"open-ret"
           (strip_annots ce_pred)) in
    let prop = mk_eq ce1 ce_val in
    let rinfo = mk_rinfo ~goal:(RProg.LpfGoal prop) pos delta bool_sort Effect.Spec in
    let checked = RefinedExpr.mk_lpf rinfo (RefinedExpr.LOpenRet checked_rpf) in
    return (checked, prop, delta', ct)

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
    let* (ce2, sort2) = elab_se rs gamma Effect.Spec se2 in
    let pred_sort =
      Sort.mk (object method loc = SourcePos.dummy end) (Sort.Pred sort2)
    in
    let* ce1 = elab_se_check rs gamma se1 pred_sort Effect.Spec in
    let* (checked_rpf', delta', ct) = check_rpf rs delta rpf' ce1 ce2 in
    let rinfo = mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2)) pos delta bool_sort Effect.Spec in
    let checked = RefinedExpr.mk_rpf rinfo (RefinedExpr.RAnnot (checked_rpf', ce1, ce2)) in
    return (checked, ce1, ce2, delta', ct)

  | RefinedExpr.RMakeRet _ ->
    let err = Error.cannot_synthesize ~loc:pos
                ~construct:"make-ret (add a : pred @ value annotation)" in
    let placeholder_pred =
      CoreExpr.mk (mk_info bool_sort)
        (CoreExpr.Hole "rpf-make-ret-unsynth-pred") in
    let placeholder_value =
      CoreExpr.mk (mk_info bool_sort)
        (CoreExpr.Hole "rpf-make-ret-unsynth-value") in
    let rinfo = mk_rinfo_err
      ~goal:(RProg.RpfGoal (placeholder_pred, placeholder_value))
      pos delta bool_sort Effect.Spec err in
    let inner_rinfo = mk_rinfo
      ~goal:(RProg.LpfGoal placeholder_pred) pos delta
      bool_sort Effect.Spec in
    let inner_lpf = RefinedExpr.mk_lpf inner_rinfo
      (RefinedExpr.LHole "make-ret-unsynth") in
    let checked =
      RefinedExpr.mk_rpf rinfo (RefinedExpr.RMakeRet inner_lpf) in
    return (checked, placeholder_pred, placeholder_value, delta,
            Constraint.top pos)
  | RefinedExpr.RMakeTake _ ->
    let err = Error.cannot_synthesize ~loc:pos
                ~construct:"make-take (add a : pred @ value annotation)" in
    let placeholder_pred =
      CoreExpr.mk (mk_info bool_sort)
        (CoreExpr.Hole "rpf-make-take-unsynth-pred") in
    let placeholder_value =
      CoreExpr.mk (mk_info bool_sort)
        (CoreExpr.Hole "rpf-make-take-unsynth-value") in
    let rinfo = mk_rinfo_err
      ~goal:(RProg.RpfGoal (placeholder_pred, placeholder_value))
      pos delta bool_sort Effect.Spec err in
    let inner_rinfo = mk_rinfo
      ~goal:RProg.NoGoal pos delta bool_sort Effect.Spec in
    let inner_crt = RefinedExpr.mk_crt inner_rinfo
      (RefinedExpr.CHole "make-take-unsynth") in
    let checked =
      RefinedExpr.mk_rpf rinfo (RefinedExpr.RMakeTake inner_crt) in
    return (checked, placeholder_pred, placeholder_value, delta,
            Constraint.top pos)
  | RefinedExpr.RUnfold _ ->
    let err = Error.cannot_synthesize ~loc:pos
                ~construct:"unfold (add a : f(ce) @ value annotation)" in
    let placeholder_pred =
      CoreExpr.mk (mk_info bool_sort)
        (CoreExpr.Hole "rpf-unfold-unsynth-pred") in
    let placeholder_value =
      CoreExpr.mk (mk_info bool_sort)
        (CoreExpr.Hole "rpf-unfold-unsynth-value") in
    let rinfo = mk_rinfo_err
      ~goal:(RProg.RpfGoal (placeholder_pred, placeholder_value))
      pos delta bool_sort Effect.Spec err in
    let inner_rinfo = mk_rinfo
      ~goal:(RProg.RpfGoal (placeholder_pred, placeholder_value))
      pos delta bool_sort Effect.Spec in
    let inner_rpf = RefinedExpr.mk_rpf inner_rinfo
      (RefinedExpr.RHole "unfold-unsynth") in
    let checked =
      RefinedExpr.mk_rpf rinfo (RefinedExpr.RUnfold inner_rpf) in
    return (checked, placeholder_pred, placeholder_value, delta,
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
  | RefinedExpr.RMakeRet lpf' ->
    let* ce_a =
      ElabM.lift_at pos
        (CoreExprGet.get_return ~construct:"make-ret" (strip_annots ce1)) in
    let* (checked_lpf', delta', ct) = check_lpf rs delta lpf' (mk_eq ce_a ce2) in
    let rinfo = mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2)) pos delta bool_sort Effect.Spec in
    let checked = RefinedExpr.mk_rpf rinfo (RefinedExpr.RMakeRet checked_lpf') in
    return (checked, delta', ct)

  | RefinedExpr.RMakeTake crt ->
    let* (x, pred_expr, pred_body) =
      ElabM.lift_at pos
        (CoreExprGet.get_take ~construct:"make-take" (strip_annots ce1)) in
    let pred_sort = (CoreExpr.sort_of_info (CoreExpr.info pred_expr)) in
    let* inner_sort =
      ElabM.lift_at pos
        (SortGet.get_pred ~construct:"make-take bound expression" pred_sort) in
    let pf = [
      ProofSort.Comp { info = rinfo_dummy; var = x; sort = inner_sort; eff = Effect.Spec };
      ProofSort.Res { info = rinfo_dummy; pred = pred_expr; value = ce_of_var x inner_sort };
      ProofSort.Res { info = rinfo_dummy; pred = pred_body; value = ce2 };
    ] in
    let eff = Effect.Spec in
    let* (checked_crt, delta', ct) = check_crt rs delta eff crt pf in
    let rinfo = mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2)) pos delta bool_sort Effect.Spec in
    let checked = RefinedExpr.mk_rpf rinfo (RefinedExpr.RMakeTake checked_crt) in
    return (checked, delta', ct)

  | RefinedExpr.RHole h ->
    let delta' = RCtx.affinize delta in
    let rinfo = mk_rinfo ~goal:(RProg.RpfGoal (ce1, ce2)) pos delta (CoreExpr.sort_of_info (CoreExpr.info ce1)) Effect.Spec in
    let checked = RefinedExpr.mk_rpf rinfo (RefinedExpr.RHole h) in
    return (checked, delta', Constraint.top pos)

  | RefinedExpr.RUnfold rpf' ->
    let* (f, ce_arg) =
      ElabM.lift_at pos
        (CoreExprGet.get_call ~construct:"unfold" (strip_annots ce1)) in
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

  | RefinedExpr.COpenTake rpf ->
    let* (checked_rpf, ce_pred, ce_val, delta', ct) = synth_rpf rs delta rpf in
    let* (x, ce1, ce2) =
      ElabM.lift_at binfo#loc
        (CoreExprGet.get_take ~construct:"open-take" (strip_annots ce_pred)) in
    let pred_sort = (CoreExpr.sort_of_info (CoreExpr.info ce1)) in
    let* inner_sort =
      ElabM.lift_at binfo#loc
        (SortGet.get_pred ~construct:"open-take" pred_sort) in
    let pf = [
      ProofSort.Comp { info = rinfo_dummy; var = x; sort = inner_sort; eff = Effect.Spec };
      ProofSort.Res { info = rinfo_dummy; pred = ce1; value = ce_of_var x inner_sort };
      ProofSort.Res { info = rinfo_dummy; pred = ce2; value = ce_val };
    ] in
    let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
    let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.COpenTake checked_rpf) in
    return (checked, pf, delta', ct)

  | RefinedExpr.CIter (se_pred, pat, crt1, crt2) ->
    (* iter requires impure effect *)
    if not (Effect.sub Effect.Impure eff) then
      ElabM.fail
        (Error.iter_requires_impure ~loc:binfo#loc ~actual:eff)
    else
    (* Elaborate predicate at spec effect *)
    let* (ce_pred, pred_sort) = elab_and_synth rs delta Effect.Spec se_pred in
    let* inner_sort =
      ElabM.lift_at binfo#loc (SortGet.get_pred ~construct:"iter" pred_sort) in
    let* (dsort_name, args) =
      ElabM.lift_at binfo#loc (SortGet.get_app ~construct:"iter" inner_sort) in
    let cs = RSig.comp rs in
    let* next_label =
      match Label.of_string "Next" with
      | Ok l -> return l
      | Error _ ->
        invariant_at binfo#loc ~rule:"CIter"
          "Label.of_string \"Next\" failed — the literal \"Next\" \
           is always a valid constructor name"
    in
    let* done_label =
      match Label.of_string "Done" with
      | Ok l -> return l
      | Error _ ->
        invariant_at binfo#loc ~rule:"CIter"
          "Label.of_string \"Done\" failed — the literal \"Done\" \
           is always a valid constructor name"
    in
    (* Look up constructor sorts with type parameter substitution *)
    let* a_sort = lift_at binfo#loc (CtorLookup.lookup cs dsort_name next_label args) in
    let* b_sort = lift_at binfo#loc (CtorLookup.lookup cs dsort_name done_label args) in
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
    let* (delta_pat, _ct_pat) = rpat_match rs delta' (Effect.purify eff) pat init_pf in
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
    if not (RCtx.zero delta_pat_out) then
      ElabM.fail
        (Error.resource_leak ~loc:binfo#loc ~name:None)
    else
    (* Build result proof sort: z:B [pure], y:ce @ Done(z) [res], pfnil *)
    let* zr_var = fresh SourcePos.dummy in
    let ce_zr = ce_of_var zr_var b_sort in
    let ce_done_z = CoreExpr.mk (mk_info step_sort) (CoreExpr.Inject (done_label, ce_zr)) in
    let result_pf = [
      ProofSort.Comp { info = rinfo_dummy; var = zr_var; sort = b_sort; eff = Effect.Pure };
      ProofSort.Res { info = rinfo_dummy; pred = ce_pred; value = ce_done_z };
    ] in
    (* Extract iter binder: must be a single QCore CVar pattern. *)
    (match RPat.elems pat with
     | qb :: _ ->
       (match RPat.qbase_shape qb with
        | RPat.QCore cp ->
          let* x_pat =
            ElabM.lift_at binfo#loc
              (RPatGet.get_cvar ~construct:"iter" cp)
          in
          let result_ct = Constraint.conj pos ct (Constraint.forall_ pos x_pat a_sort ct') in
          let rinfo = mk_rinfo ~goal:(RProg.CrtGoal result_pf) pos delta (ProofSort.comp result_pf) eff in
          let typed_pat = RPat.map_info (fun b -> mk_rinfo b#loc RCtx.empty (ProofSort.comp init_pf) eff) pat in
          let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CIter (ce_pred, typed_pat, checked_crt1, checked_crt2)) in
          return (checked, result_pf, delta', result_ct)
        | RPat.QLog _ ->
          ElabM.fail
            (Error.iter_pattern_shape ~loc:binfo#loc
               ~got:"a logical pattern")
        | RPat.QRes _ | RPat.QDepRes _ ->
          ElabM.fail
            (Error.iter_pattern_shape ~loc:binfo#loc
               ~got:"a resource pattern"))
     | [] ->
       ElabM.fail
         (Error.iter_pattern_shape ~loc:binfo#loc
            ~got:"an empty pattern"))

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
    let* (delta2, ct_pat) = rpat_match rs delta1 eff_pat pat pf' in
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
    let typed_pat = RPat.map_info (fun b -> mk_rinfo b#loc RCtx.empty (ProofSort.comp pf') eff) pat in
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
    let* (delta2, ct_pat) = rpat_match rs delta1 eff_pat (RPat.mk lp_b [RPat.mk_qbase lp_b (RPat.QLog lp)]) pf_log in
    let* (checked_body, delta3, ct_body) = check_crt rs delta2 eff body pf in
    let n = RCtx.length delta1 in
    let (delta_out, delta_close) = RCtx.split n delta3 in
    let ct_closed = close_ctx pos delta_close (Constraint.conj pos ct_pat ct_body) in
    let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
    let typed_lp = RPat.map_info_lpat (fun b -> mk_rinfo b#loc RCtx.empty bool_sort eff) lp in
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
    let* (delta2, ct_pat) = rpat_match rs delta1 eff_pat (RPat.mk rp_b [RPat.mk_qbase rp_b (RPat.QRes rp)]) pf_res in
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
    let typed_rp = RPat.map_info_rpat (fun b -> mk_rinfo b#loc RCtx.empty bool_sort eff) rp in
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
    let* delta_merged = lift_at pos (RCtx.merge delta1 delta2) in
    let ct = Constraint.conj pos
      (Constraint.impl pos (mk_eq ce mk_true) ct1)
      (Constraint.impl pos (mk_eq ce mk_false) ct2) in
    let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
    let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CIf (eq_var, ce, checked_crt1, checked_crt2)) in
    return (checked, delta_merged, ct)

  | RefinedExpr.CCase (_y, se, branches) ->
    (* Scrutinee runs at the purified effect (cf. the surface [Case]
       rule in [lib/elaborate.ml:410-422]); branch bodies inherit the
       outer [eff] so impure work inside a case branch is permitted
       when the enclosing refined term is impure. *)
    let eff_scrut = Effect.purify eff in
    let gamma = RCtx.erase delta in
    let cs = RSig.comp rs in
    let* (ce, ce_sort) = elab_se rs gamma eff_scrut se in
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
       let* (checked_branches, delta', ct) =
         check_case_branches pos rs delta eff _y ce ce_sort ctors branches pf in
       let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
       let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CCase (_y, ce, checked_branches)) in
       return (checked, delta', ct)
     | _ ->
       ElabM.fail (Error.scrutinee_not_data ~loc:pos ~got:ce_sort))

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
    let* (ce, sort) = elab_se rs gamma eff_pure se_ce in
    let* y = fresh pos in
    let ce_y = ce_of_var y sort in
    let prop = mk_eq ce_y ce in
    let pf_core = [
      ProofSort.Comp { info = rinfo_dummy; var = y; sort; eff = eff_pure };
      ProofSort.Log { info = rinfo_dummy; prop }
    ] in
    let cp_b = RPat.cpat_info cp in
    let pat = RPat.mk cp_b [RPat.mk_qbase cp_b (RPat.QCore cp); RPat.mk_qbase (RPat.lpat_info lp) (RPat.QLog lp)] in
    let* (delta1, ct_pat) = rpat_match rs delta eff_pure pat pf_core in
    let* (checked_body, delta2, ct_body) = check_crt rs delta1 eff body pf in
    let n = RCtx.length delta in
    let (delta_out, delta_close) = RCtx.split n delta2 in
    let ct_closed = close_ctx pos delta_close (Constraint.conj pos ct_pat ct_body) in
    let rinfo = mk_rinfo ~goal:(RProg.CrtGoal pf) pos delta (ProofSort.comp pf) eff in
    let checked =
      RefinedExpr.mk_crt rinfo
        (RefinedExpr.CLetCore (
           RPat.map_info_lpat (fun b -> mk_rinfo b#loc RCtx.empty bool_sort eff) lp,
           RPat.map_info_cpat (fun b -> mk_rinfo b#loc RCtx.empty sort eff) cp,
           ce, checked_body))
    in
    return (checked, delta_out, ct_closed)

  | _ ->
    let* (checked_crt, pf', delta', ct) = synth_crt rs delta eff crt in
    let* ct' = pf_eq pos rs delta' pf' pf in
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

(* Case branch checking *)
and check_case_branches pos rs delta eff eq_var ce ce_sort ctors branches pf =
  let* (checked_branches, branch_results) = check_branches_list pos rs delta eff eq_var ce ce_sort ctors branches pf in
  let (deltas, cts) = List.split branch_results in
  let* delta_merged = lift_at pos (RCtx.merge_n deltas) in
  let ct = List.fold_left (Constraint.conj pos) (Constraint.top pos) cts in
  return (checked_branches, delta_merged, ct)

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

(* Proof sort equality: RS; Delta |- Pf1 = Pf2 ~> Ct *)
and pf_eq (pos : SourcePos.t) (rs : RSig.t) (delta : RCtx.t) (pf1 : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t) (pf2 : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t) : Constraint.typed_ct ElabM.t =
  let _cs = RSig.comp rs in
  let _gamma = RCtx.erase delta in
  let rec go pf1 pf2 =
    match pf1, pf2 with
    | [], [] -> return (Constraint.top pos)

    | ProofSort.Comp { info = _; var = x; sort; eff } :: rest1,
      ProofSort.Comp { info = _; var = y; sort = sort2; eff = eff2 } :: rest2 ->
      if Sort.compare sort sort2 <> 0 then
        ElabM.fail
          (Error.sort_mismatch ~loc:pos ~expected:sort ~actual:sort2)
      else if Effect.compare eff eff2 <> 0 then
        ElabM.fail
          (Error.pf_effect_mismatch ~loc:pos ~sort
             ~synthesized_eff:eff ~expected_eff:eff2)
      else
        let rest2' = ProofSort.subst y (ce_of_var x sort) rest2 in
        let* ct = go rest1 rest2' in
        return (Constraint.forall_ pos x sort ct)

    | ProofSort.Log { info = _; prop = ce1 } :: rest1,
      ProofSort.Log { info = _; prop = ce2 } :: rest2 ->
      let* ct = go rest1 rest2 in
      return (Constraint.conj pos (Constraint.atom pos (mk_eq ce1 ce2)) ct)

    | ProofSort.Res { info = _; pred = p1; value = v1 } :: rest1,
      ProofSort.Res { info = _; pred = p2; value = v2 } :: rest2 ->
      let* ct = go rest1 rest2 in
      let eq_ct = Constraint.conj pos (Constraint.atom pos (mk_eq p1 p2))
                                      (Constraint.atom pos (mk_eq v1 v2)) in
      return (Constraint.conj pos eq_ct ct)

    | ProofSort.DepRes { info = _; bound_var = y1; pred = ce1 } :: rest1,
      ProofSort.DepRes { info = _; bound_var = y2; pred = ce2 } :: rest2 ->
      let pred_sort = (CoreExpr.sort_of_info (CoreExpr.info ce1)) in
      let* inner_sort =
        ElabM.lift_at pos
          (SortGet.get_pred ~construct:"dependent resource" pred_sort) in
      let* z = fresh SourcePos.dummy in
      let ce_z = ce_of_var z inner_sort in
      let rest1' = ProofSort.subst y1 ce_z rest1 in
      let rest2' = ProofSort.subst y2 ce_z rest2 in
      let* ct = go rest1' rest2' in
      return (Constraint.conj pos (Constraint.atom pos (mk_eq ce1 ce2))
                                  (Constraint.forall_ pos z inner_sort ct))

    | e1 :: _, e2 :: _ ->
      ElabM.fail
        (Error.pf_structure_mismatch ~loc:pos
           ~synthesized_entry:(pf_entry_to_string e1)
           ~expected_entry:(pf_entry_to_string e2))
    | [], _ :: _ | _ :: _, [] ->
      ElabM.fail
        (Error.pf_structure_mismatch ~loc:pos
           ~synthesized_entry:(if pf1 = [] then "(end)" else pf_entry_to_string (List.hd pf1))
           ~expected_entry:(if pf2 = [] then "(end)" else pf_entry_to_string (List.hd pf2)))
  in
  go pf1 pf2

(* Refined pattern matching: RS; Delta |- [eff] q : Pf -| Delta' ~~> Ct
   Per [doc/extended-resource-terms.md]: the judgement takes a refined
   context Delta (not erased Gamma), an effect, and returns both an
   extended context and a constraint.

   The implementation processes qbase elements left-to-right against
   proof sort entries.  Resource patterns (rpat) that destructure
   predicates do so by inspecting the head constructor of the predicate
   expression and expanding into sub-patterns that are prepended to
   the remaining work list. *)
and rpat_match (rs : RSig.t) (delta : RCtx.t) (_eff : Effect.t)
    (pat : (_, Var.t) RPat.t)
    (pf : (CoreExpr.typed_ce, _, Var.t) ProofSort.t)
  : (RCtx.t * Constraint.typed_ct) ElabM.t =
  let pat_elems = RPat.elems pat in
  let pos = (RPat.info pat)#loc in
  let cs = RSig.comp rs in
  let fail_length elems =
    ElabM.fail (Error.structured ~loc:pos
      (Error.K_rpat_length_mismatch { pat_len = List.length elems; pf_len = List.length pf }))
  in
  let fail_kind elems entries =
    let pat_kind = match RPat.qbase_shape (List.hd elems) with
      | RPat.QCore _ -> "core" | RPat.QLog _ -> "log"
      | RPat.QRes _ -> "res" | RPat.QDepRes _ -> "depres" in
    let pf_kind = match List.hd entries with
      | ProofSort.Comp _ -> "comp" | ProofSort.Log _ -> "log"
      | ProofSort.Res _ -> "res" | ProofSort.DepRes _ -> "depres" in
    ElabM.fail (Error.structured ~loc:pos
      (Error.K_rpat_kind_mismatch { pat_kind; pf_kind }))
  in
  let fail_pred_shape loc construct expected got =
    ElabM.fail (Error.structured ~loc
      (Error.K_wrong_pred_shape { construct; expected_shape = expected;
                                  got = CoreExpr.to_string got }))
  in
  let rec go elems entries delta =
    match elems, entries with
    (* ---- Base case ---- *)
    | [], [] -> return (delta, Constraint.top pos)

    | qb :: rest_elems, entry :: rest_pf ->
      (match RPat.qbase_shape qb, entry with

      (* ---- Core patterns ---- *)
      | RPat.QCore cp, ProofSort.Comp { info; var = y; sort; eff } ->
        (match RPat.cpat_shape cp with
         (* ---- Core variable ---- *)
         | RPat.CVar x ->
           let ce_x = ce_of_var x sort in
           let rest_pf' = ProofSort.subst y ce_x rest_pf in
           let delta' = RCtx.extend_comp x sort eff delta in
           go rest_elems rest_pf' delta'

         (* ---- Core tuple: expand (cpat1,...,cpatn) into individual cpat bindings ---- *)
         | RPat.CTuple cpats ->
           let* component_sorts =
             ElabM.lift_at pos (SortGet.get_record ~construct:"tuple pattern" sort) in
           if List.length cpats <> List.length component_sorts then
             ElabM.fail (Error.structured ~loc:pos
               (Error.K_rpat_length_mismatch {
                 pat_len = List.length cpats;
                 pf_len = List.length component_sorts }))
           else
             (* Generate fresh variables for each component *)
             let rec map_m f = function
               | [] -> return []
               | x :: xs -> let* y = f x in let* ys = map_m f xs in return (y :: ys) in
             let* fresh_vars = map_m (fun s ->
               let* v = fresh pos in
               return (v, s)) component_sorts in
             (* Build the tuple expression (x1, ..., xn) for substitution *)
             let tuple_ce = CoreExpr.mk (mk_info sort)
               (CoreExpr.Tuple (List.map (fun (v, s) -> ce_of_var v s) fresh_vars)) in
             let rest_pf' = ProofSort.subst y tuple_ce rest_pf in
             (* Expand: each cpat_i matches x_i : τ_i [eff] *)
             let expanded_elems =
               List.map (fun cp -> RPat.mk_qbase (RPat.cpat_info cp) (RPat.QCore cp)) cpats @ rest_elems in
             let expanded_pf =
               List.map (fun (v, s) ->
                 ProofSort.Comp { info; var = v; sort = s; eff }) fresh_vars
               @ rest_pf' in
             go expanded_elems expanded_pf delta)

      (* ---- Log patterns ---- *)
      | RPat.QLog lp, ProofSort.Log { info = _; prop } ->
        (match RPat.lpat_shape lp with
         (* ---- Log variable ---- *)
         | RPat.LVar x ->
           let delta' = RCtx.extend_log x prop delta in
           go rest_elems rest_pf delta'

         (* ---- Log auto: assert prop as constraint ---- *)
         | RPat.LAuto ->
           let* (delta', ct_rest) = go rest_elems rest_pf delta in
           return (delta', Constraint.conj pos (Constraint.atom pos prop) ct_rest))

      (* ---- DepRes: expand (do cpat = rpat) against (y).ce[res] ---- *)
      | RPat.QDepRes (cpat, rpat), ProofSort.DepRes { info; bound_var = z; pred } ->
        let pred_sort = (CoreExpr.sort_of_info (CoreExpr.info pred)) in
        (match Sort.shape pred_sort with
         | Sort.Pred inner_sort ->
           (* Expand: cpat matches x:τ[spec], rpat matches ce@x[res] *)
           let expanded_elems =
             RPat.mk_qbase (RPat.cpat_info cpat) (RPat.QCore cpat)
             :: RPat.mk_qbase (RPat.rpat_info rpat) (RPat.QRes rpat)
             :: rest_elems in
           (* We need a fresh variable for x to substitute into pred *)
           let* x_var = fresh pos in
           let ce_x = ce_of_var x_var inner_sort in
           let sub = Subst.extend_var z ce_x Subst.empty in
           let pred' = Subst.apply_ce sub pred in
           let expanded_pf =
             ProofSort.Comp { info; var = x_var; sort = inner_sort; eff = Effect.Spec }
             :: ProofSort.Res { info; pred = pred'; value = ce_x }
             :: ProofSort.subst z ce_x rest_pf
           in
           go expanded_elems expanded_pf delta
         | _ ->
           ElabM.fail (Error.structured ~loc:pos
             (Error.K_dep_res_not_pred { got = pred_sort })))

      (* ---- Resource patterns ---- *)
      | RPat.QRes rp, ProofSort.Res { info; pred; value } ->
        let rp_loc = (RPat.rpat_info rp)#loc in
        (match RPat.rpat_shape rp with
         (* ---- Res variable: bind x:pred@value[res(1)] ---- *)
         | RPat.RVar x ->
           let delta' = RCtx.extend_res x pred value Usage.Avail delta in
           go rest_elems rest_pf delta'

         (* ---- Res return: (return ce)@ce' → log lpat : ce=ce' ---- *)
         | RPat.RReturn lpat ->
           let pred' = strip_annots pred in
           let* ret_ce =
             ElabM.lift_at rp_loc
               (CoreExprGet.get_return ~construct:"return pattern" pred') in
           let eq_ce = CoreExpr.mk (CoreExpr.info pred)
             (CoreExpr.Eq (ret_ce, value)) in
           let expanded_elems =
             RPat.mk_qbase (RPat.lpat_info lpat) (RPat.QLog lpat) :: rest_elems in
           let expanded_pf = ProofSort.Log { info; prop = eq_ce } :: rest_pf in
           go expanded_elems expanded_pf delta

         (* ---- Res take: (take x=ce1;ce2)@ce' → cpat:τ, rpat1:ce1@x, rpat2:ce2@ce' ---- *)
         | RPat.RTake (cpat, rpat1, rpat2) ->
           let pred' = strip_annots pred in
           let* (x, ce1, ce2) =
             ElabM.lift_at rp_loc
               (CoreExprGet.get_take ~construct:"take pattern" pred') in
           let ce1_sort = (CoreExpr.sort_of_info (CoreExpr.info ce1)) in
           let* inner_sort =
             ElabM.lift_at rp_loc
               (SortGet.get_pred ~construct:"take pattern" ce1_sort) in
           let ce_x = ce_of_var x inner_sort in
           let expanded_elems =
             RPat.mk_qbase (RPat.cpat_info cpat) (RPat.QCore cpat)
             :: RPat.mk_qbase (RPat.rpat_info rpat1) (RPat.QRes rpat1)
             :: RPat.mk_qbase (RPat.rpat_info rpat2) (RPat.QRes rpat2)
             :: rest_elems in
           let expanded_pf =
             ProofSort.Comp { info; var = x; sort = inner_sort; eff = Effect.Spec }
             :: ProofSort.Res { info; pred = ce1; value = ce_x }
             :: ProofSort.Res { info; pred = ce2; value }
             :: rest_pf
           in
           go expanded_elems expanded_pf delta

         (* ---- Res fail: fail@ce → log lpat : false, constraint Bot ---- *)
         | RPat.RFail lpat ->
           let pred' = strip_annots pred in
           let* () =
             ElabM.lift_at rp_loc
               (CoreExprGet.get_fail ~construct:"fail pattern" pred') in
           let false_ce = CoreExpr.mk (CoreExpr.info pred) (CoreExpr.BoolLit false) in
           let expanded_elems =
             RPat.mk_qbase (RPat.lpat_info lpat) (RPat.QLog lpat) :: rest_elems in
           let expanded_pf = ProofSort.Log { info; prop = false_ce } :: rest_pf in
           let* (delta', _ct) = go expanded_elems expanded_pf delta in
           return (delta', Constraint.bot pos)

         (* ---- Res let: (let x=ce1;ce2)@ce' → cpat:τ, lpat:x=ce1, rpat:ce2@ce' ---- *)
         | RPat.RLet (lpat, cpat, rpat) ->
           let pred' = strip_annots pred in
           let* (x, ce1, ce2) =
             ElabM.lift_at rp_loc
               (CoreExprGet.get_let ~construct:"let pattern" pred') in
           let sort = (CoreExpr.sort_of_info (CoreExpr.info ce1)) in
           let ce_x = ce_of_var x sort in
           let eq_ce = CoreExpr.mk (CoreExpr.info pred) (CoreExpr.Eq (ce_x, ce1)) in
           let expanded_elems =
             RPat.mk_qbase (RPat.cpat_info cpat) (RPat.QCore cpat)
             :: RPat.mk_qbase (RPat.lpat_info lpat) (RPat.QLog lpat)
             :: RPat.mk_qbase (RPat.rpat_info rpat) (RPat.QRes rpat)
             :: rest_elems in
           let expanded_pf =
             ProofSort.Comp { info; var = x; sort; eff = Effect.Spec }
             :: ProofSort.Log { info; prop = eq_ce }
             :: ProofSort.Res { info; pred = ce2; value }
             :: rest_pf
           in
           go expanded_elems expanded_pf delta

         (* ---- Res iftrue: (if ce then ce1 else ce2)@ce' → rpat:ce1@ce', assert ce ---- *)
         | RPat.RIfTrue rpat ->
           let pred' = strip_annots pred in
           let* (ce, ce1, _ce2) =
             ElabM.lift_at rp_loc
               (CoreExprGet.get_if ~construct:"iftrue pattern" pred') in
           let expanded_elems =
             RPat.mk_qbase (RPat.rpat_info rpat) (RPat.QRes rpat) :: rest_elems in
           let expanded_pf = ProofSort.Res { info; pred = ce1; value } :: rest_pf in
           let* (delta', ct_rest) = go expanded_elems expanded_pf delta in
           return (delta', Constraint.conj pos (Constraint.atom pos ce) ct_rest)

         (* ---- Res iffalse: (if ce then ce1 else ce2)@ce' → rpat:ce2@ce', assert ¬ce ---- *)
         | RPat.RIfFalse rpat ->
           let pred' = strip_annots pred in
           let* (ce, _ce1, ce2) =
             ElabM.lift_at rp_loc
               (CoreExprGet.get_if ~construct:"iffalse pattern" pred') in
           let expanded_elems =
             RPat.mk_qbase (RPat.rpat_info rpat) (RPat.QRes rpat) :: rest_elems in
           let expanded_pf = ProofSort.Res { info; pred = ce2; value } :: rest_pf in
           let not_ce = CoreExpr.mk (CoreExpr.info ce) (CoreExpr.App (Prim.Not, ce)) in
           let* (delta', ct_rest) = go expanded_elems expanded_pf delta in
           return (delta', Constraint.conj pos (Constraint.atom pos not_ce) ct_rest)

         (* ---- Res case: case(ce, ...)@ce' → cpat:τ, lpat:L xk=ce, rpat:cek@ce' ---- *)
         | RPat.RCase (lpat, label, cpat, rpat) ->
           let pred' = strip_annots pred in
           (match CoreExpr.shape pred' with
            | CoreExpr.Case (scrutinee, branches) ->
              (* The user-written [L] in [case L(...)] must be a branch
                 of this case expression; if not, the user typoed. *)
              (match List.find_opt (fun (l, _, _, _) -> Label.compare l label = 0) branches with
               | None ->
                 let case_labels = List.map (fun (l, _, _, _) -> l) branches in
                 ElabM.fail
                   (Error.rcase_label_not_in_branches
                      ~loc:rp_loc ~label ~case_labels)
               | Some (_l, x_br, ce_br, _bi) ->
                 (* Look up the constructor's payload sort, qualified
                    by the scrutinee's head sort. *)
                 let scrut_sort = (CoreExpr.sort_of_info (CoreExpr.info scrutinee)) in
                 let* (d, args) =
                   ElabM.lift_at rp_loc
                     (SortGet.get_app ~construct:"case pattern scrutinee"
                        scrut_sort) in
                 let* payload_sort =
                   ElabM.lift_at rp_loc
                     (CtorLookup.lookup cs d label args) in
                 let ce_x = ce_of_var x_br payload_sort in
                 let inject_ce = CoreExpr.mk (CoreExpr.info scrutinee)
                   (CoreExpr.Inject (label, ce_x)) in
                 let eq_ce = CoreExpr.mk (CoreExpr.info pred)
                   (CoreExpr.Eq (inject_ce, scrutinee)) in
                 let expanded_elems =
                   RPat.mk_qbase (RPat.cpat_info cpat) (RPat.QCore cpat)
                   :: RPat.mk_qbase (RPat.lpat_info lpat) (RPat.QLog lpat)
                   :: RPat.mk_qbase (RPat.rpat_info rpat) (RPat.QRes rpat)
                   :: rest_elems in
                 let expanded_pf =
                   ProofSort.Comp { info; var = x_br; sort = payload_sort; eff = Effect.Spec }
                   :: ProofSort.Log { info; prop = eq_ce }
                   :: ProofSort.Res { info; pred = ce_br; value }
                   :: rest_pf
                 in
                 let* (delta', ct_rest) = go expanded_elems expanded_pf delta in
                 return (delta', Constraint.conj pos (Constraint.is_ pos label scrutinee) ct_rest))
            | _ ->
              fail_pred_shape rp_loc "case pattern" "case ce of { ... }" pred')

         (* ---- Res unfold: f(ce)@ce' → rpat:[ce/x]body@ce' ---- *)
         | RPat.RUnfold rpat ->
           let pred' = strip_annots pred in
           (match CoreExpr.shape pred' with
            | CoreExpr.Call (f, arg) ->
              let* (param, _arg_sort, _ret_sort, _eff, body) =
                ElabM.lift_at rp_loc (Sig.lookup_fundef f cs) in
              let sub = Subst.extend_var param arg Subst.empty in
              let unfolded = Subst.apply_ce sub body in
              let expanded_elems =
                RPat.mk_qbase (RPat.rpat_info rpat) (RPat.QRes rpat) :: rest_elems in
              let expanded_pf = ProofSort.Res { info; pred = unfolded; value } :: rest_pf in
              go expanded_elems expanded_pf delta
            | _ ->
              fail_pred_shape rp_loc "unfold pattern" "f(ce)" pred')

         (* ---- Res annot: (ce : Pred τ)@ce' → rpat:ce@ce' ---- *)
         | RPat.RAnnot rpat ->
           let pred' = strip_annots pred in
           let expanded_elems =
             RPat.mk_qbase (RPat.rpat_info rpat) (RPat.QRes rpat) :: rest_elems in
           let expanded_pf = ProofSort.Res { info; pred = pred'; value } :: rest_pf in
           go expanded_elems expanded_pf delta)

      (* ---- Kind mismatch ---- *)
      | _ -> fail_kind elems entries)

    (* ---- Length mismatch ---- *)
    | [], _ :: _ | _ :: _, [] -> fail_length elems
  in
  go pat_elems pf delta

(* ---------- Program checking ---------- *)

let elab_fundecl_body rs param arg_sort ret_sort eff body_se =
  let cs = RSig.comp rs in
  let gamma = Context.extend param arg_sort (Effect.purify eff) Context.empty in
  let* ce = Elaborate.check cs gamma body_se (Ok ret_sort) eff in
  let ce = Typecheck.annotate_subterm_errors ce in
  match (CoreExpr.info ce)#subterm_errors with
  | e :: _ -> ElabM.fail e
  | [] -> return ce

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
    let* gamma' = lift_at loc (ProofSort.bind gamma domain) in
    let* codomain = elab_pf rs gamma' eff se_codomain in
    let rf = RFunType.{ domain; codomain; eff } in
    let pat_eff = match eff with Effect.Spec -> Effect.Spec | _ -> Effect.Pure in
    let* (delta, ct_pat) = rpat_match rs RCtx.empty pat_eff pat domain in
    let rs' = match eff with
      | Effect.Pure -> rs
      | _ -> RSig.extend name (RSig.RFunSig rf) rs
    in
    let* (checked, delta', ct_body) = check_crt rs' delta eff body codomain in
    if not (RCtx.zero delta') then
      ElabM.fail
        (Error.let_pattern_resource_leak ~loc
           ~leftovers:(List.filter_map (function
             | RCtx.Res { var; pred; value; usage } when not (Usage.is_zero usage) ->
               Some (Format.asprintf "@[<hov 2>%a : %a @@ %a [%a]@]"
                       Var.print var CoreExpr.print pred
                       CoreExpr.print value Usage.print usage)
             | _ -> None) (RCtx.entries delta')))
    else
      let ct_closed = close_ctx loc delta' (Constraint.conj loc ct_pat ct_body) in
      let entry = RSig.RFunSig rf in
      let typed_pat = RPat.map_info (fun b -> mk_rinfo b#loc RCtx.empty (ProofSort.comp domain) eff) pat in
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
  match info#answer with
  | Ok _ -> []
  | Error e -> [e]

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

and collect_errors_qbase (qb : (RProg.typed_rinfo, Var.t) RPat.qbase)
    : Error.t list =
  let here = rinfo_error (RPat.qbase_info qb) in
  let inner =
    match RPat.qbase_shape qb with
    | RPat.QCore cp -> collect_errors_cpat cp
    | RPat.QLog lp -> collect_errors_lpat lp
    | RPat.QRes rp -> collect_errors_rpat_inner rp
    | RPat.QDepRes (cp, rp) ->
      collect_errors_cpat cp @ collect_errors_rpat_inner rp
  in
  here @ inner

let collect_errors_rpat (pat : (RProg.typed_rinfo, Var.t) RPat.t)
    : Error.t list =
  let here = rinfo_error (RPat.info pat) in
  here @ List.concat_map collect_errors_qbase (RPat.elems pat)

let rec collect_errors_lpf (lpf : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RefinedExpr.lpf)
    : Error.t list =
  let here = rinfo_error (RefinedExpr.lpf_info lpf) in
  let inner =
    match RefinedExpr.lpf_shape lpf with
    | RefinedExpr.LVar _ -> []
    | RefinedExpr.LAuto -> []
    | RefinedExpr.LHole _ -> []
    | RefinedExpr.LUnfold (_, ce) -> Typecheck.collect_errors ce
    | RefinedExpr.LOpenRet rp -> collect_errors_rpf rp
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
    | RefinedExpr.RMakeRet lp -> collect_errors_lpf lp
    | RefinedExpr.RMakeTake crt -> collect_errors_crt crt
    | RefinedExpr.RAnnot (rp, ce1, ce2) ->
      collect_errors_rpf rp
      @ Typecheck.collect_errors ce1
      @ Typecheck.collect_errors ce2
    | RefinedExpr.RUnfold rp -> collect_errors_rpf rp
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
    | RefinedExpr.COpenTake rpf -> collect_errors_rpf rpf
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
