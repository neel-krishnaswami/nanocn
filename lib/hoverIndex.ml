(** Spatial index over typed core expressions — see hoverIndex.mli. *)

(** A node record: the info carried by one typed node. *)
type node = {
  loc  : SourcePos.t;
  ctx  : Context.t;
  rctx : RCtx.t option;
  sort : Sort.sort;
  eff  : Effect.t;
  goal : RProg.goal;
}

(** The index is a flat list of nodes.  Lookup scans for the tightest
    enclosing span.  This is O(n) in the number of nodes — fine for
    files with a few hundred nodes; a tree-based spatial index can
    replace it later if profiling shows a need. *)
type t = node list

let empty = []

(** [safe_sort_of_info b] reads the sort from [b#answer], falling
    back to a [Bool] placeholder when the typechecker recorded an
    Error rather than a sort.  Hover queries should never crash on
    error-tainted nodes; the actual error surfaces separately as a
    Flymake diagnostic. *)
let safe_sort_of_info (b : Typecheck.typed_info) : Sort.sort =
  match b#answer with
  | Ok s -> s
  | Error _ -> Sort.mk (object method loc = b#loc end) Sort.Bool

(** Extract a node from a typed_ce info object (core — no refined context). *)
let node_of_info (b : Typecheck.typed_info) : node =
  { loc = b#loc; ctx = b#ctx; rctx = None;
    sort = safe_sort_of_info b; eff = b#eff; goal = RProg.NoGoal }

(** Collect all nodes from a typed_ce tree by structural recursion. *)
let rec collect (acc : node list) (e : Typecheck.typed_ce) : node list =
  let b = CoreExpr.info e in
  let n = node_of_info b in
  let acc = n :: acc in
  match CoreExpr.shape e with
  | CoreExpr.Var _ | CoreExpr.IntLit _ | CoreExpr.BoolLit _ | CoreExpr.Fail
  | CoreExpr.Hole _ ->
    acc
  | CoreExpr.Let ((_, _), e1, e2)
  | CoreExpr.LetTuple (_, e1, e2)
  | CoreExpr.Take ((_, _), e1, e2)
  | CoreExpr.Iter (_, e1, e2)
  | CoreExpr.Eq (e1, e2)
  | CoreExpr.And (e1, e2) ->
    let acc = collect acc e1 in
    collect acc e2
  | CoreExpr.If (e1, e2, e3) ->
    let acc = collect acc e1 in
    let acc = collect acc e2 in
    collect acc e3
  | CoreExpr.Tuple es ->
    List.fold_left collect acc es
  | CoreExpr.Inject (_, e1)
  | CoreExpr.App (_, e1)
  | CoreExpr.Call (_, e1)
  | CoreExpr.Not e1
  | CoreExpr.Return e1 ->
    collect acc e1
  | CoreExpr.Annot (e1, _) ->
    collect acc e1
  | CoreExpr.Case (scrut, branches) ->
    let acc = collect acc scrut in
    List.fold_left (fun acc (_, _, body, _) -> collect acc body) acc branches

let of_typed_decls decls =
  List.fold_left (fun acc decl ->
    match decl with
    | Prog.CoreFunDecl { body; _ } -> collect acc body
    | Prog.CoreSortDecl _ | Prog.CoreTypeDecl _ -> acc
  ) empty decls

(** Extract a hover node from a refined expression's typed_rinfo annotation. *)
let node_of_rinfo (b : RProg.typed_rinfo) : node =
  { loc = b#loc; ctx = b#ctx; rctx = Some b#rctx; sort = b#sort; eff = b#eff; goal = b#goal }

(** Collect typed nodes from refined pattern elements.  Walks
    [RPat.shape] and emits one hover node per head element
    (cpat / lpat / rpat).  The wrapper [t]'s info at each cons
    level represents the "rest of pattern" position, not a per-qbase
    position, so we use the head element's info instead. *)
let collect_rpat acc pat =
  let rec go acc t =
    match RPat.shape t with
    | RPat.QNil -> acc
    | RPat.QCore (cp, rest) ->
      go (node_of_rinfo (RPat.cpat_info cp) :: acc) rest
    | RPat.QLog (lp, rest) ->
      go (node_of_rinfo (RPat.lpat_info lp) :: acc) rest
    | RPat.QRes (rp, rest) ->
      go (node_of_rinfo (RPat.rpat_info rp) :: acc) rest
    | RPat.QDepRes (cp, rp, rest) ->
      let acc = node_of_rinfo (RPat.rpat_info rp) :: acc in
      let acc = node_of_rinfo (RPat.cpat_info cp) :: acc in
      go acc rest
  in
  go acc pat

(** Collect typed nodes from a proof sort entry list. *)
let collect_pf acc pf =
  List.fold_left (fun acc entry ->
    let acc = node_of_rinfo (ProofSort.entry_info entry) :: acc in
    match entry with
    | ProofSort.Comp { info = _; _ } -> acc
    | ProofSort.Log { info = _; prop } -> collect acc prop
    | ProofSort.Res { info = _; pred; value } -> collect (collect acc pred) value
    | ProofSort.DepRes { info = _; pred; _ } -> collect acc pred
  ) acc pf

(** Collect nodes from a core expression inside a refined term,
    enriching each node with the enclosing refined context and goal.
    This ensures hovering on core subterms shows the refined context
    (resources, logical facts) rather than the erased core context. *)
let rec collect_enriched (rinfo : RProg.typed_rinfo) (acc : node list) (e : Typecheck.typed_ce) : node list =
  let b = CoreExpr.info e in
  let n = { loc = b#loc; ctx = b#ctx; rctx = Some rinfo#rctx;
            sort = safe_sort_of_info b; eff = b#eff; goal = rinfo#goal } in
  let acc = n :: acc in
  match CoreExpr.shape e with
  | CoreExpr.Var _ | CoreExpr.IntLit _ | CoreExpr.BoolLit _
  | CoreExpr.Fail | CoreExpr.Hole _ ->
    acc
  | CoreExpr.Let (_, e1, e2) | CoreExpr.LetTuple (_, e1, e2)
  | CoreExpr.Take (_, e1, e2) | CoreExpr.Iter (_, e1, e2)
  | CoreExpr.Eq (e1, e2) | CoreExpr.And (e1, e2) ->
    let acc = collect_enriched rinfo acc e1 in
    collect_enriched rinfo acc e2
  | CoreExpr.If (e1, e2, e3) ->
    let acc = collect_enriched rinfo acc e1 in
    let acc = collect_enriched rinfo acc e2 in
    collect_enriched rinfo acc e3
  | CoreExpr.Tuple es ->
    List.fold_left (collect_enriched rinfo) acc es
  | CoreExpr.Inject (_, e1) | CoreExpr.App (_, e1)
  | CoreExpr.Call (_, e1) | CoreExpr.Not e1
  | CoreExpr.Return e1 ->
    collect_enriched rinfo acc e1
  | CoreExpr.Annot (e1, _) ->
    collect_enriched rinfo acc e1
  | CoreExpr.Case (scrut, branches) ->
    let acc = collect_enriched rinfo acc scrut in
    List.fold_left (fun acc (_, _, body, _) -> collect_enriched rinfo acc body) acc branches

(** Walk refined expression trees, collecting hover nodes from both
    the refined node annotations (typed_info) and embedded typed_ce
    expressions. Four mutually recursive functions mirror the four
    refined expression sorts. *)
let rec collect_crt acc crt =
  let ri = RefinedExpr.crt_info crt in
  let acc = node_of_rinfo ri :: acc in
  match RefinedExpr.crt_shape crt with
  | RefinedExpr.CLet (pat, crt1, crt2) ->
    collect_crt (collect_crt (collect_rpat acc pat) crt1) crt2
  | RefinedExpr.CLetLog (_, lpf, crt') ->
    collect_crt (collect_lpf acc lpf) crt'
  | RefinedExpr.CLetRes (_, rpf, crt') ->
    collect_crt (collect_rpf acc rpf) crt'
  | RefinedExpr.CLetCore (_, _, e, crt') ->
    collect_crt (collect_enriched ri acc e) crt'
  | RefinedExpr.CAnnot (crt', pf) ->
    collect_pf (collect_crt acc crt') pf
  | RefinedExpr.CPrimApp (_, spine) ->
    collect_spine acc spine
  | RefinedExpr.CCall (_, spine) ->
    collect_spine acc spine
  | RefinedExpr.CTuple spine ->
    collect_spine acc spine
  | RefinedExpr.CIter (e, pat, crt1, crt2) ->
    collect_crt (collect_crt (collect_rpat (collect_enriched ri acc e) pat) crt1) crt2
  | RefinedExpr.CIf (_, e, crt1, crt2) ->
    collect_crt (collect_crt (collect_enriched ri acc e) crt1) crt2
  | RefinedExpr.CCase (_, e, branches) ->
    List.fold_left (fun acc (_, b, _, body) ->
      collect_crt (node_of_rinfo b :: acc) body)
      (collect_enriched ri acc e) branches
  | RefinedExpr.CExfalso | RefinedExpr.CHole _ -> acc
  | RefinedExpr.COpenTake rpf ->
    collect_rpf acc rpf

and collect_lpf acc lpf =
  let ri = RefinedExpr.lpf_info lpf in
  let acc = node_of_rinfo ri :: acc in
  match RefinedExpr.lpf_shape lpf with
  | RefinedExpr.LVar _ | RefinedExpr.LAuto | RefinedExpr.LHole _ -> acc
  | RefinedExpr.LUnfold (_, e) -> collect_enriched ri acc e
  | RefinedExpr.LOpenRet rpf -> collect_rpf acc rpf
  | RefinedExpr.LAnnot (lpf', e) ->
    collect_lpf (collect_enriched ri acc e) lpf'

and collect_rpf acc rpf =
  let ri = RefinedExpr.rpf_info rpf in
  let acc = node_of_rinfo ri :: acc in
  match RefinedExpr.rpf_shape rpf with
  | RefinedExpr.RVar _ | RefinedExpr.RHole _ -> acc
  | RefinedExpr.RAnnot (rpf', e1, e2) ->
    collect_rpf (collect_enriched ri (collect_enriched ri acc e1) e2) rpf'
  | RefinedExpr.RReturn lpf -> collect_lpf acc lpf
  | RefinedExpr.RTake (r1, r2) -> collect_rpf (collect_rpf acc r1) r2
  | RefinedExpr.RFail lpf -> collect_lpf acc lpf
  | RefinedExpr.RLet (_, _, rpf') -> collect_rpf acc rpf'
  | RefinedExpr.RCase (_, _, _, rpf') -> collect_rpf acc rpf'
  | RefinedExpr.RIfTrue rpf' -> collect_rpf acc rpf'
  | RefinedExpr.RIfFalse rpf' -> collect_rpf acc rpf'
  | RefinedExpr.RUnfold rpf' -> collect_rpf acc rpf'
  | RefinedExpr.RAnnotStrip rpf' -> collect_rpf acc rpf'

and collect_spine acc spine =
  let ri = RefinedExpr.spine_info spine in
  let acc = node_of_rinfo ri :: acc in
  match RefinedExpr.spine_shape spine with
  | RefinedExpr.SNil -> acc
  | RefinedExpr.SCore (e, rest) ->
    collect_spine (collect_enriched ri acc e) rest
  | RefinedExpr.SLog (lpf, rest) ->
    collect_spine (collect_lpf acc lpf) rest
  | RefinedExpr.SRes (rpf, rest) ->
    collect_spine (collect_rpf acc rpf) rest

let of_typed_rprog (prog : RProg.typed) =
  let acc = List.fold_left (fun acc decl ->
    match decl with
    | RProg.SortDecl _ | RProg.TypeDecl _ -> acc
    | RProg.FunDecl { body; _ } -> collect acc body
    | RProg.RFunDecl { body; domain; codomain; _ } ->
      let acc = collect_pf acc domain in
      let acc = collect_pf acc codomain in
      collect_crt acc body
  ) empty prog.decls in
  let acc = collect_pf acc prog.main_pf in
  collect_crt acc prog.main_body

let add_typed_expr e idx =
  collect idx e

(** Does the source position span the given (1-based line, 0-based col)? *)
let covers loc ~line ~col =
  let sl = SourcePos.start_line loc in
  let sc = SourcePos.start_col loc in
  let el = SourcePos.end_line loc in
  let ec = SourcePos.end_col loc in
  (* Dummy positions don't cover anything. *)
  if Int.equal sl 0 && Int.equal el 0 then false
  else
    (line > sl || (Int.equal line sl && col >= sc)) &&
    (line < el || (Int.equal line el && col <= ec))

(** Span size: (line-delta, col-delta).  Smaller is tighter. *)
let span_lines loc = SourcePos.end_line loc - SourcePos.start_line loc
let span_cols  loc = SourcePos.end_col loc - SourcePos.start_col loc

(** Compare two spans: fewer lines wins; ties broken by fewer columns. *)
let tighter_span loc1 loc2 =
  let dl1 = span_lines loc1 in
  let dl2 = span_lines loc2 in
  match Int.compare dl1 dl2 with
  | n when n < 0 -> true
  | n when n > 0 -> false
  | _ -> Int.compare (span_cols loc1) (span_cols loc2) < 0

let lookup idx ~line ~col =
  List.fold_left (fun best n ->
    if covers n.loc ~line ~col then
      match best with
      | None -> Some n
      | Some prev ->
        if tighter_span n.loc prev.loc then Some n else best
    else best
  ) None idx
  |> Option.map (fun n -> (n.loc, n.ctx, n.rctx, n.sort, n.eff, n.goal))

module Test = struct
  let test = []
end
