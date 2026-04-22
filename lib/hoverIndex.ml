(** Spatial index over typed core expressions — see hoverIndex.mli. *)

(** A node record: the info carried by one typed node. *)
type node = {
  loc  : SourcePos.t;
  ctx  : Context.t;
  rctx : RCtx.t option;
  sort : Sort.sort;
  eff  : Effect.t;
  pf   : (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) ProofSort.t;
}

(** The index is a flat list of nodes.  Lookup scans for the tightest
    enclosing span.  This is O(n) in the number of nodes — fine for
    files with a few hundred nodes; a tree-based spatial index can
    replace it later if profiling shows a need. *)
type t = node list

let empty = []

(** Extract a node from a typed_ce info object (core — no refined context). *)
let node_of_info (b : Typecheck.typed_info) : node =
  { loc = b#loc; ctx = b#ctx; rctx = None; sort = b#sort; eff = b#eff; pf = [] }

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
  { loc = b#loc; ctx = b#ctx; rctx = Some b#rctx; sort = b#sort; eff = b#eff; pf = b#pf }

(** Collect typed nodes from refined pattern elements. *)
let collect_rpat acc pat =
  List.fold_left (fun acc elem ->
    node_of_rinfo (RPat.qbase_info elem) :: acc
  ) acc (RPat.elems pat)

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

(** Walk refined expression trees, collecting hover nodes from both
    the refined node annotations (typed_info) and embedded typed_ce
    expressions. Four mutually recursive functions mirror the four
    refined expression sorts. *)
let rec collect_crt acc crt =
  let acc = node_of_rinfo (RefinedExpr.crt_info crt) :: acc in
  match RefinedExpr.crt_shape crt with
  | RefinedExpr.CLet (pat, crt1, crt2) ->
    collect_crt (collect_crt (collect_rpat acc pat) crt1) crt2
  | RefinedExpr.CLetLog (_, lpf, crt') ->
    collect_crt (collect_lpf acc lpf) crt'
  | RefinedExpr.CLetRes (_, rpf, crt') ->
    collect_crt (collect_rpf acc rpf) crt'
  | RefinedExpr.CLetCore (_, _, e, crt') ->
    collect_crt (collect acc e) crt'
  | RefinedExpr.CAnnot (crt', pf) ->
    collect_pf (collect_crt acc crt') pf
  | RefinedExpr.CPrimApp (_, spine) ->
    collect_spine acc spine
  | RefinedExpr.CCall (_, spine) ->
    collect_spine acc spine
  | RefinedExpr.CTuple spine ->
    collect_spine acc spine
  | RefinedExpr.CIter (e, pat, crt1, crt2) ->
    collect_crt (collect_crt (collect_rpat (collect acc e) pat) crt1) crt2
  | RefinedExpr.CIf (_, e, crt1, crt2) ->
    collect_crt (collect_crt (collect acc e) crt1) crt2
  | RefinedExpr.CCase (_, e, branches) ->
    List.fold_left (fun acc (_, b, _, body) ->
      collect_crt (node_of_rinfo b :: acc) body)
      (collect acc e) branches
  | RefinedExpr.CExfalso | RefinedExpr.CHole _ -> acc
  | RefinedExpr.COpenTake rpf ->
    collect_rpf acc rpf

and collect_lpf acc lpf =
  let acc = node_of_rinfo (RefinedExpr.lpf_info lpf) :: acc in
  match RefinedExpr.lpf_shape lpf with
  | RefinedExpr.LVar _ | RefinedExpr.LAuto | RefinedExpr.LHole _ -> acc
  | RefinedExpr.LUnfold (_, e) -> collect acc e
  | RefinedExpr.LOpenRet rpf -> collect_rpf acc rpf
  | RefinedExpr.LAnnot (lpf', e) ->
    collect_lpf (collect acc e) lpf'

and collect_rpf acc rpf =
  let acc = node_of_rinfo (RefinedExpr.rpf_info rpf) :: acc in
  match RefinedExpr.rpf_shape rpf with
  | RefinedExpr.RVar _ | RefinedExpr.RHole _ -> acc
  | RefinedExpr.RMakeRet lpf -> collect_lpf acc lpf
  | RefinedExpr.RMakeTake crt -> collect_crt acc crt
  | RefinedExpr.RAnnot (rpf', e1, e2) ->
    collect_rpf (collect (collect acc e1) e2) rpf'

and collect_spine acc spine =
  let acc = node_of_rinfo (RefinedExpr.spine_info spine) :: acc in
  match RefinedExpr.spine_shape spine with
  | RefinedExpr.SNil -> acc
  | RefinedExpr.SCore (e, rest) ->
    collect_spine (collect acc e) rest
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
  |> Option.map (fun n -> (n.loc, n.ctx, n.rctx, n.sort, n.eff, n.pf))

module Test = struct
  let test = []
end
