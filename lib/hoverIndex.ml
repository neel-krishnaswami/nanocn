(** Spatial index over typed core expressions — see hoverIndex.mli. *)

(** A node record: the info carried by one typed_ce node. *)
type node = {
  loc  : SourcePos.t;
  ctx  : Context.t;
  sort : Sort.sort;
  eff  : Effect.t;
}

(** The index is a flat list of nodes.  Lookup scans for the tightest
    enclosing span.  This is O(n) in the number of nodes — fine for
    files with a few hundred nodes; a tree-based spatial index can
    replace it later if profiling shows a need. *)
type t = node list

let empty = []

(** Extract a node from a typed_ce info object. *)
let node_of_info (b : Typecheck.typed_info) : node =
  { loc = b#loc; ctx = b#ctx; sort = b#sort; eff = b#eff }

(** Collect all nodes from a typed_ce tree by structural recursion. *)
let rec collect (acc : node list) (e : Typecheck.typed_ce) : node list =
  let b = CoreExpr.info e in
  let n = node_of_info b in
  let acc = n :: acc in
  match CoreExpr.shape e with
  | CoreExpr.Var _ | CoreExpr.IntLit _ | CoreExpr.BoolLit _ | CoreExpr.Fail ->
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
  |> Option.map (fun n -> (n.loc, n.ctx, n.sort, n.eff))

module Test = struct
  let test = []
end
