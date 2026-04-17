(** Spatial index over typed core expressions — see hoverIndex.mli. *)

(** A node record: the info carried by one typed_ce node. *)
type node = {
  loc  : SourcePos.t;
  ctx  : Context.t;
  sort : Sort.sort;
  eff  : Effect.t;
}

(** The index is a flat list of nodes sorted by span (smallest first
    when spans are nested).  Lookup scans for the tightest enclosing
    span.  This is O(n) in the number of nodes — fine for files with
    a few hundred nodes; a tree-based spatial index can replace it
    later if profiling shows a need. *)
type t = node list

let empty = []

(** Extract a node from a typed_ce info object. *)
let node_of_info (b : Typecheck.typed_info) : node =
  { loc = b#loc; ctx = b#ctx; sort = b#sort; eff = b#eff }

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
  if sl = 0 && el = 0 then false
  else
    (line > sl || (line = sl && col >= sc)) &&
    (line < el || (line = el && col <= ec))

(** Span size heuristic for "smallest enclosing": fewer lines wins;
    ties broken by fewer columns. *)
let span_size loc =
  let dl = SourcePos.end_line loc - SourcePos.start_line loc in
  let dc = SourcePos.end_col loc - SourcePos.start_col loc in
  (dl, dc)

let lookup idx ~line ~col =
  let best = ref None in
  List.iter (fun n ->
    if covers n.loc ~line ~col then
      match !best with
      | None -> best := Some n
      | Some prev ->
        if span_size n.loc < span_size prev.loc then
          best := Some n
  ) idx;
  match !best with
  | None -> None
  | Some n -> Some (n.loc, n.ctx, n.sort, n.eff)

module Test = struct
  let test = []
end
