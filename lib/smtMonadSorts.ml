module SortSet = Set.Make (struct
  type t = Sort.sort
  let compare = Sort.compare
end)

module SortPairSet = Set.Make (struct
  type t = Sort.sort * Sort.sort
  let compare (a1, b1) (a2, b2) =
    let c = Sort.compare a1 a2 in
    if c <> 0 then c else Sort.compare b1 b2
end)

type t = {
  s : SortSet.t;
  pairs : SortPairSet.t;
  own_sorts : SortSet.t;
}

let empty = {
  s = SortSet.empty;
  pairs = SortPairSet.empty;
  own_sorts = SortSet.empty;
}

let add_sort acc tau = { acc with s = SortSet.add tau acc.s }

let add_pair acc tau sigma =
  { acc with
    s = SortSet.add sigma (SortSet.add tau acc.s);
    pairs = SortPairSet.add (tau, sigma) acc.pairs }

let add_own acc tau =
  { acc with
    s = SortSet.add tau acc.s;
    own_sorts = SortSet.add tau acc.own_sorts }

(* Destructure [Pred τ] to τ. If the sort is not a [Pred], returns
   [None] — the caller just skips the addition to S / pairs. This is
   defensive: typechecking should guarantee that Return/Take/Fail
   nodes have a [Pred _] sort. *)
let payload_of_pred s =
  match Sort.shape s with
  | Sort.Pred t -> Some t
  | _ -> None

(* Walk a typed core expression, accumulating monad-sort membership.
   Structural recursion per CLAUDE.md — no folds. *)
let rec walk_ce ce acc =
  match CoreExpr.shape ce with
  | CoreExpr.Var _ | CoreExpr.IntLit _ | CoreExpr.BoolLit _
  | CoreExpr.Fail | CoreExpr.Hole _ ->
    (match CoreExpr.shape ce with
     | CoreExpr.Fail ->
       (match payload_of_pred (CoreExpr.sort_of_info (CoreExpr.info ce)) with
        | Some t -> add_sort acc t
        | None -> acc)
     | _ -> acc)
  | CoreExpr.Let (_, e1, e2)
  | CoreExpr.LetTuple (_, e1, e2) ->
    walk_ce e2 (walk_ce e1 acc)
  | CoreExpr.Tuple es ->
    walk_list es acc
  | CoreExpr.Inject (_, e) ->
    walk_ce e acc
  | CoreExpr.Case (scrut, branches) ->
    let acc = walk_ce scrut acc in
    walk_branches branches acc
  | CoreExpr.Iter (_, e1, e2) ->
    walk_ce e2 (walk_ce e1 acc)
  | CoreExpr.App (Prim.Own tau, e) ->
    walk_ce e (add_own acc tau)
  | CoreExpr.App (_, e) | CoreExpr.Call (_, e) ->
    walk_ce e acc
  | CoreExpr.If (a, b, c) ->
    walk_ce c (walk_ce b (walk_ce a acc))
  | CoreExpr.Annot (e, _) ->
    walk_ce e acc
  | CoreExpr.Eq (a, b) | CoreExpr.And (a, b) ->
    walk_ce b (walk_ce a acc)
  | CoreExpr.Not a ->
    walk_ce a acc
  | CoreExpr.Take (_, bound, body) ->
    let acc =
      match payload_of_pred (CoreExpr.sort_of_info (CoreExpr.info bound)),
            payload_of_pred (CoreExpr.sort_of_info (CoreExpr.info body)) with
      | Some tau, Some sigma -> add_pair acc tau sigma
      | _ -> acc
    in
    walk_ce body (walk_ce bound acc)
  | CoreExpr.Return child ->
    let acc =
      add_sort acc (CoreExpr.sort_of_info (CoreExpr.info child))
    in
    walk_ce child acc

and walk_list xs acc =
  match xs with
  | [] -> acc
  | x :: rest -> walk_list rest (walk_ce x acc)

and walk_branches bs acc =
  match bs with
  | [] -> acc
  | (_, _, body, _) :: rest -> walk_branches rest (walk_ce body acc)

let rec walk_ct ct acc =
  match Constraint.shape ct with
  | Constraint.Top | Constraint.Bot -> acc
  | Constraint.And (a, b) -> walk_ct b (walk_ct a acc)
  | Constraint.Forall (_, _, a) -> walk_ct a acc
  | Constraint.Impl (e, a) -> walk_ct a (walk_ce e acc)
  | Constraint.Atom e -> walk_ce e acc
  | Constraint.Is (_, e) -> walk_ce e acc

let walk_rsig rsig acc =
  let rec loop entries acc =
    match entries with
    | [] -> acc
    | RSig.LFun (_, RSig.FunDef { body; _ }) :: rest ->
      loop rest (walk_ce body acc)
    | _ :: rest ->
      loop rest acc
  in
  loop (RSig.entries rsig) acc

let collect rsig ct =
  walk_ct ct (walk_rsig rsig empty)
