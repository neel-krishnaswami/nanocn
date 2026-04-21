type located_cmd = { pos : SourcePos.t; cmd : SmtSexp.sexp }

type config = { position_trace : bool }

let default_config = { position_trace = true }

let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

(* ---------- Sexp builder helpers ---------- *)

let loc_info loc = object method loc = loc end

let sym loc s = SmtSexp.symbol (loc_info loc) s
let num loc n = SmtSexp.numeral_s (loc_info loc) (string_of_int n)
let str loc s = SmtSexp.string_lit (loc_info loc) s
let list_of loc xs = SmtSexp.list (loc_info loc) xs
let res loc r = SmtSexp.reserved (loc_info loc) r

let pos_var i = Printf.sprintf "pos-%d" i

(* [(mk-pos "file" line col)] — builds a [Pos] value term. *)
let pos_term loc =
  let file = SourcePos.file loc in
  let line = SourcePos.start_line loc in
  let col = SourcePos.start_col loc in
  list_of loc [sym loc "mk-pos"; str loc file; num loc line; num loc col]

(* [(declare-const pos-N (PosList Pos))] *)
let decl_pos loc n =
  list_of loc [
    res loc SmtAtom.R_declare_const;
    sym loc (pos_var n);
    list_of loc [sym loc "PosList"; sym loc "Pos"];
  ]

(* [(assert (= (ptail pos-parent) pos-N))] *)
let link_tail loc n parent =
  list_of loc [
    res loc SmtAtom.R_assert;
    list_of loc [
      sym loc "=";
      list_of loc [sym loc "ptail"; sym loc (pos_var parent)];
      sym loc (pos_var n);
    ];
  ]

(* [(assert (= (phead pos-N) (mk-pos ...)))] *)
let set_head loc n =
  list_of loc [
    res loc SmtAtom.R_assert;
    list_of loc [
      sym loc "=";
      list_of loc [sym loc "phead"; sym loc (pos_var n)];
      pos_term loc;
    ];
  ]

(* [(assert (= (ptail pos-N) pnil))] *)
let close_tail loc n =
  list_of loc [
    res loc SmtAtom.R_assert;
    list_of loc [
      sym loc "=";
      list_of loc [sym loc "ptail"; sym loc (pos_var n)];
      sym loc "pnil";
    ];
  ]

let scope_enter_cmds loc n parent =
  [ decl_pos loc n; link_tail loc n parent; set_head loc n ]

(* ---------- Walker ---------- *)

(* Walk a constraint tree, accumulating located commands. State is
   threaded explicitly: [parent] is the enclosing scope index (pos-0
   at the top), [next] is the next fresh index to allocate. *)
let rec walk config ct parent next
  : (located_cmd list * int, string) result =
  let pos = (Constraint.info ct)#loc in
  let located cmd = { pos; cmd } in
  let scope_cmds n =
    if config.position_trace
    then List.map located (scope_enter_cmds pos n parent)
    else []
  in
  let close_cmd n =
    if config.position_trace
    then [located (close_tail pos n)]
    else []
  in
  match Constraint.shape ct with
  | Constraint.Top ->
    Ok ([], next)
  | Constraint.Bot ->
    let n = next in
    let pre = scope_cmds n in
    let post = close_cmd n in
    let cs = located (list_of pos [res pos SmtAtom.R_check_sat]) in
    Ok (pre @ post @ [cs], n + 1)
  | Constraint.And (a, b) ->
    let* (ca, next_a) = walk config a parent next in
    let* (cb, next_b) = walk config b parent next_a in
    Ok (ca @ cb, next_b)
  | Constraint.Impl (e, a) ->
    let* e' = SmtExpr.of_ce e in
    let n = next in
    let pre = scope_cmds n in
    let assert_cmd = located (list_of pos [res pos SmtAtom.R_assert; e']) in
    let* (ca, next') = walk config a n (n + 1) in
    Ok (pre @ [assert_cmd] @ ca, next')
  | Constraint.Forall (x, tau, a) ->
    let n = next in
    let push_cmd = located (list_of pos [res pos SmtAtom.R_push; num pos 1]) in
    let decl = located (list_of pos [
      res pos SmtAtom.R_declare_const;
      sym pos (SmtSym.of_var x);
      SmtExpr.of_sort tau;
    ]) in
    let pre = scope_cmds n in
    let pop_cmd = located (list_of pos [res pos SmtAtom.R_pop; num pos 1]) in
    let* (ca, next') = walk config a n (n + 1) in
    Ok ([push_cmd; decl] @ pre @ ca @ [pop_cmd], next')
  | Constraint.Atom e ->
    let* e' = SmtExpr.of_ce e in
    let n = next in
    let pre = scope_cmds n in
    let post = close_cmd n in
    let neg = list_of pos [sym pos "not"; e'] in
    let assert_cmd = located (list_of pos [res pos SmtAtom.R_assert; neg]) in
    let cs = located (list_of pos [res pos SmtAtom.R_check_sat]) in
    Ok (pre @ [assert_cmd] @ post @ [cs], n + 1)
  | Constraint.Is (l, e) ->
    let* e' = SmtExpr.of_ce e in
    let n = next in
    let pre = scope_cmds n in
    let post = close_cmd n in
    let tester = sym pos (Format.asprintf "is-%a" Label.print l) in
    let is_expr = list_of pos [tester; e'] in
    let neg = list_of pos [sym pos "not"; is_expr] in
    let assert_cmd = located (list_of pos [res pos SmtAtom.R_assert; neg]) in
    let cs = located (list_of pos [res pos SmtAtom.R_check_sat]) in
    Ok (pre @ [assert_cmd] @ post @ [cs], n + 1)

(* ---------- Free-variable collection ----------

   Constraints produced by the refined typechecker may mention
   variables introduced by enclosing program-level binders that do
   not appear as [Forall] nodes in the tree (e.g. the parameters of
   a refined function, or the [main] function's arguments). For each
   such free variable we must emit a top-level [declare-const] so
   the solver can interpret its references. *)

module VarMap = Map.Make (Var)

let rec free_ce bound ce acc =
  match CoreExpr.shape ce with
  | CoreExpr.Var v ->
    if VarMap.mem v bound then acc
    else VarMap.add v (CoreExpr.info ce)#sort acc
  | CoreExpr.IntLit _ | CoreExpr.BoolLit _ | CoreExpr.Fail ->
    acc
  | CoreExpr.Let ((x, _), e1, e2) ->
    let acc = free_ce bound e1 acc in
    free_ce (VarMap.add x () bound) e2 acc
  | CoreExpr.LetTuple (xs, e1, e2) ->
    let acc = free_ce bound e1 acc in
    let bound' =
      List.fold_left (fun b (x, _) -> VarMap.add x () b) bound xs
    in
    free_ce bound' e2 acc
  | CoreExpr.Tuple es ->
    List.fold_left (fun a e -> free_ce bound e a) acc es
  | CoreExpr.Inject (_, e) | CoreExpr.Annot (e, _)
  | CoreExpr.Return e | CoreExpr.Not e ->
    free_ce bound e acc
  | CoreExpr.Case (scrut, branches) ->
    let acc = free_ce bound scrut acc in
    List.fold_left (fun a (_, x, body, _) ->
      free_ce (VarMap.add x () bound) body a)
      acc branches
  | CoreExpr.Iter (x, e1, e2) ->
    let acc = free_ce bound e1 acc in
    free_ce (VarMap.add x () bound) e2 acc
  | CoreExpr.App (_, e) | CoreExpr.Call (_, e) ->
    free_ce bound e acc
  | CoreExpr.If (a, b, c) ->
    free_ce bound c (free_ce bound b (free_ce bound a acc))
  | CoreExpr.Eq (a, b) | CoreExpr.And (a, b) ->
    free_ce bound b (free_ce bound a acc)
  | CoreExpr.Take ((x, _), e1, e2) ->
    let acc = free_ce bound e1 acc in
    free_ce (VarMap.add x () bound) e2 acc

let rec free_ct bound ct acc =
  match Constraint.shape ct with
  | Constraint.Top | Constraint.Bot -> acc
  | Constraint.And (a, b) ->
    free_ct bound b (free_ct bound a acc)
  | Constraint.Forall (x, _, a) ->
    free_ct (VarMap.add x () bound) a acc
  | Constraint.Impl (e, a) ->
    free_ct bound a (free_ce bound e acc)
  | Constraint.Atom e ->
    free_ce bound e acc
  | Constraint.Is (_, e) ->
    free_ce bound e acc

let free_vars_of_ct ct =
  free_ct VarMap.empty ct VarMap.empty

(* Emit [(declare-const v τ)] for each free variable, at the root
   source position. *)
let free_var_decls root_pos ct =
  let fv = free_vars_of_ct ct in
  VarMap.fold (fun v tau acc ->
    let decl =
      list_of root_pos [
        res root_pos SmtAtom.R_declare_const;
        sym root_pos (SmtSym.of_var v);
        SmtExpr.of_sort tau;
      ]
    in
    { pos = root_pos; cmd = decl } :: acc
  ) fv []

let of_ct ?(config = default_config) ct =
  let root_pos = (Constraint.info ct)#loc in
  let root_init =
    if config.position_trace then
      [ { pos = root_pos; cmd = decl_pos root_pos 0 };
        { pos = root_pos; cmd = set_head root_pos 0 } ]
    else []
  in
  let fv_decls = free_var_decls root_pos ct in
  match walk config ct 0 1 with
  | Ok (cmds, _) -> Ok (root_init @ fv_decls @ cmds)
  | Error _ as e -> e
