(** Type-directed pattern-expansion code actions — see [.mli]. *)

type edit = {
  range : SourcePos.t;
  new_text : string;
}

type action = {
  title : string;
  edits : edit list;
}

type body_crt =
  (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RefinedExpr.crt

(** [scope] is the crt body in whose scope the pattern's bound
    variable lives.  Distinct candidate kinds let the dispatcher
    apply the right expansion rule. *)
type core_var_candidate = {
  var : Var.t;
  info : RProg.typed_rinfo;
  pat_loc : SourcePos.t;
  sort : Sort.sort;
  scope : body_crt;
}

type resource_var_candidate = {
  var : Var.t;
  info : RProg.typed_rinfo;
  pat_loc : SourcePos.t;
  pred : CoreExpr.typed_ce;
  value : CoreExpr.typed_ce;
  scope : body_crt;
}

type candidate =
  | Cand_core of core_var_candidate
  | Cand_resource of resource_var_candidate

let candidate_loc = function
  | Cand_core c -> c.pat_loc
  | Cand_resource c -> c.pat_loc

(* ================================================================== *)
(* Cursor / span helpers                                              *)
(* ================================================================== *)

(** Does [loc] cover the cursor at [(line, col)]?  Lines are 1-based
    and columns are 0-based, matching the existing [HoverIndex.covers]
    convention. *)
let covers loc ~line ~col =
  let sl = SourcePos.start_line loc in
  let sc = SourcePos.start_col loc in
  let el = SourcePos.end_line loc in
  let ec = SourcePos.end_col loc in
  if Int.equal sl 0 && Int.equal el 0 then false
  else
    (line > sl || (Int.equal line sl && col >= sc)) &&
    (line < el || (Int.equal line el && col <= ec))

(** Lexicographic span size — smaller is tighter. *)
let span_size loc =
  ( SourcePos.end_line loc - SourcePos.start_line loc,
    SourcePos.end_col  loc - SourcePos.start_col  loc )

let tighter a b =
  let cmp = compare (span_size a) (span_size b) in
  cmp < 0

(* ================================================================== *)
(* Pattern walker                                                     *)
(* ================================================================== *)

(** [walk_for_pattern prog ~line ~col] descends through [prog] looking
    for the tightest variable-binding pattern subterm whose source loc
    covers the cursor.  Records the enclosing crt body so we know the
    scope in which the bound variable is used. *)
let walk_for_pattern (prog : RProg.typed) ~line ~col : candidate option =
  let best : candidate option ref = ref None in
  let consider cand =
    match !best with
    | None -> best := Some cand
    | Some b when tighter (candidate_loc cand) (candidate_loc b) ->
      best := Some cand
    | _ -> ()
  in

  let rec go_cpat ~scope cp =
    let b = RPat.cpat_info cp in
    if not (covers b#loc ~line ~col) then ()
    else
      match RPat.cpat_shape cp with
      | RPat.CVar v ->
        consider (Cand_core {
          var = v;
          info = b;
          pat_loc = b#loc;
          sort = b#sort;
          scope;
        })
      | RPat.CTuple cps ->
        List.iter (go_cpat ~scope) cps

  and go_lpat ~scope:_ _lp = ()  (* No actions for logical patterns. *)

  and go_rpat ~scope rp =
    let b = RPat.rpat_info rp in
    if not (covers b#loc ~line ~col) then ()
    else
      match RPat.rpat_shape rp with
      | RPat.RVar v ->
        (match b#goal with
         | RProg.RPatGoal (pred, value) ->
           consider (Cand_resource {
             var = v;
             info = b;
             pat_loc = b#loc;
             pred;
             value;
             scope;
           })
         | _ -> ())
      | RPat.RReturn lp | RPat.RFail lp -> go_lpat ~scope lp
      | RPat.RTake (cp, rp1, rp2) ->
        go_cpat ~scope cp; go_rpat ~scope rp1; go_rpat ~scope rp2
      | RPat.RLet (lp, cp, rp')
      | RPat.RCase (lp, _, cp, rp') ->
        go_lpat ~scope lp; go_cpat ~scope cp; go_rpat ~scope rp'
      | RPat.RIfTrue rp'
      | RPat.RIfFalse rp'
      | RPat.RUnfold rp'
      | RPat.RAnnot rp' ->
        go_rpat ~scope rp'

  and go_q ~scope q =
    let b = RPat.info q in
    if not (covers b#loc ~line ~col) then ()
    else
      match RPat.shape q with
      | RPat.QNil -> ()
      | RPat.QCore (cp, rest) ->
        go_cpat ~scope cp; go_q ~scope rest
      | RPat.QLog (lp, rest) ->
        go_lpat ~scope lp; go_q ~scope rest
      | RPat.QRes (rp, rest) ->
        go_rpat ~scope rp; go_q ~scope rest
      | RPat.QDepRes (cp, rp, rest) ->
        go_cpat ~scope cp; go_rpat ~scope rp; go_q ~scope rest

  and go_crt crt =
    match RefinedExpr.crt_shape crt with
    | RefinedExpr.CLet (pat, crt1, crt2) ->
      go_q ~scope:crt2 pat;
      go_crt crt1;
      go_crt crt2
    | RefinedExpr.CLetLog (lp, _lpf, body) ->
      go_lpat ~scope:body lp;
      go_crt body
    | RefinedExpr.CLetRes (rp, _rpf, body) ->
      go_rpat ~scope:body rp;
      go_crt body
    | RefinedExpr.CLetCore (lp, cp, _ce, body) ->
      go_lpat ~scope:body lp;
      go_cpat ~scope:body cp;
      go_crt body
    | RefinedExpr.CAnnot (crt', _pf) ->
      go_crt crt'
    | RefinedExpr.CIter (_e, pat, crt1, _crt2) ->
      (* Pat is in scope for the loop body crt1.  Skip crt2 (post-loop
         continuation does not see the loop's binders). *)
      go_q ~scope:crt1 pat;
      go_crt crt1
    | RefinedExpr.CIf (_, _, crt1, crt2) ->
      go_crt crt1; go_crt crt2
    | RefinedExpr.CCase (_, _, branches) ->
      List.iter (fun (_, _, _, body) -> go_crt body) branches
    | RefinedExpr.CPrimApp _ | RefinedExpr.CCall _ | RefinedExpr.CTuple _
    | RefinedExpr.CExfalso | RefinedExpr.CHole _ -> ()
  in

  List.iter (fun decl ->
    match decl with
    | RProg.SortDecl _ | RProg.TypeDecl _ | RProg.FunDecl _ -> ()
    | RProg.RFunDecl { pat; body; _ } ->
      go_q ~scope:body pat;
      go_crt body
  ) prog.RProg.decls;
  go_crt prog.RProg.main_body;
  !best

(* ================================================================== *)
(* Name generation                                                    *)
(* ================================================================== *)

(** All variable names visible at a pattern site, gathered from both
    the core context and the refined context.  Used for collision
    checking when generating new pattern names. *)
let names_in_scope (ctx : Context.t) (rctx : RCtx.t) : string list =
  let core_names = List.filter_map (fun b ->
    match b with
    | Context.Term (v, _, _) -> Some (Var.name v)
    | Context.Unknown v -> Some (Var.name v)
    | _ -> None
  ) (Context.to_list ctx) in
  let r_names = List.filter_map (fun e ->
    match e with
    | RCtx.Comp { var; _ }
    | RCtx.Log  { var; _ }
    | RCtx.Res  { var; _ }
    | RCtx.Unknown { var } -> Some (Var.name var)
  ) (RCtx.entries rctx) in
  core_names @ r_names

(** [fresh ~taken base] returns the first of [base, base ^ "'",
    base ^ "''", …] that is not present in [taken].  Mutating [taken]
    so subsequent calls don't pick the same name. *)
let fresh ~taken base =
  let rec try_ s =
    if List.mem s !taken then try_ (s ^ "'")
    else (taken := s :: !taken; s)
  in
  try_ base

(* ================================================================== *)
(* Variable occurrences in a typed crt                                *)
(* ================================================================== *)

(** Find every source loc at which [target] appears as a free use
    inside a typed core expression.  Respects shadowing — if an
    inner binder rebinds the same [Var.t] (rare under post-resolve
    Var.t identity, but possible if the supply was reset), the
    inner scope is skipped. *)
let occurrences_in_ce (target : Var.t) (e : CoreExpr.typed_ce) : SourcePos.t list =
  let acc = ref [] in
  let same x = Int.equal (Var.compare x target) 0 in
  let rec go bound e =
    let b = CoreExpr.info e in
    match CoreExpr.shape e with
    | CoreExpr.Var x ->
      if same x && not bound then acc := b#loc :: !acc
    | CoreExpr.IntLit _ | CoreExpr.BoolLit _
    | CoreExpr.Fail | CoreExpr.Hole _ -> ()
    | CoreExpr.Let ((x, _), e1, e2) ->
      go bound e1;
      let bound' = bound || same x in
      go bound' e2
    | CoreExpr.LetTuple (xs, e1, e2) ->
      go bound e1;
      let bound' = bound || List.exists (fun (x, _) -> same x) xs in
      go bound' e2
    | CoreExpr.Take ((x, _), e1, e2) ->
      go bound e1;
      let bound' = bound || same x in
      go bound' e2
    | CoreExpr.Iter (x, e1, e2) ->
      go bound e1;
      let bound' = bound || same x in
      go bound' e2
    | CoreExpr.Eq (e1, e2) | CoreExpr.And (e1, e2) ->
      go bound e1; go bound e2
    | CoreExpr.If (e1, e2, e3) ->
      go bound e1; go bound e2; go bound e3
    | CoreExpr.Tuple es -> List.iter (go bound) es
    | CoreExpr.Inject (_, e1) | CoreExpr.App (_, e1)
    | CoreExpr.Call (_, e1) | CoreExpr.Not e1
    | CoreExpr.Return e1 -> go bound e1
    | CoreExpr.Annot (e1, _) -> go bound e1
    | CoreExpr.Case (scrut, branches) ->
      go bound scrut;
      List.iter (fun (_, x_br, body, _) ->
        let bound' = bound || same x_br in
        go bound' body
      ) branches
  in
  go false e;
  List.rev !acc

(** Recurse through every embedded typed_ce in a typed crt, collecting
    core-variable [target]-occurrences. *)
let occurrences_of_cvar_in_crt (target : Var.t) (crt : body_crt) : SourcePos.t list =
  let acc = ref [] in
  let add locs = acc := List.rev_append locs !acc in
  let rec go_crt c =
    match RefinedExpr.crt_shape c with
    | RefinedExpr.CLet (_pat, c1, c2) -> go_crt c1; go_crt c2
    | RefinedExpr.CLetLog (_, lpf, c') -> go_lpf lpf; go_crt c'
    | RefinedExpr.CLetRes (_, rpf, c') -> go_rpf rpf; go_crt c'
    | RefinedExpr.CLetCore (_, _, e, c') ->
      add (occurrences_in_ce target e); go_crt c'
    | RefinedExpr.CAnnot (c', _) -> go_crt c'
    | RefinedExpr.CPrimApp (_, sp) | RefinedExpr.CCall (_, sp)
    | RefinedExpr.CTuple sp -> go_spine sp
    | RefinedExpr.CIter (e, _, c1, c2) ->
      add (occurrences_in_ce target e); go_crt c1; go_crt c2
    | RefinedExpr.CIf (_, e, c1, c2) ->
      add (occurrences_in_ce target e); go_crt c1; go_crt c2
    | RefinedExpr.CCase (_, e, branches) ->
      add (occurrences_in_ce target e);
      List.iter (fun (_, _, _, body) -> go_crt body) branches
    | RefinedExpr.CExfalso | RefinedExpr.CHole _ -> ()
  and go_lpf lpf =
    match RefinedExpr.lpf_shape lpf with
    | RefinedExpr.LVar _ | RefinedExpr.LAuto | RefinedExpr.LHole _ -> ()
    | RefinedExpr.LUnfold (_, e) -> add (occurrences_in_ce target e)
    | RefinedExpr.LAnnot (lpf', e) -> go_lpf lpf'; add (occurrences_in_ce target e)
  and go_rpf rpf =
    match RefinedExpr.rpf_shape rpf with
    | RefinedExpr.RVar _ | RefinedExpr.RHole _ -> ()
    | RefinedExpr.RAnnot (rpf', e1, e2) ->
      go_rpf rpf';
      add (occurrences_in_ce target e1);
      add (occurrences_in_ce target e2)
    | RefinedExpr.RReturn lpf | RefinedExpr.RFail lpf -> go_lpf lpf
    | RefinedExpr.RTake (r1, r2) -> go_rpf r1; go_rpf r2
    | RefinedExpr.RLet (_, _, rpf')
    | RefinedExpr.RCase (_, _, _, rpf')
    | RefinedExpr.RIfTrue rpf'
    | RefinedExpr.RIfFalse rpf'
    | RefinedExpr.RUnfold rpf'
    | RefinedExpr.RAnnotStrip rpf' -> go_rpf rpf'
  and go_spine sp =
    match RefinedExpr.spine_shape sp with
    | RefinedExpr.SNil -> ()
    | RefinedExpr.SCore (e, rest) ->
      add (occurrences_in_ce target e); go_spine rest
    | RefinedExpr.SLog (lpf, rest) -> go_lpf lpf; go_spine rest
    | RefinedExpr.SRes (rpf, rest) -> go_rpf rpf; go_spine rest
  in
  go_crt crt;
  List.rev !acc

(** Recurse through a typed crt, collecting resource-variable
    [target]-occurrences — every [rpf_var v] node where [v = target].
    Variable identity is by [Var.compare] (unique IDs from scope
    resolution); shadowing isn't possible because inner binders
    receive fresh [Var.t]s. *)
let occurrences_of_rvar_in_crt (target : Var.t) (crt : body_crt) : SourcePos.t list =
  let acc = ref [] in
  let same v = Int.equal (Var.compare v target) 0 in
  let rec go_crt c =
    match RefinedExpr.crt_shape c with
    | RefinedExpr.CLet (_pat, c1, c2) -> go_crt c1; go_crt c2
    | RefinedExpr.CLetLog (_, _, c') -> go_crt c'
    | RefinedExpr.CLetRes (_, rpf, c') -> go_rpf rpf; go_crt c'
    | RefinedExpr.CLetCore (_, _, _, c') -> go_crt c'
    | RefinedExpr.CAnnot (c', _) -> go_crt c'
    | RefinedExpr.CPrimApp (_, sp) | RefinedExpr.CCall (_, sp)
    | RefinedExpr.CTuple sp -> go_spine sp
    | RefinedExpr.CIter (_, _, c1, c2) -> go_crt c1; go_crt c2
    | RefinedExpr.CIf (_, _, c1, c2) -> go_crt c1; go_crt c2
    | RefinedExpr.CCase (_, _, branches) ->
      List.iter (fun (_, _, _, body) -> go_crt body) branches
    | RefinedExpr.CExfalso | RefinedExpr.CHole _ -> ()
  and go_rpf rpf =
    let b = RefinedExpr.rpf_info rpf in
    match RefinedExpr.rpf_shape rpf with
    | RefinedExpr.RVar v ->
      if same v then acc := b#loc :: !acc
    | RefinedExpr.RHole _ -> ()
    | RefinedExpr.RAnnot (rpf', _, _) -> go_rpf rpf'
    | RefinedExpr.RReturn _ | RefinedExpr.RFail _ -> ()
    | RefinedExpr.RTake (r1, r2) -> go_rpf r1; go_rpf r2
    | RefinedExpr.RLet (_, _, rpf')
    | RefinedExpr.RCase (_, _, _, rpf')
    | RefinedExpr.RIfTrue rpf'
    | RefinedExpr.RIfFalse rpf'
    | RefinedExpr.RUnfold rpf'
    | RefinedExpr.RAnnotStrip rpf' -> go_rpf rpf'
  and go_spine sp =
    match RefinedExpr.spine_shape sp with
    | RefinedExpr.SNil -> ()
    | RefinedExpr.SCore (_, rest) -> go_spine rest
    | RefinedExpr.SLog (_, rest) -> go_spine rest
    | RefinedExpr.SRes (rpf, rest) -> go_rpf rpf; go_spine rest
  in
  go_crt crt;
  List.rev !acc

(* ================================================================== *)
(* Core-tuple expansion                                               *)
(* ================================================================== *)

let core_tuple_action (cand : core_var_candidate) : action option =
  match Sort.shape cand.sort with
  | Sort.Record taus when List.length taus >= 2 ->
    let base = Var.name cand.var in
    let taken =
      ref (names_in_scope (cand.info)#ctx (cand.info)#rctx) in
    let component_names =
      List.mapi (fun i _ ->
        let n = base ^ string_of_int (i + 1) in
        fresh ~taken n
      ) taus
    in
    let witness_text =
      "(" ^ String.concat ", " component_names ^ ")"
    in
    let pat_edit = { range = cand.pat_loc; new_text = witness_text } in
    let use_edits =
      List.map (fun pos -> { range = pos; new_text = witness_text })
        (occurrences_of_cvar_in_crt cand.var cand.scope)
    in
    let title =
      Printf.sprintf "Expand pattern \"%s\" to \"%s\"" base witness_text
    in
    Some { title; edits = pat_edit :: use_edits }
  | _ -> None

(* ================================================================== *)
(* Resource-pattern expansion                                         *)
(* ================================================================== *)

(** Strip leading [Annot] wrappers from a typed core expression so we
    can pattern-match on the underlying shape.  [rCheck.strip_annots]
    is the same idea but inlines [Let] aliases too — for code-action
    UX we want the structural form the user sees in hover, which
    matches [strip_annots_shallow]'s convention. *)
let rec strip_annots ce =
  match CoreExpr.shape ce with
  | CoreExpr.Annot (inner, _) -> strip_annots inner
  | _ -> ce

(** Format a [(string * string)] list of (lpat_text, rpat_text)
    fragments and produce the new-pattern source text plus the
    witness rpf source text.  Each resource-case helper computes
    these and feeds [resource_action] for the rest. *)
let resource_action
    (cand : resource_var_candidate)
    ~(pattern_text : string)
    ~(witness_text : string) : action =
  let base = Var.name cand.var in
  let pat_edit = { range = cand.pat_loc; new_text = pattern_text } in
  let use_edits =
    List.map (fun pos -> { range = pos; new_text = witness_text })
      (occurrences_of_rvar_in_crt cand.var cand.scope)
  in
  let title =
    Printf.sprintf "Expand resource pattern \"%s\" to \"%s\""
      base pattern_text
  in
  { title; edits = pat_edit :: use_edits }

(** Best-effort display name for a typed predicate binder: prefer the
    user's name, fall back to a [v_N] derived form for generated
    [_vNN] supply names. *)
let display_binder_name (v : Var.t) ~default : string =
  let n = Var.name v in
  if String.length n > 0 && n.[0] = '_' then default else n

let resource_var_action (cand : resource_var_candidate) : action option =
  let base = Var.name cand.var in
  let taken =
    ref (names_in_scope (cand.info)#ctx (cand.info)#rctx) in
  let fresh_from suffix = fresh ~taken (base ^ suffix) in
  let pred = strip_annots cand.pred in
  match CoreExpr.shape pred with
  | CoreExpr.Return _ ->
    let xeq = fresh_from "eq" in
    let pattern_text = Printf.sprintf "return [%s]" xeq in
    let witness_text = Printf.sprintf "return %s" xeq in
    Some (resource_action cand ~pattern_text ~witness_text)
  | CoreExpr.Take ((y, _), _ce1, _ce2) ->
    let y_name = display_binder_name y ~default:(base ^ "_y") in
    let y' = fresh ~taken y_name in
    let x1 = fresh_from "1" in
    let x2 = fresh_from "2" in
    let pattern_text = Printf.sprintf "take(%s, %s); %s" y' x1 x2 in
    let witness_text = Printf.sprintf "take(%s, %s)" x1 x2 in
    Some (resource_action cand ~pattern_text ~witness_text)
  | CoreExpr.Let ((y, _), _ce1, _ce2) ->
    let xeq = fresh_from "eq" in
    let y_name = display_binder_name y ~default:(base ^ "_y") in
    let y' = fresh ~taken y_name in
    let x1 = fresh_from "1" in
    let xeq_w = fresh ~taken (xeq ^ "_w") in
    let pattern_text = Printf.sprintf "let[%s] %s; %s" xeq y' x1 in
    let witness_text = Printf.sprintf "let[%s] %s; %s" xeq_w y' x1 in
    Some (resource_action cand ~pattern_text ~witness_text)
  | CoreExpr.LetTuple (ys, _ce1, _ce2) ->
    let xeq = fresh_from "eq" in
    let y_names =
      List.mapi (fun i (y, _) ->
        let default = Printf.sprintf "%s_y%d" base (i + 1) in
        let n = display_binder_name y ~default in
        fresh ~taken n
      ) ys
    in
    let x1 = fresh_from "1" in
    let xeq_w = fresh ~taken (xeq ^ "_w") in
    let ys_text = String.concat ", " y_names in
    let pattern_text =
      Printf.sprintf "let[%s] (%s); %s" xeq ys_text x1 in
    let witness_text =
      Printf.sprintf "let[%s] (%s); %s" xeq_w ys_text x1 in
    Some (resource_action cand ~pattern_text ~witness_text)
  | CoreExpr.Call (_, _) ->
    let x1 = fresh_from "1" in
    let pattern_text = Printf.sprintf "unfold; %s" x1 in
    let witness_text = pattern_text in
    Some (resource_action cand ~pattern_text ~witness_text)
  | _ -> None

(* ================================================================== *)
(* Entry point                                                        *)
(* ================================================================== *)

let actions_at prog ~file:_ ~line ~col : action list =
  match walk_for_pattern prog ~line ~col with
  | None -> []
  | Some (Cand_core cand) ->
    (match core_tuple_action cand with
     | Some a -> [a]
     | None -> [])
  | Some (Cand_resource cand) ->
    (match resource_var_action cand with
     | Some a -> [a]
     | None -> [])
