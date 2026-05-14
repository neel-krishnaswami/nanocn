(** Type-directed hole-expansion code actions — see [.mli]. *)

type edit = {
  range : SourcePos.t;
  new_text : string;
}

type action = {
  title : string;
  edits : edit list;
}

(** {1 Candidate types} *)

(* The walker collects the tightest enclosing hole-leaf node whose
   source loc covers the cursor.  Each constructor matches one of
   the four hole flavors in the typed AST. *)
type candidate =
  | Core_hole of {
      name : string;
      loc  : SourcePos.t;
      info : CoreExpr.typed_info;
    }
  | C_hole of {
      name : string;
      loc  : SourcePos.t;
      info : RProg.typed_rinfo;
    }
  | L_hole of {
      name : string;
      loc  : SourcePos.t;
      info : RProg.typed_rinfo;
    }
  | R_hole of {
      name : string;
      loc  : SourcePos.t;
      info : RProg.typed_rinfo;
    }

let candidate_loc = function
  | Core_hole c -> c.loc
  | C_hole c    -> c.loc
  | L_hole c    -> c.loc
  | R_hole c    -> c.loc

(* ================================================================== *)
(* Cursor / span helpers (duplicated from PatternExpand)              *)
(* ================================================================== *)

let covers loc ~line ~col =
  let sl = SourcePos.start_line loc in
  let sc = SourcePos.start_col loc in
  let el = SourcePos.end_line loc in
  let ec = SourcePos.end_col loc in
  if Int.equal sl 0 && Int.equal el 0 then false
  else
    (line > sl || (Int.equal line sl && col >= sc)) &&
    (line < el || (Int.equal line el && col <= ec))

let span_size loc =
  ( SourcePos.end_line loc - SourcePos.start_line loc,
    SourcePos.end_col  loc - SourcePos.start_col  loc )

let tighter a b =
  let cmp = compare (span_size a) (span_size b) in
  cmp < 0

(* ================================================================== *)
(* Walker                                                             *)
(* ================================================================== *)

let walk_for_hole (prog : RProg.typed) ~line ~col : candidate option =
  let best : candidate option ref = ref None in
  let consider cand =
    match !best with
    | None -> best := Some cand
    | Some b when tighter (candidate_loc cand) (candidate_loc b) ->
      best := Some cand
    | _ -> ()
  in

  let rec go_ce (e : CoreExpr.typed_ce) =
    let b = CoreExpr.info e in
    if not (covers b.loc ~line ~col) then ()
    else
      match CoreExpr.shape e with
      | CoreExpr.Hole h ->
        consider (Core_hole { name = h; loc = b.loc; info = b })
      | CoreExpr.Var _ | CoreExpr.IntLit _ | CoreExpr.BoolLit _
      | CoreExpr.Fail -> ()
      | CoreExpr.Let (_, e1, e2)
      | CoreExpr.LetTuple (_, e1, e2)
      | CoreExpr.Take (_, e1, e2)
      | CoreExpr.Iter (_, e1, e2)
      | CoreExpr.Eq (e1, e2)
      | CoreExpr.And (e1, e2) ->
        go_ce e1; go_ce e2
      | CoreExpr.If (e1, e2, e3) ->
        go_ce e1; go_ce e2; go_ce e3
      | CoreExpr.Tuple es ->
        List.iter go_ce es
      | CoreExpr.Inject (_, e1) | CoreExpr.App (_, e1)
      | CoreExpr.Call (_, e1) | CoreExpr.Not e1
      | CoreExpr.Return e1 -> go_ce e1
      | CoreExpr.Annot (e1, _) -> go_ce e1
      | CoreExpr.Case (scrut, branches) ->
        go_ce scrut;
        List.iter (fun (_, _, body, _) -> go_ce body) branches
  in

  let rec go_crt c =
    let b : RProg.typed_rinfo = RefinedExpr.crt_info c in
    if not (covers b.loc ~line ~col) then ()
    else
      match RefinedExpr.crt_shape c with
      | RefinedExpr.CHole h ->
        consider (C_hole { name = h; loc = b.loc; info = b })
      | RefinedExpr.CLet (_pat, c1, c2) -> go_crt c1; go_crt c2
      | RefinedExpr.CLetLog (_, lpf, c') -> go_lpf lpf; go_crt c'
      | RefinedExpr.CLetRes (_, rpf, c') -> go_rpf rpf; go_crt c'
      | RefinedExpr.CLetCore (_, _, e, c') -> go_ce e; go_crt c'
      | RefinedExpr.CAnnot (c', _) -> go_crt c'
      | RefinedExpr.CPrimApp (_, sp) | RefinedExpr.CCall (_, sp)
      | RefinedExpr.CTuple sp -> go_spine sp
      | RefinedExpr.CIter (e, _, c1, c2) -> go_ce e; go_crt c1; go_crt c2
      | RefinedExpr.CIf (_, e, c1, c2) -> go_ce e; go_crt c1; go_crt c2
      | RefinedExpr.CCase (_, e, branches) ->
        go_ce e;
        List.iter (fun (_, _, _, body) -> go_crt body) branches
      | RefinedExpr.CExfalso -> ()

  and go_lpf l =
    let b = RefinedExpr.lpf_info l in
    if not (covers b.loc ~line ~col) then ()
    else
      match RefinedExpr.lpf_shape l with
      | RefinedExpr.LHole h ->
        consider (L_hole { name = h; loc = b.loc; info = b })
      | RefinedExpr.LVar _ | RefinedExpr.LAuto -> ()
      | RefinedExpr.LUnfold (_, e) -> go_ce e
      | RefinedExpr.LAnnot (lpf', e) -> go_lpf lpf'; go_ce e

  and go_rpf r =
    let b = RefinedExpr.rpf_info r in
    if not (covers b.loc ~line ~col) then ()
    else
      match RefinedExpr.rpf_shape r with
      | RefinedExpr.RHole h ->
        consider (R_hole { name = h; loc = b.loc; info = b })
      | RefinedExpr.RVar _ -> ()
      | RefinedExpr.RAnnot (rpf', e1, e2) -> go_rpf rpf'; go_ce e1; go_ce e2
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
    | RefinedExpr.SCore (e, rest) -> go_ce e; go_spine rest
    | RefinedExpr.SLog (lpf, rest) -> go_lpf lpf; go_spine rest
    | RefinedExpr.SRes (rpf, rest) -> go_rpf rpf; go_spine rest
  in

  List.iter (fun decl ->
    match decl with
    | RProg.SortDecl _ | RProg.TypeDecl _ | RProg.FunDecl _ -> ()
    | RProg.RFunDecl { body; _ } -> go_crt body
  ) prog.RProg.decls;
  go_crt prog.RProg.main_body;
  !best

(* ================================================================== *)
(* Name / shape helpers (duplicated from PatternExpand)               *)
(* ================================================================== *)

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

let fresh ~taken base =
  let rec try_ s =
    if List.mem s !taken then try_ (s ^ "'")
    else (taken := s :: !taken; s)
  in
  try_ base

let rec strip_annots ce =
  match CoreExpr.shape ce with
  | CoreExpr.Annot (inner, _) -> strip_annots inner
  | _ -> ce

let display_binder_name (v : Var.t) ~default : string =
  let n = Var.name v in
  if String.length n > 0 && n.[0] = '_' then default else n

(* ================================================================== *)
(* Per-flavor expansion                                               *)
(* ================================================================== *)

let mk_action ~loc ~title ~new_text : action =
  { title; edits = [{ range = loc; new_text }] }

(** Core-hole tuple expansion.  [h]'s expected sort lives in
    [info.answer] as [Ok sort] when the hole is in a checking
    position.  Synth-only holes carry [Error _] and get no action. *)
let core_hole_action (c : candidate) : action option =
  match c with
  | Core_hole { name; loc; info } ->
    (match info.answer with
     | Error _ -> None
     | Ok sort ->
       (match Sort.shape sort with
        | Sort.Record taus when List.length taus >= 2 ->
          let taken =
            ref (names_in_scope info.ctx RCtx.empty) in
          let component_names =
            List.mapi (fun i _ ->
              fresh ~taken (name ^ string_of_int (i + 1))
            ) taus
          in
          let new_text =
            "(" ^ String.concat ", " (List.map (fun s -> "$" ^ s) component_names) ^ ")"
          in
          let title =
            Printf.sprintf "Expand hole \"$%s\" to \"%s\"" name new_text
          in
          Some (mk_action ~loc ~title ~new_text)
        | _ -> None))
  | _ -> None

(** L-hole: always [auto]. *)
let l_hole_action (c : candidate) : action option =
  match c with
  | L_hole { name; loc; _ } ->
    let new_text = "auto" in
    let title =
      Printf.sprintf "Expand hole \"$%s\" to \"%s\"" name new_text
    in
    Some (mk_action ~loc ~title ~new_text)
  | _ -> None

(** R-hole: dispatch on the resource predicate's shape. *)
let r_hole_action (c : candidate) : action option =
  match c with
  | R_hole { name; loc; info } ->
    (match info.goal with
     | RProg.RpfGoal (pred, _value) ->
       let taken = ref (names_in_scope info.ctx info.rctx) in
       let fresh_h suffix = "$" ^ fresh ~taken (name ^ suffix) in
       let mk title new_text = Some (mk_action ~loc ~title ~new_text) in
       let title_for new_text =
         Printf.sprintf "Expand hole \"$%s\" to \"%s\"" name new_text
       in
       (match CoreExpr.shape (strip_annots pred) with
        | CoreExpr.Return _ ->
          let new_text = "return auto" in
          mk (title_for new_text) new_text
        | CoreExpr.Take _ ->
          let h1 = fresh_h "1" in
          let h2 = fresh_h "2" in
          let new_text = Printf.sprintf "take(%s, %s)" h1 h2 in
          mk (title_for new_text) new_text
        | CoreExpr.Let ((y, _), _, _) ->
          let xeq = fresh ~taken (name ^ "eq") in
          let y' = fresh ~taken
              (display_binder_name y ~default:(name ^ "_y")) in
          let h1 = fresh_h "1" in
          let new_text = Printf.sprintf "let[%s] %s; %s" xeq y' h1 in
          mk (title_for new_text) new_text
        | CoreExpr.LetTuple (ys, _, _) ->
          let xeq = fresh ~taken (name ^ "eq") in
          let y_names =
            List.mapi (fun i (y, _) ->
              let default = Printf.sprintf "%s_y%d" name (i + 1) in
              fresh ~taken (display_binder_name y ~default)
            ) ys
          in
          let h1 = fresh_h "1" in
          let ys_text = String.concat ", " y_names in
          let new_text = Printf.sprintf "let[%s] (%s); %s" xeq ys_text h1 in
          mk (title_for new_text) new_text
        | CoreExpr.Call _ ->
          let h1 = fresh_h "1" in
          let new_text = Printf.sprintf "unfold; %s" h1 in
          mk (title_for new_text) new_text
        | _ -> None)
     | _ -> None)
  | _ -> None

(** C-hole: walk the proof sort and emit a parenthesised sequence. *)
let c_hole_action (c : candidate) : action option =
  match c with
  | C_hole { name; loc; info } ->
    (match info.goal with
     | RProg.CrtGoal pf ->
       let taken = ref (names_in_scope info.ctx info.rctx) in
       let rec emit i = function
         | [] -> []
         | ProofSort.Comp _ :: rest ->
           let h = fresh ~taken (name ^ string_of_int i) in
           ("$" ^ h) :: emit (i + 1) rest
         | ProofSort.Log _ :: rest ->
           "log auto" :: emit i rest
         | ProofSort.Res _ :: rest ->
           let h = fresh ~taken (name ^ string_of_int i) in
           ("res $" ^ h) :: emit (i + 1) rest
         | ProofSort.DepRes { bound_var; _ } :: rest ->
           let h = fresh ~taken (name ^ string_of_int i) in
           let y = display_binder_name bound_var
                     ~default:(name ^ "_y" ^ string_of_int i) in
           Printf.sprintf "do %s = $%s" y h :: emit (i + 1) rest
       in
       let parts = emit 1 pf in
       let new_text = "(" ^ String.concat ", " parts ^ ")" in
       let title =
         Printf.sprintf "Expand hole \"$%s\" to \"%s\"" name new_text
       in
       Some (mk_action ~loc ~title ~new_text)
     | _ -> None)
  | _ -> None

(* ================================================================== *)
(* Entry point                                                        *)
(* ================================================================== *)

let actions_at prog ~line ~col : action list =
  match walk_for_hole prog ~line ~col with
  | None -> []
  | Some cand ->
    let try_each fns =
      List.find_map (fun f -> f cand) fns
    in
    (match try_each [core_hole_action; l_hole_action; r_hole_action; c_hole_action] with
     | Some a -> [a]
     | None -> [])
