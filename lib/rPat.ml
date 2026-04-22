(* ===== Shape functors ===== *)

type ('cpat, 'var) cpatF =
  | CVar of 'var
  | CTuple of 'cpat list

type 'var lpatF =
  | LVar of 'var
  | LAuto

type ('cpat, 'lpat, 'rpat, 'var) rpatF =
  | RVar of 'var
  | RReturn of 'lpat
  | RTake of 'cpat * 'rpat * 'rpat
  | RFail of 'lpat
  | RLet of 'lpat * 'cpat * 'rpat
  | RCase of 'lpat * Label.t * 'cpat * 'rpat
  | RIfTrue of 'rpat
  | RIfFalse of 'rpat
  | RUnfold of 'rpat
  | RAnnot of 'rpat

type ('cpat, 'lpat, 'rpat) qbaseF =
  | QCore of 'cpat
  | QLog of 'lpat
  | QRes of 'rpat
  | QDepRes of 'cpat * 'rpat

(* ===== Mapper ===== *)

type ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 'v1, 'v2) mapper = {
  cpat : 'c1 -> 'c2;
  lpat : 'l1 -> 'l2;
  rpat : 'r1 -> 'r2;
  var  : 'v1 -> 'v2;
}

(* ===== Shape mapping ===== *)

let map_cpatF m = function
  | CVar x -> CVar (m.var x)
  | CTuple cps -> CTuple (List.map m.cpat cps)

let map_lpatF m = function
  | LVar x -> LVar (m.var x)
  | LAuto -> LAuto

let map_rpatF m = function
  | RVar x -> RVar (m.var x)
  | RReturn lp -> RReturn (m.lpat lp)
  | RTake (cp, rp1, rp2) -> RTake (m.cpat cp, m.rpat rp1, m.rpat rp2)
  | RFail lp -> RFail (m.lpat lp)
  | RLet (lp, cp, rp) -> RLet (m.lpat lp, m.cpat cp, m.rpat rp)
  | RCase (lp, l, cp, rp) -> RCase (m.lpat lp, l, m.cpat cp, m.rpat rp)
  | RIfTrue rp -> RIfTrue (m.rpat rp)
  | RIfFalse rp -> RIfFalse (m.rpat rp)
  | RUnfold rp -> RUnfold (m.rpat rp)
  | RAnnot rp -> RAnnot (m.rpat rp)

let map_qbaseF m = function
  | QCore cp -> QCore (m.cpat cp)
  | QLog lp -> QLog (m.lpat lp)
  | QRes rp -> QRes (m.rpat rp)
  | QDepRes (cp, rp) -> QDepRes (m.cpat cp, m.rpat rp)

(* ===== Knot-tied types ===== *)

type ('b, 'var) cpat = CIn of 'b * (('b, 'var) cpat, 'var) cpatF
and  ('b, 'var) lpat = LIn of 'b * 'var lpatF
and  ('b, 'var) rpat = RIn of 'b * (('b, 'var) cpat, ('b, 'var) lpat, ('b, 'var) rpat, 'var) rpatF

type ('b, 'var) qbase = QIn of 'b * (('b, 'var) cpat, ('b, 'var) lpat, ('b, 'var) rpat) qbaseF

type ('b, 'var) t = 'b * ('b, 'var) qbase list

(* ===== Constructors ===== *)

let mk_cpat b s = CIn (b, s)
let mk_lpat b s = LIn (b, s)
let mk_rpat b s = RIn (b, s)
let mk_qbase b s = QIn (b, s)
let mk b elems = (b, elems)

(* ===== Accessors ===== *)

let cpat_info (CIn (b, _)) = b
let lpat_info (LIn (b, _)) = b
let rpat_info (RIn (b, _)) = b
let qbase_info (QIn (b, _)) = b
let info (b, _) = b
let elems (_, es) = es

let cpat_shape (CIn (_, s)) = s
let lpat_shape (LIn (_, s)) = s
let rpat_shape (RIn (_, s)) = s
let qbase_shape (QIn (_, s)) = s

(* ===== Whole-tree info mapping ===== *)

let rec map_info_cpat f (CIn (b, shape)) =
  let m = { cpat = map_info_cpat f; lpat = map_info_lpat f;
            rpat = map_info_rpat f; var = Fun.id } in
  CIn (f b, map_cpatF m shape)
and map_info_lpat f (LIn (b, shape)) =
  let m = { cpat = map_info_cpat f; lpat = map_info_lpat f;
            rpat = map_info_rpat f; var = Fun.id } in
  LIn (f b, map_lpatF m shape)
and map_info_rpat f (RIn (b, shape)) =
  let m = { cpat = map_info_cpat f; lpat = map_info_lpat f;
            rpat = map_info_rpat f; var = Fun.id } in
  RIn (f b, map_rpatF m shape)

let map_info_qbase f (QIn (b, shape)) =
  let m = { cpat = map_info_cpat f; lpat = map_info_lpat f;
            rpat = map_info_rpat f; var = Fun.id } in
  QIn (f b, map_qbaseF m shape)

let map_info f (b, elems) =
  (f b, List.map (map_info_qbase f) elems)

(* ===== Whole-tree variable mapping ===== *)

let rec map_var_cpat f (CIn (b, shape)) =
  let m = { cpat = map_var_cpat f; lpat = map_var_lpat f;
            rpat = map_var_rpat f; var = f } in
  CIn (b, map_cpatF m shape)
and map_var_lpat f (LIn (b, shape)) =
  let m = { cpat = map_var_cpat f; lpat = map_var_lpat f;
            rpat = map_var_rpat f; var = f } in
  LIn (b, map_lpatF m shape)
and map_var_rpat f (RIn (b, shape)) =
  let m = { cpat = map_var_cpat f; lpat = map_var_lpat f;
            rpat = map_var_rpat f; var = f } in
  RIn (b, map_rpatF m shape)

let map_var_qbase f (QIn (b, shape)) =
  let m = { cpat = map_var_cpat f; lpat = map_var_lpat f;
            rpat = map_var_rpat f; var = f } in
  QIn (b, map_qbaseF m shape)

let map_var f (b, elems) =
  (b, List.map (map_var_qbase f) elems)

(* ===== Printing ===== *)

let rec print_cpat pp_var fmt cp =
  match cpat_shape cp with
  | CVar x -> pp_var fmt x
  | CTuple cps ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (print_cpat pp_var))
      cps

let print_lpat pp_var fmt lp =
  match lpat_shape lp with
  | LVar x -> pp_var fmt x
  | LAuto -> Format.fprintf fmt "auto"

let rec print_rpat pp_var fmt rp =
  match rpat_shape rp with
  | RVar x -> pp_var fmt x
  | RReturn lp -> Format.fprintf fmt "@[return %a@]" (print_lpat pp_var) lp
  | RTake (cp, rp1, rp2) ->
    Format.fprintf fmt "@[take(%a, %a);@ %a@]"
      (print_cpat pp_var) cp (print_rpat pp_var) rp1 (print_rpat pp_var) rp2
  | RFail lp -> Format.fprintf fmt "fail[%a]" (print_lpat pp_var) lp
  | RLet (lp, cp, rp) ->
    Format.fprintf fmt "@[let[%a] %a;@ %a@]"
      (print_lpat pp_var) lp (print_cpat pp_var) cp (print_rpat pp_var) rp
  | RCase (lp, l, cp, rp) ->
    Format.fprintf fmt "@[case[%a] %a(%a);@ %a@]"
      (print_lpat pp_var) lp Label.print l (print_cpat pp_var) cp (print_rpat pp_var) rp
  | RIfTrue rp' -> Format.fprintf fmt "@[iftrue;@ %a@]" (print_rpat pp_var) rp'
  | RIfFalse rp' -> Format.fprintf fmt "@[iffalse;@ %a@]" (print_rpat pp_var) rp'
  | RUnfold rp' -> Format.fprintf fmt "@[unfold;@ %a@]" (print_rpat pp_var) rp'
  | RAnnot rp' -> Format.fprintf fmt "@[annot;@ %a@]" (print_rpat pp_var) rp'

let print_qbase pp_var fmt qb =
  match qbase_shape qb with
  | QCore cp -> print_cpat pp_var fmt cp
  | QLog lp -> Format.fprintf fmt "log %a" (print_lpat pp_var) lp
  | QRes rp -> Format.fprintf fmt "res %a" (print_rpat pp_var) rp
  | QDepRes (cp, rp) ->
    Format.fprintf fmt "do %a = %a" (print_cpat pp_var) cp (print_rpat pp_var) rp

let print_gen pp_var fmt t =
  match elems t with
  | [] -> Format.fprintf fmt "()"
  | es ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (print_qbase pp_var))
      es

let print fmt t = print_gen Var.print fmt t
let to_string t = Format.asprintf "%a" (print_gen Var.print_unique) t

module Test = struct
  let test = []
end
