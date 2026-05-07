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

type ('cpat, 'lpat, 'rpat, 'q) q_shapeF =
  | QNil
  | QCore of 'cpat * 'q
  | QLog of 'lpat * 'q
  | QRes of 'rpat * 'q
  | QDepRes of 'cpat * 'rpat * 'q

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

let map_q_shapeF f = function
  | QNil -> QNil
  | QCore (cp, q) -> QCore (cp, f q)
  | QLog (lp, q) -> QLog (lp, f q)
  | QRes (rp, q) -> QRes (rp, f q)
  | QDepRes (cp, rp, q) -> QDepRes (cp, rp, f q)

(* ===== Knot-tied types ===== *)

type ('b, 'var) cpat = CIn of 'b * (('b, 'var) cpat, 'var) cpatF
and  ('b, 'var) lpat = LIn of 'b * 'var lpatF
and  ('b, 'var) rpat = RIn of 'b * (('b, 'var) cpat, ('b, 'var) lpat, ('b, 'var) rpat, 'var) rpatF

type ('b, 'var) t =
  TIn of 'b *
    (('b, 'var) cpat, ('b, 'var) lpat, ('b, 'var) rpat, ('b, 'var) t) q_shapeF

(* ===== Constructors ===== *)

let mk_cpat b s = CIn (b, s)
let mk_lpat b s = LIn (b, s)
let mk_rpat b s = RIn (b, s)
let mk b s = TIn (b, s)

(* ===== Accessors ===== *)

let cpat_info (CIn (b, _)) = b
let lpat_info (LIn (b, _)) = b
let rpat_info (RIn (b, _)) = b
let info (TIn (b, _)) = b

let cpat_shape (CIn (_, s)) = s
let lpat_shape (LIn (_, s)) = s
let rpat_shape (RIn (_, s)) = s
let shape (TIn (_, s)) = s

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

let rec map_info f (TIn (b, shape)) =
  let shape' = match shape with
    | QNil -> QNil
    | QCore (cp, q) -> QCore (map_info_cpat f cp, map_info f q)
    | QLog (lp, q) -> QLog (map_info_lpat f lp, map_info f q)
    | QRes (rp, q) -> QRes (map_info_rpat f rp, map_info f q)
    | QDepRes (cp, rp, q) -> QDepRes (map_info_cpat f cp, map_info_rpat f rp, map_info f q)
  in
  TIn (f b, shape')

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

let rec map_var f (TIn (b, shape)) =
  let shape' = match shape with
    | QNil -> QNil
    | QCore (cp, q) -> QCore (map_var_cpat f cp, map_var f q)
    | QLog (lp, q) -> QLog (map_var_lpat f lp, map_var f q)
    | QRes (rp, q) -> QRes (map_var_rpat f rp, map_var f q)
    | QDepRes (cp, rp, q) -> QDepRes (map_var_cpat f cp, map_var_rpat f rp, map_var f q)
  in
  TIn (b, shape')

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

let print_gen pp_var fmt t =
  let rec print_seq fmt t =
    match shape t with
    | QNil -> ()
    | QCore (cp, rest) ->
      print_cpat pp_var fmt cp;
      print_sep_and_rest fmt rest
    | QLog (lp, rest) ->
      Format.fprintf fmt "log %a" (print_lpat pp_var) lp;
      print_sep_and_rest fmt rest
    | QRes (rp, rest) ->
      Format.fprintf fmt "res %a" (print_rpat pp_var) rp;
      print_sep_and_rest fmt rest
    | QDepRes (cp, rp, rest) ->
      Format.fprintf fmt "do %a = %a"
        (print_cpat pp_var) cp (print_rpat pp_var) rp;
      print_sep_and_rest fmt rest
  and print_sep_and_rest fmt rest =
    match shape rest with
    | QNil -> ()
    | _ -> Format.fprintf fmt ",@ "; print_seq fmt rest
  in
  match shape t with
  | QNil -> Format.fprintf fmt "()"
  | _ -> Format.fprintf fmt "(%a)" print_seq t

let print fmt t = print_gen Var.print fmt t
let to_string t = Format.asprintf "%a" (print_gen Var.print_unique) t

module Test = struct
  let test = []
end
