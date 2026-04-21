type ('var, 'b) cpat =
  | CVar of 'b * 'var
  | CTuple of 'b * ('var, 'b) cpat list

type ('var, 'b) lpat =
  | LVar of 'b * 'var
  | LAuto of 'b

type ('var, 'b) rpat =
  | RVar of 'b * 'var
  | RReturn of 'b * ('var, 'b) lpat
  | RTake of 'b * ('var, 'b) cpat * ('var, 'b) rpat * ('var, 'b) rpat
  | RFail of 'b * ('var, 'b) lpat
  | RLet of 'b * ('var, 'b) lpat * ('var, 'b) cpat * ('var, 'b) rpat
  | RCase of 'b * ('var, 'b) lpat * Label.t * ('var, 'b) cpat * ('var, 'b) rpat
  | RIfTrue of 'b * ('var, 'b) rpat
  | RIfFalse of 'b * ('var, 'b) rpat
  | RUnfold of 'b * ('var, 'b) rpat
  | RAnnot of 'b * ('var, 'b) rpat

type ('var, 'b) qbase =
  | QCore of ('var, 'b) cpat
  | QLog of ('var, 'b) lpat
  | QRes of ('var, 'b) rpat
  | QDepRes of ('var, 'b) cpat * ('var, 'b) rpat

type ('var, 'b) t = ('var, 'b) qbase list

(* Info extraction *)

let cpat_info = function
  | CVar (b, _) -> b
  | CTuple (b, _) -> b

let lpat_info = function
  | LVar (b, _) -> b
  | LAuto b -> b

let rpat_info = function
  | RVar (b, _) | RReturn (b, _) | RTake (b, _, _, _) | RFail (b, _)
  | RLet (b, _, _, _) | RCase (b, _, _, _, _) | RIfTrue (b, _)
  | RIfFalse (b, _) | RUnfold (b, _) | RAnnot (b, _) -> b

let qbase_info = function
  | QCore cp -> cpat_info cp
  | QLog lp -> lpat_info lp
  | QRes rp -> rpat_info rp
  | QDepRes (cp, _) -> cpat_info cp

(* Variable mapping *)

let rec map_var_cpat f = function
  | CVar (b, x) -> CVar (b, f x)
  | CTuple (b, cps) -> CTuple (b, List.map (map_var_cpat f) cps)

let map_var_lpat f = function
  | LVar (b, x) -> LVar (b, f x)
  | LAuto b -> LAuto b

let rec map_var_rpat f = function
  | RVar (b, x) -> RVar (b, f x)
  | RReturn (b, lp) -> RReturn (b, map_var_lpat f lp)
  | RTake (b, cp, rp1, rp2) -> RTake (b, map_var_cpat f cp, map_var_rpat f rp1, map_var_rpat f rp2)
  | RFail (b, lp) -> RFail (b, map_var_lpat f lp)
  | RLet (b, lp, cp, rp) -> RLet (b, map_var_lpat f lp, map_var_cpat f cp, map_var_rpat f rp)
  | RCase (b, lp, l, cp, rp) -> RCase (b, map_var_lpat f lp, l, map_var_cpat f cp, map_var_rpat f rp)
  | RIfTrue (b, rp) -> RIfTrue (b, map_var_rpat f rp)
  | RIfFalse (b, rp) -> RIfFalse (b, map_var_rpat f rp)
  | RUnfold (b, rp) -> RUnfold (b, map_var_rpat f rp)
  | RAnnot (b, rp) -> RAnnot (b, map_var_rpat f rp)

let map_var_qbase f = function
  | QCore cp -> QCore (map_var_cpat f cp)
  | QLog lp -> QLog (map_var_lpat f lp)
  | QRes rp -> QRes (map_var_rpat f rp)
  | QDepRes (cp, rp) -> QDepRes (map_var_cpat f cp, map_var_rpat f rp)

let map_var f = List.map (map_var_qbase f)

(* Info mapping *)

let rec map_info_cpat f = function
  | CVar (b, x) -> CVar (f b, x)
  | CTuple (b, cps) -> CTuple (f b, List.map (map_info_cpat f) cps)

let map_info_lpat f = function
  | LVar (b, x) -> LVar (f b, x)
  | LAuto b -> LAuto (f b)

let rec map_info_rpat f = function
  | RVar (b, x) -> RVar (f b, x)
  | RReturn (b, lp) -> RReturn (f b, map_info_lpat f lp)
  | RTake (b, cp, rp1, rp2) -> RTake (f b, map_info_cpat f cp, map_info_rpat f rp1, map_info_rpat f rp2)
  | RFail (b, lp) -> RFail (f b, map_info_lpat f lp)
  | RLet (b, lp, cp, rp) -> RLet (f b, map_info_lpat f lp, map_info_cpat f cp, map_info_rpat f rp)
  | RCase (b, lp, l, cp, rp) -> RCase (f b, map_info_lpat f lp, l, map_info_cpat f cp, map_info_rpat f rp)
  | RIfTrue (b, rp) -> RIfTrue (f b, map_info_rpat f rp)
  | RIfFalse (b, rp) -> RIfFalse (f b, map_info_rpat f rp)
  | RUnfold (b, rp) -> RUnfold (f b, map_info_rpat f rp)
  | RAnnot (b, rp) -> RAnnot (f b, map_info_rpat f rp)

let map_info_qbase f = function
  | QCore cp -> QCore (map_info_cpat f cp)
  | QLog lp -> QLog (map_info_lpat f lp)
  | QRes rp -> QRes (map_info_rpat f rp)
  | QDepRes (cp, rp) -> QDepRes (map_info_cpat f cp, map_info_rpat f rp)

let map_info f = List.map (map_info_qbase f)

(* Printing *)

let rec print_cpat pp_var fmt = function
  | CVar (_, x) -> pp_var fmt x
  | CTuple (_, cps) ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (print_cpat pp_var))
      cps

let print_lpat pp_var fmt = function
  | LVar (_, x) -> pp_var fmt x
  | LAuto _ -> Format.fprintf fmt "auto"

let rec print_rpat pp_var fmt = function
  | RVar (_, x) -> pp_var fmt x
  | RReturn (_, lp) -> Format.fprintf fmt "@[return %a@]" (print_lpat pp_var) lp
  | RTake (_, cp, rp1, rp2) ->
    Format.fprintf fmt "@[take(%a, %a);@ %a@]"
      (print_cpat pp_var) cp (print_rpat pp_var) rp1 (print_rpat pp_var) rp2
  | RFail (_, lp) -> Format.fprintf fmt "fail[%a]" (print_lpat pp_var) lp
  | RLet (_, lp, cp, rp) ->
    Format.fprintf fmt "@[let[%a] %a;@ %a@]"
      (print_lpat pp_var) lp (print_cpat pp_var) cp (print_rpat pp_var) rp
  | RCase (_, lp, l, cp, rp) ->
    Format.fprintf fmt "@[case[%a] %a(%a);@ %a@]"
      (print_lpat pp_var) lp Label.print l (print_cpat pp_var) cp (print_rpat pp_var) rp
  | RIfTrue (_, rp) -> Format.fprintf fmt "@[iftrue;@ %a@]" (print_rpat pp_var) rp
  | RIfFalse (_, rp) -> Format.fprintf fmt "@[iffalse;@ %a@]" (print_rpat pp_var) rp
  | RUnfold (_, rp) -> Format.fprintf fmt "@[unfold;@ %a@]" (print_rpat pp_var) rp
  | RAnnot (_, rp) -> Format.fprintf fmt "@[annot;@ %a@]" (print_rpat pp_var) rp

let print_qbase pp_var fmt = function
  | QCore cp -> print_cpat pp_var fmt cp
  | QLog lp -> Format.fprintf fmt "log %a" (print_lpat pp_var) lp
  | QRes rp -> Format.fprintf fmt "res %a" (print_rpat pp_var) rp
  | QDepRes (cp, rp) ->
    Format.fprintf fmt "do %a = %a" (print_cpat pp_var) cp (print_rpat pp_var) rp

let print_gen pp_var fmt = function
  | [] -> Format.fprintf fmt "()"
  | elems ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (print_qbase pp_var))
      elems

let print fmt t = print_gen Var.print fmt t
let to_string t = Format.asprintf "%a" (print_gen Var.print_unique) t

module Test = struct
  let test = []
end
