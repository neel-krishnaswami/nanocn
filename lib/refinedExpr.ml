(* ===== Shape functors ===== *)

type ('crt, 'lpf, 'rpf, 'spine, 'e, 'var) crtF =
  | CLet of 'var RPat.t * 'crt * 'crt
  | CLetLog of 'var * 'lpf * 'crt
  | CLetRes of 'var * 'rpf * 'crt
  | CLetCore of 'var list * 'var * 'e * 'crt
  | CAnnot of 'crt * ('e, 'var) ProofSort.t
  | CPrimApp of Prim.t * 'spine
  | CCall of string * 'spine
  | CTuple of 'spine
  | CIter of 'e * 'var RPat.t * 'crt * 'crt
  | CIf of 'var * 'e * 'crt * 'crt
  | CCase of 'var * 'e * (Label.t * 'var * 'crt) list
  | CExfalso
  | COpenTake of 'rpf

type ('crt, 'lpf, 'rpf, 'spine, 'e, 'var) lpfF =
  | LVar of 'var
  | LAuto
  | LUnfold of string * 'e
  | LOpenRet of 'rpf
  | LAnnot of 'lpf * 'e

type ('crt, 'lpf, 'rpf, 'spine, 'e, 'var) rpfF =
  | RVar of 'var
  | RMakeRet of 'lpf
  | RMakeTake of 'crt
  | RAnnot of 'rpf * 'e * 'e

type ('crt, 'lpf, 'rpf, 'spine, 'e) spineF =
  | SNil
  | SCore of 'e * 'spine
  | SLog of 'lpf * 'spine
  | SRes of 'rpf * 'spine

(* ===== Mapper ===== *)

type ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 's1, 's2, 'e1, 'e2, 'v1, 'v2) mapper = {
  crt : 'c1 -> 'c2;
  lpf : 'l1 -> 'l2;
  rpf : 'r1 -> 'r2;
  spine : 's1 -> 's2;
  expr : 'e1 -> 'e2;
  var : 'v1 -> 'v2;
}

(* ===== Shape mapping ===== *)

let map_crtF m = function
  | CLet (q, c1, c2) -> CLet (RPat.map_var m.var q, m.crt c1, m.crt c2)
  | CLetLog (x, l, c) -> CLetLog (m.var x, m.lpf l, m.crt c)
  | CLetRes (x, r, c) -> CLetRes (m.var x, m.rpf r, m.crt c)
  | CLetCore (xs, a, e, c) ->
    CLetCore (List.map m.var xs, m.var a, m.expr e, m.crt c)
  | CAnnot (c, pf) -> CAnnot (m.crt c, ProofSort.map m.expr (ProofSort.map_var m.var pf))
  | CPrimApp (p, s) -> CPrimApp (p, m.spine s)
  | CCall (f, s) -> CCall (f, m.spine s)
  | CTuple s -> CTuple (m.spine s)
  | CIter (e, q, c1, c2) -> CIter (m.expr e, RPat.map_var m.var q, m.crt c1, m.crt c2)
  | CIf (x, e, c1, c2) -> CIf (m.var x, m.expr e, m.crt c1, m.crt c2)
  | CCase (y, e, bs) -> CCase (m.var y, m.expr e, List.map (fun (l, x, c) -> (l, m.var x, m.crt c)) bs)
  | CExfalso -> CExfalso
  | COpenTake r -> COpenTake (m.rpf r)

let map_lpfF m = function
  | LVar x -> LVar (m.var x)
  | LAuto -> LAuto
  | LUnfold (f, e) -> LUnfold (f, m.expr e)
  | LOpenRet r -> LOpenRet (m.rpf r)
  | LAnnot (l, e) -> LAnnot (m.lpf l, m.expr e)

let map_rpfF m = function
  | RVar x -> RVar (m.var x)
  | RMakeRet l -> RMakeRet (m.lpf l)
  | RMakeTake c -> RMakeTake (m.crt c)
  | RAnnot (r, e1, e2) -> RAnnot (m.rpf r, m.expr e1, m.expr e2)

let map_spineF m = function
  | SNil -> SNil
  | SCore (e, s) -> SCore (m.expr e, m.spine s)
  | SLog (l, s) -> SLog (m.lpf l, m.spine s)
  | SRes (r, s) -> SRes (m.rpf r, m.spine s)

(* ===== Knot-tied types ===== *)

type ('e, 'b, 'var) crt = CIn of 'b * (('e, 'b, 'var) crt, ('e, 'b, 'var) lpf, ('e, 'b, 'var) rpf, ('e, 'b, 'var) spine, 'e, 'var) crtF
and ('e, 'b, 'var) lpf = LIn of 'b * (('e, 'b, 'var) crt, ('e, 'b, 'var) lpf, ('e, 'b, 'var) rpf, ('e, 'b, 'var) spine, 'e, 'var) lpfF
and ('e, 'b, 'var) rpf = RIn of 'b * (('e, 'b, 'var) crt, ('e, 'b, 'var) lpf, ('e, 'b, 'var) rpf, ('e, 'b, 'var) spine, 'e, 'var) rpfF
and ('e, 'b, 'var) spine = SIn of 'b * (('e, 'b, 'var) crt, ('e, 'b, 'var) lpf, ('e, 'b, 'var) rpf, ('e, 'b, 'var) spine, 'e) spineF

let mk_crt b s = CIn (b, s)
let mk_lpf b s = LIn (b, s)
let mk_rpf b s = RIn (b, s)
let mk_spine b s = SIn (b, s)

let crt_info (CIn (b, _)) = b
let lpf_info (LIn (b, _)) = b
let rpf_info (RIn (b, _)) = b
let spine_info (SIn (b, _)) = b

let crt_shape (CIn (_, s)) = s
let lpf_shape (LIn (_, s)) = s
let rpf_shape (RIn (_, s)) = s
let spine_shape (SIn (_, s)) = s

(* ===== Whole-tree mapping: info ===== *)

let rec map_crt f (CIn (b, shape)) =
  let m = { crt = map_crt f; lpf = map_lpf f; rpf = map_rpf f; spine = map_spine f; expr = Fun.id; var = Fun.id } in
  CIn (f b, map_crtF m shape)
and map_lpf f (LIn (b, shape)) =
  let m = { crt = map_crt f; lpf = map_lpf f; rpf = map_rpf f; spine = map_spine f; expr = Fun.id; var = Fun.id } in
  LIn (f b, map_lpfF m shape)
and map_rpf f (RIn (b, shape)) =
  let m = { crt = map_crt f; lpf = map_lpf f; rpf = map_rpf f; spine = map_spine f; expr = Fun.id; var = Fun.id } in
  RIn (f b, map_rpfF m shape)
and map_spine f (SIn (b, shape)) =
  let m = { crt = map_crt f; lpf = map_lpf f; rpf = map_rpf f; spine = map_spine f; expr = Fun.id; var = Fun.id } in
  SIn (f b, map_spineF m shape)

(* ===== Whole-tree mapping: expression type ===== *)

let rec map_crt_expr f (CIn (b, shape)) =
  let m = { crt = map_crt_expr f; lpf = map_lpf_expr f; rpf = map_rpf_expr f; spine = map_spine_expr f; expr = f; var = Fun.id } in
  CIn (b, map_crtF m shape)
and map_lpf_expr f (LIn (b, shape)) =
  let m = { crt = map_crt_expr f; lpf = map_lpf_expr f; rpf = map_rpf_expr f; spine = map_spine_expr f; expr = f; var = Fun.id } in
  LIn (b, map_lpfF m shape)
and map_rpf_expr f (RIn (b, shape)) =
  let m = { crt = map_crt_expr f; lpf = map_lpf_expr f; rpf = map_rpf_expr f; spine = map_spine_expr f; expr = f; var = Fun.id } in
  RIn (b, map_rpfF m shape)
and map_spine_expr f (SIn (b, shape)) =
  let m = { crt = map_crt_expr f; lpf = map_lpf_expr f; rpf = map_rpf_expr f; spine = map_spine_expr f; expr = f; var = Fun.id } in
  SIn (b, map_spineF m shape)

(* ===== Concrete types ===== *)

type parsed_crt = (SurfExpr.se, < loc : SourcePos.t >, Var.t) crt
type parsed_lpf = (SurfExpr.se, < loc : SourcePos.t >, Var.t) lpf
type parsed_rpf = (SurfExpr.se, < loc : SourcePos.t >, Var.t) rpf
type parsed_spine = (SurfExpr.se, < loc : SourcePos.t >, Var.t) spine

type located_crt = (CoreExpr.ce, < loc : SourcePos.t >, Var.t) crt
type located_lpf = (CoreExpr.ce, < loc : SourcePos.t >, Var.t) lpf
type located_rpf = (CoreExpr.ce, < loc : SourcePos.t >, Var.t) rpf
type located_spine = (CoreExpr.ce, < loc : SourcePos.t >, Var.t) spine

(* ===== Printing ===== *)

let rec print_gen_crt pp_var pp_e fmt t =
  match crt_shape t with
  | CLet (q, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let %a =@ %a;@]@ %a@]"
      (RPat.print_gen pp_var) q (print_gen_crt pp_var pp_e) e1 (print_gen_crt pp_var pp_e) e2
  | CLetLog (x, l, c) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let log %a =@ %a;@]@ %a@]"
      pp_var x (print_gen_lpf pp_var pp_e) l (print_gen_crt pp_var pp_e) c
  | CLetRes (x, r, c) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let res %a =@ %a;@]@ %a@]"
      pp_var x (print_gen_rpf pp_var pp_e) r (print_gen_crt pp_var pp_e) c
  | CLetCore (xs, a, ce, c) ->
    let lhs fmt =
      match xs with
      | [x] -> pp_var fmt x
      | _ ->
        Format.fprintf fmt "(%a)"
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_var) xs
    in
    Format.fprintf fmt "@[<v>@[<hov 2>let core[%a] %t =@ %a;@]@ %a@]"
      pp_var a lhs pp_e ce (print_gen_crt pp_var pp_e) c
  | CAnnot (e, pf) ->
    Format.fprintf fmt "@[<hov 2>%a :@ %a@]" (print_gen_crt pp_var pp_e) e (ProofSort.print_gen pp_var pp_e) pf
  | CPrimApp (p, sp) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" Prim.print p (print_gen_spine pp_var pp_e) sp
  | CCall (f, sp) ->
    Format.fprintf fmt "@[<hov 2>%s@ %a@]" f (print_gen_spine pp_var pp_e) sp
  | CTuple sp ->
    Format.fprintf fmt "@[<hov 2>(%a)@]" (print_gen_spine pp_var pp_e) sp
  | CIter (ce, q, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>iter[%a](%a = %a) {@ %a@]@ }@]"
      pp_e ce (RPat.print_gen pp_var) q (print_gen_crt pp_var pp_e) e1 (print_gen_crt pp_var pp_e) e2
  | CIf (x, ce, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>if[%a] %a@ then %a@]@ @[<hov 2>else %a@]@]"
      pp_var x pp_e ce (print_gen_crt pp_var pp_e) e1 (print_gen_crt pp_var pp_e) e2
  | CCase (y, ce, branches) ->
    Format.fprintf fmt "@[<v>@[<hov 2>case[%a] %a {@ %a@]@ }@]"
      pp_var y pp_e ce
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
         (fun fmt (l, x, body) ->
            Format.fprintf fmt "@[<hov 2>%a %a ->@ %a@]" Label.print l pp_var x (print_gen_crt pp_var pp_e) body))
      branches
  | CExfalso -> Format.fprintf fmt "exfalso"
  | COpenTake rpf ->
    Format.fprintf fmt "@[<hov 2>open-take@ %a@]" (print_gen_rpf pp_var pp_e) rpf

and print_gen_lpf pp_var pp_e fmt t =
  match lpf_shape t with
  | LVar x -> pp_var fmt x
  | LAuto -> Format.fprintf fmt "auto"
  | LUnfold (f, ce) ->
    Format.fprintf fmt "@[<hov 2>unfold %s(%a)@]" f pp_e ce
  | LOpenRet rpf ->
    Format.fprintf fmt "@[<hov 2>open-ret@ %a@]" (print_gen_rpf pp_var pp_e) rpf
  | LAnnot (lpf, ce) ->
    Format.fprintf fmt "@[<hov 2>%a :@ %a@]" (print_gen_lpf pp_var pp_e) lpf pp_e ce

and print_gen_rpf pp_var pp_e fmt t =
  match rpf_shape t with
  | RVar x -> pp_var fmt x
  | RMakeRet lpf ->
    Format.fprintf fmt "@[<hov 2>make-ret@ %a@]" (print_gen_lpf pp_var pp_e) lpf
  | RMakeTake crt ->
    Format.fprintf fmt "@[<hov 2>make-take@ %a@]" (print_gen_crt pp_var pp_e) crt
  | RAnnot (rpf, ce1, ce2) ->
    Format.fprintf fmt "@[<hov 2>%a :@ %a @ %a@]" (print_gen_rpf pp_var pp_e) rpf pp_e ce1 pp_e ce2

and print_gen_spine pp_var pp_e fmt t =
  match spine_shape t with
  | SNil -> ()
  | SCore (ce, rest) ->
    (match spine_shape rest with
     | SNil -> pp_e fmt ce
     | _ -> Format.fprintf fmt "%a,@ %a" pp_e ce (print_gen_spine pp_var pp_e) rest)
  | SLog (lpf, rest) ->
    (match spine_shape rest with
     | SNil -> Format.fprintf fmt "log %a" (print_gen_lpf pp_var pp_e) lpf
     | _ -> Format.fprintf fmt "log %a,@ %a" (print_gen_lpf pp_var pp_e) lpf (print_gen_spine pp_var pp_e) rest)
  | SRes (rpf, rest) ->
    (match spine_shape rest with
     | SNil -> Format.fprintf fmt "res %a" (print_gen_rpf pp_var pp_e) rpf
     | _ -> Format.fprintf fmt "res %a,@ %a" (print_gen_rpf pp_var pp_e) rpf (print_gen_spine pp_var pp_e) rest)

let print_crt pp_e = print_gen_crt Var.print pp_e
let print_lpf pp_e = print_gen_lpf Var.print pp_e
let print_rpf pp_e = print_gen_rpf Var.print pp_e
let print_spine pp_e = print_gen_spine Var.print pp_e

let to_string_crt pp_e t = Format.asprintf "%a" (print_gen_crt Var.print_unique pp_e) t
let to_string_lpf pp_e t = Format.asprintf "%a" (print_gen_lpf Var.print_unique pp_e) t
let to_string_rpf pp_e t = Format.asprintf "%a" (print_gen_rpf Var.print_unique pp_e) t
let to_string_spine pp_e t = Format.asprintf "%a" (print_gen_spine Var.print_unique pp_e) t

module Test = struct
  let test = []
end
