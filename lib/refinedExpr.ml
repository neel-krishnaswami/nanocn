(* ===== Shape functors ===== *)

type ('crt, 'lpf, 'rpf, 'spine, 'e) crtF =
  | CLet of RPat.t * 'crt * 'crt
  | CAnnot of 'crt * 'e ProofSort.t
  | CPrimApp of Prim.t * 'spine
  | CCall of Var.t * 'spine
  | CTuple of 'spine
  | CIter of 'e * RPat.t * 'crt * 'crt
  | CIf of Var.t * 'e * 'crt * 'crt
  | CCase of Var.t * 'e * (Label.t * Var.t * 'crt) list
  | CExfalso
  | COpenTake of 'rpf

type ('crt, 'lpf, 'rpf, 'spine, 'e) lpfF =
  | LVar of Var.t
  | LAuto
  | LUnfold of Var.t * 'e
  | LOpenRet of 'rpf
  | LAnnot of 'lpf * 'e

type ('crt, 'lpf, 'rpf, 'spine, 'e) rpfF =
  | RVar of Var.t
  | RMakeRet of 'lpf
  | RMakeTake of 'crt
  | RAnnot of 'rpf * 'e * 'e

type ('crt, 'lpf, 'rpf, 'spine, 'e) spineF =
  | SNil
  | SCore of 'e * 'spine
  | SLog of 'lpf * 'spine
  | SRes of 'rpf * 'spine

(* ===== Mapper ===== *)

type ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 's1, 's2, 'e1, 'e2) mapper = {
  crt : 'c1 -> 'c2;
  lpf : 'l1 -> 'l2;
  rpf : 'r1 -> 'r2;
  spine : 's1 -> 's2;
  expr : 'e1 -> 'e2;
}

(* ===== Shape mapping ===== *)

let map_crtF m = function
  | CLet (q, c1, c2) -> CLet (q, m.crt c1, m.crt c2)
  | CAnnot (c, pf) -> CAnnot (m.crt c, ProofSort.map m.expr pf)
  | CPrimApp (p, s) -> CPrimApp (p, m.spine s)
  | CCall (f, s) -> CCall (f, m.spine s)
  | CTuple s -> CTuple (m.spine s)
  | CIter (e, q, c1, c2) -> CIter (m.expr e, q, m.crt c1, m.crt c2)
  | CIf (x, e, c1, c2) -> CIf (x, m.expr e, m.crt c1, m.crt c2)
  | CCase (y, e, bs) -> CCase (y, m.expr e, List.map (fun (l, x, c) -> (l, x, m.crt c)) bs)
  | CExfalso -> CExfalso
  | COpenTake r -> COpenTake (m.rpf r)

let map_lpfF m = function
  | LVar x -> LVar x
  | LAuto -> LAuto
  | LUnfold (f, e) -> LUnfold (f, m.expr e)
  | LOpenRet r -> LOpenRet (m.rpf r)
  | LAnnot (l, e) -> LAnnot (m.lpf l, m.expr e)

let map_rpfF m = function
  | RVar x -> RVar x
  | RMakeRet l -> RMakeRet (m.lpf l)
  | RMakeTake c -> RMakeTake (m.crt c)
  | RAnnot (r, e1, e2) -> RAnnot (m.rpf r, m.expr e1, m.expr e2)

let map_spineF m = function
  | SNil -> SNil
  | SCore (e, s) -> SCore (m.expr e, m.spine s)
  | SLog (l, s) -> SLog (m.lpf l, m.spine s)
  | SRes (r, s) -> SRes (m.rpf r, m.spine s)

(* ===== Knot-tied types ===== *)

type ('e, 'b) crt = CIn of 'b * (('e, 'b) crt, ('e, 'b) lpf, ('e, 'b) rpf, ('e, 'b) spine, 'e) crtF
and ('e, 'b) lpf = LIn of 'b * (('e, 'b) crt, ('e, 'b) lpf, ('e, 'b) rpf, ('e, 'b) spine, 'e) lpfF
and ('e, 'b) rpf = RIn of 'b * (('e, 'b) crt, ('e, 'b) lpf, ('e, 'b) rpf, ('e, 'b) spine, 'e) rpfF
and ('e, 'b) spine = SIn of 'b * (('e, 'b) crt, ('e, 'b) lpf, ('e, 'b) rpf, ('e, 'b) spine, 'e) spineF

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
  let m = { crt = map_crt f; lpf = map_lpf f; rpf = map_rpf f; spine = map_spine f; expr = Fun.id } in
  CIn (f b, map_crtF m shape)
and map_lpf f (LIn (b, shape)) =
  let m = { crt = map_crt f; lpf = map_lpf f; rpf = map_rpf f; spine = map_spine f; expr = Fun.id } in
  LIn (f b, map_lpfF m shape)
and map_rpf f (RIn (b, shape)) =
  let m = { crt = map_crt f; lpf = map_lpf f; rpf = map_rpf f; spine = map_spine f; expr = Fun.id } in
  RIn (f b, map_rpfF m shape)
and map_spine f (SIn (b, shape)) =
  let m = { crt = map_crt f; lpf = map_lpf f; rpf = map_rpf f; spine = map_spine f; expr = Fun.id } in
  SIn (f b, map_spineF m shape)

(* ===== Whole-tree mapping: expression type ===== *)

let rec map_crt_expr f (CIn (b, shape)) =
  let m = { crt = map_crt_expr f; lpf = map_lpf_expr f; rpf = map_rpf_expr f; spine = map_spine_expr f; expr = f } in
  CIn (b, map_crtF m shape)
and map_lpf_expr f (LIn (b, shape)) =
  let m = { crt = map_crt_expr f; lpf = map_lpf_expr f; rpf = map_rpf_expr f; spine = map_spine_expr f; expr = f } in
  LIn (b, map_lpfF m shape)
and map_rpf_expr f (RIn (b, shape)) =
  let m = { crt = map_crt_expr f; lpf = map_lpf_expr f; rpf = map_rpf_expr f; spine = map_spine_expr f; expr = f } in
  RIn (b, map_rpfF m shape)
and map_spine_expr f (SIn (b, shape)) =
  let m = { crt = map_crt_expr f; lpf = map_lpf_expr f; rpf = map_rpf_expr f; spine = map_spine_expr f; expr = f } in
  SIn (b, map_spineF m shape)

(* ===== Concrete types ===== *)

type parsed_crt = (SurfExpr.se, < loc : SourcePos.t >) crt
type parsed_lpf = (SurfExpr.se, < loc : SourcePos.t >) lpf
type parsed_rpf = (SurfExpr.se, < loc : SourcePos.t >) rpf
type parsed_spine = (SurfExpr.se, < loc : SourcePos.t >) spine

type located_crt = (CoreExpr.ce, < loc : SourcePos.t >) crt
type located_lpf = (CoreExpr.ce, < loc : SourcePos.t >) lpf
type located_rpf = (CoreExpr.ce, < loc : SourcePos.t >) rpf
type located_spine = (CoreExpr.ce, < loc : SourcePos.t >) spine

(* ===== Printing ===== *)

let rec print_crt pp_e fmt t =
  match crt_shape t with
  | CLet (q, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let %a =@ %a;@]@ %a@]"
      RPat.print q (print_crt pp_e) e1 (print_crt pp_e) e2
  | CAnnot (e, pf) ->
    Format.fprintf fmt "@[<hov 2>%a :@ %a@]" (print_crt pp_e) e (ProofSort.print pp_e) pf
  | CPrimApp (p, sp) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" Prim.print p (print_spine pp_e) sp
  | CCall (f, sp) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" Var.print f (print_spine pp_e) sp
  | CTuple sp ->
    Format.fprintf fmt "@[<hov 2>(%a)@]" (print_spine pp_e) sp
  | CIter (ce, q, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>iter[%a](%a = %a) {@ %a@]@ }@]"
      pp_e ce RPat.print q (print_crt pp_e) e1 (print_crt pp_e) e2
  | CIf (x, ce, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>if[%a] %a@ then %a@]@ @[<hov 2>else %a@]@]"
      Var.print x pp_e ce (print_crt pp_e) e1 (print_crt pp_e) e2
  | CCase (y, ce, branches) ->
    Format.fprintf fmt "@[<v>@[<hov 2>case[%a] %a {@ %a@]@ }@]"
      Var.print y pp_e ce
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
         (fun fmt (l, x, body) ->
            Format.fprintf fmt "@[<hov 2>%a %a ->@ %a@]" Label.print l Var.print x (print_crt pp_e) body))
      branches
  | CExfalso -> Format.fprintf fmt "exfalso"
  | COpenTake rpf ->
    Format.fprintf fmt "@[<hov 2>open-take@ %a@]" (print_rpf pp_e) rpf

and print_lpf pp_e fmt t =
  match lpf_shape t with
  | LVar x -> Var.print fmt x
  | LAuto -> Format.fprintf fmt "auto"
  | LUnfold (f, ce) ->
    Format.fprintf fmt "@[<hov 2>unfold %a(%a)@]" Var.print f pp_e ce
  | LOpenRet rpf ->
    Format.fprintf fmt "@[<hov 2>open-ret@ %a@]" (print_rpf pp_e) rpf
  | LAnnot (lpf, ce) ->
    Format.fprintf fmt "@[<hov 2>%a :@ %a@]" (print_lpf pp_e) lpf pp_e ce

and print_rpf pp_e fmt t =
  match rpf_shape t with
  | RVar x -> Var.print fmt x
  | RMakeRet lpf ->
    Format.fprintf fmt "@[<hov 2>make-ret@ %a@]" (print_lpf pp_e) lpf
  | RMakeTake crt ->
    Format.fprintf fmt "@[<hov 2>make-take@ %a@]" (print_crt pp_e) crt
  | RAnnot (rpf, ce1, ce2) ->
    Format.fprintf fmt "@[<hov 2>%a :@ %a @ %a@]" (print_rpf pp_e) rpf pp_e ce1 pp_e ce2

and print_spine pp_e fmt t =
  match spine_shape t with
  | SNil -> ()
  | SCore (ce, rest) ->
    (match spine_shape rest with
     | SNil -> pp_e fmt ce
     | _ -> Format.fprintf fmt "%a,@ %a" pp_e ce (print_spine pp_e) rest)
  | SLog (lpf, rest) ->
    (match spine_shape rest with
     | SNil -> Format.fprintf fmt "log %a" (print_lpf pp_e) lpf
     | _ -> Format.fprintf fmt "log %a,@ %a" (print_lpf pp_e) lpf (print_spine pp_e) rest)
  | SRes (rpf, rest) ->
    (match spine_shape rest with
     | SNil -> Format.fprintf fmt "res %a" (print_rpf pp_e) rpf
     | _ -> Format.fprintf fmt "res %a,@ %a" (print_rpf pp_e) rpf (print_spine pp_e) rest)

module Test = struct
  let test = []
end
