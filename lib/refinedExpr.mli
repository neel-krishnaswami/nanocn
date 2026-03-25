(** Refined expression ASTs.

    Four mutually recursive types: core refined terms (crt),
    logical proof facts (lpf), resource proof facts (rpf),
    and refined spines (spine).

    Each follows the doubly-parameterized shape functor pattern.
    ['e] = embedded expression type (SurfExpr.se or CoreExpr.ce).
    ['b] = auxiliary info (typically [< loc : SourcePos.t >]). *)

(** {1 Shape functors}

    ['crt], ['lpf], ['rpf], ['spine] are recursive positions.
    ['e] is the embedded expression type. *)

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

(** {1 Mapper record} *)

type ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 's1, 's2, 'e1, 'e2) mapper = {
  crt : 'c1 -> 'c2;
  lpf : 'l1 -> 'l2;
  rpf : 'r1 -> 'r2;
  spine : 's1 -> 's2;
  expr : 'e1 -> 'e2;
}

(** {1 Shape mapping} *)

val map_crtF : ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 's1, 's2, 'e1, 'e2) mapper ->
  ('c1, 'l1, 'r1, 's1, 'e1) crtF -> ('c2, 'l2, 'r2, 's2, 'e2) crtF
val map_lpfF : ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 's1, 's2, 'e1, 'e2) mapper ->
  ('c1, 'l1, 'r1, 's1, 'e1) lpfF -> ('c2, 'l2, 'r2, 's2, 'e2) lpfF
val map_rpfF : ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 's1, 's2, 'e1, 'e2) mapper ->
  ('c1, 'l1, 'r1, 's1, 'e1) rpfF -> ('c2, 'l2, 'r2, 's2, 'e2) rpfF
val map_spineF : ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 's1, 's2, 'e1, 'e2) mapper ->
  ('c1, 'l1, 'r1, 's1, 'e1) spineF -> ('c2, 'l2, 'r2, 's2, 'e2) spineF

(** {1 Knot-tied abstract types} *)

type ('e, 'b) crt
type ('e, 'b) lpf
type ('e, 'b) rpf
type ('e, 'b) spine

val mk_crt : 'b -> (('e, 'b) crt, ('e, 'b) lpf, ('e, 'b) rpf, ('e, 'b) spine, 'e) crtF -> ('e, 'b) crt
val mk_lpf : 'b -> (('e, 'b) crt, ('e, 'b) lpf, ('e, 'b) rpf, ('e, 'b) spine, 'e) lpfF -> ('e, 'b) lpf
val mk_rpf : 'b -> (('e, 'b) crt, ('e, 'b) lpf, ('e, 'b) rpf, ('e, 'b) spine, 'e) rpfF -> ('e, 'b) rpf
val mk_spine : 'b -> (('e, 'b) crt, ('e, 'b) lpf, ('e, 'b) rpf, ('e, 'b) spine, 'e) spineF -> ('e, 'b) spine

val crt_info : ('e, 'b) crt -> 'b
val lpf_info : ('e, 'b) lpf -> 'b
val rpf_info : ('e, 'b) rpf -> 'b
val spine_info : ('e, 'b) spine -> 'b

val crt_shape : ('e, 'b) crt -> (('e, 'b) crt, ('e, 'b) lpf, ('e, 'b) rpf, ('e, 'b) spine, 'e) crtF
val lpf_shape : ('e, 'b) lpf -> (('e, 'b) crt, ('e, 'b) lpf, ('e, 'b) rpf, ('e, 'b) spine, 'e) lpfF
val rpf_shape : ('e, 'b) rpf -> (('e, 'b) crt, ('e, 'b) lpf, ('e, 'b) rpf, ('e, 'b) spine, 'e) rpfF
val spine_shape : ('e, 'b) spine -> (('e, 'b) crt, ('e, 'b) lpf, ('e, 'b) rpf, ('e, 'b) spine, 'e) spineF

(** {1 Whole-tree mapping} *)

val map_crt : ('b -> 'c) -> ('e, 'b) crt -> ('e, 'c) crt
val map_lpf : ('b -> 'c) -> ('e, 'b) lpf -> ('e, 'c) lpf
val map_rpf : ('b -> 'c) -> ('e, 'b) rpf -> ('e, 'c) rpf
val map_spine : ('b -> 'c) -> ('e, 'b) spine -> ('e, 'c) spine

val map_crt_expr : ('e1 -> 'e2) -> ('e1, 'b) crt -> ('e2, 'b) crt
val map_lpf_expr : ('e1 -> 'e2) -> ('e1, 'b) lpf -> ('e2, 'b) lpf
val map_rpf_expr : ('e1 -> 'e2) -> ('e1, 'b) rpf -> ('e2, 'b) rpf
val map_spine_expr : ('e1 -> 'e2) -> ('e1, 'b) spine -> ('e2, 'b) spine

(** {1 Concrete types} *)

type parsed_crt = (SurfExpr.se, < loc : SourcePos.t >) crt
type parsed_lpf = (SurfExpr.se, < loc : SourcePos.t >) lpf
type parsed_rpf = (SurfExpr.se, < loc : SourcePos.t >) rpf
type parsed_spine = (SurfExpr.se, < loc : SourcePos.t >) spine

type located_crt = (CoreExpr.ce, < loc : SourcePos.t >) crt
type located_lpf = (CoreExpr.ce, < loc : SourcePos.t >) lpf
type located_rpf = (CoreExpr.ce, < loc : SourcePos.t >) rpf
type located_spine = (CoreExpr.ce, < loc : SourcePos.t >) spine

(** {1 Printing} *)

val print_crt : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, _) crt -> unit
val print_lpf : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, _) lpf -> unit
val print_rpf : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, _) rpf -> unit
val print_spine : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, _) spine -> unit

module Test : sig
  val test : QCheck.Test.t list
end
