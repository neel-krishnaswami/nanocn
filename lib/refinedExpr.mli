(** Refined expression ASTs.

    Four mutually recursive types: core refined terms (crt),
    logical proof facts (lpf), resource proof facts (rpf),
    and refined spines (spine).

    Each follows the doubly-parameterized shape functor pattern.
    ['e] = embedded expression type (SurfExpr.se or CoreExpr.ce).
    ['b] = auxiliary info (typically [< loc : SourcePos.t >]).
    ['var] = variable type (string at parse time, Var.t after resolution). *)

(** {1 Shape functors}

    ['crt], ['lpf], ['rpf], ['spine] are recursive positions.
    ['e] is the embedded expression type. ['var] is the variable type. *)

type ('crt, 'lpf, 'rpf, 'spine, 'e, 'var) crtF =
  | CLet of 'var RPat.t * 'crt * 'crt
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

(** {1 Mapper record} *)

type ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 's1, 's2, 'e1, 'e2, 'v1, 'v2) mapper = {
  crt : 'c1 -> 'c2;
  lpf : 'l1 -> 'l2;
  rpf : 'r1 -> 'r2;
  spine : 's1 -> 's2;
  expr : 'e1 -> 'e2;
  var : 'v1 -> 'v2;
}

(** {1 Shape mapping} *)

val map_crtF : ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 's1, 's2, 'e1, 'e2, 'v1, 'v2) mapper ->
  ('c1, 'l1, 'r1, 's1, 'e1, 'v1) crtF -> ('c2, 'l2, 'r2, 's2, 'e2, 'v2) crtF
val map_lpfF : ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 's1, 's2, 'e1, 'e2, 'v1, 'v2) mapper ->
  ('c1, 'l1, 'r1, 's1, 'e1, 'v1) lpfF -> ('c2, 'l2, 'r2, 's2, 'e2, 'v2) lpfF
val map_rpfF : ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 's1, 's2, 'e1, 'e2, 'v1, 'v2) mapper ->
  ('c1, 'l1, 'r1, 's1, 'e1, 'v1) rpfF -> ('c2, 'l2, 'r2, 's2, 'e2, 'v2) rpfF
val map_spineF : ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 's1, 's2, 'e1, 'e2, _, _) mapper ->
  ('c1, 'l1, 'r1, 's1, 'e1) spineF -> ('c2, 'l2, 'r2, 's2, 'e2) spineF

(** {1 Knot-tied abstract types} *)

type ('e, 'b, 'var) crt
type ('e, 'b, 'var) lpf
type ('e, 'b, 'var) rpf
type ('e, 'b, 'var) spine

val mk_crt : 'b -> (('e, 'b, 'var) crt, ('e, 'b, 'var) lpf, ('e, 'b, 'var) rpf, ('e, 'b, 'var) spine, 'e, 'var) crtF -> ('e, 'b, 'var) crt
val mk_lpf : 'b -> (('e, 'b, 'var) crt, ('e, 'b, 'var) lpf, ('e, 'b, 'var) rpf, ('e, 'b, 'var) spine, 'e, 'var) lpfF -> ('e, 'b, 'var) lpf
val mk_rpf : 'b -> (('e, 'b, 'var) crt, ('e, 'b, 'var) lpf, ('e, 'b, 'var) rpf, ('e, 'b, 'var) spine, 'e, 'var) rpfF -> ('e, 'b, 'var) rpf
val mk_spine : 'b -> (('e, 'b, 'var) crt, ('e, 'b, 'var) lpf, ('e, 'b, 'var) rpf, ('e, 'b, 'var) spine, 'e) spineF -> ('e, 'b, 'var) spine

val crt_info : ('e, 'b, 'var) crt -> 'b
val lpf_info : ('e, 'b, 'var) lpf -> 'b
val rpf_info : ('e, 'b, 'var) rpf -> 'b
val spine_info : ('e, 'b, 'var) spine -> 'b

val crt_shape : ('e, 'b, 'var) crt -> (('e, 'b, 'var) crt, ('e, 'b, 'var) lpf, ('e, 'b, 'var) rpf, ('e, 'b, 'var) spine, 'e, 'var) crtF
val lpf_shape : ('e, 'b, 'var) lpf -> (('e, 'b, 'var) crt, ('e, 'b, 'var) lpf, ('e, 'b, 'var) rpf, ('e, 'b, 'var) spine, 'e, 'var) lpfF
val rpf_shape : ('e, 'b, 'var) rpf -> (('e, 'b, 'var) crt, ('e, 'b, 'var) lpf, ('e, 'b, 'var) rpf, ('e, 'b, 'var) spine, 'e, 'var) rpfF
val spine_shape : ('e, 'b, 'var) spine -> (('e, 'b, 'var) crt, ('e, 'b, 'var) lpf, ('e, 'b, 'var) rpf, ('e, 'b, 'var) spine, 'e) spineF

(** {1 Whole-tree mapping} *)

val map_crt : ('b -> 'c) -> ('e, 'b, 'var) crt -> ('e, 'c, 'var) crt
val map_lpf : ('b -> 'c) -> ('e, 'b, 'var) lpf -> ('e, 'c, 'var) lpf
val map_rpf : ('b -> 'c) -> ('e, 'b, 'var) rpf -> ('e, 'c, 'var) rpf
val map_spine : ('b -> 'c) -> ('e, 'b, 'var) spine -> ('e, 'c, 'var) spine

val map_crt_expr : ('e1 -> 'e2) -> ('e1, 'b, 'var) crt -> ('e2, 'b, 'var) crt
val map_lpf_expr : ('e1 -> 'e2) -> ('e1, 'b, 'var) lpf -> ('e2, 'b, 'var) lpf
val map_rpf_expr : ('e1 -> 'e2) -> ('e1, 'b, 'var) rpf -> ('e2, 'b, 'var) rpf
val map_spine_expr : ('e1 -> 'e2) -> ('e1, 'b, 'var) spine -> ('e2, 'b, 'var) spine

(** {1 Concrete types} *)

type parsed_crt = (SurfExpr.se, < loc : SourcePos.t >, Var.t) crt
type parsed_lpf = (SurfExpr.se, < loc : SourcePos.t >, Var.t) lpf
type parsed_rpf = (SurfExpr.se, < loc : SourcePos.t >, Var.t) rpf
type parsed_spine = (SurfExpr.se, < loc : SourcePos.t >, Var.t) spine

type located_crt = (CoreExpr.ce, < loc : SourcePos.t >, Var.t) crt
type located_lpf = (CoreExpr.ce, < loc : SourcePos.t >, Var.t) lpf
type located_rpf = (CoreExpr.ce, < loc : SourcePos.t >, Var.t) rpf
type located_spine = (CoreExpr.ce, < loc : SourcePos.t >, Var.t) spine

(** {1 Printing} *)

val print_crt : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, _, Var.t) crt -> unit
val print_lpf : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, _, Var.t) lpf -> unit
val print_rpf : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, _, Var.t) rpf -> unit
val print_spine : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, _, Var.t) spine -> unit

module Test : sig
  val test : QCheck.Test.t list
end
