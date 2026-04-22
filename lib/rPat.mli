(** Refined patterns (q): structured patterns for matching proof sorts.

    The pattern hierarchy follows the spec in [doc/surface-refinement-types.md]:
    - [cpat]: core patterns — variable or tuple destructuring
    - [lpat]: logical patterns — variable binding or [auto]
    - [rpat]: resource patterns — destructuring resource predicates
    - [qbase]: tagged union of core/log/res/depres
    - [t]: an annotated list of [qbase] elements

    All types follow the shape+knot pattern (see [doc/instructions/syntax-trees.md]).
    Parameterized by ['b] (location at parse time, typing info after checking)
    and ['var] (string at parse time, [Var.t] after resolution). *)

(** {1 Shape functors} *)

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

(** {1 Mapper record} *)

type ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 'v1, 'v2) mapper = {
  cpat : 'c1 -> 'c2;
  lpat : 'l1 -> 'l2;
  rpat : 'r1 -> 'r2;
  var  : 'v1 -> 'v2;
}

(** {1 Shape mapping} *)

val map_cpatF  : ('c1, 'c2, _, _, _, _, 'v1, 'v2) mapper ->
  ('c1, 'v1) cpatF -> ('c2, 'v2) cpatF
val map_lpatF  : (_, _, _, _, _, _, 'v1, 'v2) mapper ->
  'v1 lpatF -> 'v2 lpatF
val map_rpatF  : ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, 'v1, 'v2) mapper ->
  ('c1, 'l1, 'r1, 'v1) rpatF -> ('c2, 'l2, 'r2, 'v2) rpatF
val map_qbaseF : ('c1, 'c2, 'l1, 'l2, 'r1, 'r2, _, _) mapper ->
  ('c1, 'l1, 'r1) qbaseF -> ('c2, 'l2, 'r2) qbaseF

(** {1 Knot-tied abstract types} *)

type ('b, 'var) cpat
type ('b, 'var) lpat
type ('b, 'var) rpat
type ('b, 'var) qbase
type ('b, 'var) t

(** {1 Constructors} *)

val mk_cpat  : 'b -> (('b, 'var) cpat, 'var) cpatF -> ('b, 'var) cpat
val mk_lpat  : 'b -> 'var lpatF -> ('b, 'var) lpat
val mk_rpat  : 'b -> (('b, 'var) cpat, ('b, 'var) lpat, ('b, 'var) rpat, 'var) rpatF -> ('b, 'var) rpat
val mk_qbase : 'b -> (('b, 'var) cpat, ('b, 'var) lpat, ('b, 'var) rpat) qbaseF -> ('b, 'var) qbase
val mk       : 'b -> ('b, 'var) qbase list -> ('b, 'var) t

(** {1 Accessors} *)

val cpat_info  : ('b, _) cpat  -> 'b
val lpat_info  : ('b, _) lpat  -> 'b
val rpat_info  : ('b, _) rpat  -> 'b
val qbase_info : ('b, _) qbase -> 'b
val info       : ('b, _) t     -> 'b
val elems      : ('b, 'var) t  -> ('b, 'var) qbase list

val cpat_shape  : ('b, 'var) cpat  -> (('b, 'var) cpat, 'var) cpatF
val lpat_shape  : ('b, 'var) lpat  -> 'var lpatF
val rpat_shape  : ('b, 'var) rpat  -> (('b, 'var) cpat, ('b, 'var) lpat, ('b, 'var) rpat, 'var) rpatF
val qbase_shape : ('b, 'var) qbase -> (('b, 'var) cpat, ('b, 'var) lpat, ('b, 'var) rpat) qbaseF

(** {1 Whole-tree info mapping} *)

val map_info_cpat  : ('b -> 'c) -> ('b, 'var) cpat  -> ('c, 'var) cpat
val map_info_lpat  : ('b -> 'c) -> ('b, 'var) lpat  -> ('c, 'var) lpat
val map_info_rpat  : ('b -> 'c) -> ('b, 'var) rpat  -> ('c, 'var) rpat
val map_info_qbase : ('b -> 'c) -> ('b, 'var) qbase -> ('c, 'var) qbase
val map_info       : ('b -> 'c) -> ('b, 'var) t     -> ('c, 'var) t

(** {1 Whole-tree variable mapping} *)

val map_var_cpat  : ('v -> 'w) -> ('b, 'v) cpat  -> ('b, 'w) cpat
val map_var_lpat  : ('v -> 'w) -> ('b, 'v) lpat  -> ('b, 'w) lpat
val map_var_rpat  : ('v -> 'w) -> ('b, 'v) rpat  -> ('b, 'w) rpat
val map_var_qbase : ('v -> 'w) -> ('b, 'v) qbase -> ('b, 'w) qbase
val map_var       : ('v -> 'w) -> ('b, 'v) t     -> ('b, 'w) t

(** {1 Printing} *)

val print_cpat : (Format.formatter -> 'var -> unit) -> Format.formatter -> (_, 'var) cpat -> unit
val print_lpat : (Format.formatter -> 'var -> unit) -> Format.formatter -> (_, 'var) lpat -> unit
val print_rpat : (Format.formatter -> 'var -> unit) -> Format.formatter -> (_, 'var) rpat -> unit
val print_gen  : (Format.formatter -> 'var -> unit) -> Format.formatter -> (_, 'var) t -> unit
val print      : Format.formatter -> (_, Var.t) t -> unit
val to_string  : (_, Var.t) t -> string

module Test : sig
  val test : QCheck.Test.t list
end
