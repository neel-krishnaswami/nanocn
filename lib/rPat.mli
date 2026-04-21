(** Refined patterns (q): structured patterns for matching proof sorts.

    The pattern hierarchy follows the spec in [doc/surface-refinement-types.md]:
    - [cpat]: core patterns — variable or tuple destructuring
    - [lpat]: logical patterns — variable binding or [auto]
    - [rpat]: resource patterns — destructuring resource predicates
    - [qbase]: tagged union of core/log/res/depres
    - [t]: a list of [qbase] elements

    All types are parameterized by ['var] (string at parse time, [Var.t] after
    resolution) and ['b] (location at parse time, typing info after checking). *)

(** {1 Core patterns} *)

type ('var, 'b) cpat =
  | CVar of 'b * 'var
  | CTuple of 'b * ('var, 'b) cpat list

(** {1 Logical patterns} *)

type ('var, 'b) lpat =
  | LVar of 'b * 'var
  | LAuto of 'b

(** {1 Resource patterns} *)

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

(** {1 Pattern elements and full patterns} *)

type ('var, 'b) qbase =
  | QCore of ('var, 'b) cpat
  | QLog of ('var, 'b) lpat
  | QRes of ('var, 'b) rpat
  | QDepRes of ('var, 'b) cpat * ('var, 'b) rpat

type ('var, 'b) t = ('var, 'b) qbase list

(** {1 Info extraction} *)

val cpat_info : ('var, 'b) cpat -> 'b
val lpat_info : ('var, 'b) lpat -> 'b
val rpat_info : ('var, 'b) rpat -> 'b
val qbase_info : ('var, 'b) qbase -> 'b

(** {1 Variable mapping} *)

val map_var_cpat : ('v -> 'w) -> ('v, 'b) cpat -> ('w, 'b) cpat
val map_var_lpat : ('v -> 'w) -> ('v, 'b) lpat -> ('w, 'b) lpat
val map_var_rpat : ('v -> 'w) -> ('v, 'b) rpat -> ('w, 'b) rpat
val map_var_qbase : ('v -> 'w) -> ('v, 'b) qbase -> ('w, 'b) qbase
val map_var : ('v -> 'w) -> ('v, 'b) t -> ('w, 'b) t

(** {1 Info mapping} *)

val map_info_cpat : ('b -> 'c) -> ('var, 'b) cpat -> ('var, 'c) cpat
val map_info_lpat : ('b -> 'c) -> ('var, 'b) lpat -> ('var, 'c) lpat
val map_info_rpat : ('b -> 'c) -> ('var, 'b) rpat -> ('var, 'c) rpat
val map_info_qbase : ('b -> 'c) -> ('var, 'b) qbase -> ('var, 'c) qbase
val map_info : ('b -> 'c) -> ('var, 'b) t -> ('var, 'c) t

(** {1 Printing} *)

val print_gen : (Format.formatter -> 'var -> unit) -> Format.formatter -> ('var, _) t -> unit
val print : Format.formatter -> (Var.t, _) t -> unit
val to_string : (Var.t, _) t -> string

module Test : sig
  val test : QCheck.Test.t list
end
