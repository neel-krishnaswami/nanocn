(** Program structure: declarations followed by a main expression. *)

type 'a decl =
  | FunDecl of {
      name : Var.t;
      param : Var.t;
      arg_ty : Typ.ty;
      ret_ty : Typ.ty;
      eff : Effect.t;
      body : 'a;
      loc : SourcePos.t;
    }
  | SpecFunDecl of {
      name : Var.t;
      arg_sort : Sort.sort;
      ret_sort : Sort.sort;
      branches : (Pat.pat * SurfExpr.se) list;
      loc : SourcePos.t;
    }
  | SpecDefDecl of {
      name : Var.t;
      sort : Sort.sort;
      body : SurfExpr.se;
      loc : SourcePos.t;
    }
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type 'a t = {
  decls : 'a decl list;
  main : 'a;
  loc : SourcePos.t;
}

val map : ('a -> 'b) -> 'a t -> 'b t
val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

module Test : sig
  val test : QCheck.Test.t list
end
