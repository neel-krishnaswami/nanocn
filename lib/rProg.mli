(** Refined program structure.

    Parameterized by ['e], the type of embedded expressions.
    At parse time, ['e = SurfExpr.se]; after elaboration, ['e = CoreExpr.ce]. *)

type 'e decl =
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t
  | FunDecl of {
      name : Var.t;
      param : Var.t;
      arg_sort : Sort.sort;
      ret_sort : Sort.sort;
      eff : Effect.t;
      body : 'e;
      loc : SourcePos.t;
    }
  | RFunDecl of {
      name : Var.t;
      domain : 'e ProofSort.t;
      codomain : 'e ProofSort.t;
      eff : Effect.t;
      body : ('e, < loc : SourcePos.t >) RefinedExpr.crt;
      loc : SourcePos.t;
    }

type 'e t = {
  decls : 'e decl list;
  main_pf : 'e ProofSort.t;
  main_eff : Effect.t;
  main_body : ('e, < loc : SourcePos.t >) RefinedExpr.crt;
  loc : SourcePos.t;
}

(** Parse-time type (surface expressions). *)
type parsed = SurfExpr.se t

(** Checked type (core expressions). *)
type checked = CoreExpr.ce t

val print : (Format.formatter -> 'e -> unit) -> Format.formatter -> 'e t -> unit

module Test : sig
  val test : QCheck.Test.t list
end
