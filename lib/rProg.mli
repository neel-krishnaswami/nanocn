(** Refined program structure.

    Parameterized by ['e], the type of embedded expressions,
    and ['var], the type of variable names.
    At parse time, ['e = SurfExpr.se, 'var = Var.t].
    After elaboration, ['e = CoreExpr.ce, 'var = Var.t]. *)

type ('e, 'var) decl =
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t
  | FunDecl of {
      name : string;
      param : 'var;
      arg_sort : Sort.sort;
      ret_sort : Sort.sort;
      eff : Effect.t;
      body : 'e;
      loc : SourcePos.t;
    }
  | RFunDecl of {
      name : string;
      pat : 'var RPat.t;
      domain : ('e, 'var) ProofSort.t;
      codomain : ('e, 'var) ProofSort.t;
      eff : Effect.t;
      body : ('e, < loc : SourcePos.t >, 'var) RefinedExpr.crt;
      loc : SourcePos.t;
    }

type ('e, 'var) t = {
  decls : ('e, 'var) decl list;
  main_pf : ('e, 'var) ProofSort.t;
  main_eff : Effect.t;
  main_body : ('e, < loc : SourcePos.t >, 'var) RefinedExpr.crt;
  loc : SourcePos.t;
}

(** Raw parsed type (string names, before scope resolution). *)
type raw_parsed = (SurfExpr.parsed_se, string) t

(** Resolved parse-time type (surface expressions, Var.t names). *)
type parsed = (SurfExpr.se, Var.t) t

(** Checked type (core expressions). *)
type checked = (CoreExpr.ce, Var.t) t

val print_gen : (Format.formatter -> Var.t -> unit) -> (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, Var.t) t -> unit
val print : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, Var.t) t -> unit
val to_string : (Format.formatter -> 'e -> unit) -> ('e, Var.t) t -> string

module Test : sig
  val test : QCheck.Test.t list
end
