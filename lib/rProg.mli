(** Refined program structure.

    Parameterized by ['e], the type of embedded expressions,
    ['b], the auxiliary info on refined expression nodes,
    and ['var], the type of variable names.
    At parse time, ['e = SurfExpr.se, 'b = < loc : SourcePos.t >, 'var = Var.t].
    After typechecking, ['e = CoreExpr.typed_ce, 'b = typed_rinfo, 'var = Var.t]. *)

type ('e, 'b, 'var) decl =
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
      pat : ('b, 'var) RPat.t;
      domain : ('e, 'b, 'var) ProofSort.t;
      codomain : ('e, 'b, 'var) ProofSort.t;
      eff : Effect.t;
      body : ('e, 'b, 'var) RefinedExpr.crt;
      loc : SourcePos.t;
    }

type ('e, 'b, 'var) t = {
  decls : ('e, 'b, 'var) decl list;
  main_pf : ('e, 'b, 'var) ProofSort.t;
  main_eff : Effect.t;
  main_body : ('e, 'b, 'var) RefinedExpr.crt;
  loc : SourcePos.t;
}

(** Raw parsed type (string names, before scope resolution). *)
type raw_parsed = (SurfExpr.parsed_se, < loc : SourcePos.t >, string) t

(** Raw parsed declaration (for parser start symbol). *)
type raw_parsed_decl = (SurfExpr.parsed_se, < loc : SourcePos.t >, string) decl

(** Resolved parse-time type (surface expressions, Var.t names). *)
type parsed = (SurfExpr.se, < loc : SourcePos.t >, Var.t) t

(** Checked type (core expressions). *)
type checked = (CoreExpr.ce, < loc : SourcePos.t >, Var.t) t

(** Info annotation for typed refined expression nodes.
    Extends [CoreExpr.typed_info] with the refined context. *)
type typed_rinfo = < loc : SourcePos.t; ctx : Context.t; rctx : RCtx.t; sort : Sort.sort; eff : Effect.t; pf : (CoreExpr.typed_ce, typed_rinfo, Var.t) ProofSort.t >

(** Fully annotated type (typed core expressions with refined context/sort/effect). *)
type typed = (CoreExpr.typed_ce, typed_rinfo, Var.t) t

val print_gen : (Format.formatter -> Var.t -> unit) -> (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, 'b, Var.t) t -> unit
val print : (Format.formatter -> 'e -> unit) -> Format.formatter -> ('e, 'b, Var.t) t -> unit
val to_string : (Format.formatter -> 'e -> unit) -> ('e, 'b, Var.t) t -> string

module Test : sig
  val test : QCheck.Test.t list
end
