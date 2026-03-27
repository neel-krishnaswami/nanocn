(** Program structure: declarations followed by a main expression.
    Parameterized by ['var] for variable names. *)

(** {1 Surface programs} *)

type ('a, 'b, 'var) decl =
  | FunDecl of {
      name : string;
      arg_sort : Sort.sort;
      ret_sort : Sort.sort;
      eff : Effect.t;
      branches : ((< loc : SourcePos.t >, 'var) Pat.t * 'a * 'b) list;
      loc : SourcePos.t;
    }
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type ('a, 'b, 'var) t = {
  decls : ('a, 'b, 'var) decl list;
  main : 'a;
  main_sort : Sort.sort;
  main_eff : Effect.t;
  loc : SourcePos.t;
}

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> ('a, _, _) t -> unit
val json_decl : ('a -> Json.t) -> ('b -> Json.t) -> ('a, 'b, Var.t) decl -> Json.t
val json : ('a -> Json.t) -> ('b -> Json.t) -> ('a, 'b, Var.t) t -> Json.t

(** {1 Core programs} *)

type 'a core_decl =
  | CoreFunDecl of {
      name : string;
      param : Var.t;
      arg_sort : Sort.sort;
      ret_sort : Sort.sort;
      eff : Effect.t;
      body : 'a;
      loc : SourcePos.t;
    }
  | CoreSortDecl of DsortDecl.t
  | CoreTypeDecl of DtypeDecl.t

type 'a core_prog = {
  core_decls : 'a core_decl list;
  core_main : 'a;
  core_main_sort : Sort.sort;
  core_main_eff : Effect.t;
  core_loc : SourcePos.t;
}

val print_core_prog : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a core_prog -> unit
val json_core_decl : ('a -> Json.t) -> 'a core_decl -> Json.t
val json_core_prog : ('a -> Json.t) -> 'a core_prog -> Json.t

module Test : sig
  val test : QCheck.Test.t list
end
