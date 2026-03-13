(** Program structure: declarations followed by a main expression. *)

(** {1 Surface programs} *)

type 'a decl =
  | FunDecl of {
      name : Var.t;
      arg_sort : Sort.sort;
      ret_sort : Sort.sort;
      eff : Effect.t;
      branches : (Pat.pat * 'a) list;
      loc : SourcePos.t;
    }
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type 'a t = {
  decls : 'a decl list;
  main : 'a;
  main_sort : Sort.sort;
  main_eff : Effect.t;
  loc : SourcePos.t;
}

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

(** {1 Core programs} *)

(** Print a core program. The first argument prints a core expression. *)

type 'a core_decl =
  | CoreFunDecl of {
      name : Var.t;
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

module Test : sig
  val test : QCheck.Test.t list
end
