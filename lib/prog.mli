(** Program structure: declarations followed by a main expression. *)

(** {1 Surface programs} *)

type ('a, 'b) decl =
  | FunDecl of {
      name : Var.t;
      arg_sort : Sort.sort;
      ret_sort : Sort.sort;
      eff : Effect.t;
      branches : (Pat.pat * 'a * 'b) list;
      loc : SourcePos.t;
    }
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type ('a, 'b) t = {
  decls : ('a, 'b) decl list;
  main : 'a;
  main_sort : Sort.sort;
  main_eff : Effect.t;
  loc : SourcePos.t;
}

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> ('a, _) t -> unit
val json_decl : ('a -> Json.t) -> ('b -> Json.t) -> ('a, 'b) decl -> Json.t
val json : ('a -> Json.t) -> ('b -> Json.t) -> ('a, 'b) t -> Json.t

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
val json_core_decl : ('a -> Json.t) -> 'a core_decl -> Json.t
val json_core_prog : ('a -> Json.t) -> 'a core_prog -> Json.t

module Test : sig
  val test : QCheck.Test.t list
end
