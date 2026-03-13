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

let print pp fmt p =
  List.iter (fun d ->
    match d with
    | FunDecl d ->
      Format.fprintf fmt "@[<v>@[<hov 2>fun %a :@ %a -> %a [%a] = { ... }@]@]@.@."
        Var.print d.name
        Sort.print d.arg_sort
        Sort.print d.ret_sort
        Effect.print d.eff
    | SortDecl d ->
      Format.fprintf fmt "%a@.@." DsortDecl.print d
    | TypeDecl d ->
      Format.fprintf fmt "%a@.@." DtypeDecl.print d
  ) p.decls;
  Format.fprintf fmt "@[<hov 2>main : %a [%a] =@ %a@]"
    Sort.print p.main_sort Effect.print p.main_eff pp p.main

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

let print_core_prog pp fmt p =
  List.iter (fun d ->
    match d with
    | CoreFunDecl d ->
      Format.fprintf fmt "@[<v>@[<hov 2>fun %a (%a : %a) : %a [%a] =@ %a@]@]@.@."
        Var.print d.name
        Var.print d.param
        Sort.print d.arg_sort
        Sort.print d.ret_sort
        Effect.print d.eff
        pp d.body
    | CoreSortDecl d ->
      Format.fprintf fmt "%a@.@." DsortDecl.print d
    | CoreTypeDecl d ->
      Format.fprintf fmt "%a@.@." DtypeDecl.print d
  ) p.core_decls;
  Format.fprintf fmt "@[<hov 2>main : %a [%a] =@ %a@]"
    Sort.print p.core_main_sort Effect.print p.core_main_eff pp p.core_main

module Test = struct
  let test = []
end
