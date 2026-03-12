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

type 'a t = {
  decls : 'a decl list;
  main : 'a;
  loc : SourcePos.t;
}

let map_decl f = function
  | FunDecl d -> FunDecl { d with body = f d.body }
  | SpecFunDecl d -> SpecFunDecl d
  | SpecDefDecl d -> SpecDefDecl d
  | SortDecl d -> SortDecl d

let map f p = {
  decls = List.map (map_decl f) p.decls;
  main = f p.main;
  loc = p.loc;
}

let print pp fmt p =
  List.iter (fun d ->
    match d with
    | FunDecl d ->
      Format.fprintf fmt "@[<v>@[<hov 2>fun %a(%a : %a) ->@ %a [%a] {@ %a@]@ }@]@.@."
        Var.print d.name
        Var.print d.param
        Typ.print d.arg_ty
        Typ.print d.ret_ty
        Effect.print d.eff
        pp d.body
    | SpecFunDecl d ->
      Format.fprintf fmt "@[<v>@[<hov 2>spec %a :@ %a -> %a = { ... }@]@]@.@."
        Var.print d.name
        Sort.print d.arg_sort
        Sort.print d.ret_sort
    | SpecDefDecl d ->
      Format.fprintf fmt "@[<v>@[<hov 2>spec %a :@ %a = ...@]@]@.@."
        Var.print d.name
        Sort.print d.sort
    | SortDecl d ->
      Format.fprintf fmt "%a@.@." DsortDecl.print d
  ) p.decls;
  Format.fprintf fmt "@[<hov 2>main =@ %a@]" pp p.main

module Test = struct
  let test = []
end
