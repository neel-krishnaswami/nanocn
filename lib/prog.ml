type 'a decl = {
  name : Var.t;
  param : Var.t;
  arg_ty : Typ.ty;
  ret_ty : Typ.ty;
  eff : Effect.t;
  body : 'a;
  loc : SourcePos.t;
}

type 'a t = {
  decls : 'a decl list;
  main : 'a;
  loc : SourcePos.t;
}

let map f p = {
  decls = List.map (fun d -> { d with body = f d.body }) p.decls;
  main = f p.main;
  loc = p.loc;
}

let print pp fmt p =
  List.iter (fun d ->
    Format.fprintf fmt "@[<v>@[<hov 2>fun %a(%a : %a) ->@ %a [%a] {@ %a@]@ }@]@.@."
      Var.print d.name
      Var.print d.param
      Typ.print d.arg_ty
      Typ.print d.ret_ty
      Effect.print d.eff
      pp d.body
  ) p.decls;
  Format.fprintf fmt "@[<hov 2>main =@ %a@]" pp p.main

module Test = struct
  let test = []
end
