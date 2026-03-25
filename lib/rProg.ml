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

type parsed = SurfExpr.se t
type checked = CoreExpr.ce t

let print pp_e fmt prog =
  let pp_decl fmt = function
    | SortDecl d -> DsortDecl.print fmt d
    | TypeDecl d -> DtypeDecl.print fmt d
    | FunDecl { name; param; arg_sort; ret_sort; eff; _ } ->
      Format.fprintf fmt "@[<hov 2>fun %a (%a : %a) -> %a [%a] = <body>@]"
        Var.print name Var.print param Sort.print arg_sort Sort.print ret_sort Effect.print eff
    | RFunDecl { name; domain; codomain; eff; _ } ->
      Format.fprintf fmt "@[<hov 2>fun %a (%a) ~> %a [%a] = <body>@]"
        Var.print name (ProofSort.print pp_e) domain (ProofSort.print pp_e) codomain Effect.print eff
  in
  Format.fprintf fmt "@[<v>%a@ main : %a [%a] = %a@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ") pp_decl)
    prog.decls
    (ProofSort.print pp_e) prog.main_pf
    Effect.print prog.main_eff
    (RefinedExpr.print_crt pp_e) prog.main_body

module Test = struct
  let test = []
end
