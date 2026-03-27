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

type raw_parsed = (SurfExpr.parsed_se, string) t
type parsed = (SurfExpr.se, Var.t) t
type checked = (CoreExpr.ce, Var.t) t

let print_gen pp_var pp_e fmt prog =
  let pp_pf = ProofSort.print_gen pp_var pp_e in
  let pp_decl fmt = function
    | SortDecl d -> DsortDecl.print fmt d
    | TypeDecl d -> DtypeDecl.print fmt d
    | FunDecl { name; param; arg_sort; ret_sort; eff; _ } ->
      Format.fprintf fmt "@[<hov 2>fun %s (%a : %a) -> %a [%a] = <body>@]"
        name pp_var param Sort.print arg_sort Sort.print ret_sort Effect.print eff
    | RFunDecl { name; domain; codomain; eff; _ } ->
      Format.fprintf fmt "@[<hov 2>fun %s (%a) ~> %a [%a] = <body>@]"
        name pp_pf domain pp_pf codomain Effect.print eff
  in
  Format.fprintf fmt "@[<v>%a@ main : %a [%a] = %a@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ") pp_decl)
    prog.decls
    pp_pf prog.main_pf
    Effect.print prog.main_eff
    (RefinedExpr.print_gen_crt pp_var pp_e) prog.main_body

let print pp_e fmt prog = print_gen Var.print pp_e fmt prog
let to_string pp_e prog = Format.asprintf "%a" (print_gen Var.print_unique pp_e) prog

module Test = struct
  let test = []
end
