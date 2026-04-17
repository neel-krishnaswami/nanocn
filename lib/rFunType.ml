type ('e, 'b, 'var) t = {
  domain : ('e, 'b, 'var) ProofSort.t;
  codomain : ('e, 'b, 'var) ProofSort.t;
  eff : Effect.t;
}

let map f { domain; codomain; eff } =
  { domain = ProofSort.map f domain;
    codomain = ProofSort.map f codomain;
    eff }

let map_info f { domain; codomain; eff } =
  { domain = ProofSort.map_info f domain;
    codomain = ProofSort.map_info f codomain;
    eff }

let print_gen pp_var pp_e fmt { domain; codomain; eff } =
  Format.fprintf fmt "@[<hov 2>%a ⊸@ %a [%a]@]"
    (ProofSort.print_gen pp_var pp_e) domain (ProofSort.print_gen pp_var pp_e) codomain Effect.print eff

let print pp_e fmt rf = print_gen Var.print pp_e fmt rf
let print_ce fmt rf = print CoreExpr.print fmt rf

let to_string pp_e rf = Format.asprintf "%a" (print_gen Var.print_unique pp_e) rf
let to_string_ce (rf : (CoreExpr.ce, _, Var.t) t) = to_string (CoreExpr.print_gen Var.print_unique) rf

module Test = struct
  let test = []
end
