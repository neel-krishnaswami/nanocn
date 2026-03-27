type ('e, 'var) t = {
  domain : ('e, 'var) ProofSort.t;
  codomain : ('e, 'var) ProofSort.t;
  eff : Effect.t;
}

let map f { domain; codomain; eff } =
  { domain = ProofSort.map f domain;
    codomain = ProofSort.map f codomain;
    eff }

let print_gen pp_var pp_e fmt { domain; codomain; eff } =
  Format.fprintf fmt "@[<hov 2>%a ⊸@ %a [%a]@]"
    (ProofSort.print_gen pp_var pp_e) domain (ProofSort.print_gen pp_var pp_e) codomain Effect.print eff

let print pp_e fmt rf = print_gen Var.print pp_e fmt rf
let print_ce = print CoreExpr.print

let to_string pp_e rf = Format.asprintf "%a" (print_gen Var.print_unique pp_e) rf
let to_string_ce rf = to_string (CoreExpr.print_gen Var.print_unique) rf

module Test = struct
  let test = []
end
