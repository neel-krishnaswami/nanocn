type 'e t = {
  domain : 'e ProofSort.t;
  codomain : 'e ProofSort.t;
  eff : Effect.t;
}

let map f { domain; codomain; eff } =
  { domain = ProofSort.map f domain;
    codomain = ProofSort.map f codomain;
    eff }

let print pp_e fmt { domain; codomain; eff } =
  Format.fprintf fmt "@[<hov 2>%a ⊸@ %a [%a]@]"
    (ProofSort.print pp_e) domain (ProofSort.print pp_e) codomain Effect.print eff

let print_ce = print CoreExpr.print

module Test = struct
  let test = []
end
