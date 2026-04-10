type ('e, 'var) entry =
  | Comp of { var : 'var; sort : Sort.sort; eff : Effect.t }
  | Log of { prop : 'e }
  | Res of { pred : 'e; value : 'e }
  | DepRes of { bound_var : 'var; pred : 'e }

type ('e, 'var) t = ('e, 'var) entry list

let map_entry f = function
  | Comp c -> Comp c
  | Log { prop } -> Log { prop = f prop }
  | Res { pred; value } -> Res { pred = f pred; value = f value }
  | DepRes { bound_var; pred } ->
    DepRes { bound_var; pred = f pred }

let map f = List.map (map_entry f)

let map_var_entry f = function
  | Comp { var; sort; eff } -> Comp { var = f var; sort; eff }
  | Log { prop } -> Log { prop }
  | Res { pred; value } -> Res { pred; value }
  | DepRes { bound_var; pred } ->
    DepRes { bound_var = f bound_var; pred }

let map_var f = List.map (map_var_entry f)

let pf_types pf =
  List.filter_map (fun entry ->
    match entry with
    | Comp { sort; eff = Effect.Pure; _ } -> Some sort
    | Comp { eff = Effect.Spec; _ } -> None
    | Comp { eff = Effect.Impure; _ } -> None
    | Log _ | Res _ | DepRes _ -> None)
    pf

let comp pf =
  let loc = object method loc = SourcePos.dummy end in
  match pf_types pf with
  | [] -> Sort.mk loc (Sort.Record [])
  | [s] -> s
  | ss -> Sort.mk loc (Sort.Record ss)

let synth_bound_sort pred =
  let sort = (CoreExpr.info pred)#sort in
  match Sort.shape sort with
  | Sort.Pred inner -> Ok inner
  | _ -> Error "dep-res: expected pred sort"

let bind gamma pf =
  let ( let* ) = Result.bind in
  List.fold_left (fun acc_result entry ->
    let* acc = acc_result in
    match entry with
    | Comp { var; sort; eff } -> Ok (Context.extend var sort eff acc)
    | Log _ | Res _ -> Ok acc
    | DepRes { bound_var; pred; _ } ->
      let* inner = synth_bound_sort pred in
      Ok (Context.extend bound_var inner Effect.Spec acc))
    (Ok gamma) pf

(* Proof sort substitution per explicit-substitutions.md:
   - Comp: apply to sort, extend with x/x for tail
   - Log/Res: apply to exprs, do NOT extend (no var to extend)
   - DepRes: extend with y/y for pred, do NOT extend for tail *)
let apply_subst sub pf =
  List.map (fun entry ->
    match entry with
    | Comp { var; sort; eff } ->
      Comp { var; sort = Subst.apply sub sort; eff }
    | Log { prop } ->
      Log { prop = Subst.apply_ce sub prop }
    | Res { pred; value } ->
      Res { pred = Subst.apply_ce sub pred; value = Subst.apply_ce sub value }
    | DepRes { bound_var; pred } ->
      let sub' = Subst.extend_var bound_var
        (CoreExpr.mk (CoreExpr.info pred) (CoreExpr.Var bound_var)) sub in
      DepRes { bound_var; pred = Subst.apply_ce sub' pred })
    pf

let subst x e pf =
  apply_subst (Subst.extend_var x e Subst.empty) pf

let print_gen pp_var pp_e fmt pf =
  let pp_entry fmt = function
    | Comp { var; sort; eff } ->
      Format.fprintf fmt "@[%a : %a [%a]@]" pp_var var Sort.print sort Effect.print eff
    | Log { prop } ->
      Format.fprintf fmt "@[%a [log]@]" pp_e prop
    | Res { pred; value } ->
      Format.fprintf fmt "@[%a @ %a [res]@]" pp_e pred pp_e value
    | DepRes { bound_var; pred } ->
      Format.fprintf fmt "@[[res] (take %a = %a)@]" pp_var bound_var pp_e pred
  in
  match pf with
  | [] -> Format.fprintf fmt "·"
  | _ ->
    Format.fprintf fmt "@[<hov>%a@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_entry)
      pf

let print pp_e fmt pf = print_gen Var.print pp_e fmt pf
let print_ce = print CoreExpr.print

let to_string pp_e pf = Format.asprintf "%a" (print_gen Var.print_unique pp_e) pf
let to_string_ce pf = to_string (CoreExpr.print_gen Var.print_unique) pf

module Test = struct
  let test =
    [ QCheck.Test.make ~name:"proofSort pf_types extracts comp sorts"
        ~count:1
        QCheck.unit
        (fun () ->
           let loc = object method loc = SourcePos.dummy end in
           let s = Sort.mk loc Sort.Int in
           let (x, _supply) = Var.mk "x" SourcePos.dummy Var.empty_supply in
           let ce = CoreExpr.mk loc (CoreExpr.BoolLit true) in
           let pf = [
             Comp { var = x; sort = s; eff = Effect.Pure };
             Log { prop = ce };
           ] in
           match pf_types pf with
           | [s'] -> Sort.compare s s' = 0
           | _ -> false);
    ]
end
