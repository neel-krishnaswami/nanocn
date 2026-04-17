type ('e, 'b, 'var) entry =
  | Comp of { info : 'b; var : 'var; sort : Sort.sort; eff : Effect.t }
  | Log of { info : 'b; prop : 'e }
  | Res of { info : 'b; pred : 'e; value : 'e }
  | DepRes of { info : 'b; bound_var : 'var; pred : 'e }

type ('e, 'b, 'var) t = ('e, 'b, 'var) entry list

let entry_info = function
  | Comp { info; _ } -> info
  | Log { info; _ } -> info
  | Res { info; _ } -> info
  | DepRes { info; _ } -> info

let map_entry f = function
  | Comp c -> Comp c
  | Log { info; prop } -> Log { info; prop = f prop }
  | Res { info; pred; value } -> Res { info; pred = f pred; value = f value }
  | DepRes { info; bound_var; pred } ->
    DepRes { info; bound_var; pred = f pred }

let map f = List.map (map_entry f)

let map_info_entry f = function
  | Comp { info; var; sort; eff } -> Comp { info = f info; var; sort; eff }
  | Log { info; prop } -> Log { info = f info; prop }
  | Res { info; pred; value } -> Res { info = f info; pred; value }
  | DepRes { info; bound_var; pred } -> DepRes { info = f info; bound_var; pred }

let map_info f = List.map (map_info_entry f)

let map_var_entry f = function
  | Comp { info; var; sort; eff } -> Comp { info; var = f var; sort; eff }
  | Log { info; prop } -> Log { info; prop }
  | Res { info; pred; value } -> Res { info; pred; value }
  | DepRes { info; bound_var; pred } ->
    DepRes { info; bound_var = f bound_var; pred }

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
  | _ -> Error (Error.K_dep_res_not_pred { got = sort })

let bind gamma pf =
  let ( let* ) = Result.bind in
  List.fold_left (fun acc_result entry ->
    let* acc = acc_result in
    match entry with
    | Comp { var; sort; eff; _ } -> Ok (Context.extend var sort eff acc)
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
    | Comp { info; var; sort; eff } ->
      Comp { info; var; sort = Subst.apply sub sort; eff }
    | Log { info; prop } ->
      Log { info; prop = Subst.apply_ce sub prop }
    | Res { info; pred; value } ->
      Res { info; pred = Subst.apply_ce sub pred; value = Subst.apply_ce sub value }
    | DepRes { info; bound_var; pred } ->
      let sub' = Subst.extend_var bound_var
        (CoreExpr.mk (CoreExpr.info pred) (CoreExpr.Var bound_var)) sub in
      DepRes { info; bound_var; pred = Subst.apply_ce sub' pred })
    pf

let subst x e pf =
  apply_subst (Subst.extend_var x e Subst.empty) pf

let print_gen pp_var pp_e fmt pf =
  let pp_entry fmt = function
    | Comp { var; sort; eff; _ } ->
      Format.fprintf fmt "@[%a : %a [%a]@]" pp_var var Sort.print sort Effect.print eff
    | Log { prop; _ } ->
      Format.fprintf fmt "@[%a [log]@]" pp_e prop
    | Res { pred; value; _ } ->
      Format.fprintf fmt "@[%a @ %a [res]@]" pp_e pred pp_e value
    | DepRes { bound_var; pred; _ } ->
      Format.fprintf fmt "@[[res] (take %a = %a)@]" pp_var bound_var pp_e pred
  in
  match pf with
  | [] -> Format.fprintf fmt "·"
  | _ ->
    Format.fprintf fmt "@[<hov>%a@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_entry)
      pf

let print pp_e fmt pf = print_gen Var.print pp_e fmt pf
let print_ce fmt pf = print CoreExpr.print fmt pf

let to_string pp_e pf = Format.asprintf "%a" (print_gen Var.print_unique pp_e) pf
let to_string_ce (pf : (CoreExpr.typed_ce, _, Var.t) t) = to_string (CoreExpr.print_gen Var.print_unique) pf

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
             Comp { info = (); var = x; sort = s; eff = Effect.Pure };
             Log { info = (); prop = ce };
           ] in
           match pf_types pf with
           | [s'] -> Sort.compare s s' = 0
           | _ -> false);
    ]
end
