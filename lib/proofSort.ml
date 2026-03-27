type ('e, 'var) entry =
  | Comp of { var : 'var; sort : Sort.sort; eff : Effect.t }
  | Log of { var : 'var; prop : 'e }
  | Res of { var : 'var; pred : 'e; value : 'e }
  | DepRes of { var : 'var; bound_var : 'var; pred : 'e }

type ('e, 'var) t = ('e, 'var) entry list

let map_entry f = function
  | Comp c -> Comp c
  | Log { var; prop } -> Log { var; prop = f prop }
  | Res { var; pred; value } -> Res { var; pred = f pred; value = f value }
  | DepRes { var; bound_var; pred } ->
    DepRes { var; bound_var; pred = f pred }

let map f = List.map (map_entry f)

let map_var_entry f = function
  | Comp { var; sort; eff } -> Comp { var = f var; sort; eff }
  | Log { var; prop } -> Log { var = f var; prop }
  | Res { var; pred; value } -> Res { var = f var; pred; value }
  | DepRes { var; bound_var; pred } ->
    DepRes { var = f var; bound_var = f bound_var; pred }

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

let synth_bound_sort cs gamma pred =
  let ( let* ) = Result.bind in
  let* te = Typecheck.synth cs gamma Effect.Spec pred in
  let sort = (CoreExpr.info te)#sort in
  match Sort.shape sort with
  | Sort.Pred inner -> Ok inner
  | _ -> Error "dep-res: expected pred sort"

let bind cs gamma pf =
  let ( let* ) = Result.bind in
  List.fold_left (fun acc_result entry ->
    let* acc = acc_result in
    match entry with
    | Comp { var; sort; eff } -> Ok (Context.extend var sort eff acc)
    | Log _ | Res _ -> Ok acc
    | DepRes { bound_var; pred; _ } ->
      let* inner = synth_bound_sort cs acc pred in
      Ok (Context.extend bound_var inner Effect.Spec acc))
    (Ok gamma) pf

let pf_to_ctx cs delta pf =
  let ( let* ) = Result.bind in
  List.fold_left (fun acc_result entry ->
    let* acc = acc_result in
    match entry with
    | Comp { var; sort; eff } -> Ok (RCtx.extend_comp var sort eff acc)
    | Log { var; prop } -> Ok (RCtx.extend_log var prop acc)
    | Res { var; pred; value } -> Ok (RCtx.extend_res var pred value Usage.Avail acc)
    | DepRes { var; bound_var; pred } ->
      let gamma = RCtx.erase acc in
      let* inner = synth_bound_sort cs gamma pred in
      let ce_y = CoreExpr.mk (object method loc = SourcePos.dummy end) (CoreExpr.Var bound_var) in
      let acc = RCtx.extend_comp bound_var inner Effect.Spec acc in
      Ok (RCtx.extend_res var pred ce_y Usage.Avail acc))
    (Ok delta) pf

let subst x e pf =
  List.map (fun entry ->
    match entry with
    | Comp c ->
      if Var.compare x c.var = 0 then entry
      else Comp c
    | Log { var; prop } ->
      if Var.compare x var = 0 then entry
      else Log { var; prop = CoreExpr.subst x e prop }
    | Res { var; pred; value } ->
      if Var.compare x var = 0 then entry
      else Res { var; pred = CoreExpr.subst x e pred; value = CoreExpr.subst x e value }
    | DepRes { var; bound_var; pred } ->
      if Var.compare x var = 0 || Var.compare x bound_var = 0 then entry
      else DepRes { var; bound_var; pred = CoreExpr.subst x e pred })
    pf

let print pp_e fmt pf =
  let pp_entry fmt = function
    | Comp { var; sort; eff } ->
      Format.fprintf fmt "@[%a : %a [%a]@]" Var.print var Sort.print sort Effect.print eff
    | Log { var; prop } ->
      Format.fprintf fmt "@[%a : %a [log]@]" Var.print var pp_e prop
    | Res { var; pred; value } ->
      Format.fprintf fmt "@[%a : %a @ %a [res]@]"
        Var.print var pp_e pred pp_e value
    | DepRes { var; bound_var; pred } ->
      Format.fprintf fmt "@[[res] %a : (take %a = %a)@]"
        Var.print var Var.print bound_var pp_e pred
  in
  match pf with
  | [] -> Format.fprintf fmt "·"
  | _ ->
    Format.fprintf fmt "@[<hov>%a@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_entry)
      pf

let print_ce = print CoreExpr.print

module Test = struct
  let test =
    [ QCheck.Test.make ~name:"proofSort pf_types extracts comp sorts"
        ~count:1
        QCheck.unit
        (fun () ->
           let loc = object method loc = SourcePos.dummy end in
           let s = Sort.mk loc Sort.Int in
           let (x, supply) = Var.mk "x" SourcePos.dummy Var.empty_supply in
           let (y, _supply) = Var.mk "y" SourcePos.dummy supply in
           let ce = CoreExpr.mk loc (CoreExpr.BoolLit true) in
           let pf = [
             Comp { var = x; sort = s; eff = Effect.Pure };
             Log { var = y; prop = ce };
           ] in
           match pf_types pf with
           | [s'] -> Sort.compare s s' = 0
           | _ -> false);
    ]
end
