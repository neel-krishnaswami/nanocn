type 'e entry =
  | Comp of { var : Var.t; sort : Sort.sort; eff : Effect.t }
  | Log of { var : Var.t; prop : 'e }
  | Res of { var : Var.t; pred : 'e; value : 'e }

type 'e t = 'e entry list

let map_entry f = function
  | Comp c -> Comp c
  | Log { var; prop } -> Log { var; prop = f prop }
  | Res { var; pred; value } -> Res { var; pred = f pred; value = f value }

let map f = List.map (map_entry f)

let pf_types pf =
  List.filter_map (fun entry ->
    match entry with
    | Comp { sort; _ } -> Some sort
    | Log _ | Res _ -> None)
    pf

let comp pf =
  let loc = object method loc = SourcePos.dummy end in
  match pf_types pf with
  | [] -> Sort.mk loc (Sort.Record [])
  | [s] -> s
  | ss -> Sort.mk loc (Sort.Record ss)

let bind gamma pf =
  List.fold_left (fun acc entry ->
    match entry with
    | Comp { var; sort; eff } -> Context.extend var sort eff acc
    | Log _ | Res _ -> acc)
    gamma pf

let pf_to_ctx delta pf =
  List.fold_left (fun acc entry ->
    match entry with
    | Comp { var; sort; eff } -> RCtx.extend_comp var sort eff acc
    | Log { var; prop } -> RCtx.extend_log var prop acc
    | Res { var; pred; value } -> RCtx.extend_res var pred value Usage.Avail acc)
    delta pf

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
      else Res { var; pred = CoreExpr.subst x e pred; value = CoreExpr.subst x e value })
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
           let x = Var.of_string "x" SourcePos.dummy in
           let y = Var.of_string "y" SourcePos.dummy in
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
