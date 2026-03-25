type entry =
  | Comp of { var : Var.t; sort : Sort.sort; eff : Effect.t }
  | Log of { var : Var.t; prop : CoreExpr.ce }
  | Res of { var : Var.t; pred : CoreExpr.ce; value : CoreExpr.ce; usage : Usage.t }

type t = entry list

let empty = []

let extend_comp x sort eff ctx = ctx @ [Comp { var = x; sort; eff }]
let extend_log x prop ctx = ctx @ [Log { var = x; prop }]
let extend_res x pred value usage ctx = ctx @ [Res { var = x; pred; value; usage }]
let concat ctx1 ctx2 = ctx1 @ ctx2

let lookup_comp x ctx =
  let rec go = function
    | [] -> None
    | Comp { var; sort; eff } :: _ when Var.compare x var = 0 -> Some (sort, eff)
    | _ :: rest -> go rest
  in
  go ctx

let lookup_log x ctx =
  let rec go = function
    | [] -> None
    | Log { var; prop } :: _ when Var.compare x var = 0 -> Some prop
    | _ :: rest -> go rest
  in
  go ctx

let use_resource x ctx =
  let rec go acc = function
    | [] -> Error (Format.asprintf "resource %a not found" Var.print x)
    | Res { var; pred; value; usage } :: rest when Var.compare x var = 0 ->
      if Usage.is_avail usage then
        Ok (pred, value, List.rev acc @ [Res { var; pred; value; usage = Usage.Used }] @ rest)
      else
        Error (Format.asprintf "resource %a already consumed" Var.print x)
    | entry :: rest -> go (entry :: acc) rest
  in
  go [] ctx

let erase ctx =
  List.fold_left (fun acc entry ->
    match entry with
    | Comp { var; sort; eff } -> Context.extend var sort eff acc
    | Log _ | Res _ -> acc)
    Context.empty ctx

let affinize ctx =
  List.map (fun entry ->
    match entry with
    | Comp _ | Log _ -> entry
    | Res r -> Res { r with usage = Usage.affinize r.usage })
    ctx

let zero ctx =
  List.for_all (fun entry ->
    match entry with
    | Comp _ | Log _ -> true
    | Res { usage; _ } -> Usage.is_zero usage)
    ctx

let merge ctx1 ctx2 =
  if List.length ctx1 <> List.length ctx2 then
    Error "merge: contexts have different lengths"
  else
    let rec go acc l1 l2 =
      match l1, l2 with
      | [], [] -> Ok (List.rev acc)
      | Comp c1 :: r1, Comp _ :: r2 ->
        go (Comp c1 :: acc) r1 r2
      | Log l :: r1, Log _ :: r2 ->
        go (Log l :: acc) r1 r2
      | Res r1 :: rest1, Res r2 :: rest2 ->
        (match Usage.meet r1.usage r2.usage with
         | Some u ->
           go (Res { r1 with usage = u } :: acc) rest1 rest2
         | None ->
           Error (Format.asprintf "merge: incompatible usage for %a" Var.print r1.var))
      | _ ->
        Error "merge: entry type mismatch"
    in
    go [] ctx1 ctx2

let merge_n = function
  | [] -> Error "merge_n: empty list"
  | [ctx] -> Ok ctx
  | first :: rest ->
    List.fold_left (fun acc ctx ->
      match acc with
      | Error _ -> acc
      | Ok merged -> merge merged ctx)
      (Ok first) rest

let length = List.length

let split n ctx =
  let rec go acc n = function
    | rest when n <= 0 -> (List.rev acc, rest)
    | [] -> (List.rev acc, [])
    | x :: rest -> go (x :: acc) (n - 1) rest
  in
  go [] n ctx

let entries ctx = ctx

let print fmt ctx =
  let pp_entry fmt = function
    | Comp { var; sort; eff } ->
      Format.fprintf fmt "@[%a : %a [%a]@]" Var.print var Sort.print sort Effect.print eff
    | Log { var; prop } ->
      Format.fprintf fmt "@[%a : %a [log]@]" Var.print var CoreExpr.print prop
    | Res { var; pred; value; usage } ->
      Format.fprintf fmt "@[%a : %a @ %a [res(%a)]@]"
        Var.print var CoreExpr.print pred CoreExpr.print value Usage.print usage
  in
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_entry)
    ctx

module Test = struct
  let test =
    [ QCheck.Test.make ~name:"rctx erase drops log entries"
        ~count:1
        QCheck.unit
        (fun () ->
           let x = Var.of_string "x" SourcePos.dummy in
           let p = CoreExpr.mk (object method loc = SourcePos.dummy end) (CoreExpr.BoolLit true) in
           let ctx = extend_log x p empty in
           match Context.lookup x (erase ctx) with
           | None -> true
           | Some _ -> false);

      QCheck.Test.make ~name:"rctx erase keeps comp entries"
        ~count:1
        QCheck.unit
        (fun () ->
           let x = Var.of_string "x" SourcePos.dummy in
           let s = Sort.mk (object method loc = SourcePos.dummy end) Sort.Int in
           let ctx = extend_comp x s Effect.Pure empty in
           match Context.lookup x (erase ctx) with
           | Some _ -> true
           | None -> false);
    ]
end
