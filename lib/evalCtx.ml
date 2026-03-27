type 'b t =
  | Hole
  | Let of (Var.t * 'b) * 'b CoreExpr.t * 'b t

let rec fill ctx ce =
  match ctx with
  | Hole -> ce
  | Let ((x, b), e, rest) ->
    let inner = fill rest ce in
    CoreExpr.mk b (CoreExpr.Let ((x, b), e, inner))

let rec extend ctx x info e =
  match ctx with
  | Hole -> Let ((x, info), e, Hole)
  | Let (yb, e', rest) -> Let (yb, e', extend rest x info e)

let rec print_gen pp_var ctx fmt =
  match ctx with
  | Hole -> Format.fprintf fmt "[-]"
  | Let ((x, _), e, rest) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let %a =@ %a;@]@ %a@]"
      pp_var x (CoreExpr.print_gen pp_var) e (fun fmt c -> print_gen pp_var c fmt) rest

let print ctx fmt = print_gen Var.print ctx fmt
let to_string ctx = Format.asprintf "%a" (fun fmt c -> print_gen Var.print_unique c fmt) ctx
