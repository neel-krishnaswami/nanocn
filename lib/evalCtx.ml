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

let rec print ctx fmt =
  match ctx with
  | Hole -> Format.fprintf fmt "[-]"
  | Let ((x, _), e, rest) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let %a =@ %a;@]@ %a@]"
      Var.print x CoreExpr.print e (fun fmt c -> print c fmt) rest
