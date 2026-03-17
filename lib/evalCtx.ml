type t =
  | Hole
  | Let of Var.t * CoreExpr.ce * t

let dummy_info = object method loc = SourcePos.dummy end

let rec fill ctx ce =
  match ctx with
  | Hole -> ce
  | Let (x, e, rest) ->
    let inner = fill rest ce in
    CoreExpr.mk dummy_info (CoreExpr.Let (x, e, inner))

let rec extend ctx x e =
  match ctx with
  | Hole -> Let (x, e, Hole)
  | Let (y, e', rest) -> Let (y, e', extend rest x e)

let rec print fmt = function
  | Hole -> Format.fprintf fmt "[-]"
  | Let (x, e, rest) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let %a =@ %a;@]@ %a@]"
      Var.print x CoreExpr.print e print rest
