type t =
  | Top
  | Bot
  | And of t * t
  | Forall of Var.t * Sort.sort * t
  | Impl of CoreExpr.ce * t
  | Atom of CoreExpr.ce

let top = Top
let bot = Bot

let conj c1 c2 =
  match c1, c2 with
  | Top, c | c, Top -> c
  | _ -> And (c1, c2)

let impl ce ct = Impl (ce, ct)
let forall_ x sort ct = Forall (x, sort, ct)
let atom ce = Atom ce

let rec print fmt = function
  | Top -> Format.fprintf fmt "⊤"
  | Bot -> Format.fprintf fmt "⊥"
  | And (c1, c2) ->
    Format.fprintf fmt "@[<hov 2>%a ∧@ %a@]" print c1 print c2
  | Forall (x, s, c) ->
    Format.fprintf fmt "@[<hov 2>∀%a : %a.@ %a@]"
      Var.print x Sort.print s print c
  | Impl (ce, c) ->
    Format.fprintf fmt "@[<hov 2>%a ⇒@ %a@]" CoreExpr.print ce print c
  | Atom ce ->
    CoreExpr.print fmt ce

module Test = struct
  let test =
    [ QCheck.Test.make ~name:"constraint conj Top is identity"
        ~count:1
        QCheck.unit
        (fun () ->
           match conj Top (Atom (CoreExpr.mk (object method loc = SourcePos.dummy end) (CoreExpr.BoolLit true))) with
           | Atom _ -> true
           | _ -> false);
    ]
end
