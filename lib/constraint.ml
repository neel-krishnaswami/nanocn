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

let rec print_gen pp_var fmt = function
  | Top -> Format.fprintf fmt "⊤"
  | Bot -> Format.fprintf fmt "⊥"
  | And (c1, c2) ->
    Format.fprintf fmt "@[<hov 2>%a ∧@ %a@]" (print_gen pp_var) c1 (print_gen pp_var) c2
  | Forall (x, s, c) ->
    Format.fprintf fmt "@[<hov 2>∀%a : %a.@ %a@]"
      pp_var x Sort.print s (print_gen pp_var) c
  | Impl (ce, c) ->
    Format.fprintf fmt "@[<hov 2>%a ⇒@ %a@]" (CoreExpr.print_gen pp_var) ce (print_gen pp_var) c
  | Atom ce ->
    CoreExpr.print_gen pp_var fmt ce

let print fmt t = print_gen Var.print fmt t
let to_string t = Format.asprintf "%a" (print_gen Var.print_unique) t

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
