type ('a, 'e) ctF =
  | Top
  | Bot
  | And of 'a * 'a
  | Forall of Var.t * Sort.sort * 'a
  | Impl of 'e * 'a
  | Atom of 'e

let map_shape f = function
  | Top -> Top
  | Bot -> Bot
  | And (a, b) -> And (f a, f b)
  | Forall (x, s, a) -> Forall (x, s, f a)
  | Impl (e, a) -> Impl (e, f a)
  | Atom e -> Atom e

type ('e, 'b) t = In of 'b * (('e, 'b) t, 'e) ctF

let mk b s = In (b, s)
let info (In (b, _)) = b
let shape (In (_, s)) = s

let rec map f (In (b, s)) =
  In (f b, map_shape (map f) s)

type ct = (CoreExpr.ce, < loc : SourcePos.t >) t
type typed_ct = (CoreExpr.typed_ce, < loc : SourcePos.t >) t

let loc pos = object method loc = pos end

let top pos = In (loc pos, Top)
let bot pos = In (loc pos, Bot)

let conj pos c1 c2 =
  match shape c1, shape c2 with
  | Top, _ -> c2
  | _, Top -> c1
  | _ -> In (loc pos, And (c1, c2))

let impl pos ce ct = In (loc pos, Impl (ce, ct))
let forall_ pos x sort ct = In (loc pos, Forall (x, sort, ct))
let atom pos ce = In (loc pos, Atom ce)

let rec print_gen pp_var fmt ct =
  match shape ct with
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
           let d = SourcePos.dummy in
           match shape (conj d (top d) (atom d (CoreExpr.mk (object method loc = d end) (CoreExpr.BoolLit true)))) with
           | Atom _ -> true
           | _ -> false);
    ]
end
