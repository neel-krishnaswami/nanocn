type 'a seF =
  | Var of Var.t
  | IntLit of int
  | BoolLit of bool
  | Let of Pat.pat * 'a * 'a
  | Tuple of 'a list
  | Inject of Label.t * 'a
  | Case of 'a * (Pat.pat * 'a) list
  | Iter of Pat.pat * 'a * 'a
  | App of Prim.t * 'a
  | Call of Var.t * 'a
  | If of 'a * 'a * 'a
  | Annot of 'a * Typ.ty * Effect.t

let map f = function
  | Var x -> Var x
  | IntLit n -> IntLit n
  | BoolLit b -> BoolLit b
  | Let (p, e1, e2) -> Let (p, f e1, f e2)
  | Tuple es -> Tuple (List.map f es)
  | Inject (l, e) -> Inject (l, f e)
  | Case (scrut, branches) ->
    Case (f scrut, List.map (fun (p, e) -> (p, f e)) branches)
  | Iter (p, e1, e2) -> Iter (p, f e1, f e2)
  | App (p, e) -> App (p, f e)
  | Call (name, e) -> Call (name, f e)
  | If (e1, e2, e3) -> If (f e1, f e2, f e3)
  | Annot (e, ty, eff) -> Annot (f e, ty, eff)

type 'b t = In of 'b t seF * 'b

let extract (In (_, b)) = b
let shape (In (s, _)) = s

type se = < loc : SourcePos.t > t

let infix_op_string = function
  | Prim.Add -> "+" | Prim.Sub -> "-"
  | Prim.Mul -> "*" | Prim.Div -> "/"
  | Prim.And -> "&&" | Prim.Or -> "||"
  | _ -> assert false

let rec print fmt (In (s, _)) =
  match s with
  | Var x -> Var.print fmt x
  | IntLit n -> Format.fprintf fmt "%d" n
  | BoolLit b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Let (p, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let %a =@ %a;@]@ %a@]"
      Pat.print p print e1 print e2
  | Tuple [] -> Format.fprintf fmt "()"
  | Tuple [e] -> Format.fprintf fmt "(%a,)" print e
  | Tuple es ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") print)
      es
  | Inject (l, e) -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" Label.print l print e
  | Case (scrut, branches) ->
    Format.fprintf fmt "@[<v>@[<hov 2>case %a of {@ %a@]@ }@]"
      print scrut
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
         (fun fmt (p, body) ->
            Format.fprintf fmt "@[<hov 2>%a ->@ %a@]" Pat.print p print body))
      branches
  | Iter (p, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>iter (%a = %a) {@ %a@]@ }@]"
      Pat.print p print e1 print e2
  | App ((Prim.Add | Prim.Sub | Prim.Mul | Prim.Div | Prim.And | Prim.Or) as p,
         In (Tuple [e1; e2], _)) ->
    Format.fprintf fmt "@[<hov 2>%a %s@ %a@]" print e1 (infix_op_string p) print e2
  | App (Prim.Not, e) ->
    Format.fprintf fmt "@[<hov 2>not@ %a@]" print e
  | App (p, e) -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" Prim.print p print e
  | Call (name, e) -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" Var.print name print e
  | If (e1, e2, e3) ->
    Format.fprintf fmt "@[<v>@[<hov 2>if %a@ then %a@]@ @[<hov 2>else %a@]@]"
      print e1 print e2 print e3
  | Annot (e, ty, eff) ->
    Format.fprintf fmt "@[<hov 2>%a :@ %a [%a]@]" print e Typ.print ty Effect.print eff

module Test = struct
  let gen =
    let open QCheck.Gen in
    let mk s = In (s, object method loc = SourcePos.dummy end) in
    sized @@ fix (fun self n ->
      if n <= 0 then
        oneof [
          map (fun v -> mk (Var v)) Var.Test.gen;
          map (fun n -> mk (IntLit n)) (0 -- 100);
          map (fun b -> mk (BoolLit b)) bool;
        ]
      else
        let sub = self (n / 3) in
        oneof [
          map (fun v -> mk (Var v)) Var.Test.gen;
          map (fun n -> mk (IntLit n)) (0 -- 100);
          (let* p = Pat.Test.gen in
           let* e1 = sub in
           let* e2 = sub in
           pure (mk (Let (p, e1, e2))));
          map (fun es -> mk (Tuple es)) (list_size (0 -- 3) sub);
          (let* l = Label.Test.gen in
           let* e = sub in
           pure (mk (Inject (l, e))));
          (let* p = Prim.Test.gen in
           let* e = sub in
           pure (mk (App (p, e))));
        ])

  let test =
    [ QCheck.Test.make ~name:"surfComp map id is identity"
        ~count:50
        (QCheck.make (QCheck.Gen.map (fun n -> IntLit n) (QCheck.Gen.int)))
        (fun s ->
           match map Fun.id s with
           | IntLit m -> (match s with IntLit n -> Int.compare m n = 0 | _ -> false)
           | _ -> false);
    ]
end
