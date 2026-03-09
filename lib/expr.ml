type 'a exprF =
  | Var of Var.t
  | IntLit of int
  | Let of Var.t * 'a * 'a
  | Tuple of 'a list
  | LetTuple of Var.t list * 'a * 'a
  | Inject of Label.t * 'a
  | Case of 'a * (Label.t * Var.t * 'a) list
  | Iter of Var.t * 'a * 'a
  | App of Prim.t * 'a
  | Call of Var.t * 'a
  | Annot of 'a * Typ.ty * Effect.t

let map f = function
  | Var x -> Var x
  | IntLit n -> IntLit n
  | Let (x, e1, e2) -> Let (x, f e1, f e2)
  | Tuple es -> Tuple (List.map f es)
  | LetTuple (xs, e1, e2) -> LetTuple (xs, f e1, f e2)
  | Inject (l, e) -> Inject (l, f e)
  | Case (e, branches) ->
    Case (f e, List.map (fun (l, x, e) -> (l, x, f e)) branches)
  | Iter (x, e1, e2) -> Iter (x, f e1, f e2)
  | App (p, e) -> App (p, f e)
  | Call (name, e) -> Call (name, f e)
  | Annot (e, ty, eff) -> Annot (f e, ty, eff)

let print_exprF pp fmt = function
  | Var x -> Var.print fmt x
  | IntLit n -> Format.fprintf fmt "%d" n
  | Let (x, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let %a =@ %a;@]@ %a@]"
      Var.print x pp e1 pp e2
  | Tuple es ->
    Format.fprintf fmt "@[<hov 2>[%a]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp) es
  | LetTuple (xs, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let [%a] =@ %a;@]@ %a@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") Var.print) xs
      pp e1 pp e2
  | Inject (l, e) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" Label.print l pp e
  | Case (e, branches) ->
    Format.fprintf fmt "@[<v>@[<hov 2>case %a {@ %a@]@ }@]"
      pp e
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
         (fun fmt (l, x, body) ->
            Format.fprintf fmt "@[<hov 2>%a %a ->@ %a@]" Label.print l Var.print x pp body))
      branches
  | Iter (x, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>iter (%a = %a) {@ %a@]@ }@]"
      Var.print x pp e1 pp e2
  | App (p, e) ->
    Format.fprintf fmt "@[<hov 2>%a(%a)@]" Prim.print p pp e
  | Call (name, e) ->
    Format.fprintf fmt "@[<hov 2>%a(%a)@]" Var.print name pp e
  | Annot (e, ty, eff) ->
    Format.fprintf fmt "@[<hov 2>%a :@ %a [%a]@]" pp e Typ.print ty Effect.print eff

type 'b t = In of 'b t exprF * 'b

let extract (In (_, b)) = b
let shape (In (s, _)) = s

type expr = < loc : SourcePos.t > t

let arith_op_string = function
  | Prim.Add -> "+" | Prim.Sub -> "-"
  | Prim.Mul -> "*" | Prim.Div -> "/"
  | _ -> assert false

let rec print fmt (In (s, _)) =
  match s with
  | App ((Prim.Add | Prim.Sub | Prim.Mul | Prim.Div) as p,
         In (Tuple [e1; e2], _)) ->
    Format.fprintf fmt "@[<hov 2>%a %s@ %a@]" print e1 (arith_op_string p) print e2
  | _ -> print_exprF print fmt s

module Test = struct
  let gen =
    let open QCheck.Gen in
    let mk s =
      In (s, object method loc = SourcePos.dummy end)
    in
    sized @@ fix (fun self n ->
      if n <= 0 then
        oneof [
          map (fun v -> mk (Var v)) Var.Test.gen;
          map (fun n -> mk (IntLit n)) (0 -- 100);
        ]
      else
        let sub = self (n / 3) in
        oneof [
          map (fun v -> mk (Var v)) Var.Test.gen;
          map (fun n -> mk (IntLit n)) (0 -- 100);
          (let* x = Var.Test.gen in
           let* e1 = sub in
           let* e2 = sub in
           pure (mk (Let (x, e1, e2))));
          map (fun es -> mk (Tuple es)) (list_size (0 -- 3) sub);
          (let* p = Prim.Test.gen in
           let* e = sub in
           pure (mk (App (p, e))));
          (let* l = Label.Test.gen in
           let* e = sub in
           pure (mk (Inject (l, e))));
        ])

  let test =
    [ QCheck.Test.make ~name:"map id is identity shape"
        ~count:50
        (QCheck.make (QCheck.Gen.map (fun n -> IntLit n) (QCheck.Gen.int)))
        (fun s ->
           match map Fun.id s with
           | IntLit m -> (match s with IntLit n -> Int.compare m n = 0 | _ -> false)
           | _ -> false);
    ]
end
