type 'a ceF =
  | Var of Var.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Own of Sort.sort
  | Take of Var.t * 'a * 'a
  | Return of 'a
  | Con of Label.t * 'a
  | Case of 'a * (Label.t * Var.t * 'a) list
  | Tuple of 'a list
  | LetTuple of Var.t list * 'a * 'a
  | Call of Var.t * 'a
  | Const of Var.t
  | Let of Var.t * 'a * 'a
  | If of 'a * 'a * 'a
  | Annot of 'a * Sort.sort
  | IntLit of int
  | BoolLit of bool

let map f = function
  | Var x -> Var x
  | Eq (a, b) -> Eq (f a, f b)
  | And (a, b) -> And (f a, f b)
  | Not a -> Not (f a)
  | Own s -> Own s
  | Take (x, e1, e2) -> Take (x, f e1, f e2)
  | Return a -> Return (f a)
  | Con (l, a) -> Con (l, f a)
  | Case (scrut, branches) ->
    Case (f scrut, List.map (fun (l, x, e) -> (l, x, f e)) branches)
  | Tuple es -> Tuple (List.map f es)
  | LetTuple (xs, e1, e2) -> LetTuple (xs, f e1, f e2)
  | Call (name, a) -> Call (name, f a)
  | Const name -> Const name
  | Let (x, e1, e2) -> Let (x, f e1, f e2)
  | If (e1, e2, e3) -> If (f e1, f e2, f e3)
  | Annot (a, s) -> Annot (f a, s)
  | IntLit n -> IntLit n
  | BoolLit b -> BoolLit b

type 'b t = In of 'b t ceF * 'b

let extract (In (_, b)) = b
let shape (In (s, _)) = s

type ce = < loc : SourcePos.t > t

let rec print fmt (In (s, _)) =
  match s with
  | Var x -> Var.print fmt x
  | Eq (a, b) -> Format.fprintf fmt "@[<hov 2>%a ==@ %a@]" print a print b
  | And (a, b) -> Format.fprintf fmt "@[<hov 2>%a &&@ %a@]" print a print b
  | Not a -> Format.fprintf fmt "@[<hov 2>not@ %a@]" print a
  | Own s -> Format.fprintf fmt "@[<hov 2>Own[%a]@]" Sort.print s
  | Take (x, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>take %a =@ %a;@]@ %a@]"
      Var.print x print e1 print e2
  | Return a -> Format.fprintf fmt "@[<hov 2>return@ %a@]" print a
  | Con (l, a) -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" Label.print l print a
  | Case (scrut, branches) ->
    Format.fprintf fmt "@[<v>@[<hov 2>case %a {@ %a@]@ }@]"
      print scrut
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
         (fun fmt (l, x, body) ->
            Format.fprintf fmt "@[<hov 2>%a %a ->@ %a@]" Label.print l Var.print x print body))
      branches
  | Tuple [] -> Format.fprintf fmt "()"
  | Tuple [e] -> Format.fprintf fmt "(%a,)" print e
  | Tuple es ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") print)
      es
  | LetTuple (xs, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let (%a) =@ %a;@]@ %a@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") Var.print) xs
      print e1 print e2
  | Call (name, a) -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" Var.print name print a
  | Const name -> Var.print fmt name
  | Let (x, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let %a =@ %a;@]@ %a@]"
      Var.print x print e1 print e2
  | If (e1, e2, e3) ->
    Format.fprintf fmt "@[<v>@[<hov 2>if %a@ then %a@]@ @[<hov 2>else %a@]@]"
      print e1 print e2 print e3
  | Annot (a, s) -> Format.fprintf fmt "@[<hov 2>%a :@ %a@]" print a Sort.print s
  | IntLit n -> Format.fprintf fmt "%d" n
  | BoolLit b -> Format.fprintf fmt "%s" (if b then "true" else "false")

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
          (let* e1 = sub in
           let* e2 = sub in
           pure (mk (Eq (e1, e2))));
          (let* e1 = sub in
           let* e2 = sub in
           pure (mk (And (e1, e2))));
          map (fun e -> mk (Not e)) sub;
          (let* l = Label.Test.gen in
           let* e = sub in
           pure (mk (Con (l, e))));
          map (fun es -> mk (Tuple es)) (list_size (0 -- 3) sub);
        ])

  let test =
    [ QCheck.Test.make ~name:"coreExpr map id is identity"
        ~count:50
        (QCheck.make (QCheck.Gen.map (fun n -> IntLit n) (QCheck.Gen.int)))
        (fun s ->
           match map Fun.id s with
           | IntLit m -> (match s with IntLit n -> Int.compare m n = 0 | _ -> false)
           | _ -> false);
    ]
end
