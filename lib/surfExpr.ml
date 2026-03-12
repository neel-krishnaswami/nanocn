type 'a seF =
  | Var of Var.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Take of Pat.pat * 'a * 'a
  | Return of 'a
  | Let of Pat.pat * 'a * 'a
  | Tuple of 'a list
  | Inject of Label.t * 'a
  | Case of 'a * (Pat.pat * 'a) list
  | Call of Var.t * 'a
  | Const of Var.t
  | If of 'a * 'a * 'a
  | Annot of 'a * Sort.sort
  | IntLit of int
  | BoolLit of bool
  | Prim of Prim.t * 'a

let map f = function
  | Var x -> Var x
  | Eq (a, b) -> Eq (f a, f b)
  | And (a, b) -> And (f a, f b)
  | Not a -> Not (f a)
  | Take (p, e1, e2) -> Take (p, f e1, f e2)
  | Return a -> Return (f a)
  | Let (p, e1, e2) -> Let (p, f e1, f e2)
  | Tuple es -> Tuple (List.map f es)
  | Inject (l, a) -> Inject (l, f a)
  | Case (scrut, branches) ->
    Case (f scrut, List.map (fun (p, e) -> (p, f e)) branches)
  | Call (name, a) -> Call (name, f a)
  | Const name -> Const name
  | If (e1, e2, e3) -> If (f e1, f e2, f e3)
  | Annot (a, s) -> Annot (f a, s)
  | IntLit n -> IntLit n
  | BoolLit b -> BoolLit b
  | Prim (p, a) -> Prim (p, f a)

type 'b t = In of 'b t seF * 'b

let extract (In (_, b)) = b
let shape (In (s, _)) = s

type se = < loc : SourcePos.t > t

let rec print fmt (In (s, _)) =
  match s with
  | Var x -> Var.print fmt x
  | Eq (a, b) -> Format.fprintf fmt "@[<hov 2>%a ==@ %a@]" print a print b
  | And (a, b) -> Format.fprintf fmt "@[<hov 2>%a &&@ %a@]" print a print b
  | Not a -> Format.fprintf fmt "@[<hov 2>not@ %a@]" print a
  | Take (p, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>take %a =@ %a;@]@ %a@]"
      Pat.print p print e1 print e2
  | Return a -> Format.fprintf fmt "@[<hov 2>return@ %a@]" print a
  | Let (p, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let %a =@ %a;@]@ %a@]"
      Pat.print p print e1 print e2
  | Tuple [] -> Format.fprintf fmt "()"
  | Tuple [e] -> Format.fprintf fmt "(%a,)" print e
  | Tuple es ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") print)
      es
  | Inject (l, a) -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" Label.print l print a
  | Case (scrut, branches) ->
    Format.fprintf fmt "@[<v>@[<hov 2>case %a {@ %a@]@ }@]"
      print scrut
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
         (fun fmt (p, body) ->
            Format.fprintf fmt "@[<hov 2>%a ->@ %a@]" Pat.print p print body))
      branches
  | Call (name, a) -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" Var.print name print a
  | Const name -> Var.print fmt name
  | If (e1, e2, e3) ->
    Format.fprintf fmt "@[<v>@[<hov 2>if %a@ then %a@]@ @[<hov 2>else %a@]@]"
      print e1 print e2 print e3
  | Annot (a, s) -> Format.fprintf fmt "@[<hov 2>%a :@ %a@]" print a Sort.print s
  | IntLit n -> Format.fprintf fmt "%d" n
  | BoolLit b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Prim (p, a) -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" Prim.print p print a

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
          map (fun e -> mk (Return e)) sub;
          (let* l = Label.Test.gen in
           let* e = sub in
           pure (mk (Inject (l, e))));
          map (fun es -> mk (Tuple es)) (list_size (0 -- 3) sub);
          (let* p = Prim.Test.gen in
           let* e = sub in
           pure (mk (Prim (p, e))));
        ])

  let test =
    [ QCheck.Test.make ~name:"surfExpr map id is identity"
        ~count:50
        (QCheck.make (QCheck.Gen.map (fun n -> IntLit n) (QCheck.Gen.int)))
        (fun s ->
           match map Fun.id s with
           | IntLit m -> (match s with IntLit n -> Int.compare m n = 0 | _ -> false)
           | _ -> false);
    ]
end
