type ('a, 'b) ceF =
  | Var of Var.t
  | IntLit of int
  | BoolLit of bool
  | Let of Var.t * 'a * 'a
  | Tuple of 'a list
  | LetTuple of Var.t list * 'a * 'a
  | Inject of Label.t * 'a
  | Case of 'a * (Label.t * Var.t * 'a) list
  | Iter of Var.t * 'a * 'a
  | App of Prim.t * 'a
  | Call of Var.t * 'a
  | If of 'a * 'a * 'a
  | Annot of 'a * 'b Sort.t * Effect.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Own of 'b Sort.t
  | Take of Var.t * 'a * 'a
  | Return of 'a

let map_shape f = function
  | Var x -> Var x
  | IntLit n -> IntLit n
  | BoolLit b -> BoolLit b
  | Let (x, e1, e2) -> Let (x, f e1, f e2)
  | Tuple es -> Tuple (List.map f es)
  | LetTuple (xs, e1, e2) -> LetTuple (xs, f e1, f e2)
  | Inject (l, e) -> Inject (l, f e)
  | Case (scrut, branches) ->
    Case (f scrut, List.map (fun (l, x, e) -> (l, x, f e)) branches)
  | Iter (x, e1, e2) -> Iter (x, f e1, f e2)
  | App (p, e) -> App (p, f e)
  | Call (name, e) -> Call (name, f e)
  | If (e1, e2, e3) -> If (f e1, f e2, f e3)
  | Annot (e, s, eff) -> Annot (f e, s, eff)
  | Eq (a, b) -> Eq (f a, f b)
  | And (a, b) -> And (f a, f b)
  | Not a -> Not (f a)
  | Own s -> Own s
  | Take (x, e1, e2) -> Take (x, f e1, f e2)
  | Return a -> Return (f a)

let map_info f = function
  | Var x -> Var x
  | IntLit n -> IntLit n
  | BoolLit b -> BoolLit b
  | Let (x, e1, e2) -> Let (x, e1, e2)
  | Tuple es -> Tuple es
  | LetTuple (xs, e1, e2) -> LetTuple (xs, e1, e2)
  | Inject (l, e) -> Inject (l, e)
  | Case (scrut, branches) -> Case (scrut, branches)
  | Iter (x, e1, e2) -> Iter (x, e1, e2)
  | App (p, e) -> App (p, e)
  | Call (name, e) -> Call (name, e)
  | If (e1, e2, e3) -> If (e1, e2, e3)
  | Annot (e, s, eff) -> Annot (e, Sort.map f s, eff)
  | Eq (a, b) -> Eq (a, b)
  | And (a, b) -> And (a, b)
  | Not a -> Not a
  | Own s -> Own (Sort.map f s)
  | Take (x, e1, e2) -> Take (x, e1, e2)
  | Return a -> Return a

type 'b t = In of 'b * ('b t, 'b) ceF

let mk b sf = In (b, sf)
let info (In (b, _)) = b
let shape (In (_, sf)) = sf

let rec map f (In (b, sf)) =
  In (f b, map_info f (map_shape (map f) sf))

type ce = < loc : SourcePos.t > t

let infix_op_string = function
  | Prim.Add -> "+" | Prim.Sub -> "-"
  | Prim.Mul -> "*" | Prim.Div -> "/"
  | _ -> assert false

let rec print fmt t =
  match shape t with
  | Var x -> Var.print fmt x
  | IntLit n -> Format.fprintf fmt "%d" n
  | BoolLit b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Let (x, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let %a =@ %a;@]@ %a@]"
      Var.print x print e1 print e2
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
  | Inject (l, e) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" Label.print l print e
  | Case (scrut, branches) ->
    Format.fprintf fmt "@[<v>@[<hov 2>case %a {@ %a@]@ }@]"
      print scrut
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
         (fun fmt (l, x, body) ->
            Format.fprintf fmt "@[<hov 2>%a %a ->@ %a@]" Label.print l Var.print x print body))
      branches
  | Iter (x, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>iter (%a = %a) {@ %a@]@ }@]"
      Var.print x print e1 print e2
  | App ((Prim.Add | Prim.Sub | Prim.Mul | Prim.Div) as p, arg) ->
    (match shape arg with
     | Tuple [e1; e2] ->
       Format.fprintf fmt "@[<hov 2>%a %s@ %a@]" print e1 (infix_op_string p) print e2
     | _ ->
       Format.fprintf fmt "@[<hov 2>%a@ %a@]" Prim.print p print arg)
  | App (Prim.Not, e) ->
    Format.fprintf fmt "@[<hov 2>not@ %a@]" print e
  | App (p, e) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" Prim.print p print e
  | Call (name, e) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" Var.print name print e
  | If (e1, e2, e3) ->
    Format.fprintf fmt "@[<v>@[<hov 2>if %a@ then %a@]@ @[<hov 2>else %a@]@]"
      print e1 print e2 print e3
  | Annot (e, s, eff) ->
    Format.fprintf fmt "@[<hov 2>%a :@ %a [%a]@]" print e Sort.print s Effect.print eff
  | Eq (a, b) -> Format.fprintf fmt "@[<hov 2>%a ==@ %a@]" print a print b
  | And (a, b) -> Format.fprintf fmt "@[<hov 2>%a &&@ %a@]" print a print b
  | Not a -> Format.fprintf fmt "@[<hov 2>not@ %a@]" print a
  | Own s -> Format.fprintf fmt "@[<hov 2>Own[%a]@]" Sort.print s
  | Take (x, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>take %a =@ %a;@]@ %a@]"
      Var.print x print e1 print e2
  | Return a -> Format.fprintf fmt "@[<hov 2>return@ %a@]" print a

module Test = struct
  let gen =
    let open QCheck.Gen in
    let mk_t s = mk (object method loc = SourcePos.dummy end) s in
    sized @@ fix (fun self n ->
      if n <= 0 then
        oneof [
          map (fun v -> mk_t (Var v)) Var.Test.gen;
          map (fun n -> mk_t (IntLit n)) (0 -- 100);
          map (fun b -> mk_t (BoolLit b)) bool;
        ]
      else
        let sub = self (n / 3) in
        oneof [
          map (fun v -> mk_t (Var v)) Var.Test.gen;
          (let* e1 = sub in
           let* e2 = sub in
           pure (mk_t (Eq (e1, e2))));
          (let* e1 = sub in
           let* e2 = sub in
           pure (mk_t (And (e1, e2))));
          map (fun e -> mk_t (Not e)) sub;
          (let* l = Label.Test.gen in
           let* e = sub in
           pure (mk_t (Inject (l, e))));
          map (fun es -> mk_t (Tuple es)) (list_size (0 -- 3) sub);
          (let* p = Prim.Test.gen in
           let* e = sub in
           pure (mk_t (App (p, e))));
        ])

  let test =
    [ QCheck.Test.make ~name:"coreExpr map_shape id is identity"
        ~count:50
        (QCheck.make (QCheck.Gen.map (fun n -> IntLit n) (QCheck.Gen.int)))
        (fun s ->
           match map_shape Fun.id s with
           | IntLit m -> (match s with IntLit n -> Int.compare m n = 0 | _ -> false)
           | _ -> false);
    ]
end
