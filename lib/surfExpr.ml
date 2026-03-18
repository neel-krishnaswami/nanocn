type ('a, 'b) seF =
  | Var of Var.t
  | IntLit of int
  | BoolLit of bool
  | Let of 'b Pat.t * 'a * 'a
  | Tuple of 'a list
  | Inject of Label.t * 'a
  | Case of 'a * ('b Pat.t * 'a * 'b) list
  | Iter of 'b Pat.t * 'a * 'a
  | App of Prim.t * 'a
  | Call of Var.t * 'a
  | If of 'a * 'a * 'a
  | Annot of 'a * 'b Sort.t * Effect.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Own of 'b Sort.t
  | Take of 'b Pat.t * 'a * 'a
  | Return of 'a

let map_shape f = function
  | Var x -> Var x
  | IntLit n -> IntLit n
  | BoolLit b -> BoolLit b
  | Let (p, e1, e2) -> Let (p, f e1, f e2)
  | Tuple es -> Tuple (List.map f es)
  | Inject (l, e) -> Inject (l, f e)
  | Case (scrut, branches) ->
    Case (f scrut, List.map (fun (p, e, b) -> (p, f e, b)) branches)
  | Iter (p, e1, e2) -> Iter (p, f e1, f e2)
  | App (p, e) -> App (p, f e)
  | Call (name, e) -> Call (name, f e)
  | If (e1, e2, e3) -> If (f e1, f e2, f e3)
  | Annot (e, s, eff) -> Annot (f e, s, eff)
  | Eq (a, b) -> Eq (f a, f b)
  | And (a, b) -> And (f a, f b)
  | Not a -> Not (f a)
  | Own s -> Own s
  | Take (p, e1, e2) -> Take (p, f e1, f e2)
  | Return a -> Return (f a)

let map_info f = function
  | Var x -> Var x
  | IntLit n -> IntLit n
  | BoolLit b -> BoolLit b
  | Let (p, e1, e2) -> Let (Pat.map f p, e1, e2)
  | Tuple es -> Tuple es
  | Inject (l, e) -> Inject (l, e)
  | Case (scrut, branches) ->
    Case (scrut, List.map (fun (p, e, b) -> (Pat.map f p, e, f b)) branches)
  | Iter (p, e1, e2) -> Iter (Pat.map f p, e1, e2)
  | App (p, e) -> App (p, e)
  | Call (name, e) -> Call (name, e)
  | If (e1, e2, e3) -> If (e1, e2, e3)
  | Annot (e, s, eff) -> Annot (e, Sort.map f s, eff)
  | Eq (a, b) -> Eq (a, b)
  | And (a, b) -> And (a, b)
  | Not a -> Not a
  | Own s -> Own (Sort.map f s)
  | Take (p, e1, e2) -> Take (Pat.map f p, e1, e2)
  | Return a -> Return a

type 'b t = In of 'b * ('b t, 'b) seF

let mk b sf = In (b, sf)
let info (In (b, _)) = b
let shape (In (_, sf)) = sf

let rec map f (In (b, sf)) =
  In (f b, map_info f (map_shape (map f) sf))

type se = < loc : SourcePos.t > t

let infix_op_string = function
  | Prim.Add -> "+" | Prim.Sub -> "-"
  | Prim.Mul -> "*" | Prim.Div -> "/"
  | Prim.Lt -> "<" | Prim.Le -> "<="
  | Prim.Gt -> ">" | Prim.Ge -> ">="
  | _ -> assert false

let rec print fmt t =
  match shape t with
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
         (fun fmt (p, body, _) ->
            Format.fprintf fmt "@[<hov 2>%a ->@ %a@]" Pat.print p print body))
      branches
  | Iter (p, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>iter (%a = %a) {@ %a@]@ }@]"
      Pat.print p print e1 print e2
  | App ((Prim.Add | Prim.Sub | Prim.Mul | Prim.Div | Prim.Lt | Prim.Le | Prim.Gt | Prim.Ge) as p, arg) ->
    (match shape arg with
     | Tuple [e1; e2] ->
       Format.fprintf fmt "@[<hov 2>%a %s@ %a@]" print e1 (infix_op_string p) print e2
     | _ ->
       Format.fprintf fmt "@[<hov 2>%a@ %a@]" Prim.print p print arg)
  | App (Prim.Not, e) ->
    Format.fprintf fmt "@[<hov 2>not@ %a@]" print e
  | App (p, e) -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" Prim.print p print e
  | Call (name, e) -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" Var.print name print e
  | If (e1, e2, e3) ->
    Format.fprintf fmt "@[<v>@[<hov 2>if %a@ then %a@]@ @[<hov 2>else %a@]@]"
      print e1 print e2 print e3
  | Annot (e, s, eff) ->
    Format.fprintf fmt "@[<hov 2>%a :@ %a [%a]@]" print e Sort.print s Effect.print eff
  | Eq (a, b) -> Format.fprintf fmt "@[<hov 2>%a ==@ %a@]" print a print b
  | And (a, b) -> Format.fprintf fmt "@[<hov 2>%a &&@ %a@]" print a print b
  | Not a -> Format.fprintf fmt "@[<hov 2>not@ %a@]" print a
  | Own s -> Format.fprintf fmt "@[<hov 2>Own[%a]@]" Sort.print s
  | Take (p, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>take %a =@ %a;@]@ %a@]"
      Pat.print p print e1 print e2
  | Return a -> Format.fprintf fmt "@[<hov 2>return@ %a@]" print a

let rec json jb t =
  let node_info = ["info", jb (info t)] in
  let shape_fields = match shape t with
    | Var x -> ["tag", Json.String "Var"; "var", Var.json x]
    | IntLit n -> ["tag", Json.String "IntLit"; "value", Json.Int n]
    | BoolLit b -> ["tag", Json.String "BoolLit"; "value", Json.Bool b]
    | Let (p, e1, e2) ->
      ["tag", Json.String "Let"; "pat", Pat.json jb p;
       "bound", json jb e1; "body", json jb e2]
    | Tuple es -> ["tag", Json.String "Tuple"; "elems", Json.Array (List.map (json jb) es)]
    | Inject (l, e) -> ["tag", Json.String "Inject"; "label", Label.json l; "arg", json jb e]
    | Case (scrut, branches) ->
      ["tag", Json.String "Case"; "scrutinee", json jb scrut;
       "branches", Json.Array (List.map (fun (p, e, b) ->
         Json.Object ["pat", Pat.json jb p; "body", json jb e; "info", jb b]) branches)]
    | Iter (p, e1, e2) ->
      ["tag", Json.String "Iter"; "pat", Pat.json jb p; "init", json jb e1; "body", json jb e2]
    | App (p, e) -> ["tag", Json.String "App"; "prim", Prim.json p; "arg", json jb e]
    | Call (name, e) -> ["tag", Json.String "Call"; "name", Var.json name; "arg", json jb e]
    | If (e1, e2, e3) ->
      ["tag", Json.String "If"; "cond", json jb e1; "then", json jb e2; "else", json jb e3]
    | Annot (e, s, eff) ->
      ["tag", Json.String "Annot"; "expr", json jb e; "sort", Sort.json jb s; "effect", Effect.json eff]
    | Eq (a, b) -> ["tag", Json.String "Eq"; "left", json jb a; "right", json jb b]
    | And (a, b) -> ["tag", Json.String "And"; "left", json jb a; "right", json jb b]
    | Not a -> ["tag", Json.String "Not"; "arg", json jb a]
    | Own s -> ["tag", Json.String "Own"; "sort", Sort.json jb s]
    | Take (p, e1, e2) ->
      ["tag", Json.String "Take"; "pat", Pat.json jb p;
       "bound", json jb e1; "body", json jb e2]
    | Return a -> ["tag", Json.String "Return"; "arg", json jb a]
  in
  Json.Object (shape_fields @ node_info)

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
          map (fun n -> mk_t (IntLit n)) (0 -- 100);
          (let* p = Pat.Test.gen in
           let* e1 = sub in
           let* e2 = sub in
           pure (mk_t (Let (p, e1, e2))));
          map (fun es -> mk_t (Tuple es)) (list_size (0 -- 3) sub);
          (let* l = Label.Test.gen in
           let* e = sub in
           pure (mk_t (Inject (l, e))));
          (let* e1 = sub in
           let* e2 = sub in
           pure (mk_t (Eq (e1, e2))));
          map (fun e -> mk_t (Not e)) sub;
          map (fun e -> mk_t (Return e)) sub;
          (let* p = Prim.Test.gen in
           let* e = sub in
           pure (mk_t (App (p, e))));
        ])

  let test =
    [ QCheck.Test.make ~name:"surfExpr map_shape id is identity"
        ~count:50
        (QCheck.make (QCheck.Gen.map (fun n -> IntLit n) (QCheck.Gen.int)))
        (fun s ->
           match map_shape Fun.id s with
           | IntLit m -> (match s with IntLit n -> Int.compare m n = 0 | _ -> false)
           | _ -> false);
    ]
end
