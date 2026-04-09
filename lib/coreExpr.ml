type ('a, 'b) ceF =
  | Var of Var.t
  | IntLit of int
  | BoolLit of bool
  | Let of (Var.t * 'b) * 'a * 'a
  | Tuple of 'a list
  | LetTuple of (Var.t * 'b) list * 'a * 'a
  | Inject of Label.t * 'a
  | Case of 'a * (Label.t * Var.t * 'a * 'b) list
  | Iter of Var.t * 'a * 'a
  | App of Prim.t * 'a
  | Call of string * 'a
  | If of 'a * 'a * 'a
  | Annot of 'a * 'b Sort.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Take of (Var.t * 'b) * 'a * 'a
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
    Case (f scrut, List.map (fun (l, x, e, b) -> (l, x, f e, b)) branches)
  | Iter (x, e1, e2) -> Iter (x, f e1, f e2)
  | App (p, e) -> App (p, f e)
  | Call (name, e) -> Call (name, f e)
  | If (e1, e2, e3) -> If (f e1, f e2, f e3)
  | Annot (e, s) -> Annot (f e, s)
  | Eq (a, b) -> Eq (f a, f b)
  | And (a, b) -> And (f a, f b)
  | Not a -> Not (f a)
  | Take (x, e1, e2) -> Take (x, f e1, f e2)
  | Return a -> Return (f a)

let map_info f = function
  | Var x -> Var x
  | IntLit n -> IntLit n
  | BoolLit b -> BoolLit b
  | Let ((x, b), e1, e2) -> Let ((x, f b), e1, e2)
  | Tuple es -> Tuple es
  | LetTuple (xs, e1, e2) -> LetTuple (List.map (fun (x, b) -> (x, f b)) xs, e1, e2)
  | Inject (l, e) -> Inject (l, e)
  | Case (scrut, branches) -> Case (scrut, List.map (fun (l, x, e, b) -> (l, x, e, f b)) branches)
  | Iter (x, e1, e2) -> Iter (x, e1, e2)
  | App (p, e) -> App (p, e)
  | Call (name, e) -> Call (name, e)
  | If (e1, e2, e3) -> If (e1, e2, e3)
  | Annot (e, s) -> Annot (e, Sort.map f s)
  | Eq (a, b) -> Eq (a, b)
  | And (a, b) -> And (a, b)
  | Not a -> Not a
  | Take ((x, b), e1, e2) -> Take ((x, f b), e1, e2)
  | Return a -> Return a

type 'b t = In of 'b * ('b t, 'b) ceF

let mk b sf = In (b, sf)
let info (In (b, _)) = b
let shape (In (_, sf)) = sf

let rec map f (In (b, sf)) =
  In (f b, map_info f (map_shape (map f) sf))

type ce = < loc : SourcePos.t > t

type typed_info = < loc : SourcePos.t; ctx : Context.t; sort : Sort.sort; eff : Effect.t >
type typed_ce = typed_info t

let infix_op_string = function
  | Prim.Add -> "+" | Prim.Sub -> "-"
  | Prim.Mul -> "*" | Prim.Div -> "/"
  | Prim.Lt -> "<" | Prim.Le -> "<="
  | Prim.Gt -> ">" | Prim.Ge -> ">="
  | _ -> assert false

let rec print_gen pp_var fmt t =
  let pr = print_gen pp_var in
  match shape t with
  | Var x -> pp_var fmt x
  | IntLit n -> Format.fprintf fmt "%d" n
  | BoolLit b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Let ((x, _), e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let %a =@ %a;@]@ %a@]"
      pp_var x pr e1 pr e2
  | Tuple [] -> Format.fprintf fmt "()"
  | Tuple [e] -> Format.fprintf fmt "(%a,)" pr e
  | Tuple es ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pr)
      es
  | LetTuple (xs, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>let (%a) =@ %a;@]@ %a@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (x, _) -> pp_var fmt x)) xs
      pr e1 pr e2
  | Inject (l, e) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" Label.print l pr e
  | Case (scrut, branches) ->
    Format.fprintf fmt "@[<v>@[<hov 2>case %a {@ %a@]@ }@]"
      pr scrut
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
         (fun fmt (l, x, body, _) ->
            Format.fprintf fmt "@[<hov 2>%a %a ->@ %a@]" Label.print l pp_var x pr body))
      branches
  | Iter (x, e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>iter (%a = %a) {@ %a@]@ }@]"
      pp_var x pr e1 pr e2
  | App ((Prim.Add | Prim.Sub | Prim.Mul | Prim.Div | Prim.Lt | Prim.Le | Prim.Gt | Prim.Ge) as p, arg) ->
    (match shape arg with
     | Tuple [e1; e2] ->
       Format.fprintf fmt "@[<hov 2>%a %s@ %a@]" pr e1 (infix_op_string p) pr e2
     | _ ->
       Format.fprintf fmt "@[<hov 2>%a@ %a@]" Prim.print p pr arg)
  | App (Prim.Not, e) ->
    Format.fprintf fmt "@[<hov 2>not@ %a@]" pr e
  | App (p, e) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" Prim.print p pr e
  | Call (name, e) ->
    Format.fprintf fmt "@[<hov 2>%s@ %a@]" name pr e
  | If (e1, e2, e3) ->
    Format.fprintf fmt "@[<v>@[<hov 2>if %a@ then %a@]@ @[<hov 2>else %a@]@]"
      pr e1 pr e2 pr e3
  | Annot (e, s) ->
    Format.fprintf fmt "@[<hov 2>%a :@ %a@]" pr e Sort.print s
  | Eq (a, b) -> Format.fprintf fmt "@[<hov 2>%a ==@ %a@]" pr a pr b
  | And (a, b) -> Format.fprintf fmt "@[<hov 2>%a &&@ %a@]" pr a pr b
  | Not a -> Format.fprintf fmt "@[<hov 2>not@ %a@]" pr a
  | Take ((x, _), e1, e2) ->
    Format.fprintf fmt "@[<v>@[<hov 2>take %a =@ %a;@]@ %a@]"
      pp_var x pr e1 pr e2
  | Return a -> Format.fprintf fmt "@[<hov 2>return@ %a@]" pr a

let print fmt t = print_gen Var.print fmt t
let to_string t = Format.asprintf "%a" (print_gen Var.print_unique) t

let rec json jb t =
  let node_info = ["info", jb (info t)] in
  let shape_fields = match shape t with
    | Var x -> ["tag", Json.String "Var"; "var", Var.json x]
    | IntLit n -> ["tag", Json.String "IntLit"; "value", Json.Int n]
    | BoolLit b -> ["tag", Json.String "BoolLit"; "value", Json.Bool b]
    | Let ((x, b), e1, e2) ->
      ["tag", Json.String "Let"; "var", Var.json x; "var_info", jb b;
       "bound", json jb e1; "body", json jb e2]
    | Tuple es -> ["tag", Json.String "Tuple"; "elems", Json.Array (List.map (json jb) es)]
    | LetTuple (xs, e1, e2) ->
      ["tag", Json.String "LetTuple";
       "vars", Json.Array (List.map (fun (x, b) -> Json.Object ["var", Var.json x; "info", jb b]) xs);
       "bound", json jb e1; "body", json jb e2]
    | Inject (l, e) -> ["tag", Json.String "Inject"; "label", Label.json l; "arg", json jb e]
    | Case (scrut, branches) ->
      ["tag", Json.String "Case"; "scrutinee", json jb scrut;
       "branches", Json.Array (List.map (fun (l, x, e, b) ->
         Json.Object ["label", Label.json l; "var", Var.json x;
                       "body", json jb e; "info", jb b]) branches)]
    | Iter (x, e1, e2) ->
      ["tag", Json.String "Iter"; "var", Var.json x; "init", json jb e1; "body", json jb e2]
    | App (p, e) -> ["tag", Json.String "App"; "prim", Prim.json p; "arg", json jb e]
    | Call (name, e) -> ["tag", Json.String "Call"; "name", Json.String name; "arg", json jb e]
    | If (e1, e2, e3) ->
      ["tag", Json.String "If"; "cond", json jb e1; "then", json jb e2; "else", json jb e3]
    | Annot (e, s) ->
      ["tag", Json.String "Annot"; "expr", json jb e; "sort", Sort.json jb s]
    | Eq (a, b) -> ["tag", Json.String "Eq"; "left", json jb a; "right", json jb b]
    | And (a, b) -> ["tag", Json.String "And"; "left", json jb a; "right", json jb b]
    | Not a -> ["tag", Json.String "Not"; "arg", json jb a]
    | Take ((x, b), e1, e2) ->
      ["tag", Json.String "Take"; "var", Var.json x; "var_info", jb b;
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
