type 'a sortF =
  | Int
  | Bool
  | Loc
  | Record of 'a list
  | App of Dsort.t * 'a list
  | Pred of 'a
  | TVar of Tvar.t

let map f = function
  | Int -> Int
  | Bool -> Bool
  | Loc -> Loc
  | Record ts -> Record (List.map f ts)
  | App (d, ts) -> App (d, List.map f ts)
  | Pred t -> Pred (f t)
  | TVar a -> TVar a

type 'b t = In of 'b t sortF * 'b

let extract (In (_, b)) = b
let shape (In (s, _)) = s

type sort = < loc : SourcePos.t > t

let rec compare_sort (In (s1, _)) (In (s2, _)) =
  compare_sortF s1 s2

and compare_sortF s1 s2 =
  match s1, s2 with
  | Int, Int -> 0
  | Int, _ -> -1
  | _, Int -> 1
  | Bool, Bool -> 0
  | Bool, _ -> -1
  | _, Bool -> 1
  | Loc, Loc -> 0
  | Loc, _ -> -1
  | _, Loc -> 1
  | Record ts1, Record ts2 -> compare_list ts1 ts2
  | Record _, _ -> -1
  | _, Record _ -> 1
  | App (d1, ts1), App (d2, ts2) ->
    let c = Dsort.compare d1 d2 in
    if c <> 0 then c else compare_list ts1 ts2
  | App _, _ -> -1
  | _, App _ -> 1
  | Pred t1, Pred t2 -> compare_sort t1 t2
  | Pred _, _ -> -1
  | _, Pred _ -> 1
  | TVar a1, TVar a2 -> Tvar.compare a1 a2

and compare_list ts1 ts2 =
  match ts1, ts2 with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | t1 :: rest1, t2 :: rest2 ->
    let c = compare_sort t1 t2 in
    if c <> 0 then c else compare_list rest1 rest2

let compare = compare_sort

let rec print fmt (In (s, _)) =
  match s with
  | Int -> Format.fprintf fmt "int"
  | Bool -> Format.fprintf fmt "bool"
  | Loc -> Format.fprintf fmt "loc"
  | Record [] -> Format.fprintf fmt "()"
  | Record [t] -> Format.fprintf fmt "(%a,)" print t
  | Record ts ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") print)
      ts
  | App (d, []) -> Dsort.print fmt d
  | App (d, ts) ->
    Format.fprintf fmt "@[<hov 2>%a(%a)@]" Dsort.print d
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") print)
      ts
  | Pred t -> Format.fprintf fmt "@[<hov 2>pred@ %a@]" print t
  | TVar a -> Tvar.print fmt a

let rec is_spec_type (In (s, _)) =
  match s with
  | Int | Bool | Loc -> true
  | Record ts -> List.for_all is_spec_type ts
  | App (_, ts) -> List.for_all is_spec_type ts
  | Pred _ -> false
  | TVar _ -> true

module Test = struct
  let gen =
    let open QCheck.Gen in
    let mk s = In (s, object method loc = SourcePos.dummy end) in
    sized @@ fix (fun self n ->
      if n <= 0 then
        oneof [
          pure (mk Int);
          pure (mk Bool);
          pure (mk Loc);
        ]
      else
        let sub = self (n / 3) in
        oneof [
          pure (mk Int);
          pure (mk Bool);
          pure (mk Loc);
          map (fun ts -> mk (Record ts)) (list_size (0 -- 3) sub);
          (let* d = Dsort.Test.gen in
           let* ts = list_size (0 -- 2) sub in
           pure (mk (App (d, ts))));
          map (fun t -> mk (Pred t)) sub;
          map (fun a -> mk (TVar a)) Tvar.Test.gen;
        ])

  let test =
    [ QCheck.Test.make ~name:"sort compare is reflexive"
        ~count:100
        (QCheck.make gen)
        (fun s -> compare s s = 0);

      QCheck.Test.make ~name:"spec type has no pred"
        ~count:100
        (QCheck.make gen)
        (fun s ->
           let has_pred = not (is_spec_type s) in
           let rec contains_pred (In (sh, _)) =
             match sh with
             | Pred _ -> true
             | Int | Bool | Loc | TVar _ -> false
             | Record ts -> List.exists contains_pred ts
             | App (_, ts) -> List.exists contains_pred ts
           in
           has_pred = contains_pred s)
    ]
end
