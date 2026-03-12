type 'a tF =
  | Record of 'a list
  | App of Dsort.t * 'a list
  | TVar of Tvar.t
  | Int
  | Bool
  | Ptr of 'a

let map f = function
  | Record ts -> Record (List.map f ts)
  | App (d, ts) -> App (d, List.map f ts)
  | TVar a -> TVar a
  | Int -> Int
  | Bool -> Bool
  | Ptr t -> Ptr (f t)

let compare_tF cmp t1 t2 =
  let tag = function Record _ -> 0 | App _ -> 1 | TVar _ -> 2 | Int -> 3 | Bool -> 4 | Ptr _ -> 5 in
  match t1, t2 with
  | Record ts1, Record ts2 -> List.compare cmp ts1 ts2
  | App (d1, ts1), App (d2, ts2) ->
    let c = Dsort.compare d1 d2 in
    if c <> 0 then c else List.compare cmp ts1 ts2
  | TVar a1, TVar a2 -> Tvar.compare a1 a2
  | Int, Int -> 0
  | Bool, Bool -> 0
  | Ptr a1, Ptr a2 -> cmp a1 a2
  | _ -> Int.compare (tag t1) (tag t2)

let print_tF pp fmt = function
  | Record ts ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " *@ ") pp) ts
  | App (d, []) -> Dsort.print fmt d
  | App (d, ts) ->
    Format.fprintf fmt "@[<hov 2>%a(%a)@]" Dsort.print d
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp) ts
  | TVar a -> Tvar.print fmt a
  | Int -> Format.fprintf fmt "int"
  | Bool -> Format.fprintf fmt "bool"
  | Ptr t -> Format.fprintf fmt "@[<hov 2>ptr@ %a@]" pp t

type 'b t = In of 'b t tF * 'b

let extract (In (_, b)) = b
let shape (In (s, _)) = s

type ty = < loc : SourcePos.t > t

let rec compare t1 t2 =
  let In (s1, _) = t1 in
  let In (s2, _) = t2 in
  compare_tF compare s1 s2

let rec print fmt (In (s, _)) =
  print_tF print fmt s

let is_eqtype (In (s, _)) =
  match s with
  | Int | Bool | Ptr _ -> true
  | Record _ | App _ | TVar _ -> false

module Test = struct
  let gen_tF gen_a =
    let open QCheck.Gen in
    oneof [
      (let* ts = list_size (0 -- 4) gen_a in pure (Record ts));
      pure Int;
      pure Bool;
      map (fun a -> Ptr a) gen_a;
      (let* d = Dsort.Test.gen in
       let* ts = list_size (0 -- 2) gen_a in
       pure (App (d, ts)));
      map (fun a -> TVar a) Tvar.Test.gen;
    ]

  let gen =
    let open QCheck.Gen in
    let mk s =
      let loc = SourcePos.dummy in
      In (s, object method loc = loc end)
    in
    sized @@ fix (fun self n ->
      if n <= 0 then
        oneof [
          pure (mk Int);
          pure (mk Bool);
          pure (mk (Record []));
        ]
      else
        let sub = self (n / 2) in
        oneof [
          map (fun ts -> mk (Record ts)) (list_size (0 -- 3) sub);
          pure (mk Int);
          pure (mk Bool);
          map (fun a -> mk (Ptr a)) sub;
          (let* d = Dsort.Test.gen in
           let* ts = list_size (0 -- 2) sub in
           pure (mk (App (d, ts))));
          map (fun a -> mk (TVar a)) Tvar.Test.gen;
        ])

  let test =
    [ QCheck.Test.make ~name:"compare is reflexive"
        ~count:100
        (QCheck.make gen)
        (fun t -> compare t t = 0);
      QCheck.Test.make ~name:"map id is identity on shape"
        ~count:100
        (QCheck.make (gen_tF QCheck.Gen.int))
        (fun s -> compare_tF Int.compare (map Fun.id s) s = 0);
    ]
end
