type ('a, 'b) tF =
  | Record of 'a list
  | App of Dsort.t * 'a list
  | TVar of Tvar.t
  | Int
  | Bool
  | Ptr of 'a

let map_shape f = function
  | Record ts -> Record (List.map f ts)
  | App (d, ts) -> App (d, List.map f ts)
  | TVar a -> TVar a
  | Int -> Int
  | Bool -> Bool
  | Ptr t -> Ptr (f t)

let map_info _ = function
  | Record ts -> Record ts
  | App (d, ts) -> App (d, ts)
  | TVar a -> TVar a
  | Int -> Int
  | Bool -> Bool
  | Ptr t -> Ptr t

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
  | Int -> Format.fprintf fmt "Int"
  | Bool -> Format.fprintf fmt "Bool"
  | Ptr t -> Format.fprintf fmt "@[<hov 2>Ptr@ %a@]" pp t

type 'b t = In of 'b * ('b t, 'b) tF

let mk b sf = In (b, sf)
let info (In (b, _)) = b
let shape (In (_, sf)) = sf

let rec map f (In (b, sf)) =
  In (f b, map_shape (map f) sf)

type ty = < loc : SourcePos.t > t

let rec compare t1 t2 =
  compare_tF compare (shape t1) (shape t2)

let rec print fmt t =
  print_tF print fmt (shape t)

let rec json t =
  match shape t with
  | Int -> Json.Object ["tag", Json.String "Int"]
  | Bool -> Json.Object ["tag", Json.String "Bool"]
  | Ptr t -> Json.Object ["tag", Json.String "Ptr"; "arg", json t]
  | Record ts -> Json.Object ["tag", Json.String "Record"; "fields", Json.Array (List.map json ts)]
  | App (d, ts) -> Json.Object ["tag", Json.String "App"; "name", Dsort.json d; "args", Json.Array (List.map json ts)]
  | TVar a -> Json.Object ["tag", Json.String "TVar"; "name", Tvar.json a]

let is_eqtype t =
  match shape t with
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
    let mk_t s =
      let loc = SourcePos.dummy in
      mk (object method loc = loc end) s
    in
    sized @@ fix (fun self n ->
      if n <= 0 then
        oneof [
          pure (mk_t Int);
          pure (mk_t Bool);
          pure (mk_t (Record []));
        ]
      else
        let sub = self (n / 2) in
        oneof [
          map (fun ts -> mk_t (Record ts)) (list_size (0 -- 3) sub);
          pure (mk_t Int);
          pure (mk_t Bool);
          map (fun a -> mk_t (Ptr a)) sub;
          (let* d = Dsort.Test.gen in
           let* ts = list_size (0 -- 2) sub in
           pure (mk_t (App (d, ts))));
          map (fun a -> mk_t (TVar a)) Tvar.Test.gen;
        ])

  let test =
    [ QCheck.Test.make ~name:"compare is reflexive"
        ~count:100
        (QCheck.make gen)
        (fun t -> compare t t = 0);
      QCheck.Test.make ~name:"map_shape id is identity on shape"
        ~count:100
        (QCheck.make (gen_tF QCheck.Gen.int))
        (fun s -> compare_tF Int.compare (map_shape Fun.id s) s = 0);
      QCheck.Test.make ~name:"typ mk/info/shape round-trip"
        ~count:100
        (QCheck.make gen)
        (fun t -> compare (mk (info t) (shape t)) t = 0);
      QCheck.Test.make ~name:"typ map Fun.id is identity"
        ~count:100
        (QCheck.make gen)
        (fun t -> compare (map Fun.id t) t = 0);
    ]
end
