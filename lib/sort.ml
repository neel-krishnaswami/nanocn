type ('a, 'b) sortF =
  | Int
  | Bool
  | Ptr of 'a
  | Record of 'a list
  | App of Dsort.t * 'a list
  | Pred of 'a
  | TVar of Tvar.t

let map_shape f = function
  | Int -> Int
  | Bool -> Bool
  | Ptr t -> Ptr (f t)
  | Record ts -> Record (List.map f ts)
  | App (d, ts) -> App (d, List.map f ts)
  | Pred t -> Pred (f t)
  | TVar a -> TVar a

let map_info _ = function
  | Int -> Int
  | Bool -> Bool
  | Ptr t -> Ptr t
  | Record ts -> Record ts
  | App (d, ts) -> App (d, ts)
  | Pred t -> Pred t
  | TVar a -> TVar a

type 'b t = In of 'b * ('b t, 'b) sortF

let mk b sf = In (b, sf)
let info (In (b, _)) = b
let shape (In (_, sf)) = sf

let rec map f (In (b, sf)) =
  In (f b, map_shape (map f) sf)

type sort = < loc : SourcePos.t > t

let rec compare_sort s1 s2 =
  compare_sortF (shape s1) (shape s2)

and compare_sortF s1 s2 =
  match s1, s2 with
  | Int, Int -> 0
  | Int, _ -> -1
  | _, Int -> 1
  | Bool, Bool -> 0
  | Bool, _ -> -1
  | _, Bool -> 1
  | Ptr t1, Ptr t2 -> compare_sort t1 t2
  | Ptr _, _ -> -1
  | _, Ptr _ -> 1
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

let rec print fmt t =
  match shape t with
  | Int -> Format.fprintf fmt "int"
  | Bool -> Format.fprintf fmt "bool"
  | Ptr t -> Format.fprintf fmt "@[<hov 2>ptr@ %a@]" print t
  | Record [] -> Format.fprintf fmt "()"
  | Record [t] -> Format.fprintf fmt "(%a *)" print t
  | Record ts ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " *@ ") print)
      ts
  | App (d, []) -> Dsort.print fmt d
  | App (d, ts) ->
    Format.fprintf fmt "@[<hov 2>%a(%a)@]" Dsort.print d
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") print)
      ts
  | Pred t -> Format.fprintf fmt "@[<hov 2>pred@ %a@]" print t
  | TVar a -> Tvar.print fmt a

let rec json jb t =
  let fields = ["info", jb (info t)] in
  let shape_fields = match shape t with
    | Int -> ["tag", Json.String "Int"]
    | Bool -> ["tag", Json.String "Bool"]
    | Ptr t -> ["tag", Json.String "Ptr"; "arg", json jb t]
    | Record ts -> ["tag", Json.String "Record"; "fields", Json.Array (List.map (json jb) ts)]
    | App (d, ts) -> ["tag", Json.String "App"; "name", Dsort.json d; "args", Json.Array (List.map (json jb) ts)]
    | Pred t -> ["tag", Json.String "Pred"; "arg", json jb t]
    | TVar a -> ["tag", Json.String "TVar"; "name", Tvar.json a]
  in
  Json.Object (shape_fields @ fields)

let rec is_spec_type s =
  match shape s with
  | Int | Bool -> true
  | Ptr t -> is_spec_type t
  | Record ts -> List.for_all is_spec_type ts
  | App (_, ts) -> List.for_all is_spec_type ts
  | Pred _ -> false
  | TVar _ -> true

let rec typ_to_sort ty =
  let b = Typ.info ty in
  let sf = match Typ.shape ty with
    | Typ.Int -> Int
    | Typ.Bool -> Bool
    | Typ.Ptr t -> Ptr (typ_to_sort t)
    | Typ.Record ts -> Record (List.map typ_to_sort ts)
    | Typ.App (d, ts) -> App (d, List.map typ_to_sort ts)
    | Typ.TVar a -> TVar a
  in
  mk b sf

let rec sort_to_typ s =
  let b = info s in
  match shape s with
  | Int -> Ok (Typ.mk b Typ.Int)
  | Bool -> Ok (Typ.mk b Typ.Bool)
  | Ptr t ->
    (match sort_to_typ t with
     | Ok t' -> Ok (Typ.mk b (Typ.Ptr t'))
     | Error _ as e -> e)
  | Record ts ->
    (match sort_to_typ_list ts with
     | Ok ts' -> Ok (Typ.mk b (Typ.Record ts'))
     | Error _ as e -> e)
  | App (d, ts) ->
    (match sort_to_typ_list ts with
     | Ok ts' -> Ok (Typ.mk b (Typ.App (d, ts')))
     | Error _ as e -> e)
  | TVar a -> Ok (Typ.mk b (Typ.TVar a))
  | Pred _ -> Error "sort contains pred, cannot convert to type"

and sort_to_typ_list = function
  | [] -> Ok []
  | s :: rest ->
    match sort_to_typ s with
    | Error _ as e -> e
    | Ok t ->
      match sort_to_typ_list rest with
      | Error _ as e -> e
      | Ok rest' -> Ok (t :: rest')

module Test = struct
  let gen =
    let open QCheck.Gen in
    let mk_t s = mk (object method loc = SourcePos.dummy end) s in
    sized @@ fix (fun self n ->
      if n <= 0 then
        oneof [
          pure (mk_t Int);
          pure (mk_t Bool);
          pure (mk_t (Ptr (mk_t Int)));
        ]
      else
        let sub = self (n / 3) in
        oneof [
          pure (mk_t Int);
          pure (mk_t Bool);
          map (fun t -> mk_t (Ptr t)) sub;
          map (fun ts -> mk_t (Record ts)) (list_size (0 -- 3) sub);
          (let* d = Dsort.Test.gen in
           let* ts = list_size (0 -- 2) sub in
           pure (mk_t (App (d, ts))));
          map (fun t -> mk_t (Pred t)) sub;
          map (fun a -> mk_t (TVar a)) Tvar.Test.gen;
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
           let rec contains_pred s =
             match shape s with
             | Pred _ -> true
             | Int | Bool | TVar _ -> false
             | Ptr t -> contains_pred t
             | Record ts -> List.exists contains_pred ts
             | App (_, ts) -> List.exists contains_pred ts
           in
           has_pred = contains_pred s);

      QCheck.Test.make ~name:"sort mk/info/shape round-trip"
        ~count:100
        (QCheck.make gen)
        (fun s -> compare (mk (info s) (shape s)) s = 0);

      QCheck.Test.make ~name:"sort map Fun.id is identity"
        ~count:100
        (QCheck.make gen)
        (fun s -> compare (map Fun.id s) s = 0);
    ]
end
