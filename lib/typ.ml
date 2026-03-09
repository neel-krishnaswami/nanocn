type 'a tF =
  | Record of 'a list
  | Sum of (Label.t * 'a) list
  | Int
  | Ptr of 'a

let map f = function
  | Record ts -> Record (List.map f ts)
  | Sum cases -> Sum (List.map (fun (l, t) -> (l, f t)) cases)
  | Int -> Int
  | Ptr t -> Ptr (f t)

let compare_tF cmp t1 t2 =
  let tag = function Record _ -> 0 | Sum _ -> 1 | Int -> 2 | Ptr _ -> 3 in
  match t1, t2 with
  | Record ts1, Record ts2 -> List.compare cmp ts1 ts2
  | Sum cs1, Sum cs2 ->
    List.compare (fun (l1, a1) (l2, a2) ->
      let c = Label.compare l1 l2 in
      if c <> 0 then c else cmp a1 a2) cs1 cs2
  | Int, Int -> 0
  | Ptr a1, Ptr a2 -> cmp a1 a2
  | _ -> Int.compare (tag t1) (tag t2)

let print_tF pp fmt = function
  | Record ts ->
    Format.fprintf fmt "@[<hov 2>[%a]@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp) ts
  | Sum cases ->
    Format.fprintf fmt "@[<hov 2>{%a}@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
         (fun fmt (l, t) -> Format.fprintf fmt "%a:@ %a" Label.print l pp t))
      cases
  | Int -> Format.fprintf fmt "int"
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

module Test = struct
  let gen_tF gen_a =
    let open QCheck.Gen in
    oneof [
      (let* ts = list_size (0 -- 4) gen_a in pure (Record ts));
      (let* cs = list_size (1 -- 4) (pair Label.Test.gen gen_a) in pure (Sum cs));
      pure Int;
      map (fun a -> Ptr a) gen_a;
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
          pure (mk (Record []));
        ]
      else
        let sub = self (n / 2) in
        oneof [
          map (fun ts -> mk (Record ts)) (list_size (0 -- 3) sub);
          map (fun cs -> mk (Sum cs)) (list_size (1 -- 3) (pair Label.Test.gen sub));
          pure (mk Int);
          map (fun a -> mk (Ptr a)) sub;
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
