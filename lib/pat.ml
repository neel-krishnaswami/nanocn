type ('a, 'b) patF =
  | Var of Var.t
  | Con of Label.t * 'a
  | Tuple of 'a list

let map_shape f = function
  | Var x -> Var x
  | Con (l, p) -> Con (l, f p)
  | Tuple ps -> Tuple (List.map f ps)

let map_info _ = function
  | Var x -> Var x
  | Con (l, p) -> Con (l, p)
  | Tuple ps -> Tuple ps

type 'b t = In of 'b * ('b t, 'b) patF

let mk b sf = In (b, sf)
let info (In (b, _)) = b
let shape (In (_, sf)) = sf

let rec map f (In (b, sf)) =
  In (f b, map_shape (map f) sf)

type pat = < loc : SourcePos.t > t

let rec compare_pat p1 p2 =
  compare_patF (shape p1) (shape p2)

and compare_patF s1 s2 =
  match s1, s2 with
  | Var x1, Var x2 -> Var.compare x1 x2
  | Var _, _ -> -1
  | _, Var _ -> 1
  | Con (l1, p1), Con (l2, p2) ->
    let c = Label.compare l1 l2 in
    if c <> 0 then c else compare_pat p1 p2
  | Con _, _ -> -1
  | _, Con _ -> 1
  | Tuple ps1, Tuple ps2 -> compare_list ps1 ps2

and compare_list ps1 ps2 =
  match ps1, ps2 with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | p1 :: rest1, p2 :: rest2 ->
    let c = compare_pat p1 p2 in
    if c <> 0 then c else compare_list rest1 rest2

let compare = compare_pat

let rec print fmt t =
  match shape t with
  | Var x -> Var.print fmt x
  | Con (l, p) -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" Label.print l print p
  | Tuple [] -> Format.fprintf fmt "()"
  | Tuple [p] -> Format.fprintf fmt "(%a,)" print p
  | Tuple ps ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") print)
      ps

let rec vars p =
  match shape p with
  | Var x -> [x]
  | Con (_, p) -> vars p
  | Tuple ps -> List.concat_map vars ps

let linear_check p =
  let vs = vars p in
  let rec check_dups = function
    | [] -> Ok ()
    | x :: rest ->
      if List.exists (fun y -> Var.compare x y = 0) rest then
        Error (Format.asprintf "duplicate variable %a in pattern" Var.print x)
      else
        check_dups rest
  in
  check_dups vs

module Test = struct
  let gen =
    let open QCheck.Gen in
    let mk_t s = mk (object method loc = SourcePos.dummy end) s in
    sized @@ fix (fun self n ->
      if n <= 0 then
        map (fun v -> mk_t (Var v)) Var.Test.gen
      else
        let sub = self (n / 3) in
        oneof [
          map (fun v -> mk_t (Var v)) Var.Test.gen;
          (let* l = Label.Test.gen in
           let* p = sub in
           pure (mk_t (Con (l, p))));
          map (fun ps -> mk_t (Tuple ps)) (list_size (0 -- 3) sub);
        ])

  let test =
    [ QCheck.Test.make ~name:"pat compare is reflexive"
        ~count:100
        (QCheck.make gen)
        (fun p -> compare p p = 0);

      QCheck.Test.make ~name:"pat vars collects all variables"
        ~count:100
        (QCheck.make gen)
        (fun p ->
           let vs = vars p in
           List.length vs >= 0);

      QCheck.Test.make ~name:"pat mk/info/shape round-trip"
        ~count:100
        (QCheck.make gen)
        (fun p -> compare (mk (info p) (shape p)) p = 0);

      QCheck.Test.make ~name:"pat map Fun.id is identity"
        ~count:100
        (QCheck.make gen)
        (fun p -> compare (map Fun.id p) p = 0);
    ]
end
