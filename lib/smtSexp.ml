type ('a, 'b) sexpF =
  | Atom of SmtAtom.t
  | List of 'a list

let map_shape f = function
  | Atom a -> Atom a
  | List xs -> List (List.map f xs)

let map_info _f = function
  | Atom a -> Atom a
  | List xs -> List xs

type 'b t = In of 'b * ('b t, 'b) sexpF

let mk b sf = In (b, sf)
let info (In (b, _)) = b
let shape (In (_, sf)) = sf

let rec map f (In (b, sf)) =
  In (f b, map_info f (map_shape (map f) sf))

type sexp = < loc : SourcePos.t > t

(* Smart constructors *)

let atom b a = mk b (Atom a)
let symbol b s = atom b (SmtAtom.Symbol s)
let keyword b s = atom b (SmtAtom.Keyword s)
let numeral_s b s = atom b (SmtAtom.Numeral s)
let string_lit b s = atom b (SmtAtom.String s)
let reserved b r = atom b (SmtAtom.Reserved r)
let list b xs = mk b (List xs)
let app b head args = list b (head :: args)

(* Views *)

let as_atom t = match shape t with Atom a -> Some a | List _ -> None
let as_list t = match shape t with List xs -> Some xs | Atom _ -> None

let as_symbol t = match shape t with
  | Atom (SmtAtom.Symbol s) -> Some s
  | _ -> None

let as_keyword t = match shape t with
  | Atom (SmtAtom.Keyword s) -> Some s
  | _ -> None

(* Compare: lexicographic on (info, shape). Info compare is supplied by
   the caller so the same [compare] works for any annotation type. *)
let rec compare cmp_info (In (b1, s1)) (In (b2, s2)) =
  let c = cmp_info b1 b2 in
  if c <> 0 then c
  else compare_shape cmp_info s1 s2

and compare_shape cmp_info s1 s2 =
  match s1, s2 with
  | Atom a, Atom b -> SmtAtom.compare a b
  | List xs, List ys -> compare_list cmp_info xs ys
  | Atom _, List _ -> -1
  | List _, Atom _ -> 1

and compare_list cmp_info xs ys =
  match xs, ys with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | x :: xs', y :: ys' ->
    let c = compare cmp_info x y in
    if c <> 0 then c
    else compare_list cmp_info xs' ys'

(* Print. Proper-list spine walks via [shape] so the info annotation at
   each node is ignored — the concrete syntax has no place to render
   it. A list is printed as ( e1 e2 ... en ) with elements separated
   by single spaces; nested lists are recursively printed. *)
let rec print fmt t =
  match shape t with
  | Atom a -> SmtAtom.print fmt a
  | List xs ->
    Format.pp_print_char fmt '(';
    List.iteri (fun i x ->
      if i > 0 then Format.pp_print_char fmt ' ';
      print fmt x
    ) xs;
    Format.pp_print_char fmt ')'

let to_string t = Format.asprintf "%a" print t

let rec json jb t =
  let node_info = ["info", jb (info t)] in
  let shape_fields = match shape t with
    | Atom a ->
      ["tag", Json.String "Atom"; "atom", Json.String (SmtAtom.to_string a)]
    | List xs ->
      ["tag", Json.String "List";
       "elems", Json.Array (List.map (json jb) xs)]
  in
  Json.Object (shape_fields @ node_info)

module Test = struct
  open QCheck.Gen

  let dummy_info () = object method loc = SourcePos.dummy end

  let gen =
    let mk_t s = mk (dummy_info ()) s in
    sized @@ fix (fun self n ->
      if n <= 0 then
        map (fun a -> mk_t (Atom a)) SmtAtom.Test.gen
      else
        let sub = self (n / 3) in
        oneof [
          map (fun a -> mk_t (Atom a)) SmtAtom.Test.gen;
          map (fun xs -> mk_t (List xs)) (list_size (0 -- 4) sub);
        ])

  (* Compare that ignores info (a function-typed info would otherwise
     be incomparable). Useful for structural roundtrip tests later. *)
  let compare_ignore_info a b = compare (fun _ _ -> 0) a b

  let test = [
    QCheck.Test.make ~name:"smtSexp compare_ignore_info is reflexive"
      ~count:100
      (QCheck.make gen)
      (fun s -> compare_ignore_info s s = 0);

    QCheck.Test.make ~name:"smtSexp compare_ignore_info is antisymmetric"
      ~count:100
      (QCheck.make (pair gen gen))
      (fun (a, b) ->
         let x = compare_ignore_info a b in
         let y = compare_ignore_info b a in
         (x = 0 && y = 0) || (x > 0 && y < 0) || (x < 0 && y > 0));

    QCheck.Test.make ~name:"smtSexp print never raises"
      ~count:200
      (QCheck.make gen)
      (fun s -> let _ = to_string s in true);

    QCheck.Test.make ~name:"smtSexp as_atom matches Atom constructor"
      ~count:100
      (QCheck.make gen)
      (fun s ->
         match shape s, as_atom s with
         | Atom _, Some _ -> true
         | List _, None -> true
         | _ -> false);

    QCheck.Test.make ~name:"smtSexp as_list matches List constructor"
      ~count:100
      (QCheck.make gen)
      (fun s ->
         match shape s, as_list s with
         | List _, Some _ -> true
         | Atom _, None -> true
         | _ -> false);

    QCheck.Test.make ~name:"smtSexp print of list produces non-empty output"
      ~count:100
      (QCheck.make gen)
      (fun s ->
         let str = to_string s in
         String.length str > 0);
  ]
end
