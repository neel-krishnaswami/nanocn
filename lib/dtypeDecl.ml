type t = {
  name : Dsort.t;
  params : Tvar.t list;
  ctors : (Label.t * Sort.sort) list;
  loc : SourcePos.t;
}

let lookup_ctor l d =
  let rec go = function
    | [] -> None
    | (l', s) :: rest ->
      if Label.compare l l' = 0 then Some s else go rest
  in
  go d.ctors

let ctor_labels d = List.map fst d.ctors

let compare d1 d2 =
  let c = Dsort.compare d1.name d2.name in
  if c <> 0 then c
  else
    let rec compare_tvars a b =
      match a, b with
      | [], [] -> 0
      | [], _ :: _ -> -1
      | _ :: _, [] -> 1
      | x :: xs, y :: ys ->
        let c = Tvar.compare x y in
        if c <> 0 then c else compare_tvars xs ys
    in
    let c = compare_tvars d1.params d2.params in
    if c <> 0 then c
    else
      let rec compare_ctors a b =
        match a, b with
        | [], [] -> 0
        | [], _ :: _ -> -1
        | _ :: _, [] -> 1
        | (l1, t1) :: rest1, (l2, t2) :: rest2 ->
          let c = Label.compare l1 l2 in
          if c <> 0 then c
          else
            let c = Sort.compare t1 t2 in
            if c <> 0 then c else compare_ctors rest1 rest2
      in
      compare_ctors d1.ctors d2.ctors

let json d =
  Json.Object [
    "name", Dsort.json d.name;
    "params", Json.Array (List.map Tvar.json d.params);
    "ctors", Json.Array (List.map (fun (l, s) ->
      Json.Object ["label", Label.json l; "type", Sort.json (fun b -> SourcePos.json b#loc) s]) d.ctors);
    "loc", SourcePos.json d.loc;
  ]

let print fmt d =
  let pp_param fmt tv = Tvar.print fmt tv in
  let pp_ctor fmt (l, s) =
    Format.fprintf fmt "@[<hov 2>%a :@ %a@]" Label.print l Sort.print s
  in
  match d.params with
  | [] ->
    Format.fprintf fmt "@[<v 2>type %a = {@ %a@ }@]"
      Dsort.print d.name
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ") pp_ctor)
      d.ctors
  | _ ->
    Format.fprintf fmt "@[<v 2>type %a(%a) = {@ %a@ }@]"
      Dsort.print d.name
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_param)
      d.params
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ") pp_ctor)
      d.ctors

module Test = struct
  let gen =
    let open QCheck.Gen in
    let mk_sort s = Sort.mk (object method loc = SourcePos.dummy end) s in
    let simple_sort = oneof [
      pure (mk_sort Sort.Int);
      pure (mk_sort Sort.Bool);
    ] in
    let* name = Dsort.Test.gen in
    let* n_params = 0 -- 2 in
    let* params = list_repeat n_params Tvar.Test.gen in
    let* n_ctors = 1 -- 3 in
    let* ctors = list_repeat n_ctors (
      let* l = Label.Test.gen in
      let* s = simple_sort in
      pure (l, s)
    ) in
    pure { name; params; ctors; loc = SourcePos.dummy }

  let test =
    [ QCheck.Test.make ~name:"dtypeDecl compare is reflexive"
        ~count:100
        (QCheck.make gen)
        (fun d -> compare d d = 0);

      QCheck.Test.make ~name:"dtypeDecl lookup_ctor finds ctors"
        ~count:100
        (QCheck.make gen)
        (fun d ->
           List.for_all (fun (l, s) ->
             match lookup_ctor l d with
             | Some s' -> Sort.compare s s' = 0
             | None -> false)
             d.ctors);

      QCheck.Test.make ~name:"dtypeDecl ctor_labels returns all labels"
        ~count:100
        (QCheck.make gen)
        (fun d ->
           let labels = ctor_labels d in
           List.length labels = List.length d.ctors);

    ]
end
