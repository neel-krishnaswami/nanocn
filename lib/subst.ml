type t = (Tvar.t * Sort.sort) list

let empty = []

let extend a s sub = (a, s) :: sub

let rec lookup a = function
  | [] -> None
  | (a', s) :: rest ->
    if Tvar.compare a a' = 0 then Some s else lookup a rest

let rec apply sub (Sort.In (sf, b)) =
  match sf with
  | Sort.TVar a ->
    (match lookup a sub with
     | Some s -> s
     | None -> Sort.In (sf, b))
  | _ -> Sort.In (Sort.map (apply sub) sf, b)

let of_lists tvars sorts =
  if List.length tvars <> List.length sorts then
    Error "substitution: mismatched lengths"
  else
    Ok (List.combine tvars sorts)

let compare s1 s2 =
  let rec go a b =
    match a, b with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | (tv1, s1) :: rest1, (tv2, s2) :: rest2 ->
      let c = Tvar.compare tv1 tv2 in
      if c <> 0 then c
      else
        let c = Sort.compare s1 s2 in
        if c <> 0 then c else go rest1 rest2
  in
  go s1 s2

let print fmt sub =
  let pp_entry fmt (tv, s) =
    Format.fprintf fmt "@[<hov 2>%a@ :=@ %a@]" Tvar.print tv Sort.print s
  in
  Format.fprintf fmt "@[<hov 2>[%a]@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_entry)
    sub

module Test = struct
  let gen =
    let open QCheck.Gen in
    let mk s = Sort.In (s, object method loc = SourcePos.dummy end) in
    let simple_sort = oneof [
      pure (mk Sort.Int);
      pure (mk Sort.Bool);
      pure (mk Sort.Loc);
    ] in
    let* n = 0 -- 3 in
    let* entries = list_repeat n (
      let* tv = Tvar.Test.gen in
      let* s = simple_sort in
      pure (tv, s)
    ) in
    pure entries

  let test =
    let mk s = Sort.In (s, object method loc = SourcePos.dummy end) in
    [ QCheck.Test.make ~name:"subst compare is reflexive"
        ~count:100
        (QCheck.make gen)
        (fun s -> compare s s = 0);

      QCheck.Test.make ~name:"subst empty is identity"
        ~count:100
        (QCheck.make Sort.Test.gen)
        (fun s -> Sort.compare (apply empty s) s = 0);

      QCheck.Test.make ~name:"subst apply replaces tvar"
        ~count:100
        (QCheck.make Tvar.Test.gen)
        (fun a ->
           let target = mk Sort.Int in
           let sub = extend a target empty in
           let input = mk (Sort.TVar a) in
           Sort.compare (apply sub input) target = 0);

      QCheck.Test.make ~name:"subst of_lists succeeds on equal lengths"
        ~count:100
        (QCheck.make gen)
        (fun entries ->
           let tvars = List.map fst entries in
           let sorts = List.map snd entries in
           match of_lists tvars sorts with
           | Ok _ -> true
           | Error _ -> false);
    ]
end
