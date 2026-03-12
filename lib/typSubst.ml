type t = (Tvar.t * Typ.ty) list

let empty = []

let extend a ty sub = (a, ty) :: sub

let rec lookup a = function
  | [] -> None
  | (a', ty) :: rest ->
    if Tvar.compare a a' = 0 then Some ty else lookup a rest

let rec apply sub (Typ.In (tf, b)) =
  match tf with
  | Typ.TVar a ->
    (match lookup a sub with
     | Some ty -> ty
     | None -> Typ.In (tf, b))
  | _ -> Typ.In (Typ.map (apply sub) tf, b)

let of_lists tvars tys =
  if List.length tvars <> List.length tys then
    Error "type substitution: mismatched lengths"
  else
    Ok (List.combine tvars tys)

let compare s1 s2 =
  let rec go a b =
    match a, b with
    | [], [] -> 0
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
    | (tv1, t1) :: rest1, (tv2, t2) :: rest2 ->
      let c = Tvar.compare tv1 tv2 in
      if c <> 0 then c
      else
        let c = Typ.compare t1 t2 in
        if c <> 0 then c else go rest1 rest2
  in
  go s1 s2

let print fmt sub =
  let pp_entry fmt (tv, ty) =
    Format.fprintf fmt "@[<hov 2>%a@ :=@ %a@]" Tvar.print tv Typ.print ty
  in
  Format.fprintf fmt "@[<hov 2>[%a]@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_entry)
    sub

module Test = struct
  let gen =
    let open QCheck.Gen in
    let mk t = Typ.In (t, object method loc = SourcePos.dummy end) in
    let simple_ty = oneof [
      pure (mk Typ.Int);
      pure (mk Typ.Bool);
    ] in
    let* n = 0 -- 3 in
    let* entries = list_repeat n (
      let* tv = Tvar.Test.gen in
      let* ty = simple_ty in
      pure (tv, ty)
    ) in
    pure entries

  let test =
    let mk t = Typ.In (t, object method loc = SourcePos.dummy end) in
    [ QCheck.Test.make ~name:"typSubst compare is reflexive"
        ~count:100
        (QCheck.make gen)
        (fun s -> compare s s = 0);

      QCheck.Test.make ~name:"typSubst empty is identity"
        ~count:100
        (QCheck.make Typ.Test.gen)
        (fun ty -> Typ.compare (apply empty ty) ty = 0);

      QCheck.Test.make ~name:"typSubst apply replaces tvar"
        ~count:100
        (QCheck.make Tvar.Test.gen)
        (fun a ->
           let target = mk Typ.Int in
           let sub = extend a target empty in
           let input = mk (Typ.TVar a) in
           Typ.compare (apply sub input) target = 0);

      QCheck.Test.make ~name:"typSubst of_lists succeeds on equal lengths"
        ~count:100
        (QCheck.make gen)
        (fun entries ->
           let tvars = List.map fst entries in
           let tys = List.map snd entries in
           match of_lists tvars tys with
           | Ok _ -> true
           | Error _ -> false);
    ]
end
