type t = {
  name : Dsort.t;
  params : Tvar.t list;
  ctors : (Label.t * Typ.ty) list;
  loc : SourcePos.t;
}

let lookup_ctor l d =
  let rec go = function
    | [] -> None
    | (l', ty) :: rest ->
      if Label.compare l l' = 0 then Some ty else go rest
  in
  go d.ctors

let ctor_labels d = List.map fst d.ctors

let resolve_tvars d =
  let param_names =
    List.map Tvar.to_string d.params
  in
  let rec resolve_ty t =
    let b = Typ.info t in
    let tf = Typ.shape t in
    match tf with
    | Typ.App (dsname, []) ->
      let name_str = Dsort.to_string dsname in
      if List.exists (fun p -> String.compare p name_str = 0) param_names then
        Typ.mk b (Typ.TVar (Tvar.of_string name_str))
      else
        t
    | _ -> Typ.mk b (Typ.map_shape resolve_ty tf)
  in
  let ctors = List.map (fun (l, ty) -> (l, resolve_ty ty)) d.ctors in
  { d with ctors }

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
            let c = Typ.compare t1 t2 in
            if c <> 0 then c else compare_ctors rest1 rest2
      in
      compare_ctors d1.ctors d2.ctors

let print fmt d =
  let pp_param fmt tv = Tvar.print fmt tv in
  let pp_ctor fmt (l, ty) =
    Format.fprintf fmt "@[<hov 2>%a :@ %a@]" Label.print l Typ.print ty
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
    let mk_ty s = Typ.mk (object method loc = SourcePos.dummy end) s in
    let simple_ty = oneof [
      pure (mk_ty Typ.Int);
      pure (mk_ty Typ.Bool);
    ] in
    let* name = Dsort.Test.gen in
    let* n_params = 0 -- 2 in
    let* params = list_repeat n_params Tvar.Test.gen in
    let* n_ctors = 1 -- 3 in
    let* ctors = list_repeat n_ctors (
      let* l = Label.Test.gen in
      let* ty = simple_ty in
      pure (l, ty)
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
           List.for_all (fun (l, ty) ->
             match lookup_ctor l d with
             | Some ty' -> Typ.compare ty ty' = 0
             | None -> false)
             d.ctors);

      QCheck.Test.make ~name:"dtypeDecl ctor_labels returns all labels"
        ~count:100
        (QCheck.make gen)
        (fun d ->
           let labels = ctor_labels d in
           List.length labels = List.length d.ctors);

      QCheck.Test.make ~name:"dtypeDecl resolve_tvars replaces matching params"
        ~count:100
        (QCheck.make gen)
        (fun d ->
           let d' = resolve_tvars d in
           Dsort.compare d.name d'.name = 0
           && List.length d.ctors = List.length d'.ctors);
    ]
end
