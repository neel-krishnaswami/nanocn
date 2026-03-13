let lookup sig_ label args =
  (* Try datasort declarations first *)
  match Sig.lookup_ctor label sig_ with
  | Some (_dsort, decl) ->
    (match DsortDecl.lookup_ctor label decl with
     | None ->
       Error (Format.asprintf "constructor %a not found in datasort declaration"
                Label.print label)
     | Some raw_sort ->
       match Subst.of_lists decl.DsortDecl.params args with
       | Error msg -> Error msg
       | Ok sub -> Ok (Subst.apply sub raw_sort))
  | None ->
    (* Try datatype declarations, converting sorts <-> types *)
    match Sig.lookup_type_ctor label sig_ with
    | None ->
      Error (Format.asprintf "unknown constructor %a" Label.print label)
    | Some (_dsort, decl) ->
      match DtypeDecl.lookup_ctor label decl with
      | None ->
        Error (Format.asprintf "constructor %a not found in datatype declaration"
                 Label.print label)
      | Some raw_ty ->
        (* Convert sort args to type args for substitution *)
        let rec convert_args = function
          | [] -> Ok []
          | s :: rest ->
            match Sort.sort_to_typ s with
            | Error _ as e -> e
            | Ok t ->
              match convert_args rest with
              | Error _ as e -> e
              | Ok rest' -> Ok (t :: rest')
        in
        match convert_args args with
        | Error msg -> Error msg
        | Ok ty_args ->
          match TypSubst.of_lists decl.DtypeDecl.params ty_args with
          | Error msg -> Error msg
          | Ok sub ->
            let result_ty = TypSubst.apply sub raw_ty in
            Ok (Sort.typ_to_sort result_ty)
