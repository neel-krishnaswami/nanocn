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
    (* Try datatype declarations *)
    match Sig.lookup_type_ctor label sig_ with
    | None ->
      Error (Format.asprintf "unknown constructor %a" Label.print label)
    | Some (_dsort, decl) ->
      match DtypeDecl.lookup_ctor label decl with
      | None ->
        Error (Format.asprintf "constructor %a not found in datatype declaration"
                 Label.print label)
      | Some raw_sort ->
        (* Substitute type parameters in the sort *)
        match Subst.of_lists decl.DtypeDecl.params args with
        | Error msg -> Error msg
        | Ok sub -> Ok (Subst.apply sub raw_sort)
