let lookup sig_ label args =
  match Sig.lookup_type_ctor label sig_ with
  | None ->
    Error (Format.asprintf "unknown type constructor %a" Label.print label)
  | Some (_dsort, decl) ->
    match DtypeDecl.lookup_ctor label decl with
    | None ->
      Error (Format.asprintf "constructor %a not found in datatype declaration"
               Label.print label)
    | Some raw_ty ->
      match TypSubst.of_lists decl.DtypeDecl.params args with
      | Error msg -> Error msg
      | Ok sub -> Ok (TypSubst.apply sub raw_ty)
