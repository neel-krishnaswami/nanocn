let lookup sig_ label args =
  match Sig.lookup_ctor label sig_ with
  | None ->
    Error (Format.asprintf "unknown constructor %a" Label.print label)
  | Some (_dsort, decl) ->
    match DsortDecl.lookup_ctor label decl with
    | None ->
      Error (Format.asprintf "constructor %a not found in datasort declaration"
               Label.print label)
    | Some raw_sort ->
      match Subst.of_lists decl.DsortDecl.params args with
      | Error msg -> Error msg
      | Ok sub -> Ok (Subst.apply sub raw_sort)
