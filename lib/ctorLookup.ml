let lookup sig_ label args =
  (* Try datasort declarations first *)
  match Sig.lookup_ctor label sig_ with
  | Ok (_dsort, decl) ->
    (match DsortDecl.lookup_ctor label decl with
     | None ->
       Error (Error.K_ctor_sig_inconsistent
                { label; where = "datasort" })
     | Some raw_sort ->
       match Subst.of_lists decl.DsortDecl.params args with
       | Error k -> Error k
       | Ok sub -> Ok (Subst.apply sub raw_sort))
  | Error _ ->
    (* Try datatype declarations *)
    match Sig.lookup_type_ctor label sig_ with
    | Error _ ->
      Error (Error.K_unbound_ctor label)
    | Ok (_dsort, decl) ->
      match DtypeDecl.lookup_ctor label decl with
      | None ->
        Error (Error.K_ctor_sig_inconsistent
                 { label; where = "datatype" })
      | Some raw_sort ->
        (* Substitute type parameters in the sort *)
        match Subst.of_lists decl.DtypeDecl.params args with
        | Error k -> Error k
        | Ok sub -> Ok (Subst.apply sub raw_sort)
