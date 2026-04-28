let get_cvar ~construct cp =
  match RPat.cpat_shape cp with
  | RPat.CVar x -> Ok x
  | _ ->
    Error (Error.K_wrong_pred_shape
             { construct
             ; expected_shape = "x"
             ; got = Format.asprintf "%a" (RPat.print_cpat Var.print) cp })
