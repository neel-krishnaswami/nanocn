type config = { position_trace : bool }

let default_config = { position_trace = true }

let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

let encode ?(config = default_config) rsig ct =
  let ms = SmtMonadSorts.collect rsig ct in
  let prelude_config : SmtPrelude.config =
    { SmtPrelude.position_trace = config.position_trace }
  in
  let* prelude = SmtPrelude.build ~config:prelude_config rsig ms in
  let ct_config : SmtConstraint.config =
    { SmtConstraint.position_trace = config.position_trace }
  in
  let* cmds = SmtConstraint.of_ct ~config:ct_config ct in
  Ok (prelude, cmds)

(* One-line source-position comment in SMT-LIB syntax. *)
let print_pos_comment oc pos =
  Printf.fprintf oc "; %s:%d:%d\n"
    (SourcePos.file pos)
    (SourcePos.start_line pos)
    (SourcePos.start_col pos)

let write_file oc ~prelude ~constraints =
  List.iter (fun cmd ->
    Printf.fprintf oc "%s\n\n" (SmtSexp.to_string cmd)
  ) prelude;
  List.iter (fun { SmtConstraint.pos; cmd } ->
    print_pos_comment oc pos;
    Printf.fprintf oc "%s\n" (SmtSexp.to_string cmd)
  ) constraints
