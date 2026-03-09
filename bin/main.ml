let () =
  let input = In_channel.input_all In_channel.stdin in
  match Parse.parse_expr input ~file:"<stdin>" with
  | Error msg ->
    Format.eprintf "@[<v>Parse error:@ %s@]@." msg;
    exit 1
  | Ok expr ->
    Format.printf "@[<v>Parsed:@ %a@]@.@." Expr.print expr;
    match Typecheck.synth Context.empty expr with
    | Ok te ->
      let info = Expr.extract te in
      Format.printf "@[<v>Type: %a@ Effect: %a@]@." Typ.print info#typ Effect.print info#eff
    | Error msg ->
      Format.eprintf "@[<v>Type error:@ %s@]@." msg;
      exit 1
