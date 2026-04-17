let () =
  let s = {|fun good : Int -> Int [pure] = { x -> x }

fun bad : BROKEN SYNTAX HERE

sort Color = { Red : () | Blue : () }

main : () [impure] = ()
|} in
  let r = ParseResilient.parse_prog_resilient s ~file:"multi_error.cn" in
  List.iteri (fun i chunk ->
    match chunk with
    | ParseResilient.Parsed _ -> Printf.printf "  decl %d: ok\n" i
    | ParseResilient.Failed e -> Printf.printf "  decl %d: FAIL (%s)\n" i (Error.to_string e)
  ) r.decls;
  Printf.printf "  main: %s\n" (match r.main with Some (Ok _) -> "ok" | Some (Error e) -> "FAIL: " ^ Error.to_string e | None -> "missing");
  Printf.printf "  total errors: %d\n" (List.length r.errors)
