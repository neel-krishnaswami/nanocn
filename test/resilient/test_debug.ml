let () =
  let path = Sys.argv.(1) in
  let ic = In_channel.open_text path in
  let s = In_channel.input_all ic in
  In_channel.close ic;
  let source_registry = SourceExcerpt.create () in
  SourceExcerpt.register source_registry ~file:path ~source:s;
  let is_rcn = Filename.check_suffix path ".rcn" in
  let diags =
    if is_rcn then (CompileFile.compile_rfile s ~file:path).diagnostics
    else (CompileFile.compile_file s ~file:path).diagnostics
  in
  List.iter (fun e ->
    Format.eprintf "%a@." (Error.print source_registry) e
  ) diags;
  Printf.eprintf "Total: %d errors\n" (List.length diags)
