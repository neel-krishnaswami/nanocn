let () =
  let test_file path =
    let ic = In_channel.open_text path in
    let s = In_channel.input_all ic in
    In_channel.close ic;
    let is_rcn = Filename.check_suffix path ".rcn" in
    let n_diags =
      if is_rcn then
        let r = CompileFile.compile_rfile s ~file:path in
        List.length r.diagnostics
      else
        let r = CompileFile.compile_file s ~file:path in
        List.length r.diagnostics
    in
    let status = if n_diags = 0 then "OK" else Printf.sprintf "FAIL(%d)" n_diags in
    Printf.printf "%s  %s\n" status (Filename.basename path)
  in
  let files = Sys.readdir "examples"
    |> Array.to_list
    |> List.filter (fun f ->
      (Filename.check_suffix f ".cn" || Filename.check_suffix f ".rcn")
      && not (String.contains f '#'))
    |> List.sort String.compare
    |> List.map (fun f -> "examples/" ^ f)
  in
  List.iter test_file files
