let () =
  let test_file path =
    let ic = In_channel.open_text path in
    let s = In_channel.input_all ic in
    In_channel.close ic;
    let is_rcn = Filename.check_suffix path ".rcn" in
    if is_rcn then begin
      let r = ParseResilient.parse_rprog_resilient s ~file:path in
      let n_ok = List.length (List.filter (function ParseResilient.Parsed _ -> true | _ -> false) r.rdecls) in
      let n_fail = List.length (List.filter (function ParseResilient.Failed _ -> true | _ -> false) r.rdecls) in
      let main_ok = match r.rmain with Some (Ok _) -> true | _ -> false in
      Printf.printf "%s: %d ok, %d fail, main=%b, errors=%d\n"
        (Filename.basename path) n_ok n_fail main_ok (List.length r.errors)
    end else begin
      let r = ParseResilient.parse_prog_resilient s ~file:path in
      let n_ok = List.length (List.filter (function ParseResilient.Parsed _ -> true | _ -> false) r.decls) in
      let n_fail = List.length (List.filter (function ParseResilient.Failed _ -> true | _ -> false) r.decls) in
      let main_ok = match r.main with Some (Ok _) -> true | _ -> false in
      Printf.printf "%s: %d ok, %d fail, main=%b, errors=%d\n"
        (Filename.basename path) n_ok n_fail main_ok (List.length r.errors)
    end
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
