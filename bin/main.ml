let usage () =
  Format.eprintf "@[<v>Usage:@   nanocn check <file.cn>       Typecheck a program@   nanocn check --toplevel     Start interactive toplevel@]@.";
  exit 1

let check_file filename =
  let input =
    let ic = In_channel.open_text filename in
    let s = In_channel.input_all ic in
    In_channel.close ic;
    s
  in
  match Parse.parse_prog input ~file:filename with
  | Error msg ->
    Format.eprintf "@[<v>Parse error:@ %s@]@." msg;
    exit 1
  | Ok prog ->
    match Typecheck.check_prog prog with
    | Ok _ ->
      Format.printf "OK@."
    | Error msg ->
      Format.eprintf "@[<v>Type error:@ %s@]@." msg;
      exit 1

let starts_with_prefix s prefix =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let print_fun_sig name arg_ty ret_ty eff =
  Format.printf "%a : %a -> %a [%a]@."
    Var.print name
    Typ.print arg_ty
    Typ.print ret_ty
    Effect.print eff

let toplevel () =
  let rec loop sig_ ctx =
    match LNoise.linenoise ">>> " with
    | None -> ()
    | Some line ->
      let line = String.trim line in
      if line = "" then loop sig_ ctx
      else begin
        LNoise.history_add line |> ignore;
        if starts_with_prefix line "fun " then
          handle_decl sig_ ctx line
        else if starts_with_prefix line "sort " then
          handle_spec_decl sig_ ctx line
        else if starts_with_prefix line "spec " then
          handle_spec_decl sig_ ctx line
        else if starts_with_prefix line "let " then
          handle_let sig_ ctx line
        else
          handle_expr sig_ ctx line
      end
  and handle_decl sig_ ctx line =
    match Parse.parse_decl line ~file:"<toplevel>" with
    | Error msg ->
      Format.eprintf "@[<v>Parse error:@ %s@]@." msg;
      loop sig_ ctx
    | Ok d ->
      match Typecheck.check_decl sig_ d with
      | Error msg ->
        Format.eprintf "@[<v>ERROR:@ %s@]@." msg;
        loop sig_ ctx
      | Ok _ ->
        match d with
        | Prog.FunDecl fd ->
          let entry = Sig.FunSig { arg = fd.arg_ty; ret = fd.ret_ty; eff = fd.eff } in
          let sig' = Sig.extend fd.name entry sig_ in
          print_fun_sig fd.name fd.arg_ty fd.ret_ty fd.eff;
          loop sig' ctx
        | _ ->
          Format.eprintf "@[<v>ERROR:@ unexpected declaration form@]@.";
          loop sig_ ctx
  and handle_spec_decl sig_ ctx line =
    match Parse.parse_decl line ~file:"<toplevel>" with
    | Error msg ->
      Format.eprintf "@[<v>Parse error:@ %s@]@." msg;
      loop sig_ ctx
    | Ok d ->
      match Typecheck.check_spec_decl sig_ d with
      | Error msg ->
        Format.eprintf "@[<v>ERROR:@ %s@]@." msg;
        loop sig_ ctx
      | Ok sig' ->
        (match d with
         | Prog.SortDecl dd ->
           if dd.DsortDecl.params = [] then
             Format.printf "sort %a@." Dsort.print dd.name
           else
             Format.printf "sort %a(%a)@." Dsort.print dd.name
               (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                  Tvar.print) dd.DsortDecl.params;
           loop sig' ctx
         | Prog.SpecFunDecl dd ->
           Format.printf "spec %a : %a -> %a@."
             Var.print dd.name Sort.print dd.arg_sort Sort.print dd.ret_sort;
           loop sig' ctx
         | Prog.SpecDefDecl dd ->
           Format.printf "spec %a : %a@."
             Var.print dd.name Sort.print dd.sort;
           loop sig' ctx
         | Prog.FunDecl fd ->
           print_fun_sig fd.name fd.arg_ty fd.ret_ty fd.eff;
           loop sig' ctx)
  and handle_let sig_ ctx line =
    match Parse.parse_let line ~file:"<toplevel>" with
    | Error msg ->
      Format.eprintf "@[<v>Parse error:@ %s@]@." msg;
      loop sig_ ctx
    | Ok (x, e) ->
      match Typecheck.synth sig_ ctx e with
      | Error msg ->
        Format.eprintf "@[<v>ERROR:@ %s@]@." msg;
        loop sig_ ctx
      | Ok te ->
        let ty = (Expr.extract te)#typ in
        let ctx' = Context.extend_comp x ty ctx in
        let eff = (Expr.extract te)#eff in
        Format.printf "%a : %a [%a]@." Var.print x Typ.print ty Effect.print eff;
        loop sig_ ctx'
  and handle_expr sig_ ctx line =
    match Parse.parse_expr line ~file:"<toplevel>" with
    | Error msg ->
      Format.eprintf "@[<v>Parse error:@ %s@]@." msg;
      loop sig_ ctx
    | Ok e ->
      match Typecheck.synth sig_ ctx e with
      | Error msg ->
        Format.eprintf "@[<v>ERROR:@ %s@]@." msg;
        loop sig_ ctx
      | Ok te ->
        let ty = (Expr.extract te)#typ in
        let eff = (Expr.extract te)#eff in
        Format.printf "_ : %a [%a]@." Typ.print ty Effect.print eff;
        loop sig_ ctx
  in
  loop Typecheck.initial_sig Context.empty

let () =
  match Sys.argv with
  | [| _; "check"; "--toplevel" |] -> toplevel ()
  | [| _; "check"; file |] -> check_file file
  | _ -> usage ()
