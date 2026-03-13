let help () =
  Format.printf "@[<v>\
    nanocn - a typechecker for the nano-CN language@,\
    @,\
    Usage: nanocn <command>@,\
    @,\
    Commands:@,\
    @   help                   Show this help message@,\
    @   check <file.cn>        Typecheck a program@,\
    @   check --toplevel       Start interactive toplevel@,\
    @   elaborate <file.cn>    Elaborate surface syntax and print core syntax@]@."

let usage () =
  Format.eprintf "Usage: nanocn <command> (try 'nanocn help')@.";
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

let print_fun_sig name arg_sort ret_sort eff =
  Format.printf "%a : %a -> %a [%a]@."
    Var.print name
    Sort.print arg_sort
    Sort.print ret_sort
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
          handle_decl sig_ ctx line
        else if starts_with_prefix line "type " then
          handle_decl sig_ ctx line
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
      match Typecheck.check_spec_decl sig_ d with
      | Error msg ->
        Format.eprintf "@[<v>ERROR:@ %s@]@." msg;
        loop sig_ ctx
      | Ok sig' ->
        (match d with
         | Prog.FunDecl dd ->
           print_fun_sig dd.name dd.arg_sort dd.ret_sort dd.eff;
           loop sig' ctx
         | Prog.SortDecl dd ->
           if dd.DsortDecl.params = [] then
             Format.printf "sort %a@." Dsort.print dd.name
           else
             Format.printf "sort %a(%a)@." Dsort.print dd.name
               (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                  Tvar.print) dd.DsortDecl.params;
           loop sig' ctx
         | Prog.TypeDecl dd ->
           if dd.DtypeDecl.params = [] then
             Format.printf "type %a@." Dsort.print dd.name
           else
             Format.printf "type %a(%a)@." Dsort.print dd.name
               (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                  Tvar.print) dd.DtypeDecl.params;
           loop sig' ctx)
  and handle_let sig_ ctx line =
    match Parse.parse_let line ~file:"<toplevel>" with
    | Error msg ->
      Format.eprintf "@[<v>Parse error:@ %s@]@." msg;
      loop sig_ ctx
    | Ok (x, se) ->
      let result = ElabM.run (
        Elaborate.synth sig_ ctx Effect.Impure se
      ) in
      (match result with
       | Error msg ->
         Format.eprintf "@[<v>ERROR:@ %s@]@." msg;
         loop sig_ ctx
       | Ok (core_e, sort, eff) ->
         match Typecheck.synth sig_ ctx Effect.Impure core_e with
         | Error msg ->
           Format.eprintf "@[<v>ERROR:@ %s@]@." msg;
           loop sig_ ctx
         | Ok _te ->
           let ctx' = Context.extend x sort (Effect.purify eff) ctx in
           Format.printf "%a : %a [%a]@." Var.print x Sort.print sort Effect.print eff;
           loop sig_ ctx')
  and handle_expr sig_ ctx line =
    match Parse.parse_expr line ~file:"<toplevel>" with
    | Error msg ->
      Format.eprintf "@[<v>Parse error:@ %s@]@." msg;
      loop sig_ ctx
    | Ok se ->
      let result = ElabM.run (
        Elaborate.synth sig_ ctx Effect.Impure se
      ) in
      (match result with
       | Error msg ->
         Format.eprintf "@[<v>ERROR:@ %s@]@." msg;
         loop sig_ ctx
       | Ok (_core_e, sort, eff) ->
         Format.printf "_ : %a [%a]@." Sort.print sort Effect.print eff;
         loop sig_ ctx)
  in
  loop Typecheck.initial_sig Context.empty

let elaborate_file filename =
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
    | Error msg ->
      Format.eprintf "@[<v>Type error:@ %s@]@." msg;
      exit 1
    | Ok cprog ->
      Format.printf "%a@." (Prog.print_core_prog CoreExpr.print) cprog

let () =
  match Sys.argv with
  | [| _; "help" |] -> help ()
  | [| _; "check"; "--toplevel" |] -> toplevel ()
  | [| _; "check"; file |] -> check_file file
  | [| _; "elaborate"; file |] -> elaborate_file file
  | _ -> usage ()
