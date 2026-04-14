let help () =
  Format.printf "@[<v>\
    nanocn - a typechecker for the nano-CN language@,\
    @,\
    Usage: nanocn <command>@,\
    @,\
    Commands:@,\
    @   help                       Show this help message@,\
    @   check <file.cn>            Typecheck a program@,\
    @   check --toplevel           Start interactive toplevel@,\
    @   check-refined <file.rcn>   Typecheck a refined program@,\
    @   smt-check <file.rcn>       Emit SMT-LIB + run Z3 on a refined program@,\
    @   elaborate <file.cn>        Elaborate surface syntax and print core syntax@,\
    @   json <file.cn>             Typecheck and output core AST as JSON@]@."

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
  match ElabM.run Var.empty_supply (Parse.parse_prog input ~file:filename) with
  | Error msg ->
    Format.eprintf "@[<v>Parse error:@ %s@]@." msg;
    exit 1
  | Ok (prog, supply) ->
    match Typecheck.check_prog supply prog with
    | Ok (_sig, _cprog) ->
      Format.printf "OK@."
    | Error msg ->
      Format.eprintf "@[<v>Type error:@ %s@]@." msg;
      exit 1

let starts_with_prefix s prefix =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let print_fun_sig name arg_sort ret_sort eff =
  Format.printf "%s : %a -> %a [%a]@."
    name
    Sort.print arg_sort
    Sort.print ret_sort
    Effect.print eff

let toplevel () =
  let rec loop supply sig_ ctx =
    match LNoise.linenoise ">>> " with
    | None -> ()
    | Some line ->
      let line = String.trim line in
      if line = "" then loop supply sig_ ctx
      else begin
        LNoise.history_add line |> ignore;
        if starts_with_prefix line "fun " then
          handle_decl supply sig_ ctx line
        else if starts_with_prefix line "sort " then
          handle_decl supply sig_ ctx line
        else if starts_with_prefix line "type " then
          handle_decl supply sig_ ctx line
        else if starts_with_prefix line "let " then
          handle_let supply sig_ ctx line
        else
          handle_expr supply sig_ ctx line
      end
  and handle_decl supply sig_ ctx line =
    match ElabM.run supply (Parse.parse_decl line ~file:"<toplevel>") with
    | Error msg ->
      Format.eprintf "@[<v>Parse error:@ %s@]@." msg;
      loop supply sig_ ctx
    | Ok (d, parse_supply) ->
      match Typecheck.check_spec_decl parse_supply sig_ d with
      | Error msg ->
        Format.eprintf "@[<v>ERROR:@ %s@]@." msg;
        loop supply sig_ ctx
      | Ok (supply', sig') ->
        (match d with
         | Prog.FunDecl dd ->
           print_fun_sig dd.name dd.arg_sort dd.ret_sort dd.eff;
           loop supply' sig' ctx
         | Prog.SortDecl dd ->
           if dd.DsortDecl.params = [] then
             Format.printf "sort %a@." Dsort.print dd.name
           else
             Format.printf "sort %a(%a)@." Dsort.print dd.name
               (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                  Tvar.print) dd.DsortDecl.params;
           loop supply' sig' ctx
         | Prog.TypeDecl dd ->
           if dd.DtypeDecl.params = [] then
             Format.printf "type %a@." Dsort.print dd.name
           else
             Format.printf "type %a(%a)@." Dsort.print dd.name
               (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
                  Tvar.print) dd.DtypeDecl.params;
           loop supply' sig' ctx)
  and handle_let supply sig_ ctx line =
    match ElabM.run supply (
      let open ElabM in
      let* (x, se) = Parse.parse_let line ~file:"<toplevel>" in
      let* (_typed_e, sort) = Elaborate.synth sig_ ctx Effect.Impure se in
      return (x, sort)
    ) with
    | Error msg ->
      Format.eprintf "@[<v>ERROR:@ %s@]@." msg;
      loop supply sig_ ctx
    | Ok ((x, sort), supply') ->
      let eff = Effect.Impure in
      let ctx' = Context.extend x sort (Effect.purify eff) ctx in
      Format.printf "%a : %a [%a]@." Var.print x Sort.print sort Effect.print eff;
      loop supply' sig_ ctx'
  and handle_expr supply sig_ ctx line =
    match ElabM.run supply (
      let open ElabM in
      let* se = Parse.parse_expr line ~file:"<toplevel>" in
      Elaborate.synth sig_ ctx Effect.Impure se
    ) with
    | Error msg ->
      Format.eprintf "@[<v>ERROR:@ %s@]@." msg;
      loop supply sig_ ctx
    | Ok ((_core_e, sort), supply') ->
      Format.printf "_ : %a [impure]@." Sort.print sort;
      loop supply' sig_ ctx
  in
  loop Var.empty_supply Typecheck.initial_sig Context.empty

let elaborate_file filename =
  let input =
    let ic = In_channel.open_text filename in
    let s = In_channel.input_all ic in
    In_channel.close ic;
    s
  in
  match ElabM.run Var.empty_supply (Parse.parse_prog input ~file:filename) with
  | Error msg ->
    Format.eprintf "@[<v>Parse error:@ %s@]@." msg;
    exit 1
  | Ok (prog, supply) ->
    match Typecheck.check_prog supply prog with
    | Error msg ->
      Format.eprintf "@[<v>Type error:@ %s@]@." msg;
      exit 1
    | Ok (_sig, cprog) ->
      Format.printf "%a@." (Prog.print_core_prog CoreExpr.print) cprog

let json_file filename =
  let input =
    let ic = In_channel.open_text filename in
    let s = In_channel.input_all ic in
    In_channel.close ic;
    s
  in
  match ElabM.run Var.empty_supply (Parse.parse_prog input ~file:filename) with
  | Error msg ->
    Format.eprintf "@[<v>Parse error:@ %s@]@." msg;
    exit 1
  | Ok (prog, supply) ->
    match Typecheck.check_prog supply prog with
    | Error msg ->
      Format.eprintf "@[<v>Type error:@ %s@]@." msg;
      exit 1
    | Ok (_sig, cprog) ->
      let jb b = Json.Object [
        "loc", SourcePos.json b#loc;
        "ctx", Json.String "<ctx>";
        "sort", Sort.json (fun b' -> SourcePos.json b'#loc) b#sort;
        "eff", Effect.json b#eff;
      ] in
      let j = Prog.json_core_prog (CoreExpr.json jb) cprog in
      Format.printf "%a@." Json.print j

let check_refined_file filename =
  let input =
    let ic = In_channel.open_text filename in
    let s = In_channel.input_all ic in
    In_channel.close ic;
    s
  in
  match ElabM.run Var.empty_supply (
    let open ElabM in
    let* rprog = Parse.parse_rprog input ~file:filename in
    RCheck.check_rprog rprog
  ) with
  | Ok ((_rsig, ct), _supply) ->
    Format.printf "OK@.";
    Format.printf "@[<v>Constraint:@ %a@]@." Constraint.print ct
  | Error msg ->
    Format.eprintf "@[<v>Error:@ %s@]@." msg;
    exit 1

let smt_check_file filename =
  let input =
    let ic = In_channel.open_text filename in
    let s = In_channel.input_all ic in
    In_channel.close ic;
    s
  in
  let run =
    let open ElabM in
    let* rprog = Parse.parse_rprog input ~file:filename in
    RCheck.check_rprog rprog
  in
  match ElabM.run Var.empty_supply run with
  | Error msg ->
    Format.eprintf "@[<v>Error:@ %s@]@." msg;
    exit 1
  | Ok ((rsig, ct), _supply) ->
    match SmtEncode.encode rsig ct with
    | Error msg ->
      Format.eprintf "@[<v>SMT encode error:@ %s@]@." msg;
      exit 1
    | Ok (prelude, constraints) ->
      let smt_path =
        Filename.remove_extension filename ^ "-constraints.smt"
      in
      let oc = Out_channel.open_text smt_path in
      SmtEncode.write_file oc ~prelude ~constraints;
      Out_channel.close oc;
      Format.printf "Wrote %s@." smt_path;
      let z3 = Option.value (Sys.getenv_opt "Z3") ~default:"z3" in
      match SolverInvoke.run_z3 ~exe:z3 ~smt_path with
      | Error msg ->
        Format.eprintf "@[<v>Solver error:@ %s@]@." msg;
        exit 1
      | Ok answers ->
        List.iteri (fun i a ->
          match List.nth_opt constraints i with
          | Some { SmtConstraint.pos; _ } ->
            Format.printf "%a: %a@."
              SourcePos.print pos SolverInvoke.print_answer a
          | None ->
            Format.printf "(extra answer %d): %a@."
              i SolverInvoke.print_answer a
        ) answers

let () =
  match Sys.argv with
  | [| _; "help" |] -> help ()
  | [| _; "check"; "--toplevel" |] -> toplevel ()
  | [| _; "check"; file |] -> check_file file
  | [| _; "check-refined"; file |] -> check_refined_file file
  | [| _; "smt-check"; file |] -> smt_check_file file
  | [| _; "elaborate"; file |] -> elaborate_file file
  | [| _; "json"; file |] -> json_file file
  | _ -> usage ()
