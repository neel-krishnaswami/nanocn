(** Per-decl compile driver — see compileFile.mli. *)

(* ================================================================== *)
(* Surface programs (.cn)                                              *)
(* ================================================================== *)

type file_outcome = {
  final_sig   : Typecheck.typed_ce Sig.t;
  typed_decls : Typecheck.typed_ce Prog.core_decl list;
  diagnostics : Error.t list;
}

let compile_file source ~file =
  let parsed = ParseResilient.parse_prog_resilient source ~file in
  let supply = ref Var.empty_supply in
  let sig_ = ref Typecheck.initial_sig in
  let diags = ref parsed.errors in
  let typed = ref [] in
  let resolve_and_check_decl raw_decl =
    match ElabM.run !supply (Resolve.resolve_decl [] raw_decl) with
    | Error e ->
      diags := e :: !diags
    | Ok ((resolved, _env), supply') ->
      supply := supply';
      (* Header: extend sig before checking body *)
      match Typecheck.extend_sig_with_header !sig_ resolved with
      | Error e ->
        diags := e :: !diags
      | Ok sig1 ->
        sig_ := sig1;
        (* Body *)
        match Typecheck.check_decl !supply !sig_ resolved with
        | Error e ->
          diags := e :: !diags
          (* sig_ keeps the header extension *)
        | Ok (supply', core_decl) ->
          supply := supply';
          typed := core_decl :: !typed
  in
  List.iter (fun chunk ->
    match chunk with
    | ParseResilient.Failed _ -> ()
    | ParseResilient.Parsed raw_decl -> resolve_and_check_decl raw_decl
  ) parsed.decls;
  (* Main *)
  (match parsed.main with
   | Some (Ok prog) ->
     (match ElabM.run !supply (
       let open ElabM in
       let* resolved = Resolve.resolve_prog [] prog in
       Elaborate.check !sig_ Context.empty resolved.Prog.main
         resolved.Prog.main_sort resolved.Prog.main_eff
     ) with
     | Error e -> diags := e :: !diags
     | Ok (_main_typed, _supply') -> ())
   | Some (Error _) -> ()
   | None -> ());
  { final_sig = !sig_;
    typed_decls = List.rev !typed;
    diagnostics = List.rev !diags }

(* ================================================================== *)
(* Refined programs (.rcn)                                             *)
(* ================================================================== *)

type rfile_outcome = {
  final_rsig  : RSig.t;
  constraints : Constraint.typed_ct;
  diagnostics : Error.t list;
}

let compile_rfile source ~file =
  let parsed = ParseResilient.parse_rprog_resilient source ~file in
  (* For refined, resilient parsing gives us multi-error parse
     reporting.  Typechecking is still monolithic: if all chunks
     parsed, reconstruct a whole RProg.raw_parsed and run
     check_rprog.  If some chunks failed, report parse errors and
     skip typechecking. *)
  let parse_errors = parsed.errors in
  if parse_errors <> [] then
    (* Some chunks failed to parse — report all parse errors,
       skip typecheck. *)
    { final_rsig = RSig.empty;
      constraints = Constraint.top SourcePos.dummy;
      diagnostics = parse_errors }
  else
    (* All chunks parsed.  Reconstruct the full program. *)
    let decls = List.filter_map (function
      | ParseResilient.Parsed d -> Some d
      | ParseResilient.Failed _ -> None
    ) parsed.rdecls in
    match parsed.rmain with
    | None ->
      { final_rsig = RSig.empty;
        constraints = Constraint.top SourcePos.dummy;
        diagnostics = [Error.parse_error ~loc:None ~msg:"missing `main` declaration"] }
    | Some (Error e) ->
      { final_rsig = RSig.empty;
        constraints = Constraint.top SourcePos.dummy;
        diagnostics = [e] }
    | Some (Ok main_prog) ->
      (* Rebuild a complete raw_parsed with the split-out decls +
         the main from rprog_eof (which has empty decls). *)
      let full_prog : RProg.raw_parsed = {
        decls;
        main_pf = main_prog.RProg.main_pf;
        main_eff = main_prog.RProg.main_eff;
        main_body = main_prog.RProg.main_body;
        loc = main_prog.RProg.loc;
      } in
      match ElabM.run Var.empty_supply (
        let open ElabM in
        let* resolved = Resolve.resolve_rprog [] full_prog in
        RCheck.check_rprog resolved
      ) with
      | Error e ->
        { final_rsig = RSig.empty;
          constraints = Constraint.top SourcePos.dummy;
          diagnostics = [e] }
      | Ok ((rsig, ct), _supply) ->
        { final_rsig = rsig;
          constraints = ct;
          diagnostics = [] }

module Test = struct
  let test = []
end
