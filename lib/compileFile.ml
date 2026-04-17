(** Per-decl compile driver — see compileFile.mli. *)

(* ================================================================== *)
(* Surface programs (.cn)                                              *)
(* ================================================================== *)

type file_outcome = {
  final_sig   : Typecheck.typed_ce Sig.t;
  typed_decls : Typecheck.typed_ce Prog.core_decl list;
  diagnostics : Error.t list;
}

(** Per-decl accumulator threaded through the fold. *)
type decl_acc = {
  supply : Var.supply;
  sig_   : Typecheck.typed_ce Sig.t;
  decls_rev : Typecheck.typed_ce Prog.core_decl list;
  diags_rev : Error.t list;
}

let resolve_and_check_decl acc raw_decl =
  match ElabM.run acc.supply (Resolve.resolve_decl [] raw_decl) with
  | Error e ->
    { acc with diags_rev = e :: acc.diags_rev }
  | Ok ((resolved, _env), supply) ->
    (* Header: extend sig before checking body *)
    match Typecheck.extend_sig_with_header acc.sig_ resolved with
    | Error e ->
      { acc with supply; diags_rev = e :: acc.diags_rev }
    | Ok sig1 ->
      (* Body *)
      match Typecheck.check_decl supply sig1 resolved with
      | Error e ->
        (* sig1 keeps the header extension *)
        { acc with supply; sig_ = sig1; diags_rev = e :: acc.diags_rev }
      | Ok (supply', core_decl) ->
        { supply = supply'; sig_ = sig1;
          decls_rev = core_decl :: acc.decls_rev;
          diags_rev = acc.diags_rev }

let compile_file source ~file =
  let parsed = ParseResilient.parse_prog_resilient source ~file in
  let init = {
    supply = Var.empty_supply;
    sig_ = Typecheck.initial_sig;
    decls_rev = [];
    diags_rev = parsed.errors;
  } in
  let acc = List.fold_left (fun acc chunk ->
    match chunk with
    | ParseResilient.Failed _ -> acc
    | ParseResilient.Parsed raw_decl -> resolve_and_check_decl acc raw_decl
  ) init parsed.decls in
  (* Main *)
  let diags_rev = match parsed.main with
    | Some (Ok prog) ->
      begin match ElabM.run acc.supply (
        let open ElabM in
        let* resolved = Resolve.resolve_prog [] prog in
        Elaborate.check acc.sig_ Context.empty resolved.Prog.main
          resolved.Prog.main_sort resolved.Prog.main_eff
      ) with
      | Error e -> e :: acc.diags_rev
      | Ok _ -> acc.diags_rev
      end
    | Some (Error _) | None -> acc.diags_rev
  in
  { final_sig = acc.sig_;
    typed_decls = List.rev acc.decls_rev;
    diagnostics = List.rev diags_rev }

(* ================================================================== *)
(* Refined programs (.rcn)                                             *)
(* ================================================================== *)

type rfile_outcome = {
  final_rsig  : RSig.t;
  constraints : Constraint.typed_ct;
  diagnostics : Error.t list;
  hover       : HoverIndex.t;
}

(** Build a hover index from RSig's FunDef entries (pure/spec functions
    retain their typed body in the signature after elaboration). *)
let hover_of_rsig rsig =
  List.fold_left (fun idx entry ->
    match entry with
    | RSig.LFun (_, RSig.FunDef { body; _ }) ->
      HoverIndex.add_typed_expr body idx
    | _ -> idx
  ) HoverIndex.empty (RSig.entries rsig)

let empty_rfile_outcome diags =
  { final_rsig = RSig.empty;
    constraints = Constraint.top SourcePos.dummy;
    diagnostics = diags;
    hover = HoverIndex.empty }

let compile_rfile source ~file =
  let parsed = ParseResilient.parse_rprog_resilient source ~file in
  match parsed.errors with
  | _ :: _ ->
    empty_rfile_outcome parsed.errors
  | [] ->
    let decls = List.filter_map (function
      | ParseResilient.Parsed d -> Some d
      | ParseResilient.Failed _ -> None
    ) parsed.rdecls in
    match parsed.rmain with
    | None ->
      empty_rfile_outcome
        [Error.parse_error ~loc:None ~msg:"missing `main` declaration"]
    | Some (Error e) ->
      empty_rfile_outcome [e]
    | Some (Ok main_prog) ->
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
        empty_rfile_outcome [e]
      | Ok ((rsig, ct), _supply) ->
        { final_rsig = rsig;
          constraints = ct;
          diagnostics = [];
          hover = hover_of_rsig rsig }

module Test = struct
  let test = []
end
