(** Per-decl compile driver — see compileFile.mli. *)

(** [invariant_to_error info] turns a caught [Util.Invariant_failure]
    into an [Error.t] carrying [K_internal_invariant] so the rest of
    the diagnostic pipeline (CLI, LSP) can render it exactly like any
    other failure — except that its header reads "Internal error"
    rather than "Type error", visibly distinguishing compiler bugs
    from user bugs. *)
let invariant_to_error (info : Util.invariant_failure_info) : Error.t =
  Error.internal_invariant
    ~loc:info.Util.loc
    ~rule:info.Util.rule
    ~invariant:info.Util.invariant

(* ================================================================== *)
(* Surface programs (.cn)                                              *)
(* ================================================================== *)

type file_outcome = {
  final_sig   : Typecheck.typed_ce Sig.t;
  typed_decls : Typecheck.typed_ce Prog.core_decl list;
  diagnostics : Error.t list;
  warnings    : Warning.t list;
}

(** Per-decl accumulator threaded through the fold. *)
type decl_acc = {
  supply : Var.supply;
  sig_   : Typecheck.typed_ce Sig.t;
  decls_rev : Typecheck.typed_ce Prog.core_decl list;
  diags_rev : Error.t list;
  warns_rev : Warning.t list;
}

let resolve_and_check_decl acc raw_decl =
  try
    match ElabM.run_full acc.supply (Resolve.resolve_decl [] raw_decl) with
    | Error e ->
      { acc with diags_rev = e :: acc.diags_rev }
    | Ok ((resolved, _env), supply, ws) ->
      let warns_rev = List.rev_append ws acc.warns_rev in
      (* Header: extend sig before checking body *)
      match Typecheck.extend_sig_with_header acc.sig_ resolved with
      | Error e ->
        { acc with supply; warns_rev; diags_rev = e :: acc.diags_rev }
      | Ok sig1 ->
        (* Body — use the multi-error variant so every diagnostic
           recorded on the typed body's tree reaches LSP, not just
           the first. *)
        match Typecheck.check_decl_multi supply sig1 resolved with
        | Error e ->
          (* sig1 keeps the header extension *)
          { acc with supply; sig_ = sig1; warns_rev;
            diags_rev = e :: acc.diags_rev }
        | Ok (supply', core_decl, body_errs) ->
          { supply = supply'; sig_ = sig1;
            decls_rev = core_decl :: acc.decls_rev;
            diags_rev = List.rev_append body_errs acc.diags_rev;
            warns_rev }
  with Util.Invariant_failure info ->
    { acc with diags_rev = invariant_to_error info :: acc.diags_rev }

let compile_file source ~file =
  let parsed = ParseResilient.parse_prog_resilient source ~file in
  let init = {
    supply = Var.empty_supply;
    sig_ = Typecheck.initial_sig;
    decls_rev = [];
    diags_rev = parsed.errors;
    warns_rev = [];
  } in
  let acc = List.fold_left (fun acc chunk ->
    match chunk with
    | ParseResilient.Failed _ -> acc
    | ParseResilient.Parsed raw_decl -> resolve_and_check_decl acc raw_decl
  ) init parsed.decls in
  (* Main *)
  let (diags_rev, warns_rev) =
    try
      match parsed.main with
      | Some (Ok prog) ->
        begin match ElabM.run_full acc.supply (
          let open ElabM in
          let* resolved = Resolve.resolve_prog [] prog in
          Elaborate.check acc.sig_ Context.empty resolved.Prog.main
            (Ok resolved.Prog.main_sort) resolved.Prog.main_eff
        ) with
        | Error e -> (e :: acc.diags_rev, acc.warns_rev)
        | Ok (typed_e, _supply, ws) ->
          (* Multi-error: prepend every error recorded on the typed
             tree so LSP shows them all. *)
          (List.rev_append (Typecheck.collect_errors typed_e) acc.diags_rev,
           List.rev_append ws acc.warns_rev)
        end
      | Some (Error _) | None -> (acc.diags_rev, acc.warns_rev)
    with Util.Invariant_failure info ->
      (invariant_to_error info :: acc.diags_rev, acc.warns_rev)
  in
  { final_sig = acc.sig_;
    typed_decls = List.rev acc.decls_rev;
    diagnostics = List.rev diags_rev;
    warnings = List.rev warns_rev }

(* ================================================================== *)
(* Refined programs (.rcn)                                             *)
(* ================================================================== *)

type rfile_outcome = {
  final_rsig  : RSig.t;
  constraints : Constraint.typed_ct;
  diagnostics : Error.t list;
  hover       : HoverIndex.t;
}

let empty_rfile_outcome diags =
  { final_rsig = RSig.empty;
    constraints = Constraint.top SourcePos.dummy;
    diagnostics = diags;
    hover = HoverIndex.empty }

let compile_rfile source ~file =
  try
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
        | Ok ((typed_prog, rsig, ct), _supply) ->
          { final_rsig = rsig;
            constraints = ct;
            diagnostics = [];
            hover = HoverIndex.of_typed_rprog typed_prog }
  with Util.Invariant_failure info ->
    empty_rfile_outcome [invariant_to_error info]

module Test = struct
  let test = []
end
