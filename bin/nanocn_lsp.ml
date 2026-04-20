(** nanocn-lsp — Language Server Protocol server for nanoCN.

    Synchronous stdio-based server using the [lsp] and [jsonrpc] opam
    libraries.  Handles:
    - textDocument/didOpen, didChange, didSave → recompile, publish diagnostics
    - textDocument/hover → context, sort, effect at cursor
    - textDocument/definition → variable binding site
    - textDocument/documentSymbol → top-level declarations

    Phase 2: synchronous SMT (blocks on didSave for .rcn files).
    Phase 3 will add async SMT via SmtAsync. *)

(* ==================================================================
   IO functor for Lsp.Io.Make — blocking stdin/stdout
   ================================================================== *)

module SyncIo = struct
  type 'a t = 'a

  let return x = x
  let raise exn = Stdlib.raise exn

  module O = struct
    let ( let+ ) x f = f x
    let ( let* ) x f = f x
  end
end

module SyncChan = struct
  type input = in_channel
  type output = out_channel

  let read_line ic =
    try Some (Stdlib.input_line ic)
    with End_of_file -> None

  let read_exactly ic n =
    try
      let buf = Bytes.create n in
      Stdlib.really_input ic buf 0 n;
      Some (Bytes.to_string buf)
    with End_of_file -> None

  let write oc strings =
    List.iter (Stdlib.output_string oc) strings;
    Stdlib.flush oc
end

module Io = Lsp.Io.Make (SyncIo) (SyncChan)

(* ==================================================================
   Server state
   ================================================================== *)

type doc_state = {
  uri     : Lsp.Types.DocumentUri.t;
  text    : string;
  version : int;
  file    : string;    (* filesystem path derived from URI *)
  outcome : CompileFile.file_outcome option;
  rfile   : CompileFile.rfile_outcome option;
  hover   : HoverIndex.t;
}

type state = {
  mutable docs : (Lsp.Types.DocumentUri.t * doc_state) list;
  source_registry : SourceExcerpt.registry;
}

let state : state = {
  docs = [];
  source_registry = SourceExcerpt.create ();
}

let find_doc uri =
  match List.find_opt (fun (u, _) -> Lsp.Types.DocumentUri.equal u uri) state.docs with
  | Some (_, doc) -> Some doc
  | None -> None

let set_doc uri doc =
  state.docs <- (uri, doc) ::
    List.filter (fun (u, _) -> not (Lsp.Types.DocumentUri.equal u uri)) state.docs

(* ==================================================================
   Compilation + diagnostics
   ================================================================== *)

let is_rcn path = Filename.check_suffix path ".rcn"

let compile_and_diagnose (doc : doc_state) : doc_state =
  SourceExcerpt.register state.source_registry ~file:doc.file ~source:doc.text;
  if is_rcn doc.file then
    let r = CompileFile.compile_rfile doc.text ~file:doc.file in
    { doc with rfile = Some r; outcome = None; hover = r.hover }
  else
    let r = CompileFile.compile_file doc.text ~file:doc.file in
    let hover = HoverIndex.of_typed_decls r.typed_decls in
    { doc with outcome = Some r; rfile = None; hover }

let error_to_lsp_diagnostic (e : Error.t) : Lsp.Types.Diagnostic.t =
  let range = match Error.loc e with
    | Some loc ->
      let start_line = max 0 (SourcePos.start_line loc - 1) in
      let end_line = max 0 (SourcePos.end_line loc - 1) in
      { Lsp.Types.Range.start =
          { line = start_line; character = SourcePos.start_col loc };
        end_ =
          { line = end_line; character = SourcePos.end_col loc } }
    | None ->
      { Lsp.Types.Range.start = { line = 0; character = 0 };
        end_ = { line = 0; character = 0 } }
  in
  Lsp.Types.Diagnostic.create
    ~range
    ~severity:Lsp.Types.DiagnosticSeverity.Error
    ~source:"nanocn"
    ~message:(`String (Error.to_string e))
    ()

let diagnostics_of_doc (doc : doc_state) : Lsp.Types.Diagnostic.t list =
  let errors = match doc.outcome with
    | Some o -> o.diagnostics
    | None ->
      match doc.rfile with
      | Some r -> r.diagnostics
      | None -> []
  in
  List.map error_to_lsp_diagnostic errors

let publish_diagnostics oc (doc : doc_state) =
  let diags = diagnostics_of_doc doc in
  let params = Lsp.Types.PublishDiagnosticsParams.create
    ~uri:doc.uri
    ~diagnostics:diags
    () in
  let notif = Lsp.Server_notification.to_jsonrpc
    (Lsp.Server_notification.PublishDiagnostics params) in
  Io.write oc (Jsonrpc.Packet.Notification notif)

(* ==================================================================
   Request handlers
   ================================================================== *)

let uri_to_path uri =
  Lsp.Types.DocumentUri.to_path uri

let handle_initialize _params : Lsp.Types.InitializeResult.t =
  let capabilities = Lsp.Types.ServerCapabilities.create
    ~textDocumentSync:(`TextDocumentSyncKind Lsp.Types.TextDocumentSyncKind.Full)
    ~hoverProvider:(`Bool true)
    ~definitionProvider:(`Bool true)
    ~documentSymbolProvider:(`Bool true)
    ()
  in
  Lsp.Types.InitializeResult.create ~capabilities ()

let handle_hover (doc : doc_state) (params : Lsp.Types.HoverParams.t) : Lsp.Types.Hover.t option =
  let pos = params.position in
  let line = pos.line + 1 in  (* LSP is 0-based, SourcePos is 1-based *)
  let col = pos.character in
  match HoverIndex.lookup doc.hover ~line ~col with
  | None -> None
  | Some (_loc, ctx, rctx, sort, eff) ->
    let sort_str = Format.asprintf "%a" Sort.print sort in
    let eff_str = Format.asprintf "%a" Effect.print eff in
    (* Format context: prefer refined context (with logical facts and
       resources) when available; fall back to core context. *)
    let ctx_str = match rctx with
      | Some delta ->
        let lines = List.filter_map (fun entry ->
          match entry with
          | RCtx.Comp { var; sort = s; eff = e } when not (Var.is_generated var) ->
            Some (Format.asprintf "%a : %a [%a]" Var.print var Sort.print s Effect.print e)
          | RCtx.Log { var; prop } when not (Var.is_generated var) ->
            Some (Format.asprintf "%a : %a [log]" Var.print var CoreExpr.print prop)
          | RCtx.Res { var; pred; value; usage }
            when not (Var.is_generated var) && Usage.is_avail usage ->
            Some (Format.asprintf "%a : %a @@ %a [res]"
              Var.print var CoreExpr.print pred CoreExpr.print value)
          | _ -> None
        ) (RCtx.entries delta) in
        (match lines with [] -> "(empty)" | ls -> String.concat "\n" ls)
      | None ->
        let user_bindings = List.filter_map (fun binding ->
          match binding with
          | Context.Term (v, s, e) when not (Var.is_generated v) ->
            Some (Format.asprintf "%a : %a [%a]" Var.print v Sort.print s Effect.print e)
          | _ -> None
        ) (Context.to_list ctx) in
        (match user_bindings with [] -> "(empty)" | bs -> String.concat "\n" bs)
    in
    (* First line is concise (for echo area); full context follows
       (for *eldoc* buffer). *)
    let contents = Printf.sprintf "`%s` [%s]\n\n**Context:**\n```nanocn\n%s\n```"
      sort_str eff_str ctx_str in
    let markup = Lsp.Types.MarkupContent.create
      ~kind:Lsp.Types.MarkupKind.Markdown
      ~value:contents in
    Some (Lsp.Types.Hover.create ~contents:(`MarkupContent markup) ())

let handle_document_symbol (doc : doc_state) _params : Lsp.Types.DocumentSymbol.t list =
  let mk_symbol name kind loc =
    let start_line = max 0 (SourcePos.start_line loc - 1) in
    let end_line = max 0 (SourcePos.end_line loc - 1) in
    let range = { Lsp.Types.Range.start =
                    { line = start_line; character = SourcePos.start_col loc };
                  end_ =
                    { line = end_line; character = SourcePos.end_col loc } } in
    Lsp.Types.DocumentSymbol.create
      ~name ~kind ~range ~selectionRange:range ()
  in
  match doc.outcome with
  | Some o ->
    List.filter_map (fun decl ->
      match decl with
      | Prog.CoreFunDecl { name; loc; _ } ->
        Some (mk_symbol name Lsp.Types.SymbolKind.Function loc)
      | Prog.CoreSortDecl d ->
        Some (mk_symbol (Format.asprintf "%a" Dsort.print d.DsortDecl.name)
                Lsp.Types.SymbolKind.Struct d.DsortDecl.loc)
      | Prog.CoreTypeDecl d ->
        Some (mk_symbol (Format.asprintf "%a" Dsort.print d.DtypeDecl.name)
                Lsp.Types.SymbolKind.Class d.DtypeDecl.loc)
    ) o.typed_decls
  | None -> []

(* ==================================================================
   Main loop
   ================================================================== *)

let handle_request : type a. doc_state option -> a Lsp.Client_request.t -> a =
  fun doc_opt req ->
  match req with
  | Lsp.Client_request.Initialize params ->
    handle_initialize params
  | Lsp.Client_request.TextDocumentHover params ->
    (match doc_opt with
     | Some doc -> handle_hover doc params
     | None -> None)
  | Lsp.Client_request.TextDocumentDefinition _params ->
    None  (* TODO: implement in Phase 2 polish *)
  | Lsp.Client_request.DocumentSymbol params ->
    let doc_opt = find_doc params.textDocument.uri in
    (match doc_opt with
     | Some doc ->
       let syms = handle_document_symbol doc params in
       Some (`DocumentSymbol syms)
     | None -> None)
  | Lsp.Client_request.Shutdown -> ()
  | _ -> raise Exit  (* unhandled request *)

(* ==================================================================
   Async SMT
   ================================================================== *)

let current_smt_run : SmtAsync.run_id option ref = ref None

let smt_pos_to_diagnostic pos answer =
  let start_line = max 0 (SourcePos.start_line pos - 1) in
  let end_line = max 0 (SourcePos.end_line pos - 1) in
  let range = { Lsp.Types.Range.start =
                  { line = start_line; character = SourcePos.start_col pos };
                end_ =
                  { line = end_line; character = SourcePos.end_col pos } } in
  let severity, message = match answer with
    | SolverOutput.Sat ->
      Lsp.Types.DiagnosticSeverity.Hint, "sat (constraint satisfied)"
    | SolverOutput.Unsat ->
      Lsp.Types.DiagnosticSeverity.Error, "unsat (constraint violated)"
    | SolverOutput.Unknown ->
      Lsp.Types.DiagnosticSeverity.Warning, "unknown (solver timeout or incomplete)"
    | SolverOutput.Error msg ->
      Lsp.Types.DiagnosticSeverity.Error, "solver error: " ^ msg
  in
  Lsp.Types.Diagnostic.create ~range ~severity ~source:"nanocn-smt"
    ~message:(`String message) ()

let start_smt_run oc _doc (r : CompileFile.rfile_outcome) =
  (* Cancel any previous run *)
  (match !current_smt_run with
   | Some id -> SmtAsync.cancel id; current_smt_run := None
   | None -> ());
  (* Encode constraints and write to a temp SMT file *)
  match SmtEncode.encode r.final_rsig r.constraints with
  | Error _msg -> ()
  | Ok (prelude, constraints) ->
    let smt_path = Filename.temp_file "nanocn" ".smt2" in
    let oc_smt = Out_channel.open_text smt_path in
    SmtEncode.write_file oc_smt ~prelude ~constraints;
    Out_channel.close oc_smt;
    let positions = List.map (fun c -> c.SmtConstraint.pos) constraints in
    let z3 = Option.value (Sys.getenv_opt "Z3") ~default:"z3" in
    match SmtAsync.start ~exe:z3 ~smt_path ~query_positions:positions
            ~on_event:(fun _ev -> ()) with
    | Error _msg -> ()
    | Ok run_id ->
      current_smt_run := Some run_id;
      ignore (oc : out_channel)  (* used by event handler via drain *)

let handle_smt_events oc =
  let events = SmtAsync.drain_events () in
  List.iter (fun ev ->
    match ev with
    | SmtAsync.Query_result { run; pos; answer; _ } ->
      (match !current_smt_run with
       | Some id when Int.equal id run ->
         (* Find the doc this run belongs to — for now, publish
            individual diagnostics by accumulating them. *)
         let diag = smt_pos_to_diagnostic pos answer in
         (* We'd need to know the URI to publish. For now, broadcast
            to all .rcn docs. *)
         List.iter (fun (uri, doc) ->
           if is_rcn doc.file then begin
             let existing = diagnostics_of_doc doc in
             let params = Lsp.Types.PublishDiagnosticsParams.create
               ~uri ~diagnostics:(existing @ [diag]) () in
             let notif = Lsp.Server_notification.to_jsonrpc
               (Lsp.Server_notification.PublishDiagnostics params) in
             Io.write oc (Jsonrpc.Packet.Notification notif)
           end
         ) state.docs
       | _ -> ())  (* stale run *)
    | SmtAsync.Run_finished run ->
      (match !current_smt_run with
       | Some id when Int.equal id run -> current_smt_run := None
       | _ -> ())
    | SmtAsync.Run_failed { run; msg = _ } ->
      (match !current_smt_run with
       | Some id when Int.equal id run -> current_smt_run := None
       | _ -> ())
  ) events

let handle_notification oc (notif : Lsp.Client_notification.t) =
  match notif with
  | Lsp.Client_notification.TextDocumentDidOpen params ->
    let uri = params.textDocument.uri in
    let file = uri_to_path uri in
    let doc = { uri; text = params.textDocument.text;
                version = params.textDocument.version;
                file; outcome = None; rfile = None;
                hover = HoverIndex.empty } in
    let doc = compile_and_diagnose doc in
    set_doc uri doc;
    publish_diagnostics oc doc
  | Lsp.Client_notification.TextDocumentDidChange params ->
    let uri = params.textDocument.uri in
    (match find_doc uri with
     | None -> ()
     | Some doc ->
       (* Full sync: take the last content change *)
       let text = match List.rev params.contentChanges with
         | last :: _ ->
           (match last.range with
            | None -> last.text  (* full document *)
            | Some _ -> doc.text)  (* incremental — not supported with Full sync *)
         | [] -> doc.text
       in
       let doc = { doc with text; version = params.textDocument.version } in
       let doc = compile_and_diagnose doc in
       set_doc uri doc;
       publish_diagnostics oc doc)
  | Lsp.Client_notification.DidSaveTextDocument params ->
    (* For .rcn files: trigger async SMT check if we have constraints. *)
    let uri = params.textDocument.uri in
    (match find_doc uri with
     | Some doc when is_rcn doc.file ->
       (match doc.rfile with
        | Some r when List.length r.diagnostics = 0 ->
          start_smt_run oc doc r
        | _ -> ())
     | _ -> ())
  | Lsp.Client_notification.Initialized ->
    ()
  | Lsp.Client_notification.Exit ->
    exit 0
  | _ -> ()

let handle_packet oc packet =
  match packet with
  | Jsonrpc.Packet.Request req ->
    (match Lsp.Client_request.of_jsonrpc req with
     | Error _ ->
       let err = Jsonrpc.Response.Error.make
         ~code:Jsonrpc.Response.Error.Code.MethodNotFound
         ~message:"unknown request" () in
       let resp = Jsonrpc.Response.error req.id err in
       Io.write oc (Jsonrpc.Packet.Response resp)
     | Ok (Lsp.Client_request.E r) ->
       let doc_opt =
         match req.params with
         | Some (`Assoc fields) ->
           (match List.assoc_opt "textDocument" fields with
            | Some (`Assoc td_fields) ->
              (match List.assoc_opt "uri" td_fields with
               | Some (`String uri_str) ->
                 find_doc (Lsp.Types.DocumentUri.of_string uri_str)
               | _ -> None)
            | _ -> None)
         | _ -> None
       in
       (try
          let result = handle_request doc_opt r in
          let json = Lsp.Client_request.yojson_of_result r result in
          let resp = Jsonrpc.Response.ok req.id json in
          Io.write oc (Jsonrpc.Packet.Response resp)
        with Exit ->
          let err = Jsonrpc.Response.Error.make
            ~code:Jsonrpc.Response.Error.Code.MethodNotFound
            ~message:("unhandled: " ^ req.method_) () in
          let resp = Jsonrpc.Response.error req.id err in
          Io.write oc (Jsonrpc.Packet.Response resp)))
  | Jsonrpc.Packet.Notification notif ->
    (match Lsp.Client_notification.of_jsonrpc notif with
     | Ok n -> handle_notification oc n
     | Error _ -> ())
  | Jsonrpc.Packet.Response _ | Jsonrpc.Packet.Batch_response _
  | Jsonrpc.Packet.Batch_call _ ->
    ()

let () =
  let oc = stdout in
  let ic = stdin in
  let stdin_fd = Unix.descr_of_in_channel ic in
  let wakeup_fd = SmtAsync.wakeup_fd () in
  let rec loop () =
    (* Select on stdin (LSP messages) and wakeup pipe (SMT events). *)
    let ready, _, _ = Unix.select [stdin_fd; wakeup_fd] [] [] (-1.0) in
    List.iter (fun fd ->
      if fd == wakeup_fd then
        handle_smt_events oc
      else begin
        match Io.read ic with
        | None -> exit 0
        | Some packet -> handle_packet oc packet
      end
    ) ready;
    loop ()
  in
  loop ()
