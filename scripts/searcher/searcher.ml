(** Searcher: read .cmt files and emit locations.txt + callsites.txt.

    locations.txt — toplevel function definitions, one per line:
      Module.func : file:line:col : file:line:col

    callsites.txt — calls from one toplevel function to another:
      Caller.qname Callee.qname file:line:col

    Usage:
      searcher [--out-dir DIR] CMT_FILE...
*)

open Typedtree

type def = {
  qname : string;
  start_pos : Lexing.position;
  end_pos : Lexing.position;
}

type call = {
  caller : string;
  callee : string;
  pos : Lexing.position;
}

(* ---------- Helpers ---------- *)

(* "Foo__Bar" -> "Foo.Bar". Defensive: nanocn is (wrapped false), but other
   libraries built with dune wrapping mangle module names this way. *)
let demangle_modname (name : string) : string =
  let buf = Buffer.create (String.length name) in
  let i = ref 0 in
  let n = String.length name in
  while !i < n do
    if !i + 1 < n && name.[!i] = '_' && name.[!i + 1] = '_' then begin
      Buffer.add_char buf '.';
      i := !i + 2
    end else begin
      Buffer.add_char buf name.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* "lib/dsort.ml:3:4" — line 1-based, column 0-based byte offset within line. *)
let format_pos (p : Lexing.position) : string =
  Printf.sprintf "%s:%d:%d"
    p.pos_fname
    p.pos_lnum
    (p.pos_cnum - p.pos_bol)

(* Use the recorded source path verbatim — dune sandbox prefixes like
   /workspace_root would be unhelpful, and the cmt's pos_fname is already
   the path the user-facing source has (e.g. "lib/dsort.ml"). *)
let canonicalise_pos (_cmt : Cmt_format.cmt_infos) (p : Lexing.position) : Lexing.position =
  p

(* All toplevel [let pat = expr] bindings with a simple variable pattern
   are recorded as defs, regardless of the binding's type. The spec calls
   them "functions" but in practice we want every navigable toplevel name
   in the index — including type-aliased function values like
   [let gen : 'a QCheck.Gen.t = ...], whose head type is Tconstr but
   which are still callable. callsites.txt is naturally restricted to
   real calls because only [Texp_apply] generates entries. *)

(* ---------- Per-file processing ---------- *)

(* Resolve a callee Path to a fully-qualified name, tagged so the driver
   knows whether to record or skip it.
   - `Toplevel s: an unqualified Pident that matches a toplevel def we
     collected; the qualified name (with module prefix) is stored in the table.
   - `External s: any path with a dot, or any non-Pident — use Path.name.
   - `Local: an unqualified Pident that's not a toplevel def in this module —
     this is a local variable, function parameter, or inner let-binding. Skip. *)
let resolve_callee
    (toplevel_idents : (Ident.t, string) Hashtbl.t)
    (path : Path.t)
  : [ `Toplevel of string | `External of string | `Local ] =
  match path with
  | Path.Pident id ->
    (match Hashtbl.find_opt toplevel_idents id with
     | Some qname -> `Toplevel qname
     | None -> `Local)
  | _ -> `External (Path.name path)

let process_cmt (cmt_path : string) : def list * call list =
  let cmt = Cmt_format.read_cmt cmt_path in
  match cmt.cmt_annots with
  | Cmt_format.Implementation s ->
    let module_prefix = ref (demangle_modname cmt.cmt_modname) in
    let current_caller : string option ref = ref None in
    let toplevel_idents : (Ident.t, string) Hashtbl.t = Hashtbl.create 32 in
    let defs : def list ref = ref [] in
    let calls : call list ref = ref [] in

    let record_def id vb =
      let qname = !module_prefix ^ "." ^ Ident.name id in
      Hashtbl.replace toplevel_idents id qname;
      let loc = vb.vb_loc in
      if not loc.loc_ghost then begin
        let start_pos = canonicalise_pos cmt loc.loc_start in
        let end_pos = canonicalise_pos cmt loc.loc_end in
        defs := { qname; start_pos; end_pos } :: !defs
      end;
      qname
    in

    let record_call callee_kind loc =
      match !current_caller, callee_kind with
      | Some caller, `Toplevel callee
      | Some caller, `External callee ->
        if not loc.Location.loc_ghost then
          let pos = canonicalise_pos cmt loc.loc_start in
          calls := { caller; callee; pos } :: !calls
      | _, `Local -> ()
      | None, _ -> ()
    in

    let it : Tast_iterator.iterator = { Tast_iterator.default_iterator with
      structure_item = (fun self si ->
        match si.str_desc with
        | Tstr_value (_, bindings) ->
          (* First sub-pass: register every simple-binder toplevel let.
             This handles mutual recursion and forward references within
             a `let ... and ...` group: by adding all idents to the
             toplevel hashtable before walking any bodies, an unqualified
             intra-group call site can be resolved to the right qualified
             name regardless of which binding it appears in. *)
          let to_walk = List.filter_map (fun (vb : value_binding) ->
            match vb.vb_pat.pat_desc with
            | Tpat_var (id, _, _) ->
              let qname = record_def id vb in
              Some (qname, vb)
            | _ -> None) bindings
          in
          (* Walk bindings whose pattern isn't a simple variable (tuple
             destructuring, etc.) via the default value_binding hook so
             we still descend into any inner expressions. *)
          List.iter (fun (vb : value_binding) ->
            match vb.vb_pat.pat_desc with
            | Tpat_var (_, _, _) -> ()
            | _ -> self.value_binding self vb
          ) bindings;
          (* Walk each registered binding's body with current_caller set. *)
          List.iter (fun (qname, (vb : value_binding)) ->
            let prev = !current_caller in
            current_caller := Some qname;
            self.expr self vb.vb_expr;
            current_caller := prev
          ) to_walk
        | Tstr_module mb ->
          (match mb.mb_id with
           | Some id ->
             let prev = !module_prefix in
             module_prefix := prev ^ "." ^ Ident.name id;
             Tast_iterator.default_iterator.structure_item self si;
             module_prefix := prev
           | None ->
             Tast_iterator.default_iterator.structure_item self si)
        | Tstr_recmodule mbs ->
          List.iter (fun (mb : module_binding) ->
            match mb.mb_id with
            | Some id ->
              let prev = !module_prefix in
              module_prefix := prev ^ "." ^ Ident.name id;
              Tast_iterator.default_iterator.module_binding self mb;
              module_prefix := prev
            | None ->
              Tast_iterator.default_iterator.module_binding self mb
          ) mbs
        | _ ->
          Tast_iterator.default_iterator.structure_item self si);
      expr = (fun self e ->
        (match e.exp_desc with
         | Texp_apply (fn_expr, _) ->
           (match fn_expr.exp_desc with
            | Texp_ident (path, _, _) ->
              let kind = resolve_callee toplevel_idents path in
              record_call kind fn_expr.exp_loc
            | _ -> ())
         | _ -> ());
        Tast_iterator.default_iterator.expr self e);
    } in
    it.structure it s;
    (List.rev !defs, List.rev !calls)
  | _ -> ([], [])

(* ---------- Output ---------- *)

let write_locations (path : string) (defs : def list) : unit =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    List.iter (fun d ->
      Printf.fprintf oc "%s : %s : %s\n"
        d.qname (format_pos d.start_pos) (format_pos d.end_pos)
    ) defs)

let write_callsites (path : string) (calls : call list) : unit =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    List.iter (fun c ->
      Printf.fprintf oc "%s %s %s\n"
        c.caller c.callee (format_pos c.pos)
    ) calls)

(* ---------- Main ---------- *)

let () =
  let out_dir = ref "." in
  let cmts = ref [] in
  let usage = "searcher [--out-dir DIR] CMT_FILE..." in
  Arg.parse
    [("--out-dir", Arg.Set_string out_dir, "DIR  output directory (default: .)")]
    (fun f -> cmts := f :: !cmts)
    usage;
  let cmts = List.rev !cmts in
  if cmts = [] then begin
    prerr_endline "searcher: no .cmt files given";
    Arg.usage
      [("--out-dir", Arg.Set_string out_dir, "DIR  output directory (default: .)")]
      usage;
    exit 2
  end;
  let all_defs, all_calls =
    List.fold_left (fun (ds, cs) cmt_path ->
      try
        let d, c = process_cmt cmt_path in
        (List.rev_append d ds, List.rev_append c cs)
      with e ->
        Printf.eprintf "searcher: %s: %s\n" cmt_path (Printexc.to_string e);
        (ds, cs)
    ) ([], []) cmts
  in
  write_locations (Filename.concat !out_dir "locations.txt") (List.rev all_defs);
  write_callsites (Filename.concat !out_dir "callsites.txt") (List.rev all_calls)
