type answer = Sat | Unsat | Unknown | Error of string

let print_answer fmt = function
  | Sat -> Format.pp_print_string fmt "sat"
  | Unsat -> Format.pp_print_string fmt "unsat"
  | Unknown -> Format.pp_print_string fmt "unknown"
  | Error msg -> Format.fprintf fmt "error: %s" msg

(* Classify a single top-level sexp emitted by the solver. Anything
   that isn't a recognised verdict or an (error ...) form is folded
   into Error with a description of the unparsed form. *)
let classify_sexp sexp =
  match SmtSexp.shape sexp with
  | SmtSexp.Atom (SmtAtom.Symbol "sat") -> Sat
  | SmtSexp.Atom (SmtAtom.Symbol "unsat") -> Unsat
  | SmtSexp.Atom (SmtAtom.Symbol "unknown") -> Unknown
  | SmtSexp.List items ->
    (match List.map SmtSexp.shape items with
     | SmtSexp.Atom (SmtAtom.Symbol "error") :: rest ->
       let msg =
         List.map (function
           | SmtSexp.Atom (SmtAtom.String s) -> s
           | other ->
             let sexp = SmtSexp.mk (object method loc = SourcePos.dummy end) other in
             SmtSexp.to_string sexp)
           rest
         |> String.concat " "
       in
       Error msg
     | _ -> Error ("unparsed solver output: " ^ SmtSexp.to_string sexp))
  | _ -> Error ("unparsed solver output: " ^ SmtSexp.to_string sexp)

let read_all_output ic =
  let buf = Buffer.create 4096 in
  (try
    while true do
      Buffer.add_string buf (input_line ic);
      Buffer.add_char buf '\n'
    done
  with End_of_file -> ());
  Buffer.contents buf

let run_z3 ~exe ~smt_path =
  let cmd = Filename.quote_command exe [smt_path] in
  let ic = Unix.open_process_in cmd in
  let output = read_all_output ic in
  let status = Unix.close_process_in ic in
  match status with
  | Unix.WEXITED 0 ->
    (match SmtParse.parse_sexps output ~file:smt_path with
     | Ok sexps -> Ok (List.map classify_sexp sexps)
     | Error msg -> Error ("solver output parse error: " ^ msg))
  | Unix.WEXITED n ->
    Error (Printf.sprintf "solver exited with code %d; output: %s" n output)
  | Unix.WSIGNALED n ->
    Error (Printf.sprintf "solver killed by signal %d" n)
  | Unix.WSTOPPED n ->
    Error (Printf.sprintf "solver stopped by signal %d" n)
