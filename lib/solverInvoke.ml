(** One-shot solver invocation — see solverInvoke.mli. *)

type answer = SolverOutput.answer = Sat | Unsat | Unknown | Error of string

let print_answer = SolverOutput.print

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
    SolverOutput.classify_output output ~file:smt_path
  | Unix.WEXITED n ->
    Error (Printf.sprintf "solver exited with code %d; output: %s" n output)
  | Unix.WSIGNALED n ->
    Error (Printf.sprintf "solver killed by signal %d" n)
  | Unix.WSTOPPED n ->
    Error (Printf.sprintf "solver stopped by signal %d" n)
