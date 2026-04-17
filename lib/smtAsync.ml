(** Asynchronous Z3 driver — see smtAsync.mli.

    Mutable state: run counter, active-run table, event queue, wakeup
    pipe.  All guarded by a single mutex.  Permitted per CLAUDE.md. *)

type run_id = int

type event =
  | Query_result of { run : run_id; index : int;
                      pos : SourcePos.t;
                      answer : SolverOutput.answer }
  | Run_finished of run_id
  | Run_failed of { run : run_id; msg : string }

(* ================================================================== *)
(* Internal mutable state                                              *)
(* ================================================================== *)

type run_state = {
  id : run_id;
  pid : int;
  mutable cancelled : bool;
}

let mutex = Mutex.create ()
let next_id = ref 0
let active_runs : (run_id, run_state) Hashtbl.t = Hashtbl.create 4
let event_queue : event Queue.t = Queue.create ()

(* Wakeup pipe: the reader thread pushes a byte when an event is
   queued; the main loop selects on the read end. *)
let (wakeup_read, wakeup_write) = Unix.pipe ()

let wakeup_fd () = wakeup_read

let wake () =
  (* Write a single byte to signal the main loop. *)
  ignore (Unix.write wakeup_write (Bytes.of_string "x") 0 1)

let push_event run_st ev =
  Mutex.lock mutex;
  let dominated = run_st.cancelled in
  if not dominated then begin
    Queue.push ev event_queue;
    wake ()
  end;
  Mutex.unlock mutex

let drain_events () =
  Mutex.lock mutex;
  let events = ref [] in
  while not (Queue.is_empty event_queue) do
    events := Queue.pop event_queue :: !events
  done;
  (* Drain the wakeup pipe *)
  let buf = Bytes.create 64 in
  (try
     Unix.set_nonblock wakeup_read;
     while Unix.read wakeup_read buf 0 64 > 0 do () done
   with Unix.Unix_error (Unix.EAGAIN, _, _) | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> ());
  Unix.clear_nonblock wakeup_read;
  Mutex.unlock mutex;
  List.rev !events

(* ================================================================== *)
(* Background reader thread                                            *)
(* ================================================================== *)

let reader_thread run_st ic positions =
  let n_positions = List.length positions in
  let index = ref 0 in
  (try
     let buf = Buffer.create 4096 in
     let rec read_lines () =
       let line = input_line ic in
       Buffer.add_string buf line;
       Buffer.add_char buf '\n';
       (* Try to parse what we have so far. SMT-LIB verdicts are
          single-line s-expressions: "sat", "unsat", "unknown",
          or "(error ...)". *)
       let text = Buffer.contents buf in
       (match SmtParse.parse_sexps text ~file:"<solver>" with
        | Ok sexps when List.length sexps > !index ->
          (* New sexp(s) arrived — classify and deliver. *)
          let new_sexps = List.filteri (fun i _ -> i >= !index) sexps in
          List.iter (fun sexp ->
            let answer = SolverOutput.classify_sexp sexp in
            let pos = match List.nth_opt positions !index with
              | Some p -> p
              | None -> SourcePos.dummy
            in
            push_event run_st
              (Query_result { run = run_st.id; index = !index;
                              pos; answer });
            index := !index + 1
          ) new_sexps
        | _ -> ());
       (* If we've seen all expected answers, we could stop early,
          but let Z3 finish naturally. *)
       if !index < n_positions then read_lines ()
     in
     read_lines ()
   with
   | End_of_file -> ()
   | exn ->
     push_event run_st
       (Run_failed { run = run_st.id; msg = Printexc.to_string exn }));
  (* Clean up *)
  (try ignore (Unix.close_process_in ic) with _ -> ());
  push_event run_st (Run_finished run_st.id);
  Mutex.lock mutex;
  Hashtbl.remove active_runs run_st.id;
  Mutex.unlock mutex

(* ================================================================== *)
(* Public API                                                          *)
(* ================================================================== *)

let start ~exe ~smt_path ~query_positions ~on_event:_ =
  let cmd = Filename.quote_command exe [smt_path] in
  try
    let ic = Unix.open_process_in cmd in
    let pid = match Unix.process_in_pid ic with
      | pid -> pid
    in
    Mutex.lock mutex;
    let id = !next_id in
    next_id := id + 1;
    let run_st = { id; pid; cancelled = false } in
    Hashtbl.replace active_runs id run_st;
    Mutex.unlock mutex;
    let _thread = Thread.create (reader_thread run_st ic) query_positions in
    Ok id
  with
  | Unix.Unix_error (err, _, _) ->
    Error (Printf.sprintf "failed to start solver: %s" (Unix.error_message err))
  | exn ->
    Error (Printf.sprintf "failed to start solver: %s" (Printexc.to_string exn))

let cancel id =
  Mutex.lock mutex;
  (match Hashtbl.find_opt active_runs id with
   | Some run_st ->
     run_st.cancelled <- true;
     (try Unix.kill run_st.pid Sys.sigterm with _ -> ())
   | None -> ());
  Mutex.unlock mutex

module Test = struct
  let test = []
end
