(** Asynchronous Z3 solver driver.

    Spawns Z3 as a subprocess reading an SMT-LIB file, reads verdicts
    line-by-line on a background thread, and delivers events to the
    caller via a callback.  Monotonic run IDs ensure stale results
    from cancelled runs are discarded.

    This module uses mutable state internally (thread, pipe, run
    counter) — permitted per CLAUDE.md rule 3 and user confirmation
    in open question 2 of the editor-support plan. *)

type run_id = int

type event =
  | Query_result of { run : run_id; index : int;
                      pos : SourcePos.t;
                      answer : SolverOutput.answer }
  | Run_finished of run_id
  | Run_failed of { run : run_id; msg : string }

val start :
  exe:string ->
  smt_path:string ->
  query_positions:SourcePos.t list ->
  on_event:(event -> unit) ->
  (run_id, string) result
(** [start ~exe ~smt_path ~query_positions ~on_event] spawns [exe]
    on [smt_path] and reads answers on a background thread.  Each
    answer is paired with the corresponding position from
    [query_positions] and delivered via [on_event].

    [on_event] is called from the background thread.  The caller is
    responsible for synchronization (e.g. writing to a wakeup pipe
    that the main thread selects on). *)

val cancel : run_id -> unit
(** [cancel id] marks run [id] as cancelled.  No further events
    for that run will be delivered.  If the Z3 subprocess is still
    running, it receives SIGTERM. *)

val wakeup_fd : unit -> Unix.file_descr
(** A readable file descriptor that becomes ready whenever an event
    is delivered.  The main loop should [Unix.select] on this fd
    alongside stdin. *)

val drain_events : unit -> event list
(** Read all pending events from the wakeup pipe and return them.
    Call this when [wakeup_fd] is ready. *)

module Test : sig
  val test : QCheck.Test.t list
end
