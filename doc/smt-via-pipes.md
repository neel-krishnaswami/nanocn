# Driving Z3 and CVC5 over pipes from OCaml

Notes on how to spawn Z3 or CVC5 as a child process, send SMT-LIB commands
on its stdin, and read responses from its stdout — and the small set of
gotchas you have to handle to make that actually work.

## Starting an interactive toplevel manually

Both solvers are installed under `install/bin/`:

```
install/bin/z3 --version    # Z3 version 4.17.0
install/bin/cvc5 --version  # cvc5 1.3.4.dev+main@5a72641
```

### Z3 — line-streaming stdin mode, no prompt

```
$ install/bin/z3 -in
(declare-const x Int)        ← you type
(assert (= x 5))             ← you type
(check-sat)                  ← you type
sat                          ← z3 responds
(get-value (x))              ← you type
((x 5))                      ← z3 responds
(exit)
```

- `-in` tells Z3 to read from stdin instead of treating arguments as filenames.
  Default input format is already SMT-LIB 2; `-smt2` is implicit.
- There is **no prompt**. Z3 just streams responses as soon as a command
  produces output.
- Model generation is on by default in Z3, so `(get-value …)` / `(get-model)`
  work without extra flags.
- `(set-option :print-success true)` makes every command echo `success`,
  which is the closest thing to a REPL ack.

### CVC5 — true REPL with prompt

```
$ install/bin/cvc5 --interactive
cvc5 1.3.4.dev+main@5a72641 …
cvc5> (set-logic QF_LIA)
cvc5> (declare-const x Int)
cvc5> (assert (= x 5))
cvc5> (check-sat)
sat
cvc5> (exit)
```

- `--interactive` forces interactive mode and prints a `cvc5>` prompt
  (also auto-detected if stdin is a TTY).
- Without `--interactive`, you can still pipe SMT-LIB into `cvc5` reading
  from stdin, but you get no prompt.
- Useful companion flags: `--produce-models` (otherwise `(get-value …)`
  errors with "cannot get value unless model generation is enabled"),
  `--incremental` for `push`/`pop`, `--print-success` to get the SMT-LIB
  `success` ack after each command, `-q` to suppress the multi-page banner.

Neither binary links readline, so there's no built-in line editing or
history. `rlwrap z3 -in` / `rlwrap cvc5 --interactive` is the usual fix
(but `rlwrap` is not installed on this machine).

## Talking to a solver from a program

Manual REPL use is fine for one-off questions, but the goal here is to
drive the solver from another program — send a batch of commands, read
the response, decide what to send next, repeat. That introduces three
problems that the manual REPL hides from you.

### Problem 1: stdio buffering on pipes

C stdio fully-buffers stdout when stdout is not a TTY. That means a
naive C program writing to a pipe will hold its output in a 4 KB buffer
and only flush at process exit, which would deadlock any line-by-line
driver waiting for a response.

**Both Z3 and CVC5 explicitly flush stdout after each command**, even
when stdout is a pipe. So this problem doesn't actually bite us — plain
pipes are fine, no `unbuffer` / `stdbuf -o0` / pty trickery required.
Worth knowing in case a future solver version regresses on this, but
the current ones are well-behaved.

### Problem 2: framing — when does a response end?

The SMT-LIB wire protocol gives you no length prefix and no end-of-message
marker. Worse, different commands produce wildly different amounts of
output:

| Command                                  | Lines of output                            |
|------------------------------------------|--------------------------------------------|
| `(declare-const x Int)`                  | **0**                                      |
| `(assert (= x 5))`                       | **0**                                      |
| `(set-option :produce-models true)`      | **0**                                      |
| `(check-sat)`                            | **1** (`sat` / `unsat` / `unknown`)        |
| `(get-value (x))`                        | **1 on CVC5**, **multiple on Z3**          |
| `(get-model)`                            | **many**, depends on the model             |
| any command, on warning                  | **+1 or more** unstructured warning lines  |
| any command, on error                    | **+1 or more** lines of an `(error …)` form |

So if you naively call `input_line` once after every command you send,
you will block forever on the silent commands and read only the first
line of multi-line responses. And if you instead try to "read everything
that's available," you race the solver — it might not have produced its
output yet at the moment you check.

The fix is **sentinel framing**. After every batch of user commands the
driver appends:

```
(echo "<<<SOLVER_SYNC>>>")
```

Both solvers process commands strictly in order, so the echo line will
not appear until every preceding command has finished producing its
output. The driver reads lines in a loop and stops the moment it sees
the sentinel. Everything before it is the response (possibly empty).
This is the same trick people use to drive `gdb` or any other
line-oriented REPL over a pipe: pick a string the program would never
produce on its own, ask the program to echo it, and use that as your
end-of-message marker.

You might be tempted to use `(set-option :print-success true)` instead,
but it only acks the silent commands — `check-sat` / `get-value` /
`get-model` still return their normal value-shaped responses with no
trailing `success`. So you'd need a per-command-type response parser to
know how many lines to read. Sentinel framing is uniform across every
command and doesn't care what the response looks like.

### Problem 2a: Z3 and CVC5 echo differently

When you run `(echo "SYNC123")`:

```
$ printf '(echo "SYNC123")\n' | z3 -in
SYNC123
$ printf '(echo "SYNC123")\n' | cvc5 -q
"SYNC123"
```

Z3 prints the string contents bare; CVC5 prints it with the surrounding
double quotes. The driver's sentinel matcher has to accept either form.

### Problem 3: solver-specific invocation flags

The flags the driver passes to each solver matter:

- **Z3** needs `-in` to read SMT-LIB from stdin instead of treating its
  arguments as filenames. Z3 is always incremental and model generation
  is on by default, so nothing else is required.
- **CVC5** needs `-q` to suppress the multi-page license/banner that
  would otherwise show up as the first thing the driver reads, and
  `--incremental` (or `-i`) for `push`/`pop` support. Model generation
  needs to be enabled separately via `(set-option :produce-models true)`
  before any `(get-value …)` call, otherwise CVC5 returns an error
  *"cannot get value unless model generation is enabled"*.

## Other things worth knowing

- **`(get-value …)` formatting differs.** Z3 wraps each binding on its
  own line; CVC5 puts them all on one line. The line-by-line read loop
  doesn't care because we're framing on the sentinel, not counting lines.
- **Process cleanup matters.** The `close` function writes `(exit)`,
  closes both channels, and `waitpid`s. Skip the `waitpid` and you
  accumulate zombies.
- **No timeouts.** The current `input_line`-based read loop will block
  forever waiting for the sentinel if the solver hangs (e.g. on a
  hard `check-sat`). For a production driver you'd want `Unix.select`
  on the read fd with a deadline.
- **Sentinel collision.** Pick a sentinel a real response could never
  produce. `<<<SOLVER_SYNC>>>` is fine for ad-hoc work; if you wanted
  to be paranoid you could include a per-query nonce so stale output
  from a previous query can't be mistaken for the current one's sync.
- **Step up to S-expressions when you need structured replies.** For
  this experiment we treat responses as opaque strings. To extract
  values from `(get-value …)` or model entries from `(get-model)` you'll
  want a tiny S-expression parser; `csexp` is dependency-light and
  matches SMT-LIB's quoting closely.

## Building and running the example driver

```
ocamlfind ocamlopt -package unix -linkpkg solver_pipe.ml -o solver_pipe
./solver_pipe
```

Or as a script (no compilation):

```
ocaml unix.cma solver_pipe.ml
```

Expected output (abbreviated):

```
============================================================
  Z3
============================================================
--- set-option produce-models ---
--- declare + assert ---
--- check-sat ---
sat
--- get-value ---
((x 5)
 (y 6)
 ((+ x y) 11))
--- push / contradict / check / pop ---
unsat
--- check-sat after pop ---
sat

============================================================
  CVC5
============================================================
…same shape, with CVC5 putting (get-value …) bindings on one line…
```

Notice the empty bodies after `set-option` and `declare + assert`: those
commands produce no output, and the sentinel framing tells the driver
"the response is the empty string" rather than blocking forever waiting
for a line that will never come.

## Annotated source

What follows is the full text of `solver_pipe.ml` with extra inline
commentary explaining each piece in light of the discussion above.

```ocaml
(* solver_pipe.ml — talk to Z3 / CVC5 over pipes from OCaml.

   Build & run:
     ocamlfind ocamlopt -package unix -linkpkg solver_pipe.ml -o solver_pipe
     ./solver_pipe

   Or as a script (no compilation):
     ocaml unix.cma solver_pipe.ml
*)

(* Absolute paths to the two solver binaries we want to drive. Hard-coded
   here for the experiment; in real code you'd take these from a config
   file, an env var, or PATH lookup. *)
let z3_path   = "/local/scratch/nk480/smt-solvers/install/bin/z3"
let cvc5_path = "/local/scratch/nk480/smt-solvers/install/bin/cvc5"

(* Everything we need to talk to a single running solver:
   - [name] is just for logging,
   - [pid]  lets us [waitpid] on it during cleanup,
   - [ic]   is the channel we READ from (the solver's stdout),
   - [oc]   is the channel we WRITE to (the solver's stdin).

   Note the channel directions are reversed from the child's point of view:
   what is "in" for the child is "out" for us, and vice versa. *)
type solver = {
  name : string;
  pid  : int;
  ic   : in_channel;
  oc   : out_channel;
}

(* Spawn a solver as a child process and return a [solver] record wired up
   to its stdin/stdout. We use [Unix.create_process] (rather than the
   higher-level [Unix.open_process]) so we have explicit control over which
   file descriptors the child inherits, which makes it easy to merge the
   child's stderr into its stdout. *)
let spawn name argv =
  (* Two pipes: one for parent->child commands, one for child->parent
     responses. [cloexec:true] makes sure these fds aren't accidentally
     leaked into any further child processes we might spawn later. *)
  let child_stdin_r,  child_stdin_w  = Unix.pipe ~cloexec:true () in
  let child_stdout_r, child_stdout_w = Unix.pipe ~cloexec:true () in
  (* Fork+exec the solver. The three fd arguments after [argv] become the
     child's stdin, stdout, and stderr respectively. We pass
     [child_stdout_w] for BOTH stdout and stderr so that any error messages
     or warnings the solver prints land in the same stream we're reading
     from — otherwise stderr would just spill onto our terminal and we'd
     never see it programmatically. *)
  let pid =
    Unix.create_process argv.(0) argv
      child_stdin_r child_stdout_w child_stdout_w
  in
  (* The parent doesn't need the child's ends of the pipes — only the
     child does. Close them in the parent so that EOF propagates correctly
     when the child exits, and so we don't leak fds. *)
  Unix.close child_stdin_r;
  Unix.close child_stdout_w;
  { name;
    pid;
    ic = Unix.in_channel_of_descr child_stdout_r;
    oc = Unix.out_channel_of_descr child_stdin_w;
  }

(* The framing marker. Both solvers will [echo] this string back to us so
   we know when a batch of commands is finished producing output. Pick
   something a normal SMT-LIB response could never produce; <<<...>>> is
   well outside the syntax for sat/unsat/unknown/values/models/errors. *)
let sentinel = "<<<SOLVER_SYNC>>>"

(* Recognise the sentinel line in the solver's output stream.

   Z3 and CVC5 disagree on how (echo "...") prints:
     Z3:   prints the string contents bare       -> <<<SOLVER_SYNC>>>
     CVC5: prints the string with its quotes     -> "<<<SOLVER_SYNC>>>"
   so we accept either form. *)
let is_sentinel line =
  String.equal line sentinel
  || String.equal line ("\"" ^ sentinel ^ "\"")

(* Send a chunk of commands and read everything the solver prints back.

   This is the heart of the framing trick. We:
     1. write the user's commands,
     2. append (echo "<sentinel>") so the solver will print a marker
        AFTER it finishes processing the user's commands (SMT-LIB
        commands are processed strictly in order, so the echo cannot
        be reordered ahead of any earlier output),
     3. flush so the bytes actually leave our process,
     4. read lines until we see the sentinel,
     5. return everything we read before it.

   Why the flush is critical: OCaml's [out_channel] is line-buffered when
   connected to a TTY but BLOCK-buffered when connected to a pipe. Without
   the explicit [flush], our commands would sit in the OCaml buffer
   forever and the solver would never see them — a deadlock where both
   sides are waiting for the other to speak first.

   This loop will block indefinitely if the solver hangs on a hard
   query. Adding a timeout means switching from [input_line] to a
   non-blocking read on the underlying fd via [Unix.select] with a
   deadline; left out here for clarity. *)
let query s cmds =
  output_string s.oc cmds;
  output_char s.oc '\n';
  Printf.fprintf s.oc "(echo \"%s\")\n" sentinel;
  flush s.oc;
  let buf = Buffer.create 256 in
  let rec loop () =
    match input_line s.ic with
    | line when is_sentinel line ->
        (* Done — the sentinel is consumed but NOT included in the output. *)
        ()
    | line ->
        Buffer.add_string buf line;
        Buffer.add_char buf '\n';
        loop ()
    | exception End_of_file ->
        (* The solver closed its stdout before printing the sentinel.
           Usually this means it crashed or got killed; in real code
           you'd want to surface this as an error rather than a string. *)
        Buffer.add_string buf "<EOF before sentinel>\n"
  in
  loop ();
  Buffer.contents buf

(* Cleanly shut a solver down. We:
   - politely send (exit) so the solver gets a chance to flush stats etc.,
   - close our end of its stdin so it sees EOF if it's still reading,
   - close the read channel,
   - reap the child with [waitpid] to avoid leaving a zombie.
   The [try ... with _] wrappers are because the channels may already be
   closed if the solver died early. *)
let close s =
  (try
    output_string s.oc "(exit)\n";
    flush s.oc;
    close_out s.oc
  with _ -> ());
  (try close_in s.ic with _ -> ());
  let _, _ = Unix.waitpid [] s.pid in
  ()

(* ---- pretty-printing helpers for the demo ---- *)

let banner name =
  print_endline "";
  print_endline (String.make 60 '=');
  Printf.printf "  %s\n" name;
  print_endline (String.make 60 '=')

let show label body =
  Printf.printf "--- %s ---\n%s%!" label body

(* Run an identical sequence of SMT-LIB commands against any solver and
   print the response after each batch. The empty bodies after [set-option]
   and [declare + assert] are not bugs — they're a direct demonstration
   that the sentinel framing handles silent commands correctly: the loop
   reads zero lines before the sentinel, returns "", and we move on
   without blocking. *)
let exercise s =
  banner s.name;
  show "set-option produce-models"
    (query s "(set-option :produce-models true)");
  show "declare + assert"
    (query s "(declare-const x Int)\n\
              (declare-const y Int)\n\
              (assert (= x 5))\n\
              (assert (= y (+ x 1)))");
  show "check-sat"
    (query s "(check-sat)");
  show "get-value"
    (query s "(get-value (x y (+ x y)))");
  show "push / contradict / check / pop"
    (query s "(push 1)\n\
              (assert (= x 6))\n\
              (check-sat)\n\
              (pop 1)");
  show "check-sat after pop"
    (query s "(check-sat)");
  close s

(* Entry point. Note the solver-specific argv:
     Z3 needs [-in] to read SMT-LIB from stdin (otherwise it would
       interpret the argument list as filenames).
     CVC5 needs [-q] to suppress the multi-page banner that would
       otherwise be the first thing we read, and [--incremental] for
       [push]/[pop] support. (Z3 is always incremental.)
   Model generation is on by default in Z3 but must be turned on
   explicitly in CVC5; both versions get the same
   (set-option :produce-models true) inside [exercise], which is a no-op
   on Z3 and required on CVC5. *)
let () =
  let z3   = spawn "Z3"   [| z3_path; "-in" |] in
  let cvc5 = spawn "CVC5" [| cvc5_path; "-q"; "--incremental" |] in
  exercise z3;
  exercise cvc5
```
