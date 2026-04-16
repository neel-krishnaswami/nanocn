(* [Format.formatter] exposes no way to retrieve its underlying output
   channel, so tty detection happens by comparing the formatter against
   the two built-ins we know about. It's a best-effort check; callers
   who need a different policy can pass [?force]. *)
let is_tty fmt =
  if fmt == Format.err_formatter then
    (try Unix.isatty Unix.stderr with _ -> false)
  else if fmt == Format.std_formatter then
    (try Unix.isatty Unix.stdout with _ -> false)
  else false

let configure_formatter ?(force = `Auto) fmt =
  let enable =
    match force with
    | `Always -> true
    | `Never -> false
    | `Auto -> is_tty fmt
  in
  if enable then
    Ocolor_format.prettify_formatter fmt
  else begin
    (* Ensure tags still parse (so printers can emit them
       unconditionally) but produce no output. Format's default is
       `mark_tags = false` which already turns the stag calls into
       no-ops at mark time. Setting [print_tags] to [false] ensures
       nothing leaks out as raw tag text if some code path calls
       pp_print_as_string with an unknown stag. *)
    Format.pp_set_mark_tags fmt false;
    Format.pp_set_print_tags fmt false
  end
