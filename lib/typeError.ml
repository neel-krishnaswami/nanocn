(* Phase 1: only the Legacy bridge is populated. The Structured variant
   will be added as per-premise migrations land in Phases 2–5. *)

type t =
  | Legacy of SourcePos.t option * string

let legacy pos msg = Legacy (pos, msg)

let loc = function
  | Legacy (pos, _) -> pos

(* Tag names here are ocolor tag names (see Ocolor_format.mli): once
   the formatter has been prettified via [ErrorRender.configure_formatter],
   these render as the corresponding ANSI styles; on non-tty output
   (configured with [`Never]) they become no-ops and the text prints
   plain. *)
let print_header fmt s =
  Format.pp_open_stag fmt (Format.String_tag "red;bold");
  Format.pp_print_string fmt s;
  Format.pp_close_stag fmt ()

let print_to_buffer f =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  f fmt;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let rec to_string e =
  print_to_buffer (fun fmt -> print (SourceExcerpt.create ()) fmt e)

and print reg fmt = function
  | Legacy (pos_opt, msg) ->
    (* Phase 1: the header is deliberately generic ("Error") because
       [Legacy] wraps a pre-formatted string that could come from the
       parser, the surface checker, or the refined checker. Phase 2+
       will introduce variant-specific headers (e.g. "Type error:
       sort mismatch"). *)
    Format.fprintf fmt "@[<v>";
    print_header fmt "Error";
    (match pos_opt with
     | Some pos ->
       Format.fprintf fmt "@ at %a:@ " SourcePos.print pos;
       SourceExcerpt.excerpt reg ~context:1 pos fmt;
       Format.pp_print_cut fmt ()
     | None ->
       Format.pp_print_cut fmt ());
    Format.fprintf fmt "  %s" msg;
    Format.fprintf fmt "@]"

module Test = struct
  let test_legacy_preserves_message =
    QCheck.Test.make ~name:"TypeError: Legacy preserves its message"
      ~count:1
      QCheck.unit
      (fun () ->
        let e = legacy None "hello" in
        let buf = Buffer.create 16 in
        let fmt = Format.formatter_of_buffer buf in
        print (SourceExcerpt.create ()) fmt e;
        Format.pp_print_flush fmt ();
        let s = Buffer.contents buf in
        let contains_substring s sub =
          let n = String.length s and m = String.length sub in
          let rec aux i = i + m <= n && (String.sub s i m = sub || aux (i+1)) in
          m = 0 || (n >= m && aux 0)
        in
        contains_substring s "hello")

  let test = [ test_legacy_preserves_message ]
end
