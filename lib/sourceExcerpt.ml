type registry = (string, string array) Hashtbl.t
(* File name -> its contents split into lines (no trailing newline).
   We store an array for O(1) line access. Mutable; driver populates it
   once per file. *)

let create () : registry = Hashtbl.create 8

let split_lines (source : string) : string array =
  (* String.split_on_char keeps an empty trailing element if source
     ends with '\n'; drop it so line counts match editor expectations. *)
  let lines = String.split_on_char '\n' source in
  let lines =
    match List.rev lines with
    | "" :: rest -> List.rev rest
    | _ -> lines
  in
  Array.of_list lines

let register reg ~file ~source =
  Hashtbl.replace reg file (split_lines source)

(* 1-indexed line access with bounds check. *)
let get_line lines n =
  if n >= 1 && n <= Array.length lines then Some lines.(n - 1) else None

let excerpt reg ~context pos fmt =
  match Hashtbl.find_opt reg (SourcePos.file pos) with
  | None -> ()
  | Some lines ->
    let start_line = SourcePos.start_line pos in
    let end_line = SourcePos.end_line pos in
    let start_col = SourcePos.start_col pos in
    let end_col = SourcePos.end_col pos in
    let first = max 1 (start_line - context) in
    let last = min (Array.length lines) (end_line + context) in
    if last < first then ()
    else begin
      let gutter_width =
        String.length (string_of_int last)
      in
      Format.fprintf fmt "@[<v>";
      for n = first to last do
        match get_line lines n with
        | None -> ()
        | Some line ->
          (* Gutter: line number + " | " rendered faint (ocolor tag
             name — inert on non-tty). *)
          Format.pp_open_stag fmt (Format.String_tag "faint");
          Format.fprintf fmt "%*d | " gutter_width n;
          Format.pp_close_stag fmt ();
          if n < start_line || n > end_line then
            (* Context line: print verbatim. *)
            Format.pp_print_string fmt line
          else begin
            (* Line within the erroneous range. Split into pre/hl/post
               based on the start_col/end_col. Columns are 0-indexed,
               with start inclusive and end exclusive (Menhir's
               convention via Lexing.position). *)
            let len = String.length line in
            let lo = if n = start_line then min start_col len else 0 in
            let hi = if n = end_line   then min end_col   len else len in
            let lo = max 0 lo in
            let hi = max lo hi in
            let pre = String.sub line 0 lo in
            let mid = String.sub line lo (hi - lo) in
            let post = String.sub line hi (len - hi) in
            Format.pp_print_string fmt pre;
            (* Highlight: reverse video + red. Very visible and degrades
               to plain text when colour is off. *)
            Format.pp_open_stag fmt (Format.String_tag "red;reverse");
            (* If the range is empty (degenerate), still draw a caret
               marker so something renders. *)
            if String.length mid = 0 then
              Format.pp_print_string fmt "‸"
            else
              Format.pp_print_string fmt mid;
            Format.pp_close_stag fmt ();
            Format.pp_print_string fmt post
          end;
          if n < last then Format.pp_print_cut fmt ()
      done;
      Format.fprintf fmt "@]"
    end

module Test = struct
  let test_empty_registry =
    QCheck.Test.make ~name:"SourceExcerpt: unregistered file is a no-op"
      ~count:1
      QCheck.unit
      (fun () ->
        let reg = create () in
        let pos = SourcePos.create ~file:"nowhere.cn"
                    ~start_line:1 ~start_col:0
                    ~end_line:1 ~end_col:3 in
        let buf = Buffer.create 16 in
        let fmt = Format.formatter_of_buffer buf in
        excerpt reg ~context:1 pos fmt;
        Format.pp_print_flush fmt ();
        String.length (Buffer.contents buf) = 0)

  let contains_substring s sub =
    let n = String.length s and m = String.length sub in
    let rec aux i = i + m <= n && (String.sub s i m = sub || aux (i+1)) in
    m = 0 || (n >= m && aux 0)

  let test_context_lines_appear =
    QCheck.Test.make
      ~name:"SourceExcerpt: renders both the erroneous line and context"
      ~count:1
      QCheck.unit
      (fun () ->
        let reg = create () in
        register reg ~file:"a.cn"
          ~source:"let a = 1;\nlet b = 2;\nlet c = 3;\n";
        let pos = SourcePos.create ~file:"a.cn"
                    ~start_line:2 ~start_col:4
                    ~end_line:2 ~end_col:5 in
        let buf = Buffer.create 64 in
        let fmt = Format.formatter_of_buffer buf in
        excerpt reg ~context:1 pos fmt;
        Format.pp_print_flush fmt ();
        let s = Buffer.contents buf in
        (* The lines before, at, and after the error should all appear
           in the output. Tag markers have no effect on the default
           formatter. *)
        contains_substring s "let a = 1;"
        && contains_substring s "let b = 2;"
        && contains_substring s "let c = 3;")

  let test = [ test_empty_registry; test_context_lines_appear ]
end
