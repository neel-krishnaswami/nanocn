type kind =
  | K_sort_mismatch of
      { expected : Sort.sort
      ; actual : Sort.sort
      ; diff : SortDiff.shape_compare }
  | K_annotation_disagrees of
      { inner_sort : Sort.sort
      ; annot : Sort.sort
      ; diff : SortDiff.shape_compare }

type t =
  | Legacy of SourcePos.t option * string
  | Structured of { loc : SourcePos.t; kind : kind }

let legacy pos msg = Legacy (pos, msg)
let structured ~loc kind = Structured { loc; kind }

let sort_mismatch ~loc ~expected ~actual =
  let diff = SortDiff.diff expected actual in
  structured ~loc (K_sort_mismatch { expected; actual; diff })

let annotation_disagrees ~loc ~inner ~annot =
  let diff = SortDiff.diff inner annot in
  structured ~loc
    (K_annotation_disagrees { inner_sort = inner; annot; diff })

let loc = function
  | Legacy (pos, _) -> pos
  | Structured { loc; _ } -> Some loc

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

(* Helpers for the Structured printer. *)
let print_location reg fmt pos =
  Format.fprintf fmt "@ at %a:@ " SourcePos.print pos;
  SourceExcerpt.excerpt reg ~context:1 pos fmt;
  Format.pp_print_cut fmt ()

let print_kind fmt = function
  | K_sort_mismatch { diff; _ } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  expected: @[%a@]" SortDiff.print_left diff;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  actual:   @[%a@]" SortDiff.print_right diff;
    Format.fprintf fmt "@]"
  | K_annotation_disagrees { diff; _ } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  the annotation disagrees with the expression's sort.";
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  annotation: @[%a@]" SortDiff.print_right diff;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  expression: @[%a@]" SortDiff.print_left diff;
    Format.fprintf fmt "@]"

let kind_header = function
  | K_sort_mismatch _ -> "Type error: sort mismatch"
  | K_annotation_disagrees _ -> "Type error: annotation mismatch"

let rec to_string e =
  print_to_buffer (fun fmt -> print (SourceExcerpt.create ()) fmt e)

and print reg fmt = function
  | Legacy (pos_opt, msg) ->
    (* The header for [Legacy] is deliberately generic ("Error")
       because it wraps a pre-formatted string that could come from
       the parser or any check path. Structured variants use more
       specific headers. *)
    Format.fprintf fmt "@[<v>";
    print_header fmt "Error";
    (match pos_opt with
     | Some pos -> print_location reg fmt pos
     | None -> Format.pp_print_cut fmt ());
    Format.fprintf fmt "  %s" msg;
    Format.fprintf fmt "@]"
  | Structured { loc; kind } ->
    Format.fprintf fmt "@[<v>";
    print_header fmt (kind_header kind);
    print_location reg fmt loc;
    print_kind fmt kind;
    Format.fprintf fmt "@]"

module Test = struct
  let contains_substring s sub =
    let n = String.length s and m = String.length sub in
    let rec aux i = i + m <= n && (String.sub s i m = sub || aux (i+1)) in
    m = 0 || (n >= m && aux 0)

  let test_legacy_preserves_message =
    QCheck.Test.make ~name:"TypeError: Legacy preserves its message"
      ~count:1
      QCheck.unit
      (fun () ->
        let e = legacy None "hello" in
        contains_substring (to_string e) "hello")

  let test_sort_mismatch_mentions_both =
    QCheck.Test.make
      ~name:"TypeError: sort_mismatch printer mentions expected and actual"
      ~count:1
      QCheck.unit
      (fun () ->
        let mk s =
          Sort.mk (object method loc = SourcePos.dummy end) s in
        let int_s = mk Sort.Int in
        let bool_s = mk Sort.Bool in
        let e = sort_mismatch ~loc:SourcePos.dummy
                  ~expected:int_s ~actual:bool_s in
        let s = to_string e in
        contains_substring s "sort mismatch"
        && contains_substring s "expected"
        && contains_substring s "actual")

  let test = [
    test_legacy_preserves_message;
    test_sort_mismatch_mentions_both;
  ]
end
