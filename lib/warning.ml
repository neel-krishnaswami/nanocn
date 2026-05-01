type kind =
  | K_pat_var_shadowed of { name : string }

type t = {
  loc : SourcePos.t;
  kind : kind;
}

let structured ~loc kind = { loc; kind }

let pat_var_shadowed ~loc ~name =
  structured ~loc (K_pat_var_shadowed { name })

let loc w = w.loc

let print_header fmt s =
  Format.pp_open_stag fmt (Format.String_tag "yellow;bold");
  Format.pp_print_string fmt s;
  Format.pp_close_stag fmt ()

let print_emph pp fmt x =
  Format.pp_open_stag fmt (Format.String_tag "yellow;bold");
  pp fmt x;
  Format.pp_close_stag fmt ()

let kind_header = function
  | K_pat_var_shadowed _ -> "Warning: shadowed pattern variable"

let print_kind fmt = function
  | K_pat_var_shadowed { name } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt
      "  the pattern variable %a is shadowed by a later binding"
      (print_emph Format.pp_print_string) name;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt
      "  in the same pattern, so its binding is unreachable.";
    Format.fprintf fmt "@]"

let print_location reg fmt pos =
  Format.fprintf fmt "@ at %a:@ " SourcePos.print pos;
  SourceExcerpt.excerpt reg ~context:1 pos fmt;
  Format.pp_print_cut fmt ()

let print reg fmt w =
  Format.fprintf fmt "@[<v>";
  print_header fmt (kind_header w.kind);
  print_location reg fmt w.loc;
  print_kind fmt w.kind;
  Format.fprintf fmt "@]"

let to_string w =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  print (SourceExcerpt.create ()) fmt w;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

module Test = struct
  let contains_substring s sub =
    let n = String.length s and m = String.length sub in
    let rec aux i = i + m <= n && (String.sub s i m = sub || aux (i+1)) in
    m = 0 || (n >= m && aux 0)

  let test_pat_var_shadowed_mentions_name =
    QCheck.Test.make
      ~name:"Warning: pat_var_shadowed mentions the variable name"
      ~count:1
      QCheck.unit
      (fun () ->
         let w =
           pat_var_shadowed ~loc:SourcePos.dummy ~name:"shadowme" in
         let s = to_string w in
         contains_substring s "shadowme"
         && contains_substring s "shadowed")

  let test = [
    test_pat_var_shadowed_mentions_name;
  ]
end
