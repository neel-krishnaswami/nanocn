type t =
  | Wild
  | Ctor of Label.t * t
  | Tuple of t list

let rec print fmt = function
  | Wild -> Format.pp_print_string fmt "_"
  | Ctor (l, Wild) ->
    Format.pp_open_stag fmt (Format.String_tag "red;bold");
    Label.print fmt l;
    Format.pp_close_stag fmt ();
    Format.fprintf fmt " _"
  | Ctor (l, inner) ->
    Format.pp_open_stag fmt (Format.String_tag "red;bold");
    Label.print fmt l;
    Format.pp_close_stag fmt ();
    Format.fprintf fmt " %a" print inner
  | Tuple [] -> Format.pp_print_string fmt "()"
  | Tuple ws ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         print)
      ws

module Test = struct
  let contains s sub =
    let n = String.length s and m = String.length sub in
    let rec aux i = i + m <= n && (String.sub s i m = sub || aux (i+1)) in
    m = 0 || (n >= m && aux 0)

  let to_string w =
    let buf = Buffer.create 16 in
    let fmt = Format.formatter_of_buffer buf in
    print fmt w;
    Format.pp_print_flush fmt ();
    Buffer.contents buf

  let wild_prints_underscore =
    QCheck.Test.make ~name:"PatWitness: Wild renders as _"
      ~count:1 QCheck.unit
      (fun () -> to_string Wild = "_")

  let tuple_prints_commas =
    QCheck.Test.make ~name:"PatWitness: Tuple renders commas"
      ~count:1 QCheck.unit
      (fun () ->
        let s = to_string (Tuple [Wild; Wild; Wild]) in
        contains s "(" && contains s "," && contains s ")")

  let ctor_prints_label =
    QCheck.Test.make ~name:"PatWitness: Ctor mentions its label"
      ~count:1 QCheck.unit
      (fun () ->
        let l = match Label.of_string "Cons" with
          | Ok l -> l | Error _ -> failwith "bad label" in
        let s = to_string (Ctor (l, Tuple [Wild; Wild])) in
        contains s "Cons")

  let test = [ wild_prints_underscore; tuple_prints_commas; ctor_prints_label ]
end
