type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Object of (string * t) list

let print_string_escaped fmt s =
  Format.fprintf fmt "\"";
  String.iter (fun c ->
    match c with
    | '"' -> Format.fprintf fmt "\\\""
    | '\\' -> Format.fprintf fmt "\\\\"
    | '\n' -> Format.fprintf fmt "\\n"
    | '\t' -> Format.fprintf fmt "\\t"
    | '\r' -> Format.fprintf fmt "\\r"
    | c when Char.code c < 0x20 ->
      Format.fprintf fmt "\\u%04x" (Char.code c)
    | c -> Format.fprintf fmt "%c" c
  ) s;
  Format.fprintf fmt "\""

let rec print fmt = function
  | Null -> Format.fprintf fmt "null"
  | Bool b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Int n -> Format.fprintf fmt "%d" n
  | Float f -> Format.fprintf fmt "%g" f
  | String s -> print_string_escaped fmt s
  | Array [] -> Format.fprintf fmt "[]"
  | Array items ->
    Format.fprintf fmt "@[<hv 2>[%a@]@,]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt v -> Format.fprintf fmt "@ %a" print v))
      items
  | Object [] -> Format.fprintf fmt "{}"
  | Object fields ->
    Format.fprintf fmt "@[<hv 2>{%a@]@,}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (k, v) ->
            Format.fprintf fmt "@ %a: %a" print_string_escaped k print v))
      fields

module Test = struct
  let gen =
    let open QCheck.Gen in
    sized @@ fix (fun self n ->
      if n <= 0 then
        oneof [
          pure Null;
          map (fun b -> Bool b) bool;
          map (fun n -> Int n) (0 -- 100);
          map (fun s -> String s) (pure "hello");
        ]
      else
        let sub = self (n / 3) in
        oneof [
          pure Null;
          map (fun b -> Bool b) bool;
          map (fun n -> Int n) (0 -- 100);
          map (fun items -> Array items) (list_size (0 -- 3) sub);
          (let* fields = list_size (0 -- 3)
             (let* v = sub in pure ("key", v)) in
           pure (Object fields));
        ])

  let test =
    [ QCheck.Test.make ~name:"json print does not raise"
        ~count:100
        (QCheck.make gen)
        (fun j ->
           let _ = Format.asprintf "%a" print j in
           true);
    ]
end
