type t = string

let is_valid s =
  String.length s >= 1
  && Char.compare 'a' s.[0] <= 0
  && Char.compare s.[0] 'z' <= 0
  && String.to_seq s |> Seq.drop 1 |> Seq.for_all (fun c ->
       (Char.compare 'a' c <= 0 && Char.compare c 'z' <= 0)
       || (Char.compare 'A' c <= 0 && Char.compare c 'Z' <= 0)
       || (Char.compare '0' c <= 0 && Char.compare c '9' <= 0)
       || Char.compare c '_' = 0
       || Char.compare c '\'' = 0)

let of_string s =
  if is_valid s then Ok s
  else Error (Format.asprintf "invalid datasort name: %s" s)

let to_string s = s
let compare = String.compare
let print fmt s = Format.fprintf fmt "%s" s
let json s = Json.String s

module Test = struct
  let gen =
    let open QCheck.Gen in
    let lower = map Char.chr (97 -- 122) in
    let rest_char = oneof [
      map Char.chr (97 -- 122);
      map Char.chr (65 -- 90);
      map Char.chr (48 -- 57);
      pure '_';
    ] in
    let* first = lower in
    let* rest = list_size (0 -- 8) rest_char in
    pure (String.init (1 + List.length rest) (fun i ->
      if Int.compare i 0 = 0 then first else List.nth rest (i - 1)))

  let test =
    [ QCheck.Test.make ~name:"generated dsort names are valid"
        ~count:100
        (QCheck.make gen)
        (fun s -> match of_string s with Ok _ -> true | Error _ -> false)
    ]
end
