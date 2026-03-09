type t = string

let of_string s = s
let to_string s = s
let compare = String.compare
let print fmt s = Format.fprintf fmt "%s" s

module Test = struct
  let gen =
    let open QCheck.Gen in
    let lower = map Char.chr (97 -- 122) in
    let rest_char = oneof [
      map Char.chr (97 -- 122);
      map Char.chr (48 -- 57);
      pure '_';
    ] in
    let* first = lower in
    let* rest = list_size (0 -- 8) rest_char in
    pure (String.init (1 + List.length rest) (fun i ->
      if Int.compare i 0 = 0 then first else List.nth rest (i - 1)))

  let test =
    [ QCheck.Test.make ~name:"variable roundtrip"
        ~count:100
        (QCheck.make gen)
        (fun v -> String.compare (to_string v) (to_string (of_string (to_string v))) = 0)
    ]
end
