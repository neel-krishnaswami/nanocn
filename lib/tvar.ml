type t = string

let of_string s = s
let to_string s = s
let compare = String.compare
let print fmt s = Format.fprintf fmt "%s" s

module Test = struct
  let gen =
    let open QCheck.Gen in
    let lower = map Char.chr (97 -- 122) in
    map (fun c -> String.make 1 c) lower

  let test =
    [ QCheck.Test.make ~name:"tvar roundtrip"
        ~count:100
        (QCheck.make gen)
        (fun v -> String.compare (to_string v) (to_string (of_string (to_string v))) = 0)
    ]
end
