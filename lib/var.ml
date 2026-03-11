type name =
  | User of string
  | Generated of int

type t = {
  name : name;
  binding_site : SourcePos.t;
}

let of_string s pos = { name = User s; binding_site = pos }

let to_string v =
  match v.name with
  | User s -> s
  | Generated n -> "_v" ^ string_of_int n

let compare a b =
  match a.name, b.name with
  | User s1, User s2 -> String.compare s1 s2
  | Generated n1, Generated n2 -> Int.compare n1 n2
  | User _, Generated _ -> -1
  | Generated _, User _ -> 1

let print fmt v = Format.fprintf fmt "%s" (to_string v)

let is_generated v =
  match v.name with
  | User _ -> false
  | Generated _ -> true

let binding_site v = v.binding_site

type supply = int

let empty_supply = 0

let fresh pos n =
  ({ name = Generated n; binding_site = pos }, n + 1)

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
    let s = String.init (1 + List.length rest) (fun i ->
      if Int.compare i 0 = 0 then first else List.nth rest (i - 1))
    in
    pure (of_string s SourcePos.dummy)

  let test =
    [ QCheck.Test.make ~name:"variable roundtrip"
        ~count:100
        (QCheck.make gen)
        (fun v -> String.compare (to_string v) (to_string (of_string (to_string v) SourcePos.dummy)) = 0)
    ]
end
