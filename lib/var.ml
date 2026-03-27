type name =
  | User of string * int
  | Generated of int

type t = {
  name : name;
  binding_site : SourcePos.t;
}

type supply = int

let empty_supply = 0

let mk s pos supply =
  ({ name = User (s, supply); binding_site = pos }, supply + 1)

let name v =
  match v.name with
  | User (s, _) -> s
  | Generated n -> "_v" ^ string_of_int n

let to_string v =
  match v.name with
  | User (s, n) -> s ^ "_" ^ string_of_int n
  | Generated n -> "_v" ^ string_of_int n

let id_of v =
  match v.name with
  | User (_, n) -> n
  | Generated n -> n

let compare a b =
  Int.compare (id_of a) (id_of b)

let print fmt v = Format.fprintf fmt "%s" (name v)
let print_unique fmt v = Format.fprintf fmt "%s" (to_string v)

let json v =
  Json.Object [
    "name", Json.String (to_string v);
    "pos", SourcePos.json v.binding_site;
  ]

let is_generated v =
  match v.name with
  | User _ -> false
  | Generated _ -> true

let binding_site v = v.binding_site

let fresh pos n =
  ({ name = Generated n; binding_site = pos }, n + 1)

module Test = struct
  let counter = ref 100_000

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
    let id = !counter in
    counter := !counter + 1;
    pure { name = User (s, id); binding_site = SourcePos.dummy }

  let test =
    [ QCheck.Test.make ~name:"variable name roundtrip"
        ~count:100
        (QCheck.make gen)
        (fun v ->
           let s = name v in
           let (v', _) = mk s SourcePos.dummy empty_supply in
           String.equal s (name v'))
    ]
end
