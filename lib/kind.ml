type t = Type | Sort

let compare k1 k2 =
  match k1, k2 with
  | Type, Type -> 0
  | Type, Sort -> -1
  | Sort, Type -> 1
  | Sort, Sort -> 0

let subkind k1 k2 =
  match k1, k2 with
  | Type, Type -> true
  | Type, Sort -> true
  | Sort, Sort -> true
  | Sort, Type -> false

let print fmt = function
  | Type -> Format.fprintf fmt "type"
  | Sort -> Format.fprintf fmt "sort"

module Test = struct
  let gen =
    let open QCheck.Gen in
    oneof [pure Type; pure Sort]

  let test =
    [ QCheck.Test.make ~name:"kind compare is reflexive"
        ~count:10
        (QCheck.make gen)
        (fun k -> compare k k = 0);

      QCheck.Test.make ~name:"kind subkind is reflexive"
        ~count:10
        (QCheck.make gen)
        (fun k -> subkind k k);

      QCheck.Test.make ~name:"type subkind sort"
        ~count:1
        (QCheck.make (QCheck.Gen.pure ()))
        (fun () -> subkind Type Sort);

      QCheck.Test.make ~name:"sort not subkind type"
        ~count:1
        (QCheck.make (QCheck.Gen.pure ()))
        (fun () -> not (subkind Sort Type));
    ]
end
