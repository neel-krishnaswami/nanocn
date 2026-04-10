type t = Pure | Impure | Spec

let sub e1 e2 =
  match e1, e2 with
  | Pure, _ -> true
  | Impure, Impure -> true
  | Spec, Spec -> true
  | Impure, (Pure | Spec) -> false
  | Spec, (Pure | Impure) -> false

let join e1 e2 =
  match e1, e2 with
  | Pure, x | x, Pure -> Some x
  | Impure, Impure -> Some Impure
  | Spec, Spec -> Some Spec
  | Impure, Spec | Spec, Impure -> None

let purify = function
  | Pure -> Pure
  | Impure -> Pure
  | Spec -> Spec

let compare e1 e2 =
  let tag = function Pure -> 0 | Impure -> 1 | Spec -> 2 in
  Int.compare (tag e1) (tag e2)

let print fmt = function
  | Pure -> Format.fprintf fmt "pure"
  | Impure -> Format.fprintf fmt "impure"
  | Spec -> Format.fprintf fmt "spec"

let json = function
  | Pure -> Json.String "pure"
  | Impure -> Json.String "impure"
  | Spec -> Json.String "spec"

module Test = struct
  let gen =
    let open QCheck.Gen in
    oneof_list [ Pure; Impure; Spec ]

  let test =
    [ QCheck.Test.make ~name:"sub is reflexive"
        ~count:10
        (QCheck.make gen)
        (fun e -> sub e e);
      QCheck.Test.make ~name:"pure sub impure"
        ~count:1
        QCheck.unit
        (fun () -> sub Pure Impure);
      QCheck.Test.make ~name:"pure sub spec"
        ~count:1
        QCheck.unit
        (fun () -> sub Pure Spec);
      QCheck.Test.make ~name:"not impure sub pure"
        ~count:1
        QCheck.unit
        (fun () -> not (sub Impure Pure));
      QCheck.Test.make ~name:"not impure sub spec"
        ~count:1
        QCheck.unit
        (fun () -> not (sub Impure Spec));
      QCheck.Test.make ~name:"not spec sub impure"
        ~count:1
        QCheck.unit
        (fun () -> not (sub Spec Impure));
      QCheck.Test.make ~name:"purify impure is pure"
        ~count:1
        QCheck.unit
        (fun () -> compare (purify Impure) Pure = 0);
      QCheck.Test.make ~name:"purify spec is spec"
        ~count:1
        QCheck.unit
        (fun () -> compare (purify Spec) Spec = 0);
      let arb = QCheck.make gen in
      QCheck.Test.make ~name:"join is commutative"
        ~count:10
        (QCheck.pair arb arb)
        (fun (e1, e2) ->
           match join e1 e2, join e2 e1 with
           | Some a, Some b -> compare a b = 0
           | None, None -> true
           | _ -> false);
    ]
end
