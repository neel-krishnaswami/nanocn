type t = Pure | Effectful

let sub e1 e2 =
  match e1, e2 with
  | Pure, _ -> true
  | Effectful, Effectful -> true
  | Effectful, Pure -> false

let join e1 e2 =
  match e1, e2 with
  | Pure, Pure -> Pure
  | _ -> Effectful

let compare e1 e2 =
  match e1, e2 with
  | Pure, Pure -> 0
  | Pure, Effectful -> -1
  | Effectful, Pure -> 1
  | Effectful, Effectful -> 0

let print fmt = function
  | Pure -> Format.fprintf fmt "pure"
  | Effectful -> Format.fprintf fmt "effectful"

module Test = struct
  let gen =
    let open QCheck.Gen in
    oneofl [ Pure; Effectful ]

  let test =
    [ QCheck.Test.make ~name:"sub is reflexive"
        ~count:10
        (QCheck.make gen)
        (fun e -> sub e e);
      QCheck.Test.make ~name:"pure sub effectful"
        ~count:1
        QCheck.unit
        (fun () -> sub Pure Effectful);
      QCheck.Test.make ~name:"not effectful sub pure"
        ~count:1
        QCheck.unit
        (fun () -> not (sub Effectful Pure));
      let arb = QCheck.make gen in
      QCheck.Test.make ~name:"join is commutative"
        ~count:10
        (QCheck.pair arb arb)
        (fun (e1, e2) -> compare (join e1 e2) (join e2 e1) = 0);
    ]
end
