type t = Pure | Impure

let sub e1 e2 =
  match e1, e2 with
  | Pure, _ -> true
  | Impure, Impure -> true
  | Impure, Pure -> false

let join e1 e2 =
  match e1, e2 with
  | Pure, Pure -> Pure
  | _ -> Impure

let compare e1 e2 =
  match e1, e2 with
  | Pure, Pure -> 0
  | Pure, Impure -> -1
  | Impure, Pure -> 1
  | Impure, Impure -> 0

let print fmt = function
  | Pure -> Format.fprintf fmt "pure"
  | Impure -> Format.fprintf fmt "impure"

module Test = struct
  let gen =
    let open QCheck.Gen in
    oneofl [ Pure; Impure ]

  let test =
    [ QCheck.Test.make ~name:"sub is reflexive"
        ~count:10
        (QCheck.make gen)
        (fun e -> sub e e);
      QCheck.Test.make ~name:"pure sub effectful"
        ~count:1
        QCheck.unit
        (fun () -> sub Pure Impure);
      QCheck.Test.make ~name:"not effectful sub pure"
        ~count:1
        QCheck.unit
        (fun () -> not (sub Impure Pure));
      let arb = QCheck.make gen in
      QCheck.Test.make ~name:"join is commutative"
        ~count:10
        (QCheck.pair arb arb)
        (fun (e1, e2) -> compare (join e1 e2) (join e2 e1) = 0);
    ]
end
