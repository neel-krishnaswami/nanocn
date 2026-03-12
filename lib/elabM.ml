type 'a t = Var.supply -> ('a * Var.supply, string) result

let return x supply = Ok (x, supply)

let ( let* ) m f supply =
  match m supply with
  | Error e -> Error e
  | Ok (a, supply') -> f a supply'

let fail msg _supply = Error msg

let fresh pos supply =
  let (v, supply') = Var.fresh pos supply in
  Ok (v, supply')

let run m =
  match m Var.empty_supply with
  | Ok (a, _) -> Ok a
  | Error e -> Error e

module Test = struct
  let test =
    [ QCheck.Test.make ~name:"elabM fresh generates distinct variables"
        ~count:1
        QCheck.unit
        (fun () ->
           match run (
             let* v1 = fresh SourcePos.dummy in
             let* v2 = fresh SourcePos.dummy in
             return (Var.compare v1 v2 <> 0)
           ) with
           | Ok b -> b
           | Error _ -> false);

      QCheck.Test.make ~name:"elabM fail propagates"
        ~count:1
        QCheck.unit
        (fun () ->
           match run (
             let* _ = fail "test error" in
             return 42
           ) with
           | Ok _ -> false
           | Error msg -> String.equal msg "test error");
    ]
end
