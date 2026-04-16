type 'a t = Var.supply -> ('a * Var.supply, TypeError.t) result

let return x supply = Ok (x, supply)

let ( let* ) m f supply =
  match m supply with
  | Error e -> Error e
  | Ok (a, supply') -> f a supply'

let fail err _supply = Error err

let lift r supply =
  match r with
  | Ok x -> Ok (x, supply)
  | Error e -> Error e

let from_supply f = f

let fresh pos supply =
  let (v, supply') = Var.fresh pos supply in
  Ok (v, supply')

let mk_var name pos supply =
  let (v, supply') = Var.mk name pos supply in
  Ok (v, supply')

let rec sequence = function
  | [] -> return []
  | m :: ms ->
    let* x = m in
    let* xs = sequence ms in
    return (x :: xs)

let run supply m =
  match m supply with
  | Ok (a, supply') -> Ok (a, supply')
  | Error e -> Error e

module Test = struct
  let test =
    [ QCheck.Test.make ~name:"elabM fresh generates distinct variables"
        ~count:1
        QCheck.unit
        (fun () ->
           match run Var.empty_supply (
             let* v1 = fresh SourcePos.dummy in
             let* v2 = fresh SourcePos.dummy in
             return (Var.compare v1 v2 <> 0)
           ) with
           | Ok (b, _) -> b
           | Error _ -> false);

      QCheck.Test.make ~name:"elabM fail propagates the error"
        ~count:1
        QCheck.unit
        (fun () ->
           let err =
             TypeError.parse_error ~loc:None ~msg:"test error"
           in
           match run Var.empty_supply (
             let* _ = fail err in
             return 42
           ) with
           | Ok _ -> false
           | Error _ -> true);
    ]
end
