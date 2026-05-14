type state = Var.supply

type 'a t = state -> 'a * state

let return x s = (x, s)

let ( let* ) m f s =
  let (a, s') = m s in
  f a s'

let fresh pos s =
  Var.fresh pos s

let mk_var name pos s =
  Var.mk name pos s

let rec sequence = function
  | [] -> return []
  | m :: ms ->
    let* x = m in
    let* xs = sequence ms in
    return (x :: xs)

let run supply m =
  let (a, supply') = m supply in
  (a, supply')

module Test = struct
  let test =
    [ QCheck.Test.make ~name:"elabM fresh generates distinct variables"
        ~count:1
        QCheck.unit
        (fun () ->
           let (b, _) = run Var.empty_supply (
             let* v1 = fresh SourcePos.dummy in
             let* v2 = fresh SourcePos.dummy in
             return (Var.compare v1 v2 <> 0)
           ) in
           b);
    ]
end
