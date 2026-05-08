(** Internal state threaded through elaboration: a fresh-variable
    supply and a reverse-order list of accumulated warnings.  The
    list is reversed only at [run_full] / boundary readout so the
    monadic plumbing can prepend in O(1). *)
type state = {
  supply : Var.supply;
  warnings_rev : Warning.t list;
}

type 'a t = state -> 'a * state

let return x s = (x, s)

let ( let* ) m f s =
  let (a, s') = m s in
  f a s'

let fresh pos s =
  let (v, supply') = Var.fresh pos s.supply in
  (v, { s with supply = supply' })

let mk_var name pos s =
  let (v, supply') = Var.mk name pos s.supply in
  (v, { s with supply = supply' })

let record_warning w s =
  ((), { s with warnings_rev = w :: s.warnings_rev })

let rec sequence = function
  | [] -> return []
  | m :: ms ->
    let* x = m in
    let* xs = sequence ms in
    return (x :: xs)

let run supply m =
  let s0 = { supply; warnings_rev = [] } in
  let (a, s') = m s0 in
  (a, s'.supply)

let run_full supply m =
  let s0 = { supply; warnings_rev = [] } in
  let (a, s') = m s0 in
  (a, s'.supply, List.rev s'.warnings_rev)

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

      QCheck.Test.make
        ~name:"elabM record_warning surfaces in run_full output"
        ~count:1
        QCheck.unit
        (fun () ->
           let w1 =
             Warning.pat_var_shadowed ~loc:SourcePos.dummy ~name:"a" in
           let w2 =
             Warning.pat_var_shadowed ~loc:SourcePos.dummy ~name:"b" in
           match run_full Var.empty_supply (
             let* () = record_warning w1 in
             let* () = record_warning w2 in
             return ()
           ) with
           | ((), _, [w1'; w2']) ->
             Warning.to_string w1 = Warning.to_string w1'
             && Warning.to_string w2 = Warning.to_string w2'
           | _ -> false);
    ]
end
