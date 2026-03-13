type binding = Var.t * Sort.sort * Effect.t

type t = binding list

let empty = []

let extend x s eff ctx = (x, s, eff) :: ctx

let rec lookup x = function
  | [] -> None
  | (y, s, eff) :: rest ->
    if Var.compare x y = 0 then Some (s, eff)
    else lookup x rest

let extend_list bindings ctx =
  List.fold_left (fun acc (x, s, eff) -> (x, s, eff) :: acc) ctx (List.rev bindings)

let print fmt ctx =
  let pp_entry fmt (x, s, eff) =
    Format.fprintf fmt "@[%a : %a [%a]@]" Var.print x Sort.print s Effect.print eff
  in
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_entry)
    ctx

module Test = struct
  let gen =
    let open QCheck.Gen in
    let* bindings = list_size (0 -- 5)
      (let* x = Var.Test.gen in
       let* s = Sort.Test.gen in
       let* eff = Effect.Test.gen in
       pure (x, s, eff))
    in
    pure (List.fold_left (fun acc (x, s, eff) -> (x, s, eff) :: acc) [] (List.rev bindings))

  let test =
    [ QCheck.Test.make ~name:"lookup finds extended variable"
        ~count:100
        QCheck.(triple (make Var.Test.gen) (make Sort.Test.gen) (make Effect.Test.gen))
        (fun (x, s, eff) ->
           let ctx = extend x s eff empty in
           match lookup x ctx with
           | Some (s', eff') -> Sort.compare s s' = 0 && Effect.compare eff eff' = 0
           | None -> false)
    ]
end
