type t = (Var.t * Typ.ty) list

let empty = []

let extend x a ctx = (x, a) :: ctx

let rec lookup x = function
  | [] -> None
  | (y, a) :: rest ->
    if Var.compare x y = 0 then Some a
    else lookup x rest

let extend_list bindings ctx =
  List.append bindings ctx

let print fmt ctx =
  let pp_entry fmt (x, a) =
    Format.fprintf fmt "@[%a : %a@]" Var.print x Typ.print a
  in
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_entry)
    ctx

module Test = struct
  let gen =
    let open QCheck.Gen in
    let* bindings = list_size (0 -- 5) (pair Var.Test.gen Typ.Test.gen) in
    pure (List.append bindings [])

  let test =
    [ QCheck.Test.make ~name:"lookup finds extended variable"
        ~count:100
        QCheck.(pair (make Var.Test.gen) (make Typ.Test.gen))
        (fun (x, a) ->
           let ctx = extend x a empty in
           match lookup x ctx with
           | Some a' -> Typ.compare a a' = 0
           | None -> false)
    ]
end
