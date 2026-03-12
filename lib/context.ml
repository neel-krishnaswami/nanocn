type binding =
  | CompVar of Var.t * Typ.ty
  | SpecVar of Var.t * Sort.sort

type t = binding list

let empty = []

let extend_comp x a ctx = CompVar (x, a) :: ctx
let extend_spec x s ctx = SpecVar (x, s) :: ctx

let rec lookup_comp x = function
  | [] -> None
  | CompVar (y, a) :: rest ->
    if Var.compare x y = 0 then Some a
    else lookup_comp x rest
  | SpecVar _ :: rest -> lookup_comp x rest

let rec lookup_spec x = function
  | [] -> None
  | SpecVar (y, s) :: rest ->
    if Var.compare x y = 0 then Some s
    else lookup_spec x rest
  | CompVar _ :: rest -> lookup_spec x rest

let extend_comp_list bindings ctx =
  List.fold_left (fun acc (x, a) -> CompVar (x, a) :: acc) ctx (List.rev bindings)

let extend_spec_list bindings ctx =
  List.fold_left (fun acc (x, s) -> SpecVar (x, s) :: acc) ctx (List.rev bindings)

let print fmt ctx =
  let pp_entry fmt = function
    | CompVar (x, a) ->
      Format.fprintf fmt "@[%a : %a@]" Var.print x Typ.print a
    | SpecVar (x, s) ->
      Format.fprintf fmt "@[spec %a : %a@]" Var.print x Sort.print s
  in
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_entry)
    ctx

module Test = struct
  let gen =
    let open QCheck.Gen in
    let* bindings = list_size (0 -- 5) (pair Var.Test.gen Typ.Test.gen) in
    pure (List.fold_left (fun acc (x, a) -> CompVar (x, a) :: acc) [] (List.rev bindings))

  let test =
    [ QCheck.Test.make ~name:"lookup_comp finds extended variable"
        ~count:100
        QCheck.(pair (make Var.Test.gen) (make Typ.Test.gen))
        (fun (x, a) ->
           let ctx = extend_comp x a empty in
           match lookup_comp x ctx with
           | Some a' -> Typ.compare a a' = 0
           | None -> false)
    ]
end
