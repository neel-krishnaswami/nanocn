type binding =
  | Term of Var.t * Sort.sort * Effect.t
  | TVar of Tvar.t * Kind.t

type t = binding list

let empty = []

let extend x s eff ctx = Term (x, s, eff) :: ctx

let extend_tvar a kind ctx = TVar (a, kind) :: ctx

let rec lookup x = function
  | [] -> None
  | Term (y, s, eff) :: rest ->
    if Var.compare x y = 0 then Some (s, eff)
    else lookup x rest
  | TVar _ :: rest -> lookup x rest

let rec lookup_tvar a = function
  | [] -> None
  | TVar (b, kind) :: rest ->
    if Tvar.compare a b = 0 then Some kind
    else lookup_tvar a rest
  | Term _ :: rest -> lookup_tvar a rest

let to_list ctx = ctx

let extend_list bindings ctx =
  List.fold_left (fun acc (x, s, eff) -> Term (x, s, eff) :: acc) ctx (List.rev bindings)

let print_gen pp_var fmt ctx =
  let pp_entry fmt = function
    | Term (x, s, eff) ->
      Format.fprintf fmt "@[%a : %a [%a]@]" pp_var x Sort.print s Effect.print eff
    | TVar (a, kind) ->
      Format.fprintf fmt "@[%a : %a@]" Tvar.print a Kind.print kind
  in
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_entry)
    ctx

let print fmt ctx = print_gen Var.print fmt ctx
let to_string ctx = Format.asprintf "%a" (print_gen Var.print_unique) ctx

module Test = struct
  let gen =
    let open QCheck.Gen in
    let* bindings = list_size (0 -- 5)
      (let* x = Var.Test.gen in
       let* s = Sort.Test.gen in
       let* eff = Effect.Test.gen in
       pure (Term (x, s, eff)))
    in
    pure (List.fold_left (fun acc b -> b :: acc) [] (List.rev bindings))

  let test =
    [ QCheck.Test.make ~name:"lookup finds extended variable"
        ~count:100
        QCheck.(triple (make Var.Test.gen) (make Sort.Test.gen) (make Effect.Test.gen))
        (fun (x, s, eff) ->
           let ctx = extend x s eff empty in
           match lookup x ctx with
           | Some (s', eff') -> Sort.compare s s' = 0 && Effect.compare eff eff' = 0
           | None -> false);

      QCheck.Test.make ~name:"lookup_tvar finds extended tvar"
        ~count:100
        QCheck.(pair (make Tvar.Test.gen) (make Kind.Test.gen))
        (fun (a, k) ->
           let ctx = extend_tvar a k empty in
           match lookup_tvar a ctx with
           | Some k' -> Kind.compare k k' = 0
           | None -> false);
    ]
end
