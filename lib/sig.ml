type entry = {
  arg : Typ.ty;
  ret : Typ.ty;
  eff : Effect.t;
}

type t = (Var.t * entry) list

let empty = []

let extend name entry sig_ = (name, entry) :: sig_

let rec lookup name = function
  | [] -> None
  | (n, e) :: rest ->
    if Var.compare name n = 0 then Some e
    else lookup name rest

let print fmt sig_ =
  let pp_entry fmt (name, entry) =
    Format.fprintf fmt "@[%a : %a -> %a [%a]@]"
      Var.print name Typ.print entry.arg Typ.print entry.ret Effect.print entry.eff
  in
  match sig_ with
  | [] -> Format.fprintf fmt {|·|}
  | _ ->
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
      pp_entry fmt sig_

module Test = struct
  let test =
    [ QCheck.Test.make ~name:"sig lookup finds extended entry"
        ~count:100
        QCheck.(pair (make Var.Test.gen) (make Typ.Test.gen))
        (fun (name, ty) ->
           let entry = { arg = ty; ret = ty; eff = Effect.Pure } in
           let s = extend name entry empty in
           match lookup name s with
           | Some e -> Typ.compare e.arg ty = 0
           | None -> false)
    ]
end
