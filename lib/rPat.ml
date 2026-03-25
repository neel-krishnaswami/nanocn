type t = Var.t list

let print fmt = function
  | [] -> Format.fprintf fmt "()"
  | [x] -> Format.fprintf fmt "(%a,)" Var.print x
  | xs ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") Var.print)
      xs

module Test = struct
  let test = []
end
