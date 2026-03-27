type 'var pat_elem = Single of 'var | Pair of 'var * 'var

type 'var t = 'var pat_elem list

let map_var_elem f = function
  | Single x -> Single (f x)
  | Pair (x, y) -> Pair (f x, f y)

let map_var f = List.map (map_var_elem f)

let print_elem fmt = function
  | Single x -> Var.print fmt x
  | Pair (x, y) ->
    Format.fprintf fmt "(%a, %a)" Var.print x Var.print y

let print fmt = function
  | [] -> Format.fprintf fmt "()"
  | [e] -> Format.fprintf fmt "(%a,)" print_elem e
  | es ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") print_elem)
      es

module Test = struct
  let test = []
end
