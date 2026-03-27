type 'var pat_elem = Single of 'var | Pair of 'var * 'var

type 'var t = 'var pat_elem list

let map_var_elem f = function
  | Single x -> Single (f x)
  | Pair (x, y) -> Pair (f x, f y)

let map_var f = List.map (map_var_elem f)

let print_gen_elem pp_var fmt = function
  | Single x -> pp_var fmt x
  | Pair (x, y) ->
    Format.fprintf fmt "(%a, %a)" pp_var x pp_var y

let print_gen pp_var fmt = function
  | [] -> Format.fprintf fmt "()"
  | [e] -> Format.fprintf fmt "(%a,)" (print_gen_elem pp_var) e
  | es ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") (print_gen_elem pp_var))
      es

let print fmt t = print_gen Var.print fmt t
let to_string t = Format.asprintf "%a" (print_gen Var.print_unique) t

module Test = struct
  let test = []
end
