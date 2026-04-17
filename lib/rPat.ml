type ('var, 'b) pat_elem = Single of 'b * 'var | Pair of 'b * 'var * 'var

type ('var, 'b) t = ('var, 'b) pat_elem list

let elem_info = function
  | Single (b, _) -> b
  | Pair (b, _, _) -> b

let map_var_elem f = function
  | Single (b, x) -> Single (b, f x)
  | Pair (b, x, y) -> Pair (b, f x, f y)

let map_var f = List.map (map_var_elem f)

let map_info_elem f = function
  | Single (b, x) -> Single (f b, x)
  | Pair (b, x, y) -> Pair (f b, x, y)

let map_info f = List.map (map_info_elem f)

let print_gen_elem pp_var fmt = function
  | Single (_, x) -> pp_var fmt x
  | Pair (_, x, y) ->
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
