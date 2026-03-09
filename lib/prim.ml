type arith = Add | Mul | Sub | Div

let arith_to_int = function
  | Add -> 0 | Mul -> 1 | Sub -> 2 | Div -> 3

let compare_arith a1 a2 = Int.compare (arith_to_int a1) (arith_to_int a2)

let arith_to_string = function
  | Add -> "Add" | Mul -> "Mul" | Sub -> "Sub" | Div -> "Div"

let print_arith fmt a = Format.fprintf fmt "%s" (arith_to_string a)

type state = New | Del | Get | Set

let state_to_int = function
  | New -> 0 | Del -> 1 | Get -> 2 | Set -> 3

let compare_state s1 s2 = Int.compare (state_to_int s1) (state_to_int s2)

let state_to_string = function
  | New -> "New" | Del -> "Del" | Get -> "Get" | Set -> "Set"

let print_state fmt s = Format.fprintf fmt "%s" (state_to_string s)

module Test = struct
  let gen_arith = QCheck.Gen.oneofl [ Add; Mul; Sub; Div ]
  let gen_state = QCheck.Gen.oneofl [ New; Del; Get; Set ]

  let test = []
end
