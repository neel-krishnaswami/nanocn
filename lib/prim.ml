type t =
  | Add | Mul | Sub | Div
  | Lt | Le | Gt | Ge
  | And | Or | Not
  | Eq of Sort.sort
  | New of Sort.sort | Del of Sort.sort | Get of Sort.sort | Set of Sort.sort
  | Own of Sort.sort

let tag = function
  | Add -> 0 | Mul -> 1 | Sub -> 2 | Div -> 3
  | Lt -> 4 | Le -> 5 | Gt -> 6 | Ge -> 7
  | And -> 8 | Or -> 9 | Not -> 10
  | Eq _ -> 11
  | New _ -> 12 | Del _ -> 13 | Get _ -> 14 | Set _ -> 15
  | Own _ -> 16

let typ_of = function
  | Eq ty | New ty | Del ty | Get ty | Set ty | Own ty -> Some ty
  | Add | Mul | Sub | Div | Lt | Le | Gt | Ge | And | Or | Not -> None

let compare a b =
  let c = Int.compare (tag a) (tag b) in
  if c <> 0 then c
  else match typ_of a, typ_of b with
  | Some t1, Some t2 -> Sort.compare t1 t2
  | _ -> 0

let print fmt = function
  | Add -> Format.fprintf fmt "Add"
  | Mul -> Format.fprintf fmt "Mul"
  | Sub -> Format.fprintf fmt "Sub"
  | Div -> Format.fprintf fmt "Div"
  | Lt -> Format.fprintf fmt "Lt"
  | Le -> Format.fprintf fmt "Le"
  | Gt -> Format.fprintf fmt "Gt"
  | Ge -> Format.fprintf fmt "Ge"
  | And -> Format.fprintf fmt "And"
  | Or -> Format.fprintf fmt "Or"
  | Not -> Format.fprintf fmt "Not"
  | Eq ty -> Format.fprintf fmt "Eq[%a]" Sort.print ty
  | New ty -> Format.fprintf fmt "New[%a]" Sort.print ty
  | Del ty -> Format.fprintf fmt "Del[%a]" Sort.print ty
  | Get ty -> Format.fprintf fmt "Get[%a]" Sort.print ty
  | Set ty -> Format.fprintf fmt "Set[%a]" Sort.print ty
  | Own ty -> Format.fprintf fmt "Own[%a]" Sort.print ty

let json_tag = function
  | Add -> "Add" | Mul -> "Mul" | Sub -> "Sub" | Div -> "Div"
  | Lt -> "Lt" | Le -> "Le" | Gt -> "Gt" | Ge -> "Ge"
  | And -> "And" | Or -> "Or" | Not -> "Not"
  | Eq _ -> "Eq" | New _ -> "New" | Del _ -> "Del"
  | Get _ -> "Get" | Set _ -> "Set" | Own _ -> "Own"

let json p =
  match typ_of p with
  | None -> Json.String (json_tag p)
  | Some ty -> Json.Object [
      "tag", Json.String (json_tag p);
      "type", Sort.json (fun b -> SourcePos.json b#loc) ty;
    ]

module Test = struct
  let gen =
    let open QCheck.Gen in
    let dummy_info = object method loc = SourcePos.dummy end in
    let int_sort = Sort.mk dummy_info Sort.Int in
    oneof_list [ Add; Mul; Sub; Div; Lt; Le; Gt; Ge; And; Or; Not; Eq int_sort; New int_sort; Del int_sort; Get int_sort; Set int_sort; Own int_sort ]

  let test = []
end
