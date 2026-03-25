type t =
  | Add | Mul | Sub | Div
  | Lt | Le | Gt | Ge
  | And | Or | Not
  | Eq of Typ.ty
  | New of Typ.ty | Del of Typ.ty | Get of Typ.ty | Set of Typ.ty
  | Own of Typ.ty

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
  | Some t1, Some t2 -> Typ.compare t1 t2
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
  | Eq ty -> Format.fprintf fmt "Eq[%a]" Typ.print ty
  | New ty -> Format.fprintf fmt "New[%a]" Typ.print ty
  | Del ty -> Format.fprintf fmt "Del[%a]" Typ.print ty
  | Get ty -> Format.fprintf fmt "Get[%a]" Typ.print ty
  | Set ty -> Format.fprintf fmt "Set[%a]" Typ.print ty
  | Own ty -> Format.fprintf fmt "Own[%a]" Typ.print ty

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
      "type", Typ.json ty;
    ]

module Test = struct
  let gen =
    let open QCheck.Gen in
    let dummy = object method loc = SourcePos.dummy end in
    let int_ty = Typ.mk dummy Typ.Int in
    oneofl [ Add; Mul; Sub; Div; Lt; Le; Gt; Ge; And; Or; Not; Eq int_ty; New int_ty; Del int_ty; Get int_ty; Set int_ty; Own int_ty ]

  let test = []
end
