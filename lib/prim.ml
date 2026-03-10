type t =
  | Add | Mul | Sub | Div
  | And | Or | Not
  | New of Typ.ty | Del of Typ.ty | Get of Typ.ty | Set of Typ.ty

let tag = function
  | Add -> 0 | Mul -> 1 | Sub -> 2 | Div -> 3
  | And -> 4 | Or -> 5 | Not -> 6
  | New _ -> 7 | Del _ -> 8 | Get _ -> 9 | Set _ -> 10

let typ_of = function
  | New ty | Del ty | Get ty | Set ty -> Some ty
  | Add | Mul | Sub | Div | And | Or | Not -> None

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
  | And -> Format.fprintf fmt "And"
  | Or -> Format.fprintf fmt "Or"
  | Not -> Format.fprintf fmt "Not"
  | New ty -> Format.fprintf fmt "New[%a]" Typ.print ty
  | Del ty -> Format.fprintf fmt "Del[%a]" Typ.print ty
  | Get ty -> Format.fprintf fmt "Get[%a]" Typ.print ty
  | Set ty -> Format.fprintf fmt "Set[%a]" Typ.print ty

module Test = struct
  let gen =
    let open QCheck.Gen in
    let dummy = object method loc = SourcePos.dummy end in
    let int_ty = Typ.In (Typ.Int, dummy) in
    oneofl [ Add; Mul; Sub; Div; And; Or; Not; New int_ty; Del int_ty; Get int_ty; Set int_ty ]

  let test = []
end
