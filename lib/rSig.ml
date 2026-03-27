type entry =
  | FunSig of { arg : Sort.sort; ret : Sort.sort; eff : Effect.t }
  | FunDef of { param : Var.t; arg : Sort.sort; ret : Sort.sort; eff : Effect.t; body : CoreExpr.typed_ce }
  | RFunSig of (CoreExpr.typed_ce, Var.t) RFunType.t
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type named_entry =
  | Named of string * entry
  | Sort of DsortDecl.t
  | Type of DtypeDecl.t

type t = named_entry list

let empty = []
let extend name entry sig_ = Named (name, entry) :: sig_
let extend_sort sig_ d = Sort d :: sig_
let extend_type sig_ d = Type d :: sig_

let rec lookup_rf name = function
  | [] -> None
  | Named (n, RFunSig rf) :: _ when String.equal name n -> Some rf
  | _ :: rest -> lookup_rf name rest

let rec lookup_fun name = function
  | [] -> None
  | Named (n, FunSig { arg; ret; eff }) :: _ when String.equal name n ->
    Some (arg, ret, eff)
  | Named (n, FunDef { arg; ret; eff; _ }) :: _ when String.equal name n ->
    Some (arg, ret, eff)
  | Named (n, RFunSig rf) :: _ when String.equal name n ->
    Some (ProofSort.comp rf.domain, ProofSort.comp rf.codomain, rf.eff)
  | _ :: rest -> lookup_fun name rest

let rec lookup_fundef name = function
  | [] -> None
  | Named (n, FunDef { param; arg; ret; eff; body }) :: _ when String.equal name n ->
    Some (param, arg, ret, eff, body)
  | _ :: rest -> lookup_fundef name rest

let rec lookup_sort dsort = function
  | [] -> None
  | Sort d :: _ when Dsort.compare dsort d.DsortDecl.name = 0 -> Some d
  | Named (_, SortDecl d) :: _ when Dsort.compare dsort d.DsortDecl.name = 0 -> Some d
  | _ :: rest -> lookup_sort dsort rest

let rec lookup_type dsort = function
  | [] -> None
  | Type d :: _ when Dsort.compare dsort d.DtypeDecl.name = 0 -> Some d
  | Named (_, TypeDecl d) :: _ when Dsort.compare dsort d.DtypeDecl.name = 0 -> Some d
  | _ :: rest -> lookup_type dsort rest

let rec lookup_ctor label = function
  | [] -> None
  | Sort d :: rest ->
    (match DsortDecl.lookup_ctor label d with
     | Some _ -> Some (d.DsortDecl.name, d)
     | None -> lookup_ctor label rest)
  | Named (_, SortDecl d) :: rest ->
    (match DsortDecl.lookup_ctor label d with
     | Some _ -> Some (d.DsortDecl.name, d)
     | None -> lookup_ctor label rest)
  | _ :: rest -> lookup_ctor label rest

let rec lookup_type_ctor label = function
  | [] -> None
  | Type d :: rest ->
    (match DtypeDecl.lookup_ctor label d with
     | Some _ -> Some (d.DtypeDecl.name, d)
     | None -> lookup_type_ctor label rest)
  | Named (_, TypeDecl d) :: rest ->
    (match DtypeDecl.lookup_ctor label d with
     | Some _ -> Some (d.DtypeDecl.name, d)
     | None -> lookup_type_ctor label rest)
  | _ :: rest -> lookup_type_ctor label rest

let comp sig_ =
  List.fold_right (fun ne acc ->
    match ne with
    | Named (name, FunSig { arg; ret; eff }) ->
      Sig.extend name (Sig.FunSig { arg; ret; eff }) acc
    | Named (name, FunDef { param; arg; ret; eff; body }) ->
      Sig.extend name (Sig.FunDef { param; arg; ret; eff; body }) acc
    | Named (name, RFunSig rf) ->
      let arg = ProofSort.comp rf.domain in
      let ret = ProofSort.comp rf.codomain in
      Sig.extend name (Sig.FunSig { arg; ret; eff = rf.eff }) acc
    | Named (_, SortDecl d) | Sort d ->
      Sig.extend_sort acc d
    | Named (_, TypeDecl d) | Type d ->
      Sig.extend_type acc d)
    sig_ Sig.empty

let print_gen pp_var fmt sig_ =
  let pp_ce = CoreExpr.print_gen pp_var in
  let pp_pf = ProofSort.print_gen pp_var pp_ce in
  let pp_rf fmt rf =
    Format.fprintf fmt "@[<hov 2>%a ⊸@ %a [%a]@]"
      pp_pf rf.RFunType.domain pp_pf rf.RFunType.codomain Effect.print rf.RFunType.eff
  in
  let pp_entry fmt = function
    | Named (name, FunSig { arg; ret; eff }) ->
      Format.fprintf fmt "@[%s : %a -> %a [%a]@]"
        name Sort.print arg Sort.print ret Effect.print eff
    | Named (name, FunDef { param; arg; ret; eff; body = _ }) ->
      Format.fprintf fmt "@[<hov 2>fun %s (%a : %a) -> %a [%a] = <body>@]"
        name pp_var param Sort.print arg Sort.print ret Effect.print eff
    | Named (name, RFunSig rf) ->
      Format.fprintf fmt "@[%s : %a@]" name pp_rf rf
    | Named (_, SortDecl d) | Sort d ->
      DsortDecl.print fmt d
    | Named (_, TypeDecl d) | Type d ->
      DtypeDecl.print fmt d
  in
  match sig_ with
  | [] -> Format.fprintf fmt "·"
  | _ ->
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
      pp_entry fmt sig_

let print fmt sig_ = print_gen Var.print fmt sig_
let to_string sig_ = Format.asprintf "%a" (print_gen Var.print_unique) sig_

module Test = struct
  let test = []
end
