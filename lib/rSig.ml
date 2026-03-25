type entry =
  | FunSig of { arg : Sort.sort; ret : Sort.sort; eff : Effect.t }
  | FunDef of { param : Var.t; arg : Sort.sort; ret : Sort.sort; eff : Effect.t; body : CoreExpr.ce }
  | RFunSig of CoreExpr.ce RFunType.t
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type named_entry =
  | Named of Var.t * entry
  | Sort of DsortDecl.t
  | Type of DtypeDecl.t

type t = named_entry list

let empty = []
let extend name entry sig_ = Named (name, entry) :: sig_
let extend_sort sig_ d = Sort d :: sig_
let extend_type sig_ d = Type d :: sig_

let lift_to_rf arg ret eff =
  let x = Var.of_string "_arg" SourcePos.dummy in
  let y = Var.of_string "_ret" SourcePos.dummy in
  RFunType.{
    domain = [ProofSort.Comp { var = x; sort = arg; eff }];
    codomain = [ProofSort.Comp { var = y; sort = ret; eff }];
    eff;
  }

let rec lookup_rf name = function
  | [] -> None
  | Named (n, RFunSig rf) :: _ when Var.compare name n = 0 -> Some rf
  | Named (n, FunSig { arg; ret; eff }) :: _ when Var.compare name n = 0 ->
    Some (lift_to_rf arg ret eff)
  | Named (n, FunDef { arg; ret; eff; _ }) :: _ when Var.compare name n = 0 ->
    Some (lift_to_rf arg ret eff)
  | _ :: rest -> lookup_rf name rest

let rec lookup_fun name = function
  | [] -> None
  | Named (n, FunSig { arg; ret; eff }) :: _ when Var.compare name n = 0 ->
    Some (arg, ret, eff)
  | Named (n, FunDef { arg; ret; eff; _ }) :: _ when Var.compare name n = 0 ->
    Some (arg, ret, eff)
  | Named (n, RFunSig rf) :: _ when Var.compare name n = 0 ->
    Some (ProofSort.comp rf.domain, ProofSort.comp rf.codomain, rf.eff)
  | _ :: rest -> lookup_fun name rest

let rec lookup_fundef name = function
  | [] -> None
  | Named (n, FunDef { param; arg; ret; eff; body }) :: _ when Var.compare name n = 0 ->
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

let print fmt sig_ =
  let pp_entry fmt = function
    | Named (name, FunSig { arg; ret; eff }) ->
      Format.fprintf fmt "@[%a : %a -> %a [%a]@]"
        Var.print name Sort.print arg Sort.print ret Effect.print eff
    | Named (name, FunDef { param; arg; ret; eff; body = _ }) ->
      Format.fprintf fmt "@[<hov 2>fun %a (%a : %a) -> %a [%a] = <body>@]"
        Var.print name Var.print param Sort.print arg Sort.print ret Effect.print eff
    | Named (name, RFunSig rf) ->
      Format.fprintf fmt "@[%a : %a@]" Var.print name RFunType.print_ce rf
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

module Test = struct
  let test = []
end
