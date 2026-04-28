type entry =
  | FunSig of { arg : Sort.sort; ret : Sort.sort; eff : Effect.t }
  | FunDef of { param : Var.t; arg : Sort.sort; ret : Sort.sort; eff : Effect.t; body : CoreExpr.typed_ce }
  | RFunSig of (CoreExpr.typed_ce, RProg.typed_rinfo, Var.t) RFunType.t
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
  | [] -> Error (Error.K_unknown_function { name })
  | Named (n, RFunSig rf) :: _ when String.equal name n -> Ok rf
  | _ :: rest -> lookup_rf name rest

let rec lookup_fun name = function
  | [] -> Error (Error.K_unknown_function { name })
  | Named (n, FunSig { arg; ret; eff }) :: _ when String.equal name n ->
    Ok (arg, ret, eff)
  | Named (n, FunDef { arg; ret; eff; _ }) :: _ when String.equal name n ->
    Ok (arg, ret, eff)
  | Named (n, RFunSig rf) :: _ when String.equal name n ->
    Ok (ProofSort.comp rf.domain, ProofSort.comp rf.codomain, rf.eff)
  | _ :: rest -> lookup_fun name rest

let rec lookup_fundef name = function
  | [] -> Error (Error.K_unfold_not_fundef { name })
  | Named (n, FunDef { param; arg; ret; eff; body }) :: _ when String.equal name n ->
    Ok (param, arg, ret, eff, body)
  | _ :: rest -> lookup_fundef name rest

let rec lookup_sort dsort = function
  | [] -> Error (Error.K_unbound_sort dsort)
  | Sort d :: _ when Dsort.compare dsort d.DsortDecl.name = 0 -> Ok d
  | Named (_, SortDecl d) :: _ when Dsort.compare dsort d.DsortDecl.name = 0 -> Ok d
  | _ :: rest -> lookup_sort dsort rest

let rec lookup_type dsort = function
  | [] -> Error (Error.K_unbound_sort dsort)
  | Type d :: _ when Dsort.compare dsort d.DtypeDecl.name = 0 -> Ok d
  | Named (_, TypeDecl d) :: _ when Dsort.compare dsort d.DtypeDecl.name = 0 -> Ok d
  | _ :: rest -> lookup_type dsort rest

let rec lookup_ctor label = function
  | [] -> Error (Error.K_unbound_ctor label)
  | Sort d :: rest ->
    (match DsortDecl.lookup_ctor label d with
     | Some _ -> Ok (d.DsortDecl.name, d)
     | None -> lookup_ctor label rest)
  | Named (_, SortDecl d) :: rest ->
    (match DsortDecl.lookup_ctor label d with
     | Some _ -> Ok (d.DsortDecl.name, d)
     | None -> lookup_ctor label rest)
  | _ :: rest -> lookup_ctor label rest

let rec lookup_type_ctor label = function
  | [] -> Error (Error.K_unbound_ctor label)
  | Type d :: rest ->
    (match DtypeDecl.lookup_ctor label d with
     | Some _ -> Ok (d.DtypeDecl.name, d)
     | None -> lookup_type_ctor label rest)
  | Named (_, TypeDecl d) :: rest ->
    (match DtypeDecl.lookup_ctor label d with
     | Some _ -> Ok (d.DtypeDecl.name, d)
     | None -> lookup_type_ctor label rest)
  | _ :: rest -> lookup_type_ctor label rest

let lookup_dsort_or_type dsort sig_ =
  match lookup_sort dsort sig_ with
  | Ok d -> Ok (Sig.LSortDecl d)
  | Error _ ->
    (match lookup_type dsort sig_ with
     | Ok d -> Ok (Sig.LTypeDecl d)
     | Error e -> Error e)

type listed_entry =
  | LFun  of string * entry
  | LSort of DsortDecl.t
  | LType of DtypeDecl.t

(* The internal representation prepends new entries, so the head of
   the list is the most recently declared item. [entries] reverses
   this so callers see declarations in source order. *)
let entries sig_ =
  let rec loop acc = function
    | [] -> acc
    | Named (_, SortDecl d) :: rest -> loop (LSort d :: acc) rest
    | Named (_, TypeDecl d) :: rest -> loop (LType d :: acc) rest
    | Named (name, entry)   :: rest -> loop (LFun (name, entry) :: acc) rest
    | Sort d :: rest -> loop (LSort d :: acc) rest
    | Type d :: rest -> loop (LType d :: acc) rest
  in
  loop [] sig_

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
