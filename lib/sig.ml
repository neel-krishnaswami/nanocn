type entry =
  | FunSig of { arg : Sort.sort; ret : Sort.sort; eff : Effect.t }
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type named_entry =
  | Named of Var.t * entry
  | Sort of DsortDecl.t
  | Type of DtypeDecl.t

type t = named_entry list

let empty = []

let extend name entry sig_ = Named (name, entry) :: sig_

let rec lookup_fun name = function
  | [] -> None
  | Named (n, FunSig { arg; ret; eff }) :: _ when Var.compare name n = 0 ->
    Some (arg, ret, eff)
  | _ :: rest -> lookup_fun name rest

let rec lookup_sort dsort = function
  | [] -> None
  | Sort d :: _ when Dsort.compare dsort d.DsortDecl.name = 0 ->
    Some d
  | Named (_, SortDecl d) :: _ when Dsort.compare dsort d.DsortDecl.name = 0 ->
    Some d
  | _ :: rest -> lookup_sort dsort rest

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

let extend_sort sig_ (d : DsortDecl.t) = Sort d :: sig_

let rec lookup_type dsort = function
  | [] -> None
  | Type d :: _ when Dsort.compare dsort d.DtypeDecl.name = 0 ->
    Some d
  | Named (_, TypeDecl d) :: _ when Dsort.compare dsort d.DtypeDecl.name = 0 ->
    Some d
  | _ :: rest -> lookup_type dsort rest

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

let extend_type sig_ (d : DtypeDecl.t) = Type d :: sig_

let print fmt sig_ =
  let pp_entry fmt = function
    | Named (name, FunSig { arg; ret; eff }) ->
      Format.fprintf fmt "@[%a : %a -> %a [%a]@]"
        Var.print name Sort.print arg Sort.print ret Effect.print eff
    | Named (_, SortDecl d) ->
      DsortDecl.print fmt d
    | Named (_, TypeDecl d) ->
      DtypeDecl.print fmt d
    | Sort d ->
      DsortDecl.print fmt d
    | Type d ->
      DtypeDecl.print fmt d
  in
  match sig_ with
  | [] -> Format.fprintf fmt {|·|}
  | _ ->
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
      pp_entry fmt sig_

module Test = struct
  let test =
    [ QCheck.Test.make ~name:"sig lookup_fun finds extended entry"
        ~count:100
        QCheck.(pair (make Var.Test.gen) (make Sort.Test.gen))
        (fun (name, sort) ->
           let entry = FunSig { arg = sort; ret = sort; eff = Effect.Pure } in
           let s = extend name entry empty in
           match lookup_fun name s with
           | Some (arg, _, _) -> Sort.compare arg sort = 0
           | None -> false)
    ]
end
