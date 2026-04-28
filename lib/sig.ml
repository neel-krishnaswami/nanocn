type 'a entry =
  | FunSig of { arg : Sort.sort; ret : Sort.sort; eff : Effect.t }
  | FunDef of { param : Var.t; arg : Sort.sort; ret : Sort.sort; eff : Effect.t; body : 'a }
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type 'a named_entry =
  | Named of string * 'a entry
  | Sort of DsortDecl.t
  | Type of DtypeDecl.t

type 'a t = 'a named_entry list

let empty = []

let extend name entry sig_ = Named (name, entry) :: sig_

let rec lookup_fun name = function
  | [] -> Error (Error.K_unknown_function { name })
  | Named (n, FunSig { arg; ret; eff }) :: _ when String.equal name n ->
    Ok (arg, ret, eff)
  | Named (n, FunDef { arg; ret; eff; _ }) :: _ when String.equal name n ->
    Ok (arg, ret, eff)
  | _ :: rest -> lookup_fun name rest

let rec lookup_fundef name = function
  | [] -> Error (Error.K_unfold_not_fundef { name })
  | Named (n, FunDef { param; arg; ret; eff; body }) :: _ when String.equal name n ->
    Ok (param, arg, ret, eff, body)
  | _ :: rest -> lookup_fundef name rest

let rec lookup_sort dsort = function
  | [] -> Error (Error.K_unbound_sort dsort)
  | Sort d :: _ when Dsort.compare dsort d.DsortDecl.name = 0 ->
    Ok d
  | Named (_, SortDecl d) :: _ when Dsort.compare dsort d.DsortDecl.name = 0 ->
    Ok d
  | _ :: rest -> lookup_sort dsort rest

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

let extend_sort sig_ (d : DsortDecl.t) = Sort d :: sig_

let rec lookup_type dsort = function
  | [] -> Error (Error.K_unbound_sort dsort)
  | Type d :: _ when Dsort.compare dsort d.DtypeDecl.name = 0 ->
    Ok d
  | Named (_, TypeDecl d) :: _ when Dsort.compare dsort d.DtypeDecl.name = 0 ->
    Ok d
  | _ :: rest -> lookup_type dsort rest

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

let extend_type sig_ (d : DtypeDecl.t) = Type d :: sig_

type sort_or_type =
  | LSortDecl of DsortDecl.t
  | LTypeDecl of DtypeDecl.t

let lookup_dsort_or_type dsort sig_ =
  match lookup_sort dsort sig_ with
  | Ok d -> Ok (LSortDecl d)
  | Error _ ->
    (match lookup_type dsort sig_ with
     | Ok d -> Ok (LTypeDecl d)
     | Error e -> Error e)

let print_gen pp_var pp_body fmt sig_ =
  let pp_entry fmt = function
    | Named (name, FunSig { arg; ret; eff }) ->
      Format.fprintf fmt "@[%s : %a -> %a [%a]@]"
        name Sort.print arg Sort.print ret Effect.print eff
    | Named (name, FunDef { param; arg; ret; eff; body }) ->
      Format.fprintf fmt "@[<hov 2>fun %s (%a : %a) -> %a [%a] =@ %a@]"
        name pp_var param Sort.print arg Sort.print ret
        Effect.print eff pp_body body
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

let print pp_body fmt sig_ = print_gen Var.print pp_body fmt sig_
let to_string pp_body sig_ = Format.asprintf "%a" (print_gen Var.print_unique pp_body) sig_

module Test = struct
  let gen_name =
    let open QCheck.Gen in
    let lower = map Char.chr (97 -- 122) in
    let* first = lower in
    let* rest = list_size (0 -- 5) lower in
    pure (String.init (1 + List.length rest) (fun i ->
      if i = 0 then first else List.nth rest (i - 1)))

  let test =
    [ QCheck.Test.make ~name:"sig lookup_fun finds extended entry"
        ~count:100
        QCheck.(pair (make gen_name) (make Sort.Test.gen))
        (fun (name, sort) ->
           let entry = FunSig { arg = sort; ret = sort; eff = Effect.Pure } in
           let s = extend name entry empty in
           match lookup_fun name s with
           | Ok (arg, _, _) -> Sort.compare arg sort = 0
           | Error _ -> false);

      QCheck.Test.make ~name:"sig lookup_fun finds FunDef entry"
        ~count:100
        QCheck.(pair (make gen_name) (make Sort.Test.gen))
        (fun (name, sort) ->
           let (param, _) = Var.mk "p" SourcePos.dummy Var.empty_supply in
           let entry = FunDef { param; arg = sort; ret = sort; eff = Effect.Pure; body = () } in
           let s = extend name entry empty in
           match lookup_fun name s with
           | Ok (arg, _, _) -> Sort.compare arg sort = 0
           | Error _ -> false);

      QCheck.Test.make ~name:"sig lookup_fundef finds FunDef entry"
        ~count:100
        QCheck.(pair (make gen_name) (make Sort.Test.gen))
        (fun (name, sort) ->
           let (param, _) = Var.mk "p" SourcePos.dummy Var.empty_supply in
           let entry = FunDef { param; arg = sort; ret = sort; eff = Effect.Pure; body = () } in
           let s = extend name entry empty in
           match lookup_fundef name s with
           | Ok (_, arg, _, _, ()) -> Sort.compare arg sort = 0
           | Error _ -> false);

      QCheck.Test.make ~name:"sig lookup_fundef returns Error for FunSig"
        ~count:100
        QCheck.(pair (make gen_name) (make Sort.Test.gen))
        (fun (name, sort) ->
           let entry = FunSig { arg = sort; ret = sort; eff = Effect.Pure } in
           let s = extend name entry empty in
           match lookup_fundef name s with
           | Error _ -> true
           | Ok _ -> false);
    ]
end
