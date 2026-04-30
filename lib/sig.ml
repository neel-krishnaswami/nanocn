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

let lookup_ctor dsort label sig_ =
  match lookup_sort dsort sig_ with
  | Error e -> Error e
  | Ok decl ->
    match DsortDecl.lookup_ctor label decl with
    | Some _ -> Ok decl
    | None -> Error (Error.K_ctor_not_in_decl { label; decl = dsort })

let extend_sort sig_ (d : DsortDecl.t) = Sort d :: sig_

let rec lookup_type dsort = function
  | [] -> Error (Error.K_unbound_sort dsort)
  | Type d :: _ when Dsort.compare dsort d.DtypeDecl.name = 0 ->
    Ok d
  | Named (_, TypeDecl d) :: _ when Dsort.compare dsort d.DtypeDecl.name = 0 ->
    Ok d
  | _ :: rest -> lookup_type dsort rest

let lookup_type_ctor dsort label sig_ =
  match lookup_type dsort sig_ with
  | Error e -> Error e
  | Ok decl ->
    match DtypeDecl.lookup_ctor label decl with
    | Some _ -> Ok decl
    | None -> Error (Error.K_ctor_not_in_decl { label; decl = dsort })

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

      (* Qualified ctor lookup: shared labels across two datasorts must
         resolve to the right decl. Regression test for the bug where
         lookup ignored the head sort name. *)
      QCheck.Test.make ~name:"sig lookup_ctor disambiguates by head sort"
        ~count:1
        QCheck.unit
        (fun () ->
           let mk_dsort s = match Dsort.of_string s with
             | Ok d -> d | Error _ -> assert false in
           let mk_label s = match Label.of_string s with
             | Ok l -> l | Error _ -> assert false in
           let mk_sort s =
             Sort.mk
               (object method loc = SourcePos.dummy end)
               s in
           let l = mk_label "La" in
           let d1 = mk_dsort "D1" in
           let d2 = mk_dsort "D2" in
           let decl1 = DsortDecl.{
             name = d1; params = [];
             ctors = [(l, mk_sort Sort.Int)];
             loc = SourcePos.dummy } in
           let decl2 = DsortDecl.{
             name = d2; params = [];
             ctors = [(l, mk_sort Sort.Bool)];
             loc = SourcePos.dummy } in
           let s = extend_sort (extend_sort empty decl1) decl2 in
           let ok_d1 = match lookup_ctor d1 l s with
             | Ok d -> Dsort.compare d.DsortDecl.name d1 = 0
             | Error _ -> false in
           let ok_d2 = match lookup_ctor d2 l s with
             | Ok d -> Dsort.compare d.DsortDecl.name d2 = 0
             | Error _ -> false in
           ok_d1 && ok_d2);

      QCheck.Test.make ~name:"sig lookup_ctor: ctor not in decl"
        ~count:1
        QCheck.unit
        (fun () ->
           let mk_dsort s = match Dsort.of_string s with
             | Ok d -> d | Error _ -> assert false in
           let mk_label s = match Label.of_string s with
             | Ok l -> l | Error _ -> assert false in
           let mk_sort s =
             Sort.mk
               (object method loc = SourcePos.dummy end)
               s in
           let d = mk_dsort "D" in
           let l_in = mk_label "Lin" in
           let l_out = mk_label "Mout" in
           let decl = DsortDecl.{
             name = d; params = [];
             ctors = [(l_in, mk_sort Sort.Int)];
             loc = SourcePos.dummy } in
           let s = extend_sort empty decl in
           match lookup_ctor d l_out s with
           | Error (Error.K_ctor_not_in_decl _) -> true
           | _ -> false);

      QCheck.Test.make ~name:"sig lookup_ctor: unbound sort"
        ~count:1
        QCheck.unit
        (fun () ->
           let mk_dsort s = match Dsort.of_string s with
             | Ok d -> d | Error _ -> assert false in
           let mk_label s = match Label.of_string s with
             | Ok l -> l | Error _ -> assert false in
           match lookup_ctor (mk_dsort "Nope") (mk_label "La") empty with
           | Error (Error.K_unbound_sort _) -> true
           | _ -> false);
    ]
end
