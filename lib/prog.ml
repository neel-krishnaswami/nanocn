type ('a, 'b) decl =
  | FunDecl of {
      name : Var.t;
      arg_sort : Sort.sort;
      ret_sort : Sort.sort;
      eff : Effect.t;
      branches : (Pat.pat * 'a * 'b) list;
      loc : SourcePos.t;
    }
  | SortDecl of DsortDecl.t
  | TypeDecl of DtypeDecl.t

type ('a, 'b) t = {
  decls : ('a, 'b) decl list;
  main : 'a;
  main_sort : Sort.sort;
  main_eff : Effect.t;
  loc : SourcePos.t;
}

let print pp fmt p =
  List.iter (fun d ->
    match d with
    | FunDecl d ->
      Format.fprintf fmt "@[<v>@[<hov 2>fun %a :@ %a -> %a [%a] = { ... }@]@]@.@."
        Var.print d.name
        Sort.print d.arg_sort
        Sort.print d.ret_sort
        Effect.print d.eff
    | SortDecl d ->
      Format.fprintf fmt "%a@.@." DsortDecl.print d
    | TypeDecl d ->
      Format.fprintf fmt "%a@.@." DtypeDecl.print d
  ) p.decls;
  Format.fprintf fmt "@[<hov 2>main : %a [%a] =@ %a@]"
    Sort.print p.main_sort Effect.print p.main_eff pp p.main

let json_sort_loc s = Sort.json (fun b -> SourcePos.json b#loc) s
let json_pat_loc p = Pat.json (fun b -> SourcePos.json b#loc) p

let json_decl ja jb = function
  | FunDecl d ->
    Json.Object [
      "tag", Json.String "FunDecl";
      "name", Var.json d.name;
      "arg_sort", json_sort_loc d.arg_sort;
      "ret_sort", json_sort_loc d.ret_sort;
      "effect", Effect.json d.eff;
      "branches", Json.Array (List.map (fun (p, a, b) ->
        Json.Object ["pat", json_pat_loc p; "body", ja a; "info", jb b]) d.branches);
      "loc", SourcePos.json d.loc;
    ]
  | SortDecl d -> Json.Object ["tag", Json.String "SortDecl"; "decl", DsortDecl.json d]
  | TypeDecl d -> Json.Object ["tag", Json.String "TypeDecl"; "decl", DtypeDecl.json d]

let json ja jb p =
  Json.Object [
    "decls", Json.Array (List.map (json_decl ja jb) p.decls);
    "main", ja p.main;
    "main_sort", json_sort_loc p.main_sort;
    "main_effect", Effect.json p.main_eff;
    "loc", SourcePos.json p.loc;
  ]

type 'a core_decl =
  | CoreFunDecl of {
      name : Var.t;
      param : Var.t;
      arg_sort : Sort.sort;
      ret_sort : Sort.sort;
      eff : Effect.t;
      body : 'a;
      loc : SourcePos.t;
    }
  | CoreSortDecl of DsortDecl.t
  | CoreTypeDecl of DtypeDecl.t

type 'a core_prog = {
  core_decls : 'a core_decl list;
  core_main : 'a;
  core_main_sort : Sort.sort;
  core_main_eff : Effect.t;
  core_loc : SourcePos.t;
}

let print_core_prog pp fmt p =
  List.iter (fun d ->
    match d with
    | CoreFunDecl d ->
      Format.fprintf fmt "@[<v>@[<hov 2>fun %a (%a : %a) : %a [%a] =@ %a@]@]@.@."
        Var.print d.name
        Var.print d.param
        Sort.print d.arg_sort
        Sort.print d.ret_sort
        Effect.print d.eff
        pp d.body
    | CoreSortDecl d ->
      Format.fprintf fmt "%a@.@." DsortDecl.print d
    | CoreTypeDecl d ->
      Format.fprintf fmt "%a@.@." DtypeDecl.print d
  ) p.core_decls;
  Format.fprintf fmt "@[<hov 2>main : %a [%a] =@ %a@]"
    Sort.print p.core_main_sort Effect.print p.core_main_eff pp p.core_main

let json_core_decl ja = function
  | CoreFunDecl d ->
    Json.Object [
      "tag", Json.String "CoreFunDecl";
      "name", Var.json d.name;
      "param", Var.json d.param;
      "arg_sort", json_sort_loc d.arg_sort;
      "ret_sort", json_sort_loc d.ret_sort;
      "effect", Effect.json d.eff;
      "body", ja d.body;
      "loc", SourcePos.json d.loc;
    ]
  | CoreSortDecl d -> Json.Object ["tag", Json.String "CoreSortDecl"; "decl", DsortDecl.json d]
  | CoreTypeDecl d -> Json.Object ["tag", Json.String "CoreTypeDecl"; "decl", DtypeDecl.json d]

let json_core_prog ja p =
  Json.Object [
    "decls", Json.Array (List.map (json_core_decl ja) p.core_decls);
    "main", ja p.core_main;
    "main_sort", json_sort_loc p.core_main_sort;
    "main_effect", Effect.json p.core_main_eff;
    "loc", SourcePos.json p.core_loc;
  ]

module Test = struct
  let test = []
end
