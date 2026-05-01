let lookup sig_ dsort label args =
  match Sig.lookup_dsort_or_type dsort sig_ with
  | Error e -> Error e
  | Ok (Sig.LSortDecl decl) ->
    (match DsortDecl.lookup_ctor label decl with
     | None -> Error (Error.K_ctor_not_in_decl { label; decl = dsort })
     | Some raw_sort ->
       match Subst.of_lists decl.DsortDecl.params args with
       | Error k -> Error k
       | Ok sub -> Ok (Subst.apply sub raw_sort))
  | Ok (Sig.LTypeDecl decl) ->
    match DtypeDecl.lookup_ctor label decl with
    | None -> Error (Error.K_ctor_not_in_decl { label; decl = dsort })
    | Some raw_sort ->
      match Subst.of_lists decl.DtypeDecl.params args with
      | Error k -> Error k
      | Ok sub -> Ok (Subst.apply sub raw_sort)

let lookup_all sig_ dsort args =
  match Sig.lookup_dsort_or_type dsort sig_ with
  | Error e -> Error e
  | Ok (Sig.LSortDecl decl) ->
    (match Subst.of_lists decl.DsortDecl.params args with
     | Error k -> Error k
     | Ok sub ->
       Ok (List.map (fun (l, raw) -> (l, Subst.apply sub raw)) decl.DsortDecl.ctors))
  | Ok (Sig.LTypeDecl decl) ->
    match Subst.of_lists decl.DtypeDecl.params args with
    | Error k -> Error k
    | Ok sub ->
      Ok (List.map (fun (l, raw) -> (l, Subst.apply sub raw)) decl.DtypeDecl.ctors)

let lookup_all_observed sig_ sort_result observed =
  (* Dedupe observed labels, preserving first-occurrence order. *)
  let seen = Hashtbl.create 8 in
  let observed_unique =
    List.filter (fun l ->
      if Hashtbl.mem seen l then false
      else begin Hashtbl.add seen l (); true end
    ) observed
  in
  match sort_result with
  | Error e ->
    List.map (fun l -> (l, Error e)) observed_unique
  | Ok s ->
    let view : (Sort.sort, Error.kind) result = Ok s in
    let (d_result, args_results) =
      SortView.Get.app ~construct:"case scrutinee" view in
    let declared : ((Label.t * Sort.sort) list, Error.kind) result =
      Result.bind d_result (fun d ->
        Result.bind (Util.result_list args_results) (fun args ->
          lookup_all sig_ d args))
    in
    match declared with
    | Error k ->
      List.map (fun l -> (l, Error k)) observed_unique
    | Ok lts ->
      let label_eq l1 l2 = Label.compare l1 l2 = 0 in
      let observed_entries =
        List.map (fun l ->
          match List.find_opt (fun (l', _) -> label_eq l l') lts with
          | Some (_, ctor_sort) -> (l, Ok ctor_sort)
          | None ->
            (match d_result with
             | Ok d ->
               (l, Error (Error.K_ctor_not_in_decl { label = l; decl = d }))
             | Error k -> (l, Error k))
        ) observed_unique
      in
      let missing_entries =
        List.filter_map (fun (l, ctor_sort) ->
          if Hashtbl.mem seen l then None
          else Some (l, Ok ctor_sort)
        ) lts
      in
      observed_entries @ missing_entries

module Test = struct
  let mk_dsort s = match Dsort.of_string s with
    | Ok d -> d | Error _ -> assert false
  let mk_label s = match Label.of_string s with
    | Ok l -> l | Error _ -> assert false
  let mk_sort s = Sort.mk (object method loc = SourcePos.dummy end) s

  let test =
    [ (* Regression: two datasorts share constructor [L]; lookup must
         return the payload sort of the requested head sort, not
         silently the other one. *)
      QCheck.Test.make ~name:"ctor lookup disambiguates by head sort"
        ~count:1
        QCheck.unit
        (fun () ->
           let l = mk_label "La" in
           let d1 = mk_dsort "D1" in
           let d2 = mk_dsort "D2" in
           let int_s = mk_sort Sort.Int in
           let bool_s = mk_sort Sort.Bool in
           let decl1 = DsortDecl.{
             name = d1; params = [];
             ctors = [(l, int_s)];
             loc = SourcePos.dummy } in
           let decl2 = DsortDecl.{
             name = d2; params = [];
             ctors = [(l, bool_s)];
             loc = SourcePos.dummy } in
           let s = Sig.extend_sort (Sig.extend_sort Sig.empty decl1) decl2 in
           let r1 = lookup s d1 l [] in
           let r2 = lookup s d2 l [] in
           match r1, r2 with
           | Ok t1, Ok t2 ->
             Sort.compare t1 int_s = 0 && Sort.compare t2 bool_s = 0
           | _ -> false);

      (* Type-parameter substitution actually fires. *)
      QCheck.Test.make ~name:"ctor lookup substitutes type parameters"
        ~count:1
        QCheck.unit
        (fun () ->
           let l = mk_label "Wrap" in
           let d = mk_dsort "Box" in
           let a = Tvar.of_string "a" in
           let raw = mk_sort (Sort.TVar a) in
           let decl = DsortDecl.{
             name = d; params = [a];
             ctors = [(l, raw)];
             loc = SourcePos.dummy } in
           let s = Sig.extend_sort Sig.empty decl in
           let int_s = mk_sort Sort.Int in
           match lookup s d l [int_s] with
           | Ok t -> Sort.compare t int_s = 0
           | Error _ -> false);

      QCheck.Test.make ~name:"ctor lookup: unbound head sort"
        ~count:1
        QCheck.unit
        (fun () ->
           match lookup Sig.empty (mk_dsort "Nope") (mk_label "La") [] with
           | Error (Error.K_unbound_sort _) -> true
           | _ -> false);

      QCheck.Test.make ~name:"ctor lookup: ctor not in decl"
        ~count:1
        QCheck.unit
        (fun () ->
           let d = mk_dsort "D" in
           let decl = DsortDecl.{
             name = d; params = [];
             ctors = [(mk_label "Lin", mk_sort Sort.Int)];
             loc = SourcePos.dummy } in
           let s = Sig.extend_sort Sig.empty decl in
           match lookup s d (mk_label "Mout") [] with
           | Error (Error.K_ctor_not_in_decl _) -> true
           | _ -> false);
    ]
end
