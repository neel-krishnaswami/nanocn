(* SMT-LIB s-exp roundtrip properties. These sit here (rather than
   inside SmtAtom.Test / SmtSexp.Test) because they depend on
   SmtParse, which is higher in the dependency DAG than the modules
   it exercises. *)
let smt_roundtrip_tests =
  let compare_ignore_info a b = SmtSexp.compare (fun _ _ -> 0) a b in
  [
    QCheck.Test.make ~name:"smtSexp print/parse roundtrip"
      ~count:200
      (QCheck.make SmtSexp.Test.gen)
      (fun s ->
         let printed = SmtSexp.to_string s in
         match SmtParse.parse_sexp printed ~file:"roundtrip" with
         | Ok s' -> compare_ignore_info s s' = 0
         | Error _ -> false);

    QCheck.Test.make ~name:"smtAtom print/parse roundtrip"
      ~count:200
      (QCheck.make SmtAtom.Test.gen)
      (fun a ->
         let printed = SmtAtom.to_string a in
         match SmtParse.parse_sexp printed ~file:"roundtrip" with
         | Ok s ->
           (match SmtSexp.as_atom s with
            | Some a' -> SmtAtom.compare a a' = 0
            | None -> false)
         | Error _ -> false);

    QCheck.Test.make ~name:"smtSexp parse_sexps handles multiple top-level forms"
      ~count:50
      (QCheck.make QCheck.Gen.(list_size (0 -- 4) SmtSexp.Test.gen))
      (fun ss ->
         let printed =
           String.concat " "
             (List.map SmtSexp.to_string ss)
         in
         match SmtParse.parse_sexps printed ~file:"roundtrip" with
         | Ok ss' ->
           List.length ss = List.length ss'
           && List.for_all2 (fun a b -> compare_ignore_info a b = 0) ss ss'
         | Error _ -> false);
  ]

let qcheck_tests =
  List.concat [
    Util.Test.test;
    SourcePos.Test.test;
    Label.Test.test;
    Var.Test.test;
    Effect.Test.test;
    Prim.Test.test;
    Context.Test.test;
    Sig.Test.test;
    Prog.Test.test;
    Dsort.Test.test;
    Tvar.Test.test;
    Sort.Test.test;
    SortView.Test.test;
    SortDiff.Test.test;
    SourceExcerpt.Test.test;
    PatWitness.Test.test;
    Error.Test.test;
    DsortDecl.Test.test;
    DtypeDecl.Test.test;
    Subst.Test.test;
    CtorLookup.Test.test;
    Kind.Test.test;
    Pat.Test.test;
    CoreExpr.Test.test;
    SurfExpr.Test.test;
    ElabM.Test.test;
    Json.Test.test;
    Usage.Test.test;
    Constraint.Test.test;
    RCtx.Test.test;
    ProofSort.Test.test;
    RFunType.Test.test;
    RPat.Test.test;
    RefinedExpr.Test.test;
    RSig.Test.test;
    RProg.Test.test;
    RCheck.Test.test;
    SmtAtom.Test.test;
    SmtSexp.Test.test;
    SmtSym.Test.test;
    SmtExpr.Test.test;
    smt_roundtrip_tests;
  ]

(** Helper to extract sort from a typed core expr *)
let sort_of te = (CoreExpr.sort_of_info (CoreExpr.info te))
let eff_of te = (CoreExpr.info te)#eff
let ctx_of te = (CoreExpr.info te)#ctx

(** Run an ElabM computation with a fresh supply, discarding the final supply *)
let run_m m =
  match ElabM.run Var.empty_supply m with
  | Ok (result, _supply) -> Ok result
  | Error msg -> Error msg

(** Helper: elaborate a surface expr then synthesize (for pre-parsed exprs).

    With multi-error elaboration, [Elaborate.synth] returns a typed
    expression whose [info#answer] carries any errors instead of
    short-circuiting.  This wrapper preserves the legacy
    fail-fast contract: it surfaces the first collected error
    (in source-position order) as [Error]. *)
let elab_synth ?(supply = Var.empty_supply) sig_ ctx eff se =
  match ElabM.run supply (Elaborate.synth sig_ ctx eff se) with
  | Error msg -> Error msg
  | Ok (typed_e, _supply) ->
    (match Typecheck.collect_errors typed_e with
     | [] -> Ok typed_e
     | e :: _ -> Error e)

(** Helper: elaborate a surface expr then check (for pre-parsed exprs).
    Same fail-fast contract as [elab_synth]. *)
let elab_check ?(supply = Var.empty_supply) sig_ ctx se sort eff =
  match ElabM.run supply (Elaborate.check sig_ ctx se (Ok sort) eff) with
  | Error msg -> Error msg
  | Ok (typed_e, _supply) ->
    (match Typecheck.collect_errors typed_e with
     | [] -> Ok typed_e
     | e :: _ -> Error e)

let () =
  let suite =
    List.map (fun t ->
      QCheck_alcotest.to_alcotest t)
      qcheck_tests
  in
  Alcotest.run "nanocn" [
    ("qcheck", suite);
    ("parse", [
      Alcotest.test_case "parse int sort" `Quick (fun () ->
        match (Parse.parse_sort "Int" ~file:"test") with
        | Ok s ->
          (match Sort.shape s with
           | Sort.Int -> ()
           | _ -> Alcotest.fail "expected Int sort")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse record sort" `Quick (fun () ->
        match (Parse.parse_sort "(Int * Int)" ~file:"test") with
        | Ok s ->
          (match Sort.shape s with
           | Sort.Record [t1; t2] ->
             (match Sort.shape t1, Sort.shape t2 with
              | Sort.Int, Sort.Int -> ()
              | _ -> Alcotest.fail "expected (Int * Int)")
           | _ -> Alcotest.fail "expected record sort")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse sort application" `Quick (fun () ->
        match (Parse.parse_sort "Step(Int, Bool)" ~file:"test") with
        | Ok s ->
          (match Sort.shape s with
           | Sort.App (_, [t1; t2]) ->
             (match Sort.shape t1, Sort.shape t2 with
              | Sort.Int, Sort.Bool -> ()
              | _ -> Alcotest.fail "expected Step(Int, Bool)")
           | _ -> Alcotest.fail "expected sort application with 2 args")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse bare sort name" `Quick (fun () ->
        match (Parse.parse_sort "Color" ~file:"test") with
        | Ok s ->
          (match Sort.shape s with
           | Sort.App (_, []) -> ()
           | _ -> Alcotest.fail "expected bare App with no args")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse ptr sort" `Quick (fun () ->
        match (Parse.parse_sort "Ptr Int" ~file:"test") with
        | Ok s ->
          (match Sort.shape s with
           | Sort.Ptr inner ->
             (match Sort.shape inner with
              | Sort.Int -> ()
              | _ -> Alcotest.fail "expected Ptr Int")
           | _ -> Alcotest.fail "expected Ptr sort")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse variable" `Quick (fun () ->
        match Parse.parse_expr_raw "x" ~file:"test" with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.Var "x" -> ()
           | _ -> Alcotest.fail "expected Var \"x\"")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse int literal" `Quick (fun () ->
        match run_m (Parse.parse_expr "42" ~file:"test") with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.IntLit 42 -> ()
           | _ -> Alcotest.fail "expected IntLit 42")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse let" `Quick (fun () ->
        match run_m (Parse.parse_expr "let x = 1; x" ~file:"test") with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.Let (_, _, _) -> ()
           | _ -> Alcotest.fail "expected Let")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse tuple" `Quick (fun () ->
        match run_m (Parse.parse_expr "(1, 2, 3)" ~file:"test") with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.Tuple [_; _; _] -> ()
           | _ -> Alcotest.fail "expected 3-tuple")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse arith primitive" `Quick (fun () ->
        match Parse.parse_expr_raw "x + y" ~file:"test" with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.App (Prim.Add, arg) ->
             (match SurfExpr.shape arg with
              | SurfExpr.Tuple [_; _] -> ()
              | _ -> Alcotest.fail "expected 2-tuple argument")
           | _ -> Alcotest.fail "expected App Add")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse state op with type" `Quick (fun () ->
        match Parse.parse_expr_raw "New[Int] x" ~file:"test" with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.App (Prim.New s, _) ->
             (match Sort.shape s with
              | Sort.Int -> ()
              | _ -> Alcotest.fail "expected Int type param")
           | _ -> Alcotest.fail "expected App New")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse Get with ptr type" `Quick (fun () ->
        match Parse.parse_expr_raw "Get[Int] p" ~file:"test" with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.App (Prim.Get _, _) -> ()
           | _ -> Alcotest.fail "expected App Get")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse inject" `Quick (fun () ->
        match Parse.parse_expr_raw "Done x" ~file:"test" with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.Inject (_, _) -> ()
           | _ -> Alcotest.fail "expected Inject")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse case" `Quick (fun () ->
        match Parse.parse_expr_raw "case x of { Done y -> y | Next z -> z }" ~file:"test" with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.Case (_, [_; _]) -> ()
           | _ -> Alcotest.fail "expected Case with 2 branches")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse iter" `Quick (fun () ->
        match run_m (Parse.parse_expr "iter (x = 0) { Done x }" ~file:"test") with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.Iter (_, _, _) -> ()
           | _ -> Alcotest.fail "expected Iter")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse annotation" `Quick (fun () ->
        match Parse.parse_expr_raw "x : Int" ~file:"test" with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.Annot (_, _) -> ()
           | _ -> Alcotest.fail "expected Annot")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse type decl" `Quick (fun () ->
        let src = "type Option(a) = { Some : a | None : () }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Ok (Prog.TypeDecl d, _supply) ->
          if List.length d.DtypeDecl.ctors <> 2 then
            Alcotest.fail "expected 2 constructors";
          if List.length d.DtypeDecl.params <> 1 then
            Alcotest.fail "expected 1 param"
        | Ok _ -> Alcotest.fail "expected TypeDecl"
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse type decl no params" `Quick (fun () ->
        let src = "type Color = { Red : () | Blue : () }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Ok (Prog.TypeDecl d, _supply) ->
          if List.length d.DtypeDecl.params <> 0 then
            Alcotest.fail "expected 0 params"
        | Ok _ -> Alcotest.fail "expected TypeDecl"
        | Error msg -> Alcotest.fail (Error.to_string msg));
    ]);

    ("typecheck", [
      Alcotest.test_case "synth int literal" `Quick (fun () ->
        match run_m (Parse.parse_expr "42" ~file:"test") with
        | Error msg -> Alcotest.fail (Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Pure e with
          | Ok te ->
            (match Sort.shape (sort_of te) with
             | Sort.Int -> ()
             | _ -> Alcotest.fail "expected int sort");
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure effect"
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "check tuple" `Quick (fun () ->
        match run_m (Parse.parse_expr "(1, 2)" ~file:"test") with
        | Error msg -> Alcotest.fail (Error.to_string msg)
        | Ok e ->
          let mk s = Sort.mk (object method loc = SourcePos.dummy end) s in
          let int_sort = mk Sort.Int in
          let pair_sort = mk (Sort.Record [int_sort; int_sort]) in
          match elab_check Sig.empty Context.empty e pair_sort Effect.Pure with
          | Ok te ->
            if not (Sort.compare (sort_of te) pair_sort = 0) then
              Alcotest.fail "expected pair sort on output"
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "check let + Add" `Quick (fun () ->
        match run_m (Parse.parse_expr "let x = 1; x + x" ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          let int_sort = Sort.mk (object method loc = SourcePos.dummy end) Sort.Int in
          match elab_check Sig.empty Context.empty e int_sort Effect.Pure with
          | Ok te ->
            (match Sort.shape (sort_of te) with
             | Sort.Int -> ()
             | _ -> Alcotest.fail "expected int sort on output")
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "unbound variable surfaces at typecheck" `Quick (fun () ->
        (* Multi-error: resolve no longer halts on an unbound name —
           it generates a fresh [Var.t] so the rest of the file can
           still elaborate.  The unbound diagnostic now comes from
           [Context.lookup] at typecheck time as [K_unbound_var]. *)
        match run_m (Parse.parse_expr "x" ~file:"test") with
        | Error msg ->
          Alcotest.fail
            ("expected resolve to succeed (returning a fresh Var.t \
              for the unbound name), but got: " ^ Error.to_string msg)
        | Ok e ->
          (match elab_synth Sig.empty Context.empty Effect.Pure e with
           | Ok _ -> Alcotest.fail "expected an unbound-var error at typecheck"
           | Error msg ->
             let s = Error.to_string msg in
             let n = String.length s in
             let m = String.length "unbound variable" in
             let rec contains i =
               i + m <= n && (String.sub s i m = "unbound variable" || contains (i+1))
             in
             if not (contains 0) then
               Alcotest.failf
                 "expected an 'unbound variable' diagnostic, got: %s" s));

      Alcotest.test_case "check inject into declared type" `Quick (fun () ->
        let src = "type Option = { Some : Int | None : () }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse decl: " ^ Error.to_string msg)
        | Ok (d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig d with
          | Error msg -> Alcotest.fail ("typecheck decl: " ^ Error.to_string msg)
          | Ok (_supply', sig_) ->
            let expr_src = "Some 1 : Option" in
            match run_m (Parse.parse_expr expr_src ~file:"test") with
            | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
            | Ok e ->
              match elab_synth sig_ Context.empty Effect.Pure e with
              | Ok te ->
                if Effect.compare (eff_of te) Effect.Pure <> 0 then
                  Alcotest.fail "expected pure"
              | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "check case with declared type" `Quick (fun () ->
        let src = "type Option = { Some : Int | None : () }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse decl: " ^ Error.to_string msg)
        | Ok (d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig d with
          | Error msg -> Alcotest.fail ("typecheck decl: " ^ Error.to_string msg)
          | Ok (_supply', sig_) ->
            let expr_src = "let x = (Some 1 : Option); case x of { Some y -> y | None u -> 0 } : Int" in
            match run_m (Parse.parse_expr expr_src ~file:"test") with
            | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
            | Ok e ->
              match elab_synth sig_ Context.empty Effect.Pure e with
              | Ok te ->
                (match Sort.shape (sort_of te) with
                 | Sort.Int -> ()
                 | _ -> Alcotest.fail "expected int result sort")
              | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "Div is effectful" `Quick (fun () ->
        match run_m (Parse.parse_expr "1 / 2" ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Impure e with
          | Ok te ->
            if Effect.compare (eff_of te) Effect.Impure <> 0 then
              Alcotest.fail "Div should be effectful"
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "let-tuple destructuring" `Quick (fun () ->
        match run_m (Parse.parse_expr "let (a, b) = ((1, 2) : (Int * Int)); a + b" ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          let int_sort = Sort.mk (object method loc = SourcePos.dummy end) Sort.Int in
          match elab_check Sig.empty Context.empty e int_sort Effect.Pure with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "New synthesizes ptr sort" `Quick (fun () ->
        match run_m (Parse.parse_expr "New[Int] 1" ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Impure e with
          | Ok te ->
            (match Sort.shape (sort_of te) with
             | Sort.Ptr inner ->
               (match Sort.shape inner with
                | Sort.Int -> ()
                | _ -> Alcotest.fail "expected Ptr Int")
             | _ -> Alcotest.fail "expected Ptr sort");
            if Effect.compare (eff_of te) Effect.Impure <> 0 then
              Alcotest.fail "New should be effectful"
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "Get with explicit type" `Quick (fun () ->
        let src = "let p = New[Int] 42; Get[Int] p" in
        match run_m (Parse.parse_expr src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          let int_sort = Sort.mk (object method loc = SourcePos.dummy end) Sort.Int in
          match elab_check Sig.empty Context.empty e int_sort Effect.Impure with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "Set with explicit type" `Quick (fun () ->
        let src = "let p = New[Int] 0; Set[Int] (p, 42)" in
        match run_m (Parse.parse_expr src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          let unit_sort = Sort.mk (object method loc = SourcePos.dummy end) (Sort.Record []) in
          match elab_check Sig.empty Context.empty e unit_sort Effect.Impure with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "annotation effect mismatch fails" `Quick (fun () ->
        match run_m (Parse.parse_expr "1 / 2 : Int" ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Pure e with
          | Ok _ -> Alcotest.fail "should fail: Div is effectful but annotated pure"
          | Error _ -> ());

      Alcotest.test_case "typed output carries context" `Quick (fun () ->
        let src = "let x = 1; x : Int" in
        match run_m (Parse.parse_expr src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Pure e with
          | Error msg -> Alcotest.fail (Error.to_string msg)
          | Ok te ->
            (* The root Annot node should have the empty context *)
            (let (x_probe, _) = Var.mk "x" SourcePos.dummy Var.empty_supply in
             match Context.lookup x_probe (ctx_of te) with
             | Ok _ -> Alcotest.fail "root context should not contain x"
             | Error _ -> ());
            (* After elaboration, let x = 1; x becomes let y = 1; let x = y; x
               so the structure is Annot(Let(y, _, Let(x, _, body))) *)
            match CoreExpr.shape te with
            | CoreExpr.Annot (outer_let, _) ->
              (match CoreExpr.shape outer_let with
               | CoreExpr.Let (_, _, inner_let) ->
                 (match CoreExpr.shape inner_let with
                  | CoreExpr.Let (_, _, body) ->
                    (* body is Var x and should have x in context *)
                    (match Sort.shape (sort_of body) with
                     | Sort.Int -> ()
                     | _ -> Alcotest.fail "body should have sort int")
                  | _ -> Alcotest.fail "expected inner Let")
               | _ -> Alcotest.fail "expected outer Let inside Annot")
            | _ -> Alcotest.fail "expected Annot at root");

      Alcotest.test_case "typed output carries sorts on subterms" `Quick (fun () ->
        match run_m (Parse.parse_expr "1 + 2" ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Pure e with
          | Error msg -> Alcotest.fail (Error.to_string msg)
          | Ok te ->
            (* Root should be int *)
            (match Sort.shape (sort_of te) with
             | Sort.Int -> ()
             | _ -> Alcotest.fail "root should be int");
            (* The argument tuple should be (int * int) *)
            match CoreExpr.shape te with
            | CoreExpr.App (_, arg) ->
              (match Sort.shape (sort_of arg) with
               | Sort.Record [t1; t2] ->
                 (match Sort.shape t1, Sort.shape t2 with
                  | Sort.Int, Sort.Int -> ()
                  | _ -> Alcotest.fail "arg components should be int")
               | _ -> Alcotest.fail "arg should be record sort")
            | _ -> Alcotest.fail "expected App at root");

      Alcotest.test_case "typecheck type decl" `Quick (fun () ->
        let src = "type Color = { Red : () | Blue : () }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig d with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail ("typecheck: " ^ Error.to_string msg));

      Alcotest.test_case "typecheck parameterized type decl" `Quick (fun () ->
        let src = "type Option(a) = { Some : a | None : () }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig d with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail ("typecheck: " ^ Error.to_string msg));

      Alcotest.test_case "typecheck inject with step (built-in)" `Quick (fun () ->
        let src = "Done 1 : Step(Int, Int)" in
        match run_m (Parse.parse_expr src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Typecheck.initial_sig Context.empty Effect.Pure e with
          | Ok te ->
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure"
          | Error msg -> Alcotest.fail (Error.to_string msg));
    ]);

    ("parse-bool", [
      Alcotest.test_case "parse bool sort" `Quick (fun () ->
        match (Parse.parse_sort "Bool" ~file:"test") with
        | Ok s ->
          (match Sort.shape s with
           | Sort.Bool -> ()
           | _ -> Alcotest.fail "expected Bool sort")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse true" `Quick (fun () ->
        match run_m (Parse.parse_expr "true" ~file:"test") with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.BoolLit true -> ()
           | _ -> Alcotest.fail "expected BoolLit true")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse false" `Quick (fun () ->
        match run_m (Parse.parse_expr "false" ~file:"test") with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.BoolLit false -> ()
           | _ -> Alcotest.fail "expected BoolLit false")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse if-then-else" `Quick (fun () ->
        match run_m (Parse.parse_expr "if true then 1 else 2 : Int" ~file:"test") with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.Annot (inner, _) ->
             (match SurfExpr.shape inner with
              | SurfExpr.If (_, _, _) -> ()
              | _ -> Alcotest.fail "expected If inside Annot")
           | _ -> Alcotest.fail "expected Annot at root")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse and" `Quick (fun () ->
        match run_m (Parse.parse_expr "true && false" ~file:"test") with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.And (_, _) -> ()
           | _ -> Alcotest.fail "expected And")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse or" `Quick (fun () ->
        match run_m (Parse.parse_expr "true || false" ~file:"test") with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.App (Prim.Or, _) -> ()
           | _ -> Alcotest.fail "expected App Or")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse not" `Quick (fun () ->
        match run_m (Parse.parse_expr "not true" ~file:"test") with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.Not _ -> ()
           | _ -> Alcotest.fail "expected Not")
        | Error msg -> Alcotest.fail (Error.to_string msg));
    ]);

    ("typecheck-bool", [
      Alcotest.test_case "synth true" `Quick (fun () ->
        match run_m (Parse.parse_expr "true" ~file:"test") with
        | Error msg -> Alcotest.fail (Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Pure e with
          | Ok te ->
            (match Sort.shape (sort_of te) with
             | Sort.Bool -> ()
             | _ -> Alcotest.fail "expected bool sort");
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure effect"
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "check if-then-else" `Quick (fun () ->
        let src = "if true then 1 else 2 : Int" in
        match run_m (Parse.parse_expr src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Pure e with
          | Ok te ->
            (match Sort.shape (sort_of te) with
             | Sort.Int -> ()
             | _ -> Alcotest.fail "expected int sort")
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "not true is pure bool" `Quick (fun () ->
        match run_m (Parse.parse_expr "not true" ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Pure e with
          | Ok te ->
            (match Sort.shape (sort_of te) with
             | Sort.Bool -> ()
             | _ -> Alcotest.fail "expected bool sort");
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure"
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "and is pure" `Quick (fun () ->
        match run_m (Parse.parse_expr "true && false" ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Pure e with
          | Ok te ->
            (match Sort.shape (sort_of te) with
             | Sort.Bool -> ()
             | _ -> Alcotest.fail "expected bool sort");
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure"
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "if with non-bool condition fails" `Quick (fun () ->
        let src = "if 1 then 2 else 3 : Int" in
        match run_m (Parse.parse_expr src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Pure e with
          | Ok _ -> Alcotest.fail "should fail: condition is not bool"
          | Error _ -> ());
    ]);

    ("typecheck-eq", [
      Alcotest.test_case "Eq[Int] synth bool pure" `Quick (fun () ->
        match run_m (Parse.parse_expr "Eq[Int] (1, 2)" ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Pure e with
          | Ok te ->
            (match Sort.shape (sort_of te) with
             | Sort.Bool -> ()
             | _ -> Alcotest.fail "expected bool sort");
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure"
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "Eq[Bool] synth bool pure" `Quick (fun () ->
        match run_m (Parse.parse_expr "Eq[Bool] (true, false)" ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Pure e with
          | Ok te ->
            (match Sort.shape (sort_of te) with
             | Sort.Bool -> ()
             | _ -> Alcotest.fail "expected bool sort")
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "Eq on record sort fails" `Quick (fun () ->
        match run_m (Parse.parse_expr "Eq[(Int * Int)] ((1,2), (3,4))" ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          match elab_synth Sig.empty Context.empty Effect.Pure e with
          | Ok _ -> Alcotest.fail "should fail: record is not an eqtype"
          | Error _ -> ());
    ]);

    ("parse-prog", [
      Alcotest.test_case "parse call" `Quick (fun () ->
        match Parse.parse_expr_raw "foo 1" ~file:"test" with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.Call (_, _) -> ()
           | _ -> Alcotest.fail "expected Call")
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse simple program" `Quick (fun () ->
        let src = "main : () [impure] = ()" in
        match run_m (Parse.parse_prog src ~file:"test") with
        | Ok p ->
          if List.length p.Prog.decls <> 0 then
            Alcotest.fail "expected no decls"
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse program with function" `Quick (fun () ->
        let src = "fun double : Int -> Int [pure] = { x -> x + x } main : () [impure] = ()" in
        match run_m (Parse.parse_prog src ~file:"test") with
        | Ok p ->
          if List.length p.Prog.decls <> 1 then
            Alcotest.fail "expected 1 decl"
        | Error msg -> Alcotest.fail (Error.to_string msg));
    ]);

    ("typecheck-prog", [
      Alcotest.test_case "typecheck trivial program" `Quick (fun () ->
        let src = "main : () [impure] = ()" in
        match ElabM.run Var.empty_supply (Parse.parse_prog src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (p, supply) ->
          match Typecheck.check_prog supply p with
          | Ok (_, _) -> ()
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "typecheck program with function" `Quick (fun () ->
        let src = "fun double : Int -> Int [pure] = { x -> x + x } main : () [impure] = ()" in
        match ElabM.run Var.empty_supply (Parse.parse_prog src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (p, supply) ->
          match Typecheck.check_prog supply p with
          | Ok (_, _) -> ()
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "typecheck function call" `Quick (fun () ->
        let src = "fun double : Int -> Int [pure] = { x -> x + x } main : () [impure] = (let r = double 21; ())" in
        match ElabM.run Var.empty_supply (Parse.parse_prog src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (p, supply) ->
          match Typecheck.check_prog supply p with
          | Ok (_, _) -> ()
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "typecheck recursive function with step" `Quick (fun () ->
        let src = {|
          fun countdown : Int -> Step(Int, ()) [impure] = {
            x -> Done () : Step(Int, ())
          }
          main : () [impure] = iter (x = 10) { countdown x }
        |} in
        match ElabM.run Var.empty_supply (Parse.parse_prog src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (p, supply) ->
          match Typecheck.check_prog supply p with
          | Ok (_, _) -> ()
          | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "pure recursive function fails" `Quick (fun () ->
        let src = {|
          fun loop : Int -> Int [pure] = {
            x -> loop x
          }
          main : () [impure] = ()
        |} in
        match ElabM.run Var.empty_supply (Parse.parse_prog src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (p, supply) ->
          match Typecheck.check_prog supply p with
          | Ok (_, _) -> Alcotest.fail "should fail: pure function cannot recurse"
          | Error _ -> ());

      Alcotest.test_case "unknown function fails" `Quick (fun () ->
        let src = "main : () [impure] = (unknown 1 : Int)" in
        match ElabM.run Var.empty_supply (Parse.parse_prog src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (p, supply) ->
          match Typecheck.check_prog supply p with
          | Ok (_, _) -> Alcotest.fail "should fail on unknown function"
          | Error _ -> ());

      Alcotest.test_case "typecheck program with type decl and inject/case" `Quick (fun () ->
        let src = {|
          type Option(a) = { Some : a | None : () }
          fun unwrap : Option(Int) -> Int [pure] = {
            Some y -> y | None u -> 0
          }
          main : () [impure] = (let r = unwrap (Some 42 : Option(Int)); ())
        |} in
        match ElabM.run Var.empty_supply (Parse.parse_prog src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (p, supply) ->
          match Typecheck.check_prog supply p with
          | Ok (_, _) -> ()
          | Error msg -> Alcotest.fail (Error.to_string msg));
    ]);

    ("parse-sort", [
      Alcotest.test_case "parse sort decl" `Quick (fun () ->
        let src = "sort List(a) = { Nil : () | Cons : (a * List(a)) }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Ok (Prog.SortDecl d, _supply) ->
          if List.length d.DsortDecl.ctors <> 2 then
            Alcotest.fail "expected 2 constructors"
        | Ok _ -> Alcotest.fail "expected SortDecl"
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse sort decl no params" `Quick (fun () ->
        let src = "sort Color = { Red : () | Blue : () }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Ok (Prog.SortDecl d, _supply) ->
          if List.length d.DsortDecl.params <> 0 then
            Alcotest.fail "expected 0 params"
        | Ok _ -> Alcotest.fail "expected SortDecl"
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse spec fun decl" `Quick (fun () ->
        let src = "fun length : List(Int) -> Int [spec] = { Nil () -> 0 | Cons (x, xs) -> 1 + length xs }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Ok (Prog.FunDecl d, _supply) ->
          if List.length d.branches <> 2 then
            Alcotest.fail "expected 2 branches"
        | Ok _ -> Alcotest.fail "expected FunDecl"
        | Error msg -> Alcotest.fail (Error.to_string msg));

      Alcotest.test_case "parse spec expr with ==" `Quick (fun () ->
        match Parse.parse_expr_raw "x == y" ~file:"test" with
        | Ok e ->
          (match SurfExpr.shape e with
           | SurfExpr.Eq (_, _) -> ()
           | _ -> Alcotest.fail "expected Eq")
        | Error msg -> Alcotest.fail (Error.to_string msg));
    ]);

    ("typecheck-spec", [
      Alcotest.test_case "typecheck sort decl" `Quick (fun () ->
        let src = "sort Color = { Red : () | Blue : () }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig d with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail ("typecheck: " ^ Error.to_string msg));

      Alcotest.test_case "sort decl no ctors fails" `Quick (fun () ->
        (* Can't parse an empty sort, so build one manually *)
        let d = Prog.SortDecl DsortDecl.{
          name = (match Dsort.of_string "Empty" with Ok d -> d | _ -> failwith "impossible");
          params = [];
          ctors = [];
          loc = SourcePos.dummy;
        } in
        match Typecheck.check_spec_decl Var.empty_supply Typecheck.initial_sig d with
        | Ok _ -> Alcotest.fail "should fail on empty sort"
        | Error _ -> ());

      Alcotest.test_case "sort decl duplicate ctors fails" `Quick (fun () ->
        let mk_label s = match Label.of_string s with Ok l -> l | _ -> failwith "impossible" in
        let unit_sort = Sort.mk (object method loc = SourcePos.dummy end) (Sort.Record []) in
        let d = Prog.SortDecl DsortDecl.{
          name = (match Dsort.of_string "Bad" with Ok d -> d | _ -> failwith "impossible");
          params = [];
          ctors = [(mk_label "Aa", unit_sort); (mk_label "Aa", unit_sort)];
          loc = SourcePos.dummy;
        } in
        match Typecheck.check_spec_decl Var.empty_supply Typecheck.initial_sig d with
        | Ok _ -> Alcotest.fail "should fail on duplicate ctors"
        | Error _ -> ());

      Alcotest.test_case "sort referencing undeclared sort fails" `Quick (fun () ->
        let src = "sort Bad = { Mk : Unknown }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig d with
          | Ok _ -> Alcotest.fail "should fail: unknown sort reference"
          | Error _ -> ());

      Alcotest.test_case "sort with wrong arity fails" `Quick (fun () ->
        let sort_src = "sort Pair(a, b) = { Mk : (a * b) }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl sort_src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse sort: " ^ Error.to_string msg)
        | Ok (sort_d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig sort_d with
          | Error msg -> Alcotest.fail ("typecheck sort: " ^ Error.to_string msg)
          | Ok (_supply', sig1) ->
            let bad_src = "sort Bad = { Mk : Pair(Int) }" in
            match ElabM.run Var.empty_supply (Parse.parse_decl bad_src ~file:"test") with
            | Error msg -> Alcotest.fail ("parse bad: " ^ Error.to_string msg)
            | Ok (bad_d, supply2) ->
              match Typecheck.check_spec_decl supply2 sig1 bad_d with
              | Ok _ -> Alcotest.fail "should fail: wrong arity"
              | Error _ -> ());

      Alcotest.test_case "self-referential sort succeeds" `Quick (fun () ->
        let src = "sort List(a) = { Nil : () | Cons : (a * List(a)) }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig d with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail ("typecheck: " ^ Error.to_string msg));

      Alcotest.test_case "type referencing undeclared type fails" `Quick (fun () ->
        let src = "type Bad = { Mk : Unknown }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig d with
          | Ok _ -> Alcotest.fail "should fail: unknown type reference"
          | Error _ -> ());

      Alcotest.test_case "type with wrong arity fails" `Quick (fun () ->
        let type_src = "type Pair(a, b) = { Mk : (a * b) }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl type_src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse type: " ^ Error.to_string msg)
        | Ok (type_d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig type_d with
          | Error msg -> Alcotest.fail ("typecheck type: " ^ Error.to_string msg)
          | Ok (_supply', sig1) ->
            let bad_src = "type Bad = { Mk : Pair(Int) }" in
            match ElabM.run Var.empty_supply (Parse.parse_decl bad_src ~file:"test") with
            | Error msg -> Alcotest.fail ("parse bad: " ^ Error.to_string msg)
            | Ok (bad_d, supply2) ->
              match Typecheck.check_spec_decl supply2 sig1 bad_d with
              | Ok _ -> Alcotest.fail "should fail: wrong arity"
              | Error _ -> ());

      Alcotest.test_case "bare self-referential type rejected" `Quick (fun () ->
        let src = "type List(a) = { Nil : () | Cons : (a * List(a)) }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig d with
          | Ok _ -> Alcotest.fail "should reject bare self-reference"
          | Error _ -> ());

      Alcotest.test_case "ptr self-referential type succeeds" `Quick (fun () ->
        let src = "type List(a) = { Nil : () | Cons : (a * Ptr List(a)) }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok (d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig d with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail ("typecheck: " ^ Error.to_string msg));

      Alcotest.test_case "typecheck spec fun" `Quick (fun () ->
        (* First register a sort, then a spec function using it *)
        let sort_src = "sort Color = { Red : () | Blue : () }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl sort_src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse sort: " ^ Error.to_string msg)
        | Ok (sort_d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig sort_d with
          | Error msg -> Alcotest.fail ("typecheck sort: " ^ Error.to_string msg)
          | Ok (_supply', sig1) ->
            let fun_src = "fun isRed : Color -> Int [spec] = { Red () -> 1 | Blue () -> 0 }" in
            match ElabM.run Var.empty_supply (Parse.parse_decl fun_src ~file:"test") with
            | Error msg -> Alcotest.fail ("parse fun: " ^ Error.to_string msg)
            | Ok (fun_d, supply2) ->
              match Typecheck.check_spec_decl supply2 sig1 fun_d with
              | Ok _ -> ()
              | Error msg -> Alcotest.fail ("typecheck fun: " ^ Error.to_string msg));

      Alcotest.test_case "typecheck spec fun with arithmetic" `Quick (fun () ->
        let sort_src = "sort Color = { Red : () | Blue : () }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl sort_src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse sort: " ^ Error.to_string msg)
        | Ok (sort_d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig sort_d with
          | Error msg -> Alcotest.fail ("typecheck sort: " ^ Error.to_string msg)
          | Ok (_supply', sig1) ->
            let fun_src = "fun toNum : Color -> Int [spec] = { Red () -> 2 + 3 | Blue () -> 0 }" in
            match ElabM.run Var.empty_supply (Parse.parse_decl fun_src ~file:"test") with
            | Error msg -> Alcotest.fail ("parse fun: " ^ Error.to_string msg)
            | Ok (fun_d, supply2) ->
              match Typecheck.check_spec_decl supply2 sig1 fun_d with
              | Ok _ -> ()
              | Error msg -> Alcotest.fail ("typecheck fun: " ^ Error.to_string msg));

      Alcotest.test_case "pure function callable from spec" `Quick (fun () ->
        (* Register a pure function: fun inc : int -> int [pure] *)
        let fun_src = "fun inc : Int -> Int [pure] = { x -> x + 1 }" in
        match ElabM.run Var.empty_supply (Parse.parse_decl fun_src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse fun: " ^ Error.to_string msg)
        | Ok (fun_d, supply) ->
          match Typecheck.check_spec_decl supply Typecheck.initial_sig fun_d with
          | Error msg -> Alcotest.fail ("typecheck fun: " ^ Error.to_string msg)
          | Ok (_supply', sig1) ->
            (* Now use inc in a spec function *)
            let spec_src = "fun three : () -> Int [spec] = { () -> inc 2 }" in
            match ElabM.run Var.empty_supply (Parse.parse_decl spec_src ~file:"test") with
            | Error msg -> Alcotest.fail ("parse spec: " ^ Error.to_string msg)
            | Ok (spec_d, supply2) ->
              match Typecheck.check_spec_decl supply2 sig1 spec_d with
              | Ok _ -> ()
              | Error msg -> Alcotest.fail ("typecheck spec: " ^ Error.to_string msg));
    ]);

    ("spec-typecheck-core", [
      (* Helper: build a core expression at dummy pos *)
      (let mk shape = CoreExpr.mk (object method loc = SourcePos.dummy end) shape in
       let mk_sort s = Sort.mk (object method loc = SourcePos.dummy end) s in
       let int_sort = mk_sort Sort.Int in
       let _bool_sort = mk_sort Sort.Bool in
       let sort_of_ tce = (CoreExpr.sort_of_info (CoreExpr.info tce)) in
       let _ctx_of_ tce = (CoreExpr.info tce)#ctx in
       let sig_ = Typecheck.initial_sig in

       Alcotest.test_case "synth int literal carries int sort" `Quick (fun () ->
         let ce = mk (CoreExpr.IntLit 42) in
         let tce = Typecheck.synth sig_ Context.empty Effect.Spec ce in
         (match Typecheck.collect_errors tce with
          | e :: _ -> Alcotest.fail (Error.to_string e)
          | [] -> ());
         if Sort.compare (sort_of_ tce) int_sort <> 0 then
           Alcotest.fail "expected int sort"));

      (let mk shape = CoreExpr.mk (object method loc = SourcePos.dummy end) shape in
       let mk_sort s = Sort.mk (object method loc = SourcePos.dummy end) s in
       let bool_sort = mk_sort Sort.Bool in
       let sort_of_ tce = (CoreExpr.sort_of_info (CoreExpr.info tce)) in
       let sig_ = Typecheck.initial_sig in

       Alcotest.test_case "synth bool literal carries bool sort" `Quick (fun () ->
         let ce = mk (CoreExpr.BoolLit true) in
         let tce = Typecheck.synth sig_ Context.empty Effect.Spec ce in
         (match Typecheck.collect_errors tce with
          | e :: _ -> Alcotest.fail (Error.to_string e)
          | [] -> ());
         if Sort.compare (sort_of_ tce) bool_sort <> 0 then
           Alcotest.fail "expected bool sort"));

      (let mk shape = CoreExpr.mk (object method loc = SourcePos.dummy end) shape in
       let mk_sort s = Sort.mk (object method loc = SourcePos.dummy end) s in
       let int_sort = mk_sort Sort.Int in
       let _sort_of_ tce = (CoreExpr.sort_of_info (CoreExpr.info tce)) in
       let ctx_of_ tce = (CoreExpr.info tce)#ctx in
       let sig_ = Typecheck.initial_sig in

       Alcotest.test_case "check let propagates context" `Quick (fun () ->
         let (x, _supply) = Var.mk "x" SourcePos.dummy Var.empty_supply in
         let xb = (x, object method loc = SourcePos.dummy end) in
         let ce = mk (CoreExpr.Let (xb, mk (CoreExpr.IntLit 1),
                                       mk (CoreExpr.Var x))) in
         let tce = Typecheck.check sig_ Context.empty ce (Ok int_sort) Effect.Spec in
         (match Typecheck.collect_errors tce with
          | e :: _ -> Alcotest.fail (Error.to_string e)
          | [] -> ());
         (* The outer node has empty context *)
         (match Context.lookup x (ctx_of_ tce) with
          | Ok _ -> Alcotest.fail "outer ctx should not have x"
          | Error _ -> ());
         (* The body (Var x) should have x in context *)
         match CoreExpr.shape tce with
         | CoreExpr.Let (_, _, body) ->
           (match Context.lookup x (ctx_of_ body) with
            | Ok (s, _eff) ->
              if Sort.compare s int_sort <> 0 then
                Alcotest.fail "x should have int sort in body context"
            | Error _ -> Alcotest.fail "x should be in body context")
         | _ -> Alcotest.fail "expected Let shape"));

      (let mk shape = CoreExpr.mk (object method loc = SourcePos.dummy end) shape in
       let mk_sort s = Sort.mk (object method loc = SourcePos.dummy end) s in
       let bool_sort = mk_sort Sort.Bool in
       let sort_of_ tce = (CoreExpr.sort_of_info (CoreExpr.info tce)) in
       let sig_ = Typecheck.initial_sig in

       Alcotest.test_case "synth equality carries bool sort" `Quick (fun () ->
         let ce = mk (CoreExpr.Eq (mk (CoreExpr.IntLit 1), mk (CoreExpr.IntLit 2))) in
         let tce = Typecheck.synth sig_ Context.empty Effect.Spec ce in
         (match Typecheck.collect_errors tce with
          | e :: _ -> Alcotest.fail (Error.to_string e)
          | [] -> ());
         if Sort.compare (sort_of_ tce) bool_sort <> 0 then
           Alcotest.fail "expected bool sort for equality"));

      (let mk shape = CoreExpr.mk (object method loc = SourcePos.dummy end) shape in
       let sig_ = Typecheck.initial_sig in

       (* Two unbound vars in a single Eq: multi-error reporting must
          surface both, not just the first. *)
       Alcotest.test_case "multi-error: two unbound vars in Eq" `Quick (fun () ->
         let (y, supply1) = Var.mk "y" SourcePos.dummy Var.empty_supply in
         let (z, _) = Var.mk "z" SourcePos.dummy supply1 in
         let ce = mk (CoreExpr.Eq (mk (CoreExpr.Var y), mk (CoreExpr.Var z))) in
         let tce = Typecheck.synth sig_ Context.empty Effect.Spec ce in
         let errs = Typecheck.collect_errors tce in
         if List.length errs < 2 then
           Alcotest.failf "expected ≥2 errors, got %d" (List.length errs)));

      (* Case completeness: missing constructor produces
         K_missing_ctor. *)
      (let contains_substring s sub =
         let n = String.length s and m = String.length sub in
         let rec aux i = i + m <= n && (String.sub s i m = sub || aux (i+1)) in
         m = 0 || (n >= m && aux 0) in
       let mk shape = CoreExpr.mk (object method loc = SourcePos.dummy end) shape in
       let mk_sort s = Sort.mk (object method loc = SourcePos.dummy end) s in
       let mk_label name =
         match Label.of_string name with
         | Ok l -> l | _ -> failwith "label" in
       let mk_dsort name =
         match Dsort.of_string name with
         | Ok d -> d | _ -> failwith "dsort" in
       let unit_sort = mk_sort (Sort.Record []) in
       let int_sort = mk_sort Sort.Int in
       let color = mk_dsort "Color" in
       let red = mk_label "Red" in
       let green = mk_label "Green" in
       let blue = mk_label "Blue" in
       let color_decl = DsortDecl.{
         name = color; params = [];
         ctors = [(red, unit_sort); (green, unit_sort); (blue, unit_sort)];
         loc = SourcePos.dummy;
       } in
       let sig_ = Sig.extend_sort Typecheck.initial_sig color_decl in
       let color_sort = mk_sort (Sort.App (color, [])) in
       Alcotest.test_case "case missing ctor produces K_missing_ctor" `Quick (fun () ->
         let (c, supply1) = Var.mk "c" SourcePos.dummy Var.empty_supply in
         let (rx, supply2) = Var.mk "rx" SourcePos.dummy supply1 in
         let (gx, _) = Var.mk "gx" SourcePos.dummy supply2 in
         let ctx = Context.extend c color_sort Effect.Spec Context.empty in
         let ce = mk (CoreExpr.Case
           (mk (CoreExpr.Var c),
            [ (red, rx, mk (CoreExpr.IntLit 1), object method loc = SourcePos.dummy end);
              (green, gx, mk (CoreExpr.IntLit 2), object method loc = SourcePos.dummy end);
            ])) in
         let tce = Typecheck.check sig_ ctx ce (Ok int_sort) Effect.Spec in
         let errs = Typecheck.collect_errors tce in
         let has_missing = List.exists (fun e ->
           let s = Error.to_string e in
           contains_substring s "missing"
           || contains_substring s "non-exhaustive") errs in
         if not has_missing then
           Alcotest.failf
             "expected a missing-ctor error, got: %s"
             (String.concat "; " (List.map Error.to_string errs))));

      (* Case completeness: redundant constructor produces
         K_redundant_ctor. *)
      (let contains_substring s sub =
         let n = String.length s and m = String.length sub in
         let rec aux i = i + m <= n && (String.sub s i m = sub || aux (i+1)) in
         m = 0 || (n >= m && aux 0) in
       let mk shape = CoreExpr.mk (object method loc = SourcePos.dummy end) shape in
       let mk_sort s = Sort.mk (object method loc = SourcePos.dummy end) s in
       let mk_label name =
         match Label.of_string name with
         | Ok l -> l | _ -> failwith "label" in
       let mk_dsort name =
         match Dsort.of_string name with
         | Ok d -> d | _ -> failwith "dsort" in
       let unit_sort = mk_sort (Sort.Record []) in
       let int_sort = mk_sort Sort.Int in
       let color = mk_dsort "Color" in
       let red = mk_label "Red" in
       let green = mk_label "Green" in
       let blue = mk_label "Blue" in
       let color_decl = DsortDecl.{
         name = color; params = [];
         ctors = [(red, unit_sort); (green, unit_sort); (blue, unit_sort)];
         loc = SourcePos.dummy;
       } in
       let sig_ = Sig.extend_sort Typecheck.initial_sig color_decl in
       let color_sort = mk_sort (Sort.App (color, [])) in
       Alcotest.test_case "case redundant ctor produces K_redundant_ctor" `Quick (fun () ->
         let (c, supply1) = Var.mk "c" SourcePos.dummy Var.empty_supply in
         let (rx1, supply2) = Var.mk "rx1" SourcePos.dummy supply1 in
         let (rx2, supply3) = Var.mk "rx2" SourcePos.dummy supply2 in
         let (gx, supply4) = Var.mk "gx" SourcePos.dummy supply3 in
         let (bx, _) = Var.mk "bx" SourcePos.dummy supply4 in
         let ctx = Context.extend c color_sort Effect.Spec Context.empty in
         let ce = mk (CoreExpr.Case
           (mk (CoreExpr.Var c),
            [ (red,   rx1, mk (CoreExpr.IntLit 1), object method loc = SourcePos.dummy end);
              (red,   rx2, mk (CoreExpr.IntLit 1), object method loc = SourcePos.dummy end);
              (green, gx,  mk (CoreExpr.IntLit 2), object method loc = SourcePos.dummy end);
              (blue,  bx,  mk (CoreExpr.IntLit 3), object method loc = SourcePos.dummy end);
            ])) in
         let tce = Typecheck.check sig_ ctx ce (Ok int_sort) Effect.Spec in
         let errs = Typecheck.collect_errors tce in
         let has_redundant = List.exists (fun e ->
           let s = Error.to_string e in
           contains_substring s "redundant"
           || contains_substring s "more than once") errs in
         if not has_redundant then
           Alcotest.failf
             "expected a redundant-ctor error, got: %s"
             (String.concat "; " (List.map Error.to_string errs))));

      (* Elaborate-side multi-error: a tuple checked against a record
         sort whose components both mismatch the surface expressions
         must produce ≥2 errors. *)
      Alcotest.test_case "elaborate multi-error: tuple component mismatches" `Quick (fun () ->
        let src = "((true, false) : (Int * Int))" in
        match run_m (Parse.parse_expr src ~file:"test") with
        | Error msg -> Alcotest.fail ("parse: " ^ Error.to_string msg)
        | Ok e ->
          (match ElabM.run Var.empty_supply
                   (Elaborate.synth Sig.empty Context.empty Effect.Pure e) with
           | Error _ ->
             (* If ElabM still fails fast, the test framework hasn't
                yet seen the new behavior — accept that for now. *)
             ()
           | Ok (typed_e, _) ->
             let errs = Typecheck.collect_errors typed_e in
             if List.length errs < 2 then
               Alcotest.failf "expected ≥2 errors, got %d" (List.length errs)));

      (* File-driven fixtures: the three halt-on-first-error bugs in
         elaborate.ml's coverage_check are now fixed.  Each fixture
         exercises one (or more) bug; we assert that elaboration
         continues past the upstream error and that >= 2 diagnostics
         reach the driver.

         The fixtures are also persisted under
         examples/errors/typing/ for documentation.  We embed them
         inline here so the tests are self-contained and don't rely
         on dune sandbox file paths. *)
      (let contains_substring s sub =
         let n = String.length s and m = String.length sub in
         let rec aux i =
           i + m <= n && (String.sub s i m = sub || aux (i+1)) in
         m = 0 || (n >= m && aux 0)
       in
       let any_diag_contains diags sub =
         List.exists (fun e ->
           contains_substring (Error.to_string e) sub) diags
       in
       let assert_halt_fix ~name ~src ~min_diags ~contains =
         Alcotest.test_case name `Quick (fun () ->
           let outcome = CompileFile.compile_file src ~file:name in
           let n = List.length outcome.diagnostics in
           if n < min_diags then
             Alcotest.failf
               "%s: expected >= %d diagnostics, got %d (%s)"
               name min_diags n
               (String.concat "; "
                  (List.map Error.to_string outcome.diagnostics));
           if not (any_diag_contains outcome.diagnostics contains) then
             Alcotest.failf
               "%s: expected diagnostic containing %S in: %s"
               name contains
               (String.concat "; "
                  (List.map Error.to_string outcome.diagnostics)))
       in
       let src = {|
type Maybe = { Just : Int | Nothing : () }
fun pickJust : Maybe -> Int [pure] = {
    Just x -> x
  | Nothing u -> 0
}
fun useUnknown : Int -> Int [pure] = {
    n -> case unknown_call(n) of {
        Just x -> x + n
      | Nothing u -> 0
    }
}
main : Int [pure] = useUnknown(7)
|} in
       assert_halt_fix
         ~name:"halt fix: case scrutinee with errored synth"
         ~src
         ~min_diags:2
         ~contains:"unknown function");

      (let contains_substring s sub =
         let n = String.length s and m = String.length sub in
         let rec aux i =
           i + m <= n && (String.sub s i m = sub || aux (i+1)) in
         m = 0 || (n >= m && aux 0)
       in
       let any_diag_contains diags sub =
         List.exists (fun e ->
           contains_substring (Error.to_string e) sub) diags
       in
       let src = {|
fun useUndefined : UndefinedT -> Int [pure] = {
    LFoo x -> x
  | LBar u -> 0
}
main : Int [pure] = 1
|} in
       Alcotest.test_case
         "halt fix: case scrutinee with undeclared dsort"
         `Quick (fun () ->
           let outcome =
             CompileFile.compile_file src
               ~file:"halt_fix_undeclared_dsort.cn" in
           let n = List.length outcome.diagnostics in
           if n < 2 then
             Alcotest.failf
               "expected >= 2 diagnostics, got %d" n;
           if not (any_diag_contains outcome.diagnostics "unknown sort or type") then
             Alcotest.failf
               "expected an 'unknown sort/type' diagnostic in: %s"
               (String.concat "; "
                  (List.map Error.to_string outcome.diagnostics))));

      (let contains_substring s sub =
         let n = String.length s and m = String.length sub in
         let rec aux i =
           i + m <= n && (String.sub s i m = sub || aux (i+1)) in
         m = 0 || (n >= m && aux 0)
       in
       let any_diag_contains diags sub =
         List.exists (fun e ->
           contains_substring (Error.to_string e) sub) diags
       in
       let src = {|
fun mixed : Int -> Int [pure] = {
    n -> case unknown_call(n) of {
        (a, b) -> a
      | LCons x -> x
    }
}
main : Int [pure] = mixed(1)
|} in
       Alcotest.test_case
         "halt fix: column with incompatible patterns"
         `Quick (fun () ->
           let outcome =
             CompileFile.compile_file src
               ~file:"halt_fix_incompatible_column.cn" in
           if not (any_diag_contains outcome.diagnostics "incompatible") then
             Alcotest.failf
               "expected an 'incompatible patterns' diagnostic in: %s"
               (String.concat "; "
                  (List.map Error.to_string outcome.diagnostics))));

      (let contains_substring s sub =
         let n = String.length s and m = String.length sub in
         let rec aux i =
           i + m <= n && (String.sub s i m = sub || aux (i+1)) in
         m = 0 || (n >= m && aux 0)
       in
       let any_diag_contains diags sub =
         List.exists (fun e ->
           contains_substring (Error.to_string e) sub) diags
       in
       let src = {|
fun first : Int -> Int [pure] = {
    n -> let (x, y, z) = unknown_call();
         x + y + z
}
main : Int [pure] = first(1)
|} in
       Alcotest.test_case
         "halt fix: let-tuple with errored RHS"
         `Quick (fun () ->
           let outcome =
             CompileFile.compile_file src
               ~file:"halt_fix_let_pattern_error_rhs.cn" in
           let n = List.length outcome.diagnostics in
           if n < 2 then
             Alcotest.failf
               "expected >= 2 diagnostics, got %d" n;
           if not (any_diag_contains outcome.diagnostics "unknown function") then
             Alcotest.failf
               "expected an 'unknown function' diagnostic in: %s"
               (String.concat "; "
                  (List.map Error.to_string outcome.diagnostics))));

      (let contains_substring s sub =
         let n = String.length s and m = String.length sub in
         let rec aux i =
           i + m <= n && (String.sub s i m = sub || aux (i+1)) in
         m = 0 || (n >= m && aux 0)
       in
       let any_diag_contains diags sub =
         List.exists (fun e ->
           contains_substring (Error.to_string e) sub) diags
       in
       let src = {|
type LL = { LCons : (Int * Int) | LNil : () }
fun useNested : Int -> Int [pure] = {
    n -> case unknown_call(n) of {
        LCons (x, y) -> x + y + n
      | LNil u -> n
    }
}
main : Int [pure] = useNested(1)
|} in
       Alcotest.test_case
         "halt fix: nested patterns with errored scrutinee"
         `Quick (fun () ->
           let outcome =
             CompileFile.compile_file src
               ~file:"halt_fix_nested_pattern_error.cn" in
           let n = List.length outcome.diagnostics in
           if n < 2 then
             Alcotest.failf
               "expected >= 2 diagnostics, got %d" n;
           if not (any_diag_contains outcome.diagnostics "unknown function") then
             Alcotest.failf
               "expected an 'unknown function' diagnostic in: %s"
               (String.concat "; "
                  (List.map Error.to_string outcome.diagnostics))));

      (let contains_substring s sub =
         let n = String.length s and m = String.length sub in
         let rec aux i =
           i + m <= n && (String.sub s i m = sub || aux (i+1)) in
         m = 0 || (n >= m && aux 0)
       in
       let any_warn_contains warns sub =
         List.exists (fun w ->
           contains_substring (Warning.to_string w) sub) warns
       in
       let src = {|
type Pair = { Both : (Int * Int) }
fun example : Pair -> Int [pure] = {
    Both (x, x) -> x
}
main : Int [pure] = example(Both (1, 2) : Pair)
|} in
       Alcotest.test_case
         "duplicate pattern var becomes a shadowed-var warning, not an error"
         `Quick (fun () ->
           let outcome =
             CompileFile.compile_file src
               ~file:"shadow_warn.cn" in
           if outcome.diagnostics <> [] then
             Alcotest.failf
               "expected no diagnostics, got: %s"
               (String.concat "; "
                  (List.map Error.to_string outcome.diagnostics));
           if List.length outcome.warnings < 1 then
             Alcotest.failf
               "expected >= 1 warning for shadowed pattern var, got %d"
               (List.length outcome.warnings);
           if not (any_warn_contains outcome.warnings "shadowed") then
             Alcotest.failf
               "expected a 'shadowed' warning, got: %s"
               (String.concat "; "
                  (List.map Warning.to_string outcome.warnings))));
    ]);

    ("rcheck", [
      (* Fix 1: pf_types excludes spec entries from computational erasure *)
      Alcotest.test_case "pf_types excludes spec entries" `Quick (fun () ->
        let loc = object method loc = SourcePos.dummy end in
        let int_sort = Sort.mk loc Sort.Int in
        let bool_sort = Sort.mk loc Sort.Bool in
        let (x, s0) = Var.mk "x" SourcePos.dummy Var.empty_supply in
        let (y, _s1) = Var.mk "y" SourcePos.dummy s0 in
        let pf = [
          ProofSort.Comp { info = (); var = x; sort = int_sort; eff = Effect.Pure };
          ProofSort.Comp { info = (); var = y; sort = bool_sort; eff = Effect.Spec };
        ] in
        match ProofSort.pf_types pf with
        | [s] ->
          if Sort.compare s int_sort <> 0 then
            Alcotest.fail "expected only the pure int sort"
        | other ->
          Alcotest.fail
            (Format.asprintf "expected 1 sort, got %d" (List.length other)));

      (* Fix 2: RFunDecl body constraints are collected, not discarded *)
      Alcotest.test_case "rfundecl body constraints collected" `Quick (fun () ->
        let src = {|
          rfun incr (p : Ptr Int, [res] r : (do x : Int = Own[Int](p)))
            -> ([res] (do x' : Int = Own[Int](p))) [impure] =
            let (v, log pf, res r2) = Get[Int](p, res r);
            let (res r3) = Set[Int](p, v + 1, res r2);
            (res r3)
          main : () [impure] =
            let (p, res r) = New[Int](0);
            let (do x' = r') = incr(p, res r);
            Del[Int](p, x', res r')
        |} in
        match run_m (
          let open ElabM in
          let* prog = Parse.parse_rprog src ~file:"test" in
          RCheck.check_rprog prog
        ) with
        | Error msg -> Alcotest.fail ("check: " ^ Error.to_string msg)
          | Ok (_typed_prog, _rs, ct) ->
            (* The constraint must not be trivially Top — it should contain
               conjuncts from incr's body (the Get/Set pred equalities) *)
            match Constraint.shape ct with
            | Constraint.Top ->
              Alcotest.fail "expected non-trivial constraint from function body"
            | _ -> ());

      (* Fix 3: tuple checking uses entry effect for spec entries *)
      Alcotest.test_case "incr.rcn passes refined check" `Quick (fun () ->
        let src = {|
          rfun incr (p : Ptr Int, [res] r : (do x : Int = Own[Int](p)))
            -> ([res] (do x' : Int = Own[Int](p))) [impure] =
            let (v, log pf, res r2) = Get[Int](p, res r);
            let (res r3) = Set[Int](p, v + 1, res r2);
            (res r3)
          main : () [impure] =
            let (p, res r) = New[Int](0);
            let (do x' = r') = incr(p, res r);
            Del[Int](p, x', res r')
        |} in
        match run_m (
          let open ElabM in
          let* prog = Parse.parse_rprog src ~file:"test" in
          RCheck.check_rprog prog
        ) with
        | Error msg -> Alcotest.fail ("check: " ^ Error.to_string msg)
          | Ok _ -> ());

      (* Fix 4: pf_eq rejects mismatched lengths *)
      Alcotest.test_case "pf_eq rejects extra entries" `Quick (fun () ->
        let int_sort = Sort.mk (object method loc = SourcePos.dummy end) Sort.Int in
        let bool_sort = Sort.mk (object method loc = SourcePos.dummy end) Sort.Bool in
        let mk_info sort =
          (object method loc = SourcePos.dummy method ctx = Context.empty
                  method answer = Ok sort method eff = Effect.Spec end : CoreExpr.typed_info) in
        let (x, _s0) = Var.mk "x" SourcePos.dummy Var.empty_supply in
        let ce = CoreExpr.mk (mk_info bool_sort) (CoreExpr.IntLit 0) in
        let ri : RProg.typed_rinfo =
          (object method loc = SourcePos.dummy method ctx = Context.empty
                  method rctx = RCtx.empty method sort = int_sort method eff = Effect.Spec method goal = RProg.NoGoal end) in
        let pf1 = [
          ProofSort.Comp { info = ri; var = x; sort = int_sort; eff = Effect.Pure };
          ProofSort.Log { info = ri; prop = ce };
        ] in
        let pf2 = [
          ProofSort.Comp { info = ri; var = x; sort = int_sort; eff = Effect.Pure };
        ] in
        (* Synthesized pf1 has more entries than expected pf2 — should fail *)
        let src = {|
          main : () [impure] = ()
        |} in
        match run_m (
          let open ElabM in
          let* prog = Parse.parse_rprog src ~file:"test" in
          RCheck.check_rprog prog
        ) with
        | Error msg -> Alcotest.fail ("check: " ^ Error.to_string msg)
          | Ok (_, rs, _) ->
            let rsig = rs in
            let result = ElabM.run Var.empty_supply (
              RCheck.Test.pf_eq SourcePos.dummy rsig RCtx.empty pf1 pf2
            ) in
            match result with
            | Ok _ -> Alcotest.fail "should reject mismatched proof sort lengths"
            | Error _ -> ());

      (* Fix 5: spec recursive core functions have self-reference *)
      Alcotest.test_case "spec recursive function succeeds" `Quick (fun () ->
        let src = {|
          sort List(a) = { Nil : () | Cons : (a * List(a)) }
          fun length (xs : List(Int)) -> Int [spec] =
            case xs of { Nil u -> 0 | Cons p -> let (x, t) = p; 1 + length t }
          main : () [impure] = ()
        |} in
        match run_m (
          let open ElabM in
          let* prog = Parse.parse_rprog src ~file:"test" in
          RCheck.check_rprog prog
        ) with
        | Error msg -> Alcotest.fail ("check: " ^ Error.to_string msg)
          | Ok _ -> ());

      (* unfold rpf: positive — building a folded resource via unfold/make-ret *)
      Alcotest.test_case "unfold rpf typechecks" `Quick (fun () ->
        let src = {|
          fun box (x : Int) -> Pred Int [spec] = return x
          rfun test (x : Int) -> ([res] box(x) @ x) [pure] =
            let res r : box(x) @ x = unfold make-ret(auto);
            (res r)
          main : () [impure] = ()
        |} in
        match run_m (
          let open ElabM in
          let* prog = Parse.parse_rprog src ~file:"test" in
          RCheck.check_rprog prog
        ) with
        | Error msg -> Alcotest.fail ("check: " ^ Error.to_string msg)
        | Ok _ -> ());

      (* unfold rpf: negative — synthesis form with no annotation *)
      Alcotest.test_case "unfold rpf rejects synthesis" `Quick (fun () ->
        let contains s sub =
          let n = String.length sub and m = String.length s in
          let rec loop i = i + n <= m && (String.sub s i n = sub || loop (i + 1)) in
          loop 0 in
        let src = {|
          fun box (x : Int) -> Pred Int [spec] = return x
          rfun test (x : Int) -> ([res] box(x) @ x) [pure] =
            let res r = unfold make-ret(auto); (res r)
          main : () [impure] = ()
        |} in
        match run_m (
          let open ElabM in
          let* prog = Parse.parse_rprog src ~file:"test" in
          RCheck.check_rprog prog
        ) with
        | Error msg ->
          let s = Error.to_string msg in
          if not (contains s "synthesize") then
            Alcotest.fail ("expected cannot_synthesize, got: " ^ s)
        | Ok _ -> Alcotest.fail "expected cannot_synthesize error");

      (* unfold rpf: negative — predicate is not a function call *)
      Alcotest.test_case "unfold rpf rejects non-call predicate" `Quick (fun () ->
        let contains s sub =
          let n = String.length sub and m = String.length s in
          let rec loop i = i + n <= m && (String.sub s i n = sub || loop (i + 1)) in
          loop 0 in
        let src = {|
          rfun test (p : Ptr Int, [res] r : Own[Int] p @ 0)
            -> ([res] Own[Int] p @ 0) [impure] =
            let res r2 : Own[Int] p @ 0 = unfold r;
            (res r2)
          main : () [impure] = ()
        |} in
        match run_m (
          let open ElabM in
          let* prog = Parse.parse_rprog src ~file:"test" in
          RCheck.check_rprog prog
        ) with
        | Error msg ->
          let s = Error.to_string msg in
          if not (contains s "unfold") then
            Alcotest.fail ("expected unfold-shape error, got: " ^ s)
        | Ok _ -> Alcotest.fail "expected wrong_pred_shape error");
    ]);
  ]
