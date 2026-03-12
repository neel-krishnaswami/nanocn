let qcheck_tests =
  List.concat [
    SourcePos.Test.test;
    Label.Test.test;
    Var.Test.test;
    Effect.Test.test;
    Prim.Test.test;
    Typ.Test.test;
    Expr.Test.test;
    Context.Test.test;
    Sig.Test.test;
    Prog.Test.test;
    Dsort.Test.test;
    Tvar.Test.test;
    Sort.Test.test;
    DsortDecl.Test.test;
    DtypeDecl.Test.test;
    Subst.Test.test;
    TypSubst.Test.test;
    Pat.Test.test;
    CoreExpr.Test.test;
    SurfExpr.Test.test;
    ElabM.Test.test;
  ]

(** Helper to extract type from a typed_expr *)
let typ_of te = (Expr.extract te)#typ
let eff_of te = (Expr.extract te)#eff
let ctx_of te = (Expr.extract te)#ctx

let () =
  let suite =
    List.map (fun t ->
      QCheck_alcotest.to_alcotest t)
      qcheck_tests
  in
  Alcotest.run "nanocn" [
    ("qcheck", suite);
    ("parse", [
      Alcotest.test_case "parse int type" `Quick (fun () ->
        match Parse.parse_typ "int" ~file:"test" with
        | Ok ty ->
          (match Typ.shape ty with
           | Typ.Int -> ()
           | _ -> Alcotest.fail "expected Int type")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse record type" `Quick (fun () ->
        match Parse.parse_typ "(int * int)" ~file:"test" with
        | Ok ty ->
          (match Typ.shape ty with
           | Typ.Record [t1; t2] ->
             (match Typ.shape t1, Typ.shape t2 with
              | Typ.Int, Typ.Int -> ()
              | _ -> Alcotest.fail "expected (int * int)")
           | _ -> Alcotest.fail "expected record type")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse type application" `Quick (fun () ->
        match Parse.parse_typ "step(int, bool)" ~file:"test" with
        | Ok ty ->
          (match Typ.shape ty with
           | Typ.App (_, [t1; t2]) ->
             (match Typ.shape t1, Typ.shape t2 with
              | Typ.Int, Typ.Bool -> ()
              | _ -> Alcotest.fail "expected step(int, bool)")
           | _ -> Alcotest.fail "expected type application with 2 args")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse bare type name" `Quick (fun () ->
        match Parse.parse_typ "color" ~file:"test" with
        | Ok ty ->
          (match Typ.shape ty with
           | Typ.App (_, []) -> ()
           | _ -> Alcotest.fail "expected bare App with no args")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse ptr type" `Quick (fun () ->
        match Parse.parse_typ "ptr int" ~file:"test" with
        | Ok ty ->
          (match Typ.shape ty with
           | Typ.Ptr inner ->
             (match Typ.shape inner with
              | Typ.Int -> ()
              | _ -> Alcotest.fail "expected ptr int")
           | _ -> Alcotest.fail "expected ptr type")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse variable" `Quick (fun () ->
        match Parse.parse_expr "x" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.Var _ -> ()
           | _ -> Alcotest.fail "expected Var")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse int literal" `Quick (fun () ->
        match Parse.parse_expr "42" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.IntLit 42 -> ()
           | _ -> Alcotest.fail "expected IntLit 42")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse let" `Quick (fun () ->
        match Parse.parse_expr "let x = 1; x" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.Let (_, _, _) -> ()
           | _ -> Alcotest.fail "expected Let")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse tuple" `Quick (fun () ->
        match Parse.parse_expr "(1, 2, 3)" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.Tuple [_; _; _] -> ()
           | _ -> Alcotest.fail "expected 3-tuple")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse arith primitive" `Quick (fun () ->
        match Parse.parse_expr "x + y" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.App (Prim.Add, arg) ->
             (match Expr.shape arg with
              | Expr.Tuple [_; _] -> ()
              | _ -> Alcotest.fail "expected 2-tuple argument")
           | _ -> Alcotest.fail "expected App Add")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse state op with type" `Quick (fun () ->
        match Parse.parse_expr "New[int] x" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.App (Prim.New ty, _) ->
             (match Typ.shape ty with
              | Typ.Int -> ()
              | _ -> Alcotest.fail "expected int type param")
           | _ -> Alcotest.fail "expected App New")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse Get with ptr type" `Quick (fun () ->
        match Parse.parse_expr "Get[int] p" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.App (Prim.Get _, _) -> ()
           | _ -> Alcotest.fail "expected App Get")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse inject" `Quick (fun () ->
        match Parse.parse_expr "Done x" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.Inject (_, _) -> ()
           | _ -> Alcotest.fail "expected Inject")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse case" `Quick (fun () ->
        match Parse.parse_expr "case x { Done y -> y | Next z -> z }" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.Case (_, [_; _]) -> ()
           | _ -> Alcotest.fail "expected Case with 2 branches")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse iter" `Quick (fun () ->
        match Parse.parse_expr "iter (x = 0) { Done x }" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.Iter (_, _, _) -> ()
           | _ -> Alcotest.fail "expected Iter")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse annotation with effect" `Quick (fun () ->
        match Parse.parse_expr "x : int [pure]" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.Annot (_, _, Effect.Pure) -> ()
           | _ -> Alcotest.fail "expected Annot with pure")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse annotation impure" `Quick (fun () ->
        match Parse.parse_expr "x : int [impure]" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.Annot (_, _, Effect.Impure) -> ()
           | _ -> Alcotest.fail "expected Annot with impure")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse type decl" `Quick (fun () ->
        let src = "type option(a) = { Some : a | None : () }" in
        match Parse.parse_decl src ~file:"test" with
        | Ok (Prog.TypeDecl d) ->
          if List.length d.DtypeDecl.ctors <> 2 then
            Alcotest.fail "expected 2 constructors";
          if List.length d.DtypeDecl.params <> 1 then
            Alcotest.fail "expected 1 param"
        | Ok _ -> Alcotest.fail "expected TypeDecl"
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse type decl no params" `Quick (fun () ->
        let src = "type color = { Red : () | Blue : () }" in
        match Parse.parse_decl src ~file:"test" with
        | Ok (Prog.TypeDecl d) ->
          if List.length d.DtypeDecl.params <> 0 then
            Alcotest.fail "expected 0 params"
        | Ok _ -> Alcotest.fail "expected TypeDecl"
        | Error msg -> Alcotest.fail msg);
    ]);

    ("typecheck", [
      Alcotest.test_case "synth int literal" `Quick (fun () ->
        match Parse.parse_expr "42" ~file:"test" with
        | Error msg -> Alcotest.fail msg
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok te ->
            (match Typ.shape (typ_of te) with
             | Typ.Int -> ()
             | _ -> Alcotest.fail "expected int type");
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure effect"
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "check tuple" `Quick (fun () ->
        match Parse.parse_expr "(1, 2)" ~file:"test" with
        | Error msg -> Alcotest.fail msg
        | Ok e ->
          let int_ty = Typ.In (Typ.Int, object method loc = SourcePos.dummy end) in
          let pair_ty = Typ.In (Typ.Record [int_ty; int_ty], object method loc = SourcePos.dummy end) in
          match Typecheck.check Sig.empty Context.empty e pair_ty Effect.Pure with
          | Ok te ->
            if not (Typ.compare (typ_of te) pair_ty = 0) then
              Alcotest.fail "expected pair type on output"
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "check let + Add" `Quick (fun () ->
        match Parse.parse_expr "let x = 1; x + x" ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          let int_ty = Typ.In (Typ.Int, object method loc = SourcePos.dummy end) in
          match Typecheck.check Sig.empty Context.empty e int_ty Effect.Pure with
          | Ok te ->
            (match Typ.shape (typ_of te) with
             | Typ.Int -> ()
             | _ -> Alcotest.fail "expected int type on output")
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "unbound variable fails" `Quick (fun () ->
        match Parse.parse_expr "x" ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok _ -> Alcotest.fail "should fail on unbound variable"
          | Error _ -> ());

      Alcotest.test_case "check inject into declared type" `Quick (fun () ->
        let src = "type option = { Some : int | None : () }" in
        match Parse.parse_decl src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse decl: " ^ msg)
        | Ok d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig d with
          | Error msg -> Alcotest.fail ("typecheck decl: " ^ msg)
          | Ok sig_ ->
            let expr_src = "Some 1 : option [pure]" in
            match Parse.parse_expr expr_src ~file:"test" with
            | Error msg -> Alcotest.fail ("parse: " ^ msg)
            | Ok e ->
              match Typecheck.synth sig_ Context.empty e with
              | Ok te ->
                if Effect.compare (eff_of te) Effect.Pure <> 0 then
                  Alcotest.fail "expected pure"
              | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "check case with declared type" `Quick (fun () ->
        let src = "type option = { Some : int | None : () }" in
        match Parse.parse_decl src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse decl: " ^ msg)
        | Ok d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig d with
          | Error msg -> Alcotest.fail ("typecheck decl: " ^ msg)
          | Ok sig_ ->
            let expr_src = "let x = (Some 1 : option [pure]); case x { Some y -> y | None u -> 0 } : int [pure]" in
            match Parse.parse_expr expr_src ~file:"test" with
            | Error msg -> Alcotest.fail ("parse: " ^ msg)
            | Ok e ->
              match Typecheck.synth sig_ Context.empty e with
              | Ok te ->
                (match Typ.shape (typ_of te) with
                 | Typ.Int -> ()
                 | _ -> Alcotest.fail "expected int result type")
              | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "Div is effectful" `Quick (fun () ->
        match Parse.parse_expr "1 / 2" ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok te ->
            if Effect.compare (eff_of te) Effect.Impure <> 0 then
              Alcotest.fail "Div should be effectful"
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "let-tuple destructuring" `Quick (fun () ->
        match Parse.parse_expr "let (a, b) = ((1, 2) : (int * int) [pure]); a + b" ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          let int_ty = Typ.In (Typ.Int, object method loc = SourcePos.dummy end) in
          match Typecheck.check Sig.empty Context.empty e int_ty Effect.Pure with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "New synthesizes ptr type" `Quick (fun () ->
        match Parse.parse_expr "New[int] 1" ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok te ->
            (match Typ.shape (typ_of te) with
             | Typ.Ptr inner ->
               (match Typ.shape inner with
                | Typ.Int -> ()
                | _ -> Alcotest.fail "expected ptr int")
             | _ -> Alcotest.fail "expected ptr type");
            if Effect.compare (eff_of te) Effect.Impure <> 0 then
              Alcotest.fail "New should be effectful"
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "Get with explicit type" `Quick (fun () ->
        let src = "let p = New[int] 42; Get[int] p" in
        match Parse.parse_expr src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          let int_ty = Typ.In (Typ.Int, object method loc = SourcePos.dummy end) in
          match Typecheck.check Sig.empty Context.empty e int_ty Effect.Impure with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "Set with explicit type" `Quick (fun () ->
        let src = "let p = New[int] 0; Set[int] (p, 42)" in
        match Parse.parse_expr src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          let unit_ty = Typ.In (Typ.Record [], object method loc = SourcePos.dummy end) in
          match Typecheck.check Sig.empty Context.empty e unit_ty Effect.Impure with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "annotation effect mismatch fails" `Quick (fun () ->
        match Parse.parse_expr "1 / 2 : int [pure]" ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok _ -> Alcotest.fail "should fail: Div is effectful but annotated pure"
          | Error _ -> ());

      Alcotest.test_case "typed output carries context" `Quick (fun () ->
        let src = "let x = 1; x : int [pure]" in
        match Parse.parse_expr src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Error msg -> Alcotest.fail msg
          | Ok te ->
            (* The root Annot node should have the empty context *)
            (match Context.lookup_comp (Var.of_string "x" SourcePos.dummy) (ctx_of te) with
             | Some _ -> Alcotest.fail "root context should not contain x"
             | None -> ());
            (* Inside the Annot is a Let; its body should have x in context *)
            match Expr.shape te with
            | Expr.Annot (let_node, _, _) ->
              (match Expr.shape let_node with
               | Expr.Let (_, _, e2) ->
                 (match Context.lookup_comp (Var.of_string "x" SourcePos.dummy) (ctx_of e2) with
                  | Some ty ->
                    (match Typ.shape ty with
                     | Typ.Int -> ()
                     | _ -> Alcotest.fail "x should have type int in body context")
                  | None -> Alcotest.fail "x should be in body context")
               | _ -> Alcotest.fail "expected Let inside Annot")
            | _ -> Alcotest.fail "expected Annot at root");

      Alcotest.test_case "typed output carries types on subterms" `Quick (fun () ->
        match Parse.parse_expr "1 + 2" ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Error msg -> Alcotest.fail msg
          | Ok te ->
            (* Root should be int *)
            (match Typ.shape (typ_of te) with
             | Typ.Int -> ()
             | _ -> Alcotest.fail "root should be int");
            (* The argument tuple should be (int * int) *)
            match Expr.shape te with
            | Expr.App (_, arg) ->
              (match Typ.shape (typ_of arg) with
               | Typ.Record [t1; t2] ->
                 (match Typ.shape t1, Typ.shape t2 with
                  | Typ.Int, Typ.Int -> ()
                  | _ -> Alcotest.fail "arg components should be int")
               | _ -> Alcotest.fail "arg should be record type")
            | _ -> Alcotest.fail "expected App at root");

      Alcotest.test_case "typecheck type decl" `Quick (fun () ->
        let src = "type color = { Red : () | Blue : () }" in
        match Parse.parse_decl src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig d with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail ("typecheck: " ^ msg));

      Alcotest.test_case "typecheck parameterized type decl" `Quick (fun () ->
        let src = "type option(a) = { Some : a | None : () }" in
        match Parse.parse_decl src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig d with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail ("typecheck: " ^ msg));

      Alcotest.test_case "typecheck inject with step (built-in)" `Quick (fun () ->
        let src = "Done 1 : step(int, int) [pure]" in
        match Parse.parse_expr src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Typecheck.initial_sig Context.empty e with
          | Ok te ->
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure"
          | Error msg -> Alcotest.fail msg);
    ]);

    ("parse-bool", [
      Alcotest.test_case "parse bool type" `Quick (fun () ->
        match Parse.parse_typ "bool" ~file:"test" with
        | Ok ty ->
          (match Typ.shape ty with
           | Typ.Bool -> ()
           | _ -> Alcotest.fail "expected Bool type")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse true" `Quick (fun () ->
        match Parse.parse_expr "true" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.BoolLit true -> ()
           | _ -> Alcotest.fail "expected BoolLit true")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse false" `Quick (fun () ->
        match Parse.parse_expr "false" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.BoolLit false -> ()
           | _ -> Alcotest.fail "expected BoolLit false")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse if-then-else" `Quick (fun () ->
        match Parse.parse_expr "if true then 1 else 2 : int [pure]" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.Annot (inner, _, _) ->
             (match Expr.shape inner with
              | Expr.If (_, _, _) -> ()
              | _ -> Alcotest.fail "expected If inside Annot")
           | _ -> Alcotest.fail "expected Annot at root")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse and" `Quick (fun () ->
        match Parse.parse_expr "true && false" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.App (Prim.And, _) -> ()
           | _ -> Alcotest.fail "expected App And")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse or" `Quick (fun () ->
        match Parse.parse_expr "true || false" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.App (Prim.Or, _) -> ()
           | _ -> Alcotest.fail "expected App Or")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse not" `Quick (fun () ->
        match Parse.parse_expr "not true" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.App (Prim.Not, _) -> ()
           | _ -> Alcotest.fail "expected App Not")
        | Error msg -> Alcotest.fail msg);
    ]);

    ("typecheck-bool", [
      Alcotest.test_case "synth true" `Quick (fun () ->
        match Parse.parse_expr "true" ~file:"test" with
        | Error msg -> Alcotest.fail msg
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok te ->
            (match Typ.shape (typ_of te) with
             | Typ.Bool -> ()
             | _ -> Alcotest.fail "expected bool type");
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure effect"
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "check if-then-else" `Quick (fun () ->
        let src = "if true then 1 else 2 : int [pure]" in
        match Parse.parse_expr src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok te ->
            (match Typ.shape (typ_of te) with
             | Typ.Int -> ()
             | _ -> Alcotest.fail "expected int type")
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "not true is pure bool" `Quick (fun () ->
        match Parse.parse_expr "not true" ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok te ->
            (match Typ.shape (typ_of te) with
             | Typ.Bool -> ()
             | _ -> Alcotest.fail "expected bool type");
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure"
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "and is pure" `Quick (fun () ->
        match Parse.parse_expr "true && false" ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok te ->
            (match Typ.shape (typ_of te) with
             | Typ.Bool -> ()
             | _ -> Alcotest.fail "expected bool type");
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure"
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "if with non-bool condition fails" `Quick (fun () ->
        let src = "if 1 then 2 else 3 : int [pure]" in
        match Parse.parse_expr src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok _ -> Alcotest.fail "should fail: condition is not bool"
          | Error _ -> ());
    ]);

    ("typecheck-eq", [
      Alcotest.test_case "Eq[int] synth bool pure" `Quick (fun () ->
        match Parse.parse_expr "Eq[int] (1, 2)" ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok te ->
            (match Typ.shape (typ_of te) with
             | Typ.Bool -> ()
             | _ -> Alcotest.fail "expected bool type");
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure"
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "Eq[bool] synth bool pure" `Quick (fun () ->
        match Parse.parse_expr "Eq[bool] (true, false)" ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok te ->
            (match Typ.shape (typ_of te) with
             | Typ.Bool -> ()
             | _ -> Alcotest.fail "expected bool type")
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "Eq on record type fails" `Quick (fun () ->
        match Parse.parse_expr "Eq[(int * int)] ((1,2), (3,4))" ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok _ -> Alcotest.fail "should fail: record is not an eqtype"
          | Error _ -> ());
    ]);

    ("parse-prog", [
      Alcotest.test_case "parse call" `Quick (fun () ->
        match Parse.parse_expr "foo 1" ~file:"test" with
        | Ok e ->
          (match Expr.shape e with
           | Expr.Call (_, _) -> ()
           | _ -> Alcotest.fail "expected Call")
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse simple program" `Quick (fun () ->
        let src = "main = () : () [impure]" in
        match Parse.parse_prog src ~file:"test" with
        | Ok p ->
          if List.length p.Prog.decls <> 0 then
            Alcotest.fail "expected no decls"
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse program with function" `Quick (fun () ->
        let src = "fun double(x : int) -> int [pure] { x + x } main = () : () [impure]" in
        match Parse.parse_prog src ~file:"test" with
        | Ok p ->
          if List.length p.Prog.decls <> 1 then
            Alcotest.fail "expected 1 decl"
        | Error msg -> Alcotest.fail msg);
    ]);

    ("typecheck-prog", [
      Alcotest.test_case "typecheck trivial program" `Quick (fun () ->
        let src = "main = () : () [impure]" in
        match Parse.parse_prog src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok p ->
          match Typecheck.check_prog p with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "typecheck program with function" `Quick (fun () ->
        let src = "fun double(x : int) -> int [pure] { x + x } main = () : () [impure]" in
        match Parse.parse_prog src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok p ->
          match Typecheck.check_prog p with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "typecheck function call" `Quick (fun () ->
        let src = "fun double(x : int) -> int [pure] { x + x } main = (let r = double 21; ()) : () [impure]" in
        match Parse.parse_prog src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok p ->
          match Typecheck.check_prog p with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "typecheck recursive function with step" `Quick (fun () ->
        let src = {|
          fun countdown(x : int) -> step(int, ()) [impure] {
            Done () : step(int, ()) [impure]
          }
          main = (iter (x = 10) { countdown x }) : () [impure]
        |} in
        match Parse.parse_prog src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok p ->
          match Typecheck.check_prog p with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "pure recursive function fails" `Quick (fun () ->
        let src = {|
          fun loop(x : int) -> int [pure] {
            loop x
          }
          main = () : () [impure]
        |} in
        match Parse.parse_prog src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok p ->
          match Typecheck.check_prog p with
          | Ok _ -> Alcotest.fail "should fail: pure function cannot recurse"
          | Error _ -> ());

      Alcotest.test_case "unknown function fails" `Quick (fun () ->
        let src = "main = (unknown 1 : int [pure]) : () [impure]" in
        match Parse.parse_prog src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok p ->
          match Typecheck.check_prog p with
          | Ok _ -> Alcotest.fail "should fail on unknown function"
          | Error _ -> ());

      Alcotest.test_case "typecheck program with type decl and inject/case" `Quick (fun () ->
        let src = {|
          type option(a) = { Some : a | None : () }
          fun unwrap(x : option(int)) -> int [pure] {
            case x { Some y -> y | None u -> 0 }
          }
          main = (let r = unwrap (Some 42 : option(int) [pure]); ()) : () [impure]
        |} in
        match Parse.parse_prog src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok p ->
          match Typecheck.check_prog p with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail msg);
    ]);

    ("parse-sort", [
      Alcotest.test_case "parse sort decl" `Quick (fun () ->
        let src = "sort list(a) = { Nil : () | Cons : (a * list(a)) }" in
        match Parse.parse_decl src ~file:"test" with
        | Ok (Prog.SortDecl d) ->
          if List.length d.DsortDecl.ctors <> 2 then
            Alcotest.fail "expected 2 constructors"
        | Ok _ -> Alcotest.fail "expected SortDecl"
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse sort decl no params" `Quick (fun () ->
        let src = "sort color = { Red : () | Blue : () }" in
        match Parse.parse_decl src ~file:"test" with
        | Ok (Prog.SortDecl d) ->
          if List.length d.DsortDecl.params <> 0 then
            Alcotest.fail "expected 0 params"
        | Ok _ -> Alcotest.fail "expected SortDecl"
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse spec fun decl" `Quick (fun () ->
        let src = "spec length : list(int) -> int = { Nil () -> 0 | Cons (x, xs) -> 1 + length xs }" in
        match Parse.parse_decl src ~file:"test" with
        | Ok (Prog.SpecFunDecl d) ->
          if List.length d.branches <> 2 then
            Alcotest.fail "expected 2 branches"
        | Ok _ -> Alcotest.fail "expected SpecFunDecl"
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse spec def decl" `Quick (fun () ->
        let src = "spec zero : int = 0" in
        match Parse.parse_decl src ~file:"test" with
        | Ok (Prog.SpecDefDecl _) -> ()
        | Ok _ -> Alcotest.fail "expected SpecDefDecl"
        | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "parse spec expr with ==" `Quick (fun () ->
        let src = "spec eq : int = if x == y then 1 else 0" in
        match Parse.parse_decl src ~file:"test" with
        | Ok (Prog.SpecDefDecl _) -> ()
        | Ok _ -> Alcotest.fail "expected SpecDefDecl"
        | Error msg -> Alcotest.fail msg);
    ]);

    ("typecheck-spec", [
      Alcotest.test_case "typecheck sort decl" `Quick (fun () ->
        let src = "sort color = { Red : () | Blue : () }" in
        match Parse.parse_decl src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig d with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail ("typecheck: " ^ msg));

      Alcotest.test_case "sort decl no ctors fails" `Quick (fun () ->
        (* Can't parse an empty sort, so build one manually *)
        let d = Prog.SortDecl DsortDecl.{
          name = (match Dsort.of_string "empty" with Ok d -> d | _ -> failwith "impossible");
          params = [];
          ctors = [];
          loc = SourcePos.dummy;
        } in
        match Typecheck.check_spec_decl Typecheck.initial_sig d with
        | Ok _ -> Alcotest.fail "should fail on empty sort"
        | Error _ -> ());

      Alcotest.test_case "sort decl duplicate ctors fails" `Quick (fun () ->
        let mk_label s = match Label.of_string s with Ok l -> l | _ -> failwith "impossible" in
        let unit_sort = Sort.In (Sort.Record [], (object method loc = SourcePos.dummy end)) in
        let d = Prog.SortDecl DsortDecl.{
          name = (match Dsort.of_string "bad" with Ok d -> d | _ -> failwith "impossible");
          params = [];
          ctors = [(mk_label "Aa", unit_sort); (mk_label "Aa", unit_sort)];
          loc = SourcePos.dummy;
        } in
        match Typecheck.check_spec_decl Typecheck.initial_sig d with
        | Ok _ -> Alcotest.fail "should fail on duplicate ctors"
        | Error _ -> ());

      Alcotest.test_case "typecheck spec def" `Quick (fun () ->
        let src = "spec zero : int = 0" in
        match Parse.parse_decl src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig d with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail ("typecheck: " ^ msg));

      Alcotest.test_case "sort referencing undeclared sort fails" `Quick (fun () ->
        let src = "sort bad = { Mk : unknown }" in
        match Parse.parse_decl src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig d with
          | Ok _ -> Alcotest.fail "should fail: unknown sort reference"
          | Error _ -> ());

      Alcotest.test_case "sort with wrong arity fails" `Quick (fun () ->
        let sort_src = "sort pair(a, b) = { Mk : (a * b) }" in
        match Parse.parse_decl sort_src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse sort: " ^ msg)
        | Ok sort_d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig sort_d with
          | Error msg -> Alcotest.fail ("typecheck sort: " ^ msg)
          | Ok sig1 ->
            let bad_src = "sort bad = { Mk : pair(int) }" in
            match Parse.parse_decl bad_src ~file:"test" with
            | Error msg -> Alcotest.fail ("parse bad: " ^ msg)
            | Ok bad_d ->
              match Typecheck.check_spec_decl sig1 bad_d with
              | Ok _ -> Alcotest.fail "should fail: wrong arity"
              | Error _ -> ());

      Alcotest.test_case "self-referential sort succeeds" `Quick (fun () ->
        let src = "sort list(a) = { Nil : () | Cons : (a * list(a)) }" in
        match Parse.parse_decl src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig d with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail ("typecheck: " ^ msg));

      Alcotest.test_case "type referencing undeclared type fails" `Quick (fun () ->
        let src = "type bad = { Mk : unknown }" in
        match Parse.parse_decl src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig d with
          | Ok _ -> Alcotest.fail "should fail: unknown type reference"
          | Error _ -> ());

      Alcotest.test_case "type with wrong arity fails" `Quick (fun () ->
        let type_src = "type pair(a, b) = { Mk : (a * b) }" in
        match Parse.parse_decl type_src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse type: " ^ msg)
        | Ok type_d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig type_d with
          | Error msg -> Alcotest.fail ("typecheck type: " ^ msg)
          | Ok sig1 ->
            let bad_src = "type bad = { Mk : pair(int) }" in
            match Parse.parse_decl bad_src ~file:"test" with
            | Error msg -> Alcotest.fail ("parse bad: " ^ msg)
            | Ok bad_d ->
              match Typecheck.check_spec_decl sig1 bad_d with
              | Ok _ -> Alcotest.fail "should fail: wrong arity"
              | Error _ -> ());

      Alcotest.test_case "bare self-referential type rejected" `Quick (fun () ->
        let src = "type list(a) = { Nil : () | Cons : (a * list(a)) }" in
        match Parse.parse_decl src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig d with
          | Ok _ -> Alcotest.fail "should reject bare self-reference"
          | Error _ -> ());

      Alcotest.test_case "ptr self-referential type succeeds" `Quick (fun () ->
        let src = "type list(a) = { Nil : () | Cons : (a * ptr list(a)) }" in
        match Parse.parse_decl src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig d with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail ("typecheck: " ^ msg));

      Alcotest.test_case "typecheck spec fun" `Quick (fun () ->
        (* First register a sort, then a spec function using it *)
        let sort_src = "sort color = { Red : () | Blue : () }" in
        match Parse.parse_decl sort_src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse sort: " ^ msg)
        | Ok sort_d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig sort_d with
          | Error msg -> Alcotest.fail ("typecheck sort: " ^ msg)
          | Ok sig1 ->
            let fun_src = "spec isRed : color -> int = { Red () -> 1 | Blue () -> 0 }" in
            match Parse.parse_decl fun_src ~file:"test" with
            | Error msg -> Alcotest.fail ("parse fun: " ^ msg)
            | Ok fun_d ->
              match Typecheck.check_spec_decl sig1 fun_d with
              | Ok _ -> ()
              | Error msg -> Alcotest.fail ("typecheck fun: " ^ msg));

      Alcotest.test_case "typecheck spec def with arithmetic" `Quick (fun () ->
        let src = "spec five : int = 2 + 3" in
        match Parse.parse_decl src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig d with
          | Ok _ -> ()
          | Error msg -> Alcotest.fail ("typecheck: " ^ msg));

      Alcotest.test_case "pure function callable from spec expr" `Quick (fun () ->
        (* Register a pure function: fun inc(x : int) -> int [pure] *)
        let fun_src = "fun inc(x : int) -> int [pure] { x + 1 }" in
        match Parse.parse_decl fun_src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse fun: " ^ msg)
        | Ok fun_d ->
          match Typecheck.check_spec_decl Typecheck.initial_sig fun_d with
          | Error msg -> Alcotest.fail ("typecheck fun: " ^ msg)
          | Ok sig1 ->
            (* Now use inc in a spec definition: spec three : int = inc 2 *)
            let spec_src = "spec three : int = inc 2" in
            match Parse.parse_decl spec_src ~file:"test" with
            | Error msg -> Alcotest.fail ("parse spec: " ^ msg)
            | Ok spec_d ->
              match Typecheck.check_spec_decl sig1 spec_d with
              | Ok _ -> ()
              | Error msg -> Alcotest.fail ("typecheck spec: " ^ msg));
    ]);

    ("spec-typecheck-core", [
      (* Helper: build a core expression at dummy pos *)
      (let mk shape = CoreExpr.In (shape, object method loc = SourcePos.dummy end) in
       let mk_sort s = Sort.In (s, object method loc = SourcePos.dummy end) in
       let int_sort = mk_sort Sort.Int in
       let _bool_sort = mk_sort Sort.Bool in
       let sort_of tce = (CoreExpr.extract tce)#sort in
       let _ctx_of tce = (CoreExpr.extract tce)#ctx in
       let sig_ = Typecheck.initial_sig in

       Alcotest.test_case "synth int literal carries int sort" `Quick (fun () ->
         let ce = mk (CoreExpr.IntLit 42) in
         match SpecTypecheck.synth sig_ Context.empty ce with
         | Error msg -> Alcotest.fail msg
         | Ok tce ->
           if Sort.compare (sort_of tce) int_sort <> 0 then
             Alcotest.fail "expected int sort"));

      (let mk shape = CoreExpr.In (shape, object method loc = SourcePos.dummy end) in
       let mk_sort s = Sort.In (s, object method loc = SourcePos.dummy end) in
       let bool_sort = mk_sort Sort.Bool in
       let sort_of tce = (CoreExpr.extract tce)#sort in
       let sig_ = Typecheck.initial_sig in

       Alcotest.test_case "synth bool literal carries bool sort" `Quick (fun () ->
         let ce = mk (CoreExpr.BoolLit true) in
         match SpecTypecheck.synth sig_ Context.empty ce with
         | Error msg -> Alcotest.fail msg
         | Ok tce ->
           if Sort.compare (sort_of tce) bool_sort <> 0 then
             Alcotest.fail "expected bool sort"));

      (let mk shape = CoreExpr.In (shape, object method loc = SourcePos.dummy end) in
       let mk_sort s = Sort.In (s, object method loc = SourcePos.dummy end) in
       let int_sort = mk_sort Sort.Int in
       let _sort_of tce = (CoreExpr.extract tce)#sort in
       let ctx_of tce = (CoreExpr.extract tce)#ctx in
       let sig_ = Typecheck.initial_sig in

       Alcotest.test_case "check let propagates context" `Quick (fun () ->
         let x = Var.of_string "x" SourcePos.dummy in
         let ce = mk (CoreExpr.Let (x, mk (CoreExpr.IntLit 1),
                                       mk (CoreExpr.Var x))) in
         match SpecTypecheck.check sig_ Context.empty ce int_sort with
         | Error msg -> Alcotest.fail msg
         | Ok tce ->
           (* The outer node has empty context *)
           if Context.lookup_spec x (ctx_of tce) <> None then
             Alcotest.fail "outer ctx should not have x";
           (* The body (Var x) should have x in context *)
           match CoreExpr.shape tce with
           | CoreExpr.Let (_, _, body) ->
             (match Context.lookup_spec x (ctx_of body) with
              | Some s ->
                if Sort.compare s int_sort <> 0 then
                  Alcotest.fail "x should have int sort in body context"
              | None -> Alcotest.fail "x should be in body context")
           | _ -> Alcotest.fail "expected Let shape"));

      (let mk shape = CoreExpr.In (shape, object method loc = SourcePos.dummy end) in
       let mk_sort s = Sort.In (s, object method loc = SourcePos.dummy end) in
       let bool_sort = mk_sort Sort.Bool in
       let sort_of tce = (CoreExpr.extract tce)#sort in
       let sig_ = Typecheck.initial_sig in

       Alcotest.test_case "synth equality carries bool sort" `Quick (fun () ->
         let ce = mk (CoreExpr.Eq (mk (CoreExpr.IntLit 1), mk (CoreExpr.IntLit 2))) in
         match SpecTypecheck.synth sig_ Context.empty ce with
         | Error msg -> Alcotest.fail msg
         | Ok tce ->
           if Sort.compare (sort_of tce) bool_sort <> 0 then
             Alcotest.fail "expected bool sort for equality"));

      (let mk shape = CoreExpr.In (shape, object method loc = SourcePos.dummy end) in
       let _mk_sort s = Sort.In (s, object method loc = SourcePos.dummy end) in
       let sig_ = Typecheck.initial_sig in

       Alcotest.test_case "synth unbound spec var fails" `Quick (fun () ->
         let x = Var.of_string "x" SourcePos.dummy in
         let ce = mk (CoreExpr.Var x) in
         match SpecTypecheck.synth sig_ Context.empty ce with
         | Ok _ -> Alcotest.fail "should fail on unbound var"
         | Error _ -> ()));

      (let mk shape = CoreExpr.In (shape, object method loc = SourcePos.dummy end) in
       let mk_sort s = Sort.In (s, object method loc = SourcePos.dummy end) in
       let int_sort = mk_sort Sort.Int in
       let sort_of tce = (CoreExpr.extract tce)#sort in
       let sig_ = Typecheck.initial_sig in

       Alcotest.test_case "synth spec var carries its sort" `Quick (fun () ->
         let x = Var.of_string "x" SourcePos.dummy in
         let ctx = Context.extend_spec x int_sort Context.empty in
         let ce = mk (CoreExpr.Var x) in
         match SpecTypecheck.synth sig_ ctx ce with
         | Error msg -> Alcotest.fail msg
         | Ok tce ->
           if Sort.compare (sort_of tce) int_sort <> 0 then
             Alcotest.fail "expected int sort for x"));

      (let mk shape = CoreExpr.In (shape, object method loc = SourcePos.dummy end) in
       let mk_sort s = Sort.In (s, object method loc = SourcePos.dummy end) in
       let _int_sort = mk_sort Sort.Int in
       let bool_sort = mk_sort Sort.Bool in
       let sig_ = Typecheck.initial_sig in

       Alcotest.test_case "check int against bool fails" `Quick (fun () ->
         let ce = mk (CoreExpr.IntLit 42) in
         match SpecTypecheck.check sig_ Context.empty ce bool_sort with
         | Ok _ -> Alcotest.fail "should fail: int checked against bool"
         | Error _ -> ()));
    ]);
  ]
