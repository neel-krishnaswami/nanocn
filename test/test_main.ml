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
    Subst.Test.test;
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

      Alcotest.test_case "parse sum type" `Quick (fun () ->
        match Parse.parse_typ "{Next:int | Done:(int * int)}" ~file:"test" with
        | Ok ty ->
          (match Typ.shape ty with
           | Typ.Sum [_; _] -> ()
           | _ -> Alcotest.fail "expected sum type with 2 cases")
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

      Alcotest.test_case "check inject into sum" `Quick (fun () ->
        let src = "Done 1 : {Next:int | Done:int} [pure]" in
        match Parse.parse_expr src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
          | Ok te ->
            if Effect.compare (eff_of te) Effect.Pure <> 0 then
              Alcotest.fail "expected pure"
          | Error msg -> Alcotest.fail msg);

      Alcotest.test_case "check case" `Quick (fun () ->
        let src = "let x = (Done 1 : {Next:int | Done:int} [pure]); case x { Done y -> y | Next z -> z } : int [pure]" in
        match Parse.parse_expr src ~file:"test" with
        | Error msg -> Alcotest.fail ("parse: " ^ msg)
        | Ok e ->
          match Typecheck.synth Sig.empty Context.empty e with
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
            (match Context.lookup (Var.of_string "x" SourcePos.dummy) (ctx_of te) with
             | Some _ -> Alcotest.fail "root context should not contain x"
             | None -> ());
            (* Inside the Annot is a Let; its body should have x in context *)
            match Expr.shape te with
            | Expr.Annot (let_node, _, _) ->
              (match Expr.shape let_node with
               | Expr.Let (_, _, e2) ->
                 (match Context.lookup (Var.of_string "x" SourcePos.dummy) (ctx_of e2) with
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

      Alcotest.test_case "typecheck recursive function" `Quick (fun () ->
        let src = {|
          fun countdown(x : int) -> {Next:int | Done:()} [impure] {
            Done () : {Next:int | Done:()} [impure]
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
    ]);
  ]
