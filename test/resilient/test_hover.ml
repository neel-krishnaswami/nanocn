let () =
  let source = {|fun double : Int -> Int [pure] = { x -> x + x }
main : () [impure] = ()|} in
  let result = CompileFile.compile_file source ~file:"test.cn" in
  Format.printf "diagnostics: %d@." (List.length result.diagnostics);
  Format.printf "typed_decls: %d@." (List.length result.typed_decls);
  let hover = HoverIndex.of_typed_decls result.typed_decls in
  (* Try various positions in the file:
     Line 1: fun double : Int -> Int [pure] = { x -> x + x }
     Col:    0123456789...
     The 'x' in the body is at line 1, col ~35 (after '{ ')
     'x + x' starts around col 40 *)
  let test_pos line col =
    match HoverIndex.lookup hover ~line ~col with
    | None -> Format.printf "  (%d,%d): no hover@." line col
    | Some (loc, _ctx, _rctx, sort, eff, _goal) ->
      Format.printf "  (%d,%d): %a [%a] at %a@."
        line col Sort.print sort Effect.print eff SourcePos.print loc
  in
  Format.printf "Hover tests:@.";
  (* Line 1 (1-based), various columns *)
  test_pos 1 0;   (* 'f' in fun *)
  test_pos 1 35;  (* should be near the body *)
  test_pos 1 40;  (* inside x + x *)
  test_pos 1 42;  (* the second x *)
  test_pos 1 44;  (* the third x *)
  (* Also dump all nodes to see what positions exist *)
  Format.printf "All nodes in hover index:@.";
  List.iter (fun decl ->
    match decl with
    | Prog.CoreFunDecl { body; _ } ->
      let rec dump e =
        let b = CoreExpr.info e in
        Format.printf "  %a : %a@." SourcePos.print b#loc Sort.print (CoreExpr.sort_of_info b);
        match CoreExpr.shape e with
        | CoreExpr.Var _ | CoreExpr.IntLit _ | CoreExpr.BoolLit _
        | CoreExpr.Fail | CoreExpr.Hole _ -> ()
        | CoreExpr.Let (_, e1, e2) | CoreExpr.Eq (e1, e2)
        | CoreExpr.And (e1, e2) | CoreExpr.LetTuple (_, e1, e2)
        | CoreExpr.Take (_, e1, e2) | CoreExpr.Iter (_, e1, e2) ->
          dump e1; dump e2
        | CoreExpr.If (e1, e2, e3) -> dump e1; dump e2; dump e3
        | CoreExpr.Tuple es -> List.iter dump es
        | CoreExpr.Inject (_, e1) | CoreExpr.App (_, e1)
        | CoreExpr.Call (_, e1) | CoreExpr.Not e1
        | CoreExpr.Return e1 | CoreExpr.Annot (e1, _) -> dump e1
        | CoreExpr.Case (s, bs) ->
          dump s; List.iter (fun (_, _, b, _) -> dump b) bs
      in
      dump body
    | _ -> ()
  ) result.typed_decls
