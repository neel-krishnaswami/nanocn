let mismatch ~construct ~expected_shape ce =
  Error (Error.K_wrong_pred_shape
           { construct
           ; expected_shape
           ; got = Format.asprintf "%a" CoreExpr.print ce })

let get_return ~construct ce =
  match CoreExpr.shape ce with
  | CoreExpr.Return inner -> Ok inner
  | _ -> mismatch ~construct ~expected_shape:"return _" ce

let get_fail ~construct ce =
  match CoreExpr.shape ce with
  | CoreExpr.Fail -> Ok ()
  | _ -> mismatch ~construct ~expected_shape:"fail" ce

let get_take ~construct ce =
  match CoreExpr.shape ce with
  | CoreExpr.Take ((x, _), e1, e2) -> Ok (x, e1, e2)
  | _ -> mismatch ~construct ~expected_shape:"take _ = _; _" ce

let get_let ~construct ce =
  match CoreExpr.shape ce with
  | CoreExpr.Let ((x, _), e1, e2) -> Ok (x, e1, e2)
  | _ -> mismatch ~construct ~expected_shape:"let _ = _; _" ce

let get_if ~construct ce =
  match CoreExpr.shape ce with
  | CoreExpr.If (c, t, e) -> Ok (c, t, e)
  | _ -> mismatch ~construct ~expected_shape:"if _ then _ else _" ce

let get_case ~construct ce =
  match CoreExpr.shape ce with
  | CoreExpr.Case (scrut, branches) -> Ok (scrut, branches)
  | _ -> mismatch ~construct ~expected_shape:"case _ of { ... }" ce

let get_call ~construct ce =
  match CoreExpr.shape ce with
  | CoreExpr.Call (f, arg) -> Ok (f, arg)
  | _ -> mismatch ~construct ~expected_shape:"f(_)" ce
