(* Helpers for building SMT sexps with source-position info. *)

let loc_info loc = object method loc = loc end

let sym_at loc s = SmtSexp.symbol (loc_info loc) s
let list_at loc xs = SmtSexp.list (loc_info loc) xs
let numeral_at loc n = SmtSexp.numeral_s (loc_info loc) (string_of_int n)
let string_at loc s = SmtSexp.string_lit (loc_info loc) s
let res_at loc r = SmtSexp.reserved (loc_info loc) r

let sort_loc s = (Sort.info s)#loc
let ce_loc ce = (CoreExpr.info ce)#loc

(* ---------- Sort translation ---------- *)

let rec of_sort s =
  let loc = sort_loc s in
  match Sort.shape s with
  | Sort.Int -> sym_at loc "Int"
  | Sort.Bool -> sym_at loc "Bool"
  | Sort.Ptr t ->
    list_at loc [sym_at loc "Ptr"; of_sort t]
  | Sort.Record [] ->
    sym_at loc (SmtSym.tuple_sort 0)
  | Sort.Record [t] ->
    of_sort t
  | Sort.Record ts ->
    let n = List.length ts in
    list_at loc (sym_at loc (SmtSym.tuple_sort n) :: List.map of_sort ts)
  | Sort.App (d, []) ->
    sym_at loc (SmtSym.of_dsort d)
  | Sort.App (d, ts) ->
    list_at loc (sym_at loc (SmtSym.of_dsort d) :: List.map of_sort ts)
  | Sort.Pred t ->
    list_at loc [sym_at loc "Pred"; of_sort t]
  | Sort.TVar a ->
    sym_at loc (SmtSym.of_tvar a)

(* ---------- Result-monad helpers ---------- *)

let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

let rec map_result f = function
  | [] -> Ok []
  | x :: xs ->
    let* y = f x in
    let* ys = map_result f xs in
    Ok (y :: ys)

(* ---------- Primitive translation ---------- *)

let prim_symbol = function
  | Prim.Add -> Ok "+"
  | Prim.Sub -> Ok "-"
  | Prim.Mul -> Ok "*"
  | Prim.Div -> Ok "div"
  | Prim.Lt  -> Ok "<"
  | Prim.Le  -> Ok "<="
  | Prim.Gt  -> Ok ">"
  | Prim.Ge  -> Ok ">="
  | Prim.And -> Ok "and"
  | Prim.Or  -> Ok "or"
  | Prim.Not -> Ok "not"
  | Prim.Eq _ -> Ok "="
  | Prim.New _ -> Error "primitive 'new' is not expressible in SMT"
  | Prim.Del _ -> Error "primitive 'del' is not expressible in SMT"
  | Prim.Get _ -> Error "primitive 'get' is not expressible in SMT"
  | Prim.Set _ -> Error "primitive 'set' is not expressible in SMT"
  (* Own is handled specially in of_ce below — it is an uninterpreted
     function per the spec, not an SMT operator with a fixed name. *)
  | Prim.Own _ -> Error "primitive 'own' should be handled upstream"

let is_binary_prim = function
  | Prim.Add | Prim.Sub | Prim.Mul | Prim.Div
  | Prim.Lt | Prim.Le | Prim.Gt | Prim.Ge
  | Prim.And | Prim.Or | Prim.Eq _ -> true
  | _ -> false

let is_unary_prim = function
  | Prim.Not -> true
  | _ -> false

(* ---------- Expression translation ---------- *)

(* Tear down a [Pred τ] sort, returning τ; errors otherwise. The
   runtime monadic operations ([return], [take], [fail]) all live at
   [Pred τ] sorts; the payload type τ is what parameterises the
   per-sort monad-op symbols. *)
let payload_of_pred s =
  match Sort.shape s with
  | Sort.Pred t -> Ok t
  | _ -> Error "expected a Pred sort"

let rec of_ce ce =
  let loc = ce_loc ce in
  match CoreExpr.shape ce with
  | CoreExpr.Var v ->
    Ok (sym_at loc (SmtSym.of_var v))
  | CoreExpr.IntLit n when n >= 0 ->
    Ok (numeral_at loc n)
  | CoreExpr.IntLit n ->
    (* Negative literal: SMT-LIB requires [(- nat)]. *)
    Ok (list_at loc [sym_at loc "-"; numeral_at loc (- n)])
  | CoreExpr.BoolLit b ->
    Ok (sym_at loc (if b then "true" else "false"))

  | CoreExpr.Let ((x, _), bound, body) ->
    let* b = of_ce bound in
    let* body' = of_ce body in
    Ok (list_at loc [
      res_at loc SmtAtom.R_let;
      list_at loc [list_at loc [sym_at loc (SmtSym.of_var x); b]];
      body';
    ])

  | CoreExpr.Tuple [] ->
    Ok (sym_at loc (SmtSym.tuple_ctor 0))
  | CoreExpr.Tuple [e] ->
    of_ce e
  | CoreExpr.Tuple es ->
    let n = List.length es in
    let* args = map_result of_ce es in
    Ok (list_at loc (sym_at loc (SmtSym.tuple_ctor n) :: args))

  | CoreExpr.LetTuple ([], _, body) ->
    (* No bindings, pure in SMT: emit body. *)
    of_ce body
  | CoreExpr.LetTuple ([(x, _)], bound, body) ->
    (* Singleton tuples are unboxed; fall through to a plain let. *)
    let* b = of_ce bound in
    let* body' = of_ce body in
    Ok (list_at loc [
      res_at loc SmtAtom.R_let;
      list_at loc [list_at loc [sym_at loc (SmtSym.of_var x); b]];
      body';
    ])
  | CoreExpr.LetTuple (xs, bound, body) ->
    (* Compile a tuple destructure to a parallel [let] over the
       per-field selectors [prj-n-k], rather than a [match] over the
       single [tuple-n] constructor. The match form requires Z3 to
       resolve the polymorphic [tuple-n] against the scrutinee's
       sort, which it can fail to do when the only context is a
       chain of [let] bindings (no prior [define-fun] has pinned
       down the [Tuple-n] instance). Selectors are first-order
       monomorphic at the user level — the [(prj-n-k)] reference is
       a non-polymorphic function call — so this avoids the
       constructor-inference problem entirely. *)
    let n = List.length xs in
    let* b = of_ce bound in
    let* body' = of_ce body in
    let bindings =
      List.mapi (fun i (x, _) ->
        let k = i + 1 in
        list_at loc [
          sym_at loc (SmtSym.of_var x);
          list_at loc [sym_at loc (SmtSym.tuple_proj n k); b];
        ])
        xs
    in
    Ok (list_at loc [
      res_at loc SmtAtom.R_let;
      list_at loc bindings;
      body';
    ])

  | CoreExpr.Inject (l, payload) ->
    (* Constructor references use the datatype-prefixed name [D-L]
       per [doc/smt-encoding.md] §Datatype declarations. We recover
       [D] from the enclosing expression's sort, which the
       typechecker has already determined to be [App(D, _)]. *)
    (match Sort.shape (CoreExpr.info ce)#sort with
     | Sort.App (dsort, _) ->
       let label_sym = sym_at loc (SmtSym.ctor_name dsort l) in
       (match Sort.shape (CoreExpr.info payload)#sort with
        | Sort.Record [] ->
          (* Nullary constructor: emit bare label per plan decision Q3. *)
          Ok label_sym
        | _ ->
          let* p = of_ce payload in
          Ok (list_at loc [label_sym; p]))
     | _ ->
       Error (Format.asprintf
                "inject of label %s requires enclosing sort App(D, _)"
                (SmtSym.of_label l)))

  | CoreExpr.Case (scrut, branches) ->
    (* The scrutinee's sort [App(D, _)] gives us the datatype whose
       constructor names must be used in the match patterns. *)
    (match Sort.shape (CoreExpr.info scrut)#sort with
     | Sort.App (dsort, _) ->
       let* s = of_ce scrut in
       let* cases =
         map_result (fun (l, x, body, info) ->
           let label_sym = sym_at loc (SmtSym.ctor_name dsort l) in
           (* The branch's [info#sort] is the constructor's payload
              sort (set by [build_sort_con_branches] /
              [build_type_con_branches] in [lib/elaborate.ml]). When
              the payload is [Record []] the constructor is nullary,
              the SMT match pattern is just the bare label, and any
              reference to the (notional) payload variable in the
              body is rewritten to the unit constructor [tuple-0].
              Otherwise the pattern is [(D-L x)] binding the payload
              to [x]. *)
           match Sort.shape info#sort with
           | Sort.Record [] ->
             let unit_ce =
               CoreExpr.mk info (CoreExpr.Tuple [])
             in
             let body_subst =
               Subst.apply_ce
                 (Subst.extend_var x unit_ce Subst.empty)
                 body
             in
             let* body' = of_ce body_subst in
             Ok (list_at loc [label_sym; body'])
           | _ ->
             let* body' = of_ce body in
             let pat = list_at loc [label_sym; sym_at loc (SmtSym.of_var x)] in
             Ok (list_at loc [pat; body']))
           branches
       in
       Ok (list_at loc [res_at loc SmtAtom.R_match; s; list_at loc cases])
     | _ ->
       Error (Format.asprintf
                "case at %a: scrutinee sort must be a datatype application, got `%a` (scrutinee: `%a`; outer case sort: `%a`)"
                SourcePos.print (CoreExpr.info scrut)#loc
                Sort.print (CoreExpr.info scrut)#sort
                CoreExpr.print scrut
                Sort.print (CoreExpr.info ce)#sort))

  | CoreExpr.Iter _ ->
    Error "iter is not expressible in SMT"

  | CoreExpr.App (Prim.Own tau, arg) ->
    let* arg' = of_ce arg in
    Ok (list_at loc [sym_at loc (SmtSym.own_sym tau); arg'])
  | CoreExpr.App (prim, arg) ->
    let* op = prim_symbol prim in
    if is_binary_prim prim then begin
      match CoreExpr.shape arg with
      | CoreExpr.Tuple [a; b] ->
        let* a' = of_ce a in
        let* b' = of_ce b in
        Ok (list_at loc [sym_at loc op; a'; b'])
      | _ ->
        Error (Printf.sprintf "binary primitive %s expects a 2-tuple argument" op)
    end else if is_unary_prim prim then begin
      let* a' = of_ce arg in
      Ok (list_at loc [sym_at loc op; a'])
    end else
      (* state primitives other than Own were already rejected via
         prim_symbol above. *)
      Error (Printf.sprintf "unsupported primitive %s" op)

  | CoreExpr.Call (f, arg) ->
    (* nanoCN functions are unary at the core level — their parameter
       sort is the (possibly tuple) sort, not a list of sorts. The
       SMT [(declare-fun f ((<arg-sort>)) <ret-sort>)] therefore
       takes one argument; we pass [arg] through verbatim and let
       the [Tuple] case in [of_ce] build the [(tuple-n …)] value
       when the argument happens to be a tuple expression. *)
    let fsym = sym_at loc (SmtSym.of_funname f) in
    let* a' = of_ce arg in
    Ok (list_at loc [fsym; a'])

  | CoreExpr.If (c, t, e) ->
    let* c' = of_ce c in
    let* t' = of_ce t in
    let* e' = of_ce e in
    Ok (list_at loc [sym_at loc "ite"; c'; t'; e'])

  | CoreExpr.Annot (e, s) ->
    (* SMT-LIB's [(as t τ)] form requires [t] to be a qualified
       identifier, not an arbitrary term — it exists specifically to
       disambiguate polymorphic constructors. For general term
       annotations we drop the wrapper and let SMT infer the sort
       from context. If the translated payload happens to be a bare
       symbol we still emit the [as] form, preserving the explicit
       annotation for polymorphic-ctor cases like [nil : List Int]. *)
    let* e' = of_ce e in
    (match SmtSexp.shape e' with
     | SmtSexp.Atom (SmtAtom.Symbol _ | SmtAtom.Reserved _) ->
       Ok (list_at loc [res_at loc SmtAtom.R_as; e'; of_sort s])
     | _ -> Ok e')

  | CoreExpr.Eq (a, b) ->
    let* a' = of_ce a in
    let* b' = of_ce b in
    Ok (list_at loc [sym_at loc "="; a'; b'])

  | CoreExpr.And (a, b) ->
    let* a' = of_ce a in
    let* b' = of_ce b in
    Ok (list_at loc [sym_at loc "and"; a'; b'])

  | CoreExpr.Not a ->
    let* a' = of_ce a in
    Ok (list_at loc [sym_at loc "not"; a'])

  | CoreExpr.Take ((x, _), bound, body) ->
    let* tau = payload_of_pred (CoreExpr.info bound)#sort in
    let* sigma = payload_of_pred (CoreExpr.info body)#sort in
    let* b = of_ce bound in
    let* body' = of_ce body in
    let lambda =
      list_at loc [
        res_at loc SmtAtom.R_lambda;
        list_at loc [list_at loc [
          sym_at loc (SmtSym.of_var x);
          of_sort tau;
        ]];
        body';
      ]
    in
    Ok (list_at loc [sym_at loc (SmtSym.bind_sym tau sigma); b; lambda])

  | CoreExpr.Return e ->
    let tau = (CoreExpr.info e)#sort in
    let* e' = of_ce e in
    Ok (list_at loc [sym_at loc (SmtSym.return_sym tau); e'])

  | CoreExpr.Fail ->
    let* tau = payload_of_pred (CoreExpr.info ce)#sort in
    Ok (sym_at loc (SmtSym.fail_sym tau))

  | CoreExpr.Hole _ ->
    Error "typed holes must not reach SMT encoding"

(* Suppress the `string_at` helper's unused warning — it's exported
   internally for future string-literal translation but no CoreExpr
   constructor currently carries a string payload. *)
let _ = string_at

module Test = struct
  (* Unit/tabular tests live here; the heavy end-to-end coverage
     comes from [SmtPrelude.Test] (structural re-parse) and the
     eventual solver-invocation integration. *)

  let dummy_info sort =
    object
      method loc = SourcePos.dummy
      method ctx = Context.empty
      method sort = sort
      method eff = Effect.Pure
    end

  let dummy_sort_info = object method loc = SourcePos.dummy end

  let mk_sort shape = Sort.mk dummy_sort_info shape
  let sort_int = mk_sort Sort.Int
  let sort_bool = mk_sort Sort.Bool

  let mk_ce sort shape = CoreExpr.mk (dummy_info sort) shape

  let roundtrip_sexp s =
    match SmtParse.parse_sexp (SmtSexp.to_string s) ~file:"t" with
    | Ok _ -> true
    | Error _ -> false

  let test = [
    QCheck.Test.make ~name:"smtExpr of_sort roundtrips through the parser"
      ~count:100
      (QCheck.make Sort.Test.gen)
      (fun s -> roundtrip_sexp (of_sort s));

    QCheck.Test.make ~name:"smtExpr of_ce on IntLit emits its numeral"
      ~count:1
      (QCheck.make QCheck.Gen.(pure 42))
      (fun n ->
         let e = mk_ce sort_int (CoreExpr.IntLit n) in
         match of_ce e with
         | Ok s -> String.equal (SmtSexp.to_string s) (string_of_int n)
         | Error _ -> false);

    QCheck.Test.make ~name:"smtExpr of_ce on BoolLit emits true/false"
      ~count:1
      (QCheck.make QCheck.Gen.(pure ()))
      (fun () ->
         let e_t = mk_ce sort_bool (CoreExpr.BoolLit true) in
         let e_f = mk_ce sort_bool (CoreExpr.BoolLit false) in
         match of_ce e_t, of_ce e_f with
         | Ok st, Ok sf ->
           String.equal (SmtSexp.to_string st) "true"
           && String.equal (SmtSexp.to_string sf) "false"
         | _ -> false);

    QCheck.Test.make ~name:"smtExpr of_ce refuses iter"
      ~count:1
      (QCheck.make QCheck.Gen.(pure ()))
      (fun () ->
         let x, _ = Var.fresh SourcePos.dummy Var.empty_supply in
         let body = mk_ce sort_int (CoreExpr.IntLit 0) in
         let init = mk_ce sort_int (CoreExpr.IntLit 0) in
         let e = mk_ce sort_int (CoreExpr.Iter (x, init, body)) in
         match of_ce e with
         | Error _ -> true
         | Ok _ -> false);
  ]
end
