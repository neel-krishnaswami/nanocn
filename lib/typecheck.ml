let err_at pos msg =
  Error (Format.asprintf "@[%a:@ %s@]" SourcePos.print pos msg)

let err_at_f pos fmt =
  Format.kasprintf (fun msg -> err_at pos msg) fmt

(* Result monad helpers *)
let ( let* ) = Result.bind

let typ_equal (a : Typ.ty) (b : Typ.ty) = Typ.compare a b = 0

let dummy_info = object method loc = SourcePos.dummy end

let mk_ty s = Typ.In (s, dummy_info)

type typed_info = < loc : SourcePos.t; ctx : Context.t; typ : Typ.ty; eff : Effect.t >
type typed_expr = typed_info Expr.t

let mk ctx pos ty eff shape : typed_expr =
  Expr.In (shape, object
    method loc = pos
    method ctx = ctx
    method typ = ty
    method eff = eff
  end)

(** Signature of a primitive. Returns (arg_type, ret_type, effect). *)
let prim_signature (p : Prim.t) =
  let int_ty = mk_ty Typ.Int in
  let bool_ty = mk_ty Typ.Bool in
  let pair_int = mk_ty (Typ.Record [int_ty; int_ty]) in
  let pair_bool = mk_ty (Typ.Record [bool_ty; bool_ty]) in
  let unit_ty = mk_ty (Typ.Record []) in
  match p with
  | Add -> (pair_int, int_ty, Effect.Pure)
  | Sub -> (pair_int, int_ty, Effect.Pure)
  | Mul -> (pair_int, int_ty, Effect.Pure)
  | Div -> (pair_int, int_ty, Effect.Impure)
  | And -> (pair_bool, bool_ty, Effect.Pure)
  | Or -> (pair_bool, bool_ty, Effect.Pure)
  | Not -> (bool_ty, bool_ty, Effect.Pure)
  | Eq a -> (mk_ty (Typ.Record [a; a]), bool_ty, Effect.Pure)
  | New a -> (a, mk_ty (Typ.Ptr a), Effect.Impure)
  | Del a -> (mk_ty (Typ.Ptr a), unit_ty, Effect.Impure)
  | Get a -> (mk_ty (Typ.Ptr a), a, Effect.Impure)
  | Set a -> (mk_ty (Typ.Record [mk_ty (Typ.Ptr a); a]), unit_ty, Effect.Impure)

(** Synthesize: Σ; Γ ⊢ e ⇒ A[ϵ] *)
let rec synth sig_ ctx (Expr.In (shape, info) as _e) =
  let pos = info#loc in
  match shape with
  | Expr.Var x ->
    (match Context.lookup_comp x ctx with
     | Some a -> Ok (mk ctx pos a Effect.Pure (Expr.Var x))
     | None -> err_at_f pos "unbound variable %a" Var.print x)

  | Expr.IntLit n ->
    Ok (mk ctx pos (mk_ty Typ.Int) Effect.Pure (Expr.IntLit n))

  | Expr.BoolLit b ->
    Ok (mk ctx pos (mk_ty Typ.Bool) Effect.Pure (Expr.BoolLit b))

  | Expr.App (p, arg) ->
    let* () = match p with
      | Prim.Eq a ->
        if Typ.is_eqtype a then Ok ()
        else err_at_f pos "Eq requires an equality type (int, bool, or ptr), got %a"
               Typ.print a
      | _ -> Ok ()
    in
    let (arg_ty, ret_ty, eff) = prim_signature p in
    let* arg' = check sig_ ctx arg arg_ty Effect.Pure in
    Ok (mk ctx pos ret_ty eff (Expr.App (p, arg')))

  | Expr.Call (name, arg) ->
    (match Sig.lookup_fun name sig_ with
     | Some (arg_ty, ret_ty, eff) ->
       let* arg' = check sig_ ctx arg arg_ty Effect.Pure in
       Ok (mk ctx pos ret_ty eff (Expr.Call (name, arg')))
     | None -> err_at_f pos "unknown function %a" Var.print name)

  | Expr.Annot (e, ty, eff) ->
    let* e' = check sig_ ctx e ty eff in
    Ok (mk ctx pos ty eff (Expr.Annot (e', ty, eff)))

  | Expr.Tuple _ | Expr.Let _ | Expr.LetTuple _ | Expr.Inject _
  | Expr.Case _ | Expr.Iter _ | Expr.If _ ->
    err_at pos "cannot synthesize type; add a type annotation"

(** Check: Σ; Γ ⊢ e ⇐ A[ϵ] *)
and check sig_ ctx (Expr.In (shape, info) as e) ty eff =
  let pos = info#loc in
  match shape with
  | Expr.Let (x, e1, e2) ->
    let* e1' = synth sig_ ctx e1 in
    let a = (Expr.extract e1')#typ in
    let eff' = (Expr.extract e1')#eff in
    if not (Effect.sub eff' eff) then
      err_at pos "effect of let binding exceeds allowed effect"
    else
      let ctx' = Context.extend_comp x a ctx in
      let* e2' = check sig_ ctx' e2 ty eff in
      Ok (mk ctx pos ty eff (Expr.Let (x, e1', e2')))

  | Expr.LetTuple (xs, e1, e2) ->
    let* e1' = synth sig_ ctx e1 in
    let a = (Expr.extract e1')#typ in
    let eff1 = (Expr.extract e1')#eff in
    if not (Effect.sub eff1 eff) then
      err_at pos "effect of let-tuple scrutinee exceeds allowed effect"
    else
      (match Typ.shape a with
       | Typ.Record ts ->
         if List.compare_lengths xs ts <> 0 then
           err_at_f pos "let-tuple: expected %d components, got %d"
             (List.length ts) (List.length xs)
         else
           let bindings = List.combine xs ts in
           let ctx' = Context.extend_comp_list bindings ctx in
           let* e2' = check sig_ ctx' e2 ty eff in
           Ok (mk ctx pos ty eff (Expr.LetTuple (xs, e1', e2')))
       | _ -> err_at pos "let-tuple: scrutinee must have record type")

  | Expr.Tuple es ->
    (match Typ.shape ty with
     | Typ.Record ts ->
       if List.compare_lengths es ts <> 0 then
         err_at_f pos "tuple: expected %d components, got %d"
           (List.length ts) (List.length es)
       else
         let* es' = check_list sig_ ctx es ts eff in
         Ok (mk ctx pos ty eff (Expr.Tuple es'))
     | _ -> err_at pos "tuple: expected record type")

  | Expr.Inject (l, e_inner) ->
    (match Typ.shape ty with
     | Typ.App (_, args) ->
       (match TypCtorLookup.lookup sig_ l args with
        | Ok a ->
          let* e_inner' = check sig_ ctx e_inner a eff in
          Ok (mk ctx pos ty eff (Expr.Inject (l, e_inner')))
        | Error msg -> err_at pos msg)
     | _ -> err_at pos "injection: expected datatype application")

  | Expr.Case (scrut, branches) ->
    let* scrut' = synth sig_ ctx scrut in
    let scrut_ty = (Expr.extract scrut')#typ in
    let eff1 = (Expr.extract scrut')#eff in
    if not (Effect.sub eff1 eff) then
      err_at pos "effect of case scrutinee exceeds allowed effect"
    else
      (match Typ.shape scrut_ty with
       | Typ.App (d, args) ->
         (match Sig.lookup_type d sig_ with
          | None -> err_at_f pos "unknown datatype %a" Dsort.print d
          | Some decl ->
            let all_labels = DtypeDecl.ctor_labels decl in
            if List.compare_lengths branches all_labels <> 0 then
              err_at pos "case: number of branches does not match number of constructors"
            else
              let* branches' = check_branches sig_ ctx branches args ty eff pos in
              Ok (mk ctx pos ty eff (Expr.Case (scrut', branches'))))
       | _ -> err_at pos "case: scrutinee must have datatype application type")

  | Expr.Iter (x, e1, body) ->
    let* e1' = synth sig_ ctx e1 in
    let a = (Expr.extract e1')#typ in
    if not (Effect.sub Effect.Impure eff) then
      err_at pos "iter requires effectful context"
    else
      let step_dsort = match Dsort.of_string "step" with Ok d -> d | Error _ -> failwith "impossible" in
      let iter_ty = mk_ty (Typ.App (step_dsort, [a; ty])) in
      let ctx' = Context.extend_comp x a ctx in
      let* body' = check sig_ ctx' body iter_ty Effect.Impure in
      Ok (mk ctx pos ty eff (Expr.Iter (x, e1', body')))

  | Expr.If (cond, e_then, e_else) ->
    let bool_ty = mk_ty Typ.Bool in
    let* cond' = check sig_ ctx cond bool_ty eff in
    let* then' = check sig_ ctx e_then ty eff in
    let* else' = check sig_ ctx e_else ty eff in
    Ok (mk ctx pos ty eff (Expr.If (cond', then', else')))

  | Expr.Annot (inner, ann_ty, ann_eff) ->
    let* inner' = check sig_ ctx inner ann_ty ann_eff in
    if not (typ_equal ann_ty ty) then
      err_at_f pos "annotation type %a does not match expected type %a"
        Typ.print ann_ty Typ.print ty
    else if not (Effect.sub ann_eff eff) then
      err_at pos "annotation effect exceeds allowed effect"
    else
      Ok (mk ctx pos ty eff (Expr.Annot (inner', ann_ty, ann_eff)))

  | _ ->
    (* Fall through to synthesis for checking *)
    let* e' = synth sig_ ctx e in
    let syn_ty = (Expr.extract e')#typ in
    let syn_eff = (Expr.extract e')#eff in
    if not (typ_equal syn_ty ty) then
      err_at_f pos "expected type %a, got %a" Typ.print ty Typ.print syn_ty
    else if not (Effect.sub syn_eff eff) then
      err_at pos "effect mismatch"
    else Ok e'

and check_list sig_ ctx es ts eff =
  match es, ts with
  | [], [] -> Ok []
  | e :: es', t :: ts' ->
    let* e' = check sig_ ctx e t eff in
    let* rest = check_list sig_ ctx es' ts' eff in
    Ok (e' :: rest)
  | _ -> Error "tuple length mismatch"

and check_branches sig_ ctx branches args ty eff pos =
  match branches with
  | [] -> Ok []
  | (l, x, body) :: rest ->
    (match TypCtorLookup.lookup sig_ l args with
     | Ok a ->
       let ctx' = Context.extend_comp x a ctx in
       let* body' = check sig_ ctx' body ty eff in
       let* rest' = check_branches sig_ ctx rest args ty eff pos in
       Ok ((l, x, body') :: rest')
     | Error msg ->
       err_at_f pos "branch label %a: %s" Label.print l msg)

(** Built-in spec functions for arithmetic desugaring *)
let mk_sort_dummy s = Sort.In (s, object method loc = SourcePos.dummy end)

(** Built-in step datatype: step(a, b) = { Next : a | Done : b } *)
let step_decl =
  let next_label = match Label.of_string "Next" with Ok l -> l | Error _ -> failwith "impossible" in
  let done_label = match Label.of_string "Done" with Ok l -> l | Error _ -> failwith "impossible" in
  let a = Tvar.of_string "a" in
  let b = Tvar.of_string "b" in
  let step_dsort = match Dsort.of_string "step" with Ok d -> d | Error _ -> failwith "impossible" in
  DtypeDecl.{
    name = step_dsort;
    params = [a; b];
    ctors = [
      (next_label, mk_ty (Typ.TVar a));
      (done_label, mk_ty (Typ.TVar b));
    ];
    loc = SourcePos.dummy;
  }

let initial_sig =
  let int_sort = mk_sort_dummy Sort.Int in
  let pair_int = mk_sort_dummy (Sort.Record [int_sort; int_sort]) in
  let mk_arith name =
    let v = Var.of_string name SourcePos.dummy in
    (v, Sig.SpecFun { arg = pair_int; ret = int_sort })
  in
  let entries = [
    mk_arith "__add";
    mk_arith "__sub";
    mk_arith "__mul";
    mk_arith "__div";
  ] in
  let sig_ = List.fold_left (fun s (v, e) -> Sig.extend v e s) Sig.empty entries in
  Sig.extend_type sig_ step_decl

(** Collect all TVars in a sort. *)
let rec sort_tvars acc (Sort.In (sf, _)) =
  match sf with
  | Sort.TVar a -> a :: acc
  | Sort.Int | Sort.Bool | Sort.Loc -> acc
  | Sort.Record ts | Sort.App (_, ts) -> List.fold_left sort_tvars acc ts
  | Sort.Pred t -> sort_tvars acc t

(** Validate a datasort declaration *)
let validate_sort_decl (d : DsortDecl.t) =
  if List.length d.ctors = 0 then
    Error "sort declaration must have at least one constructor"
  else
    let labels = DsortDecl.ctor_labels d in
    let rec check_dups = function
      | [] -> Ok ()
      | l :: rest ->
        if List.exists (fun l' -> Label.compare l l' = 0) rest then
          Error (Format.asprintf "duplicate constructor %a in sort declaration"
                   Label.print l)
        else check_dups rest
    in
    let* () = check_dups labels in
    let all_tvars = List.concat_map (fun (_, s) -> sort_tvars [] s) d.ctors in
    let rec check_bound = function
      | [] -> Ok ()
      | a :: rest ->
        if List.exists (fun p -> Tvar.compare a p = 0) d.params then
          check_bound rest
        else
          Error (Format.asprintf "unbound type variable %a in sort declaration %a"
                   Tvar.print a Dsort.print d.name)
    in
    check_bound all_tvars

(** Collect all TVars in a Typ.ty. *)
let rec typ_tvars acc (Typ.In (tf, _)) =
  match tf with
  | Typ.TVar a -> a :: acc
  | Typ.Int | Typ.Bool -> acc
  | Typ.Record ts | Typ.App (_, ts) -> List.fold_left typ_tvars acc ts
  | Typ.Ptr t -> typ_tvars acc t

(** Validate a datatype declaration *)
let validate_type_decl (d : DtypeDecl.t) =
  if List.length d.ctors = 0 then
    Error "type declaration must have at least one constructor"
  else
    let labels = DtypeDecl.ctor_labels d in
    let rec check_dups = function
      | [] -> Ok ()
      | l :: rest ->
        if List.exists (fun l' -> Label.compare l l' = 0) rest then
          Error (Format.asprintf "duplicate constructor %a in type declaration"
                   Label.print l)
        else check_dups rest
    in
    let* () = check_dups labels in
    let all_tvars = List.concat_map (fun (_, ty) -> typ_tvars [] ty) d.ctors in
    let rec check_bound = function
      | [] -> Ok ()
      | a :: rest ->
        if List.exists (fun p -> Tvar.compare a p = 0) d.params then
          check_bound rest
        else
          Error (Format.asprintf "unbound type variable %a in type declaration %a"
                   Tvar.print a Dsort.print d.name)
    in
    check_bound all_tvars

let check_decl sig_ (d : Expr.expr Prog.decl) =
  match d with
  | Prog.FunDecl d ->
    let entry = Sig.FunSig { arg = d.arg_ty; ret = d.ret_ty; eff = d.eff } in
    let sig_for_body = match d.eff with
      | Effect.Impure -> Sig.extend d.name entry sig_
      | Effect.Pure -> sig_
    in
    let ctx = Context.extend_comp d.param d.arg_ty Context.empty in
    let* body' = check sig_for_body ctx d.body d.ret_ty d.eff in
    Ok (Prog.FunDecl { d with body = body' })
  | Prog.SpecFunDecl d ->
    (* Add recursive binding to sig *)
    let sig_with_self = Sig.extend d.name
      (Sig.SpecFun { arg = d.arg_sort; ret = d.ret_sort }) sig_ in
    (* Build match matrix, elaborate, then typecheck core *)
    let result = ElabM.run (
      let open ElabM in
      let* y = fresh d.loc in
      let branches = List.map (fun (pat, body) ->
        Elaborate.({ bindings = [(pat, d.arg_sort)];
                     spec_bindings = [];
                     ectx = EvalCtx.Hole;
                     body })
      ) d.branches in
      let* ce = Elaborate.coverage_check sig_with_self Context.empty
        [y] branches d.ret_sort in
      return (y, ce)
    ) in
    let* (y, ce) = result in
    let ctx = Context.extend_spec y d.arg_sort Context.empty in
    let* _typed_ce = SpecTypecheck.check sig_with_self ctx ce d.ret_sort in
    Ok (Prog.SpecFunDecl d)
  | Prog.SpecDefDecl d ->
    let result = ElabM.run (
      let open ElabM in
      let* ce = Elaborate.check sig_ Context.empty d.body d.sort in
      return ce
    ) in
    let* ce = result in
    let* _typed_ce = SpecTypecheck.check sig_ Context.empty ce d.sort in
    Ok (Prog.SpecDefDecl d)
  | Prog.SortDecl d ->
    let* () = validate_sort_decl d in
    Ok (Prog.SortDecl d)
  | Prog.TypeDecl d ->
    let* () = validate_type_decl d in
    Ok (Prog.TypeDecl d)

let check_spec_decl sig_ (d : Expr.expr Prog.decl) =
  let* _d' = check_decl sig_ d in
  match d with
  | Prog.SortDecl dd ->
    Ok (Sig.extend_sort sig_ dd)
  | Prog.TypeDecl dd ->
    Ok (Sig.extend_type sig_ dd)
  | Prog.SpecFunDecl dd ->
    Ok (Sig.extend dd.name (Sig.SpecFun { arg = dd.arg_sort; ret = dd.ret_sort }) sig_)
  | Prog.SpecDefDecl dd ->
    Ok (Sig.extend dd.name (Sig.SpecVal { sort = dd.sort }) sig_)
  | Prog.FunDecl dd ->
    Ok (Sig.extend dd.name (Sig.FunSig { arg = dd.arg_ty; ret = dd.ret_ty; eff = dd.eff }) sig_)

let check_prog (p : Expr.expr Prog.t) : (typed_expr Prog.t, string) result =
  let unit_ty = mk_ty (Typ.Record []) in
  let rec check_decls sig_ = function
    | [] -> Ok (sig_, [])
    | d :: rest ->
      let* d' = check_decl sig_ d in
      let sig' = match d with
        | Prog.FunDecl fd ->
          let entry = Sig.FunSig { arg = fd.arg_ty; ret = fd.ret_ty; eff = fd.eff } in
          Sig.extend fd.name entry sig_
        | Prog.SpecFunDecl fd ->
          Sig.extend fd.name (Sig.SpecFun { arg = fd.arg_sort; ret = fd.ret_sort }) sig_
        | Prog.SpecDefDecl fd ->
          Sig.extend fd.name (Sig.SpecVal { sort = fd.sort }) sig_
        | Prog.SortDecl dd ->
          Sig.extend_sort sig_ dd
        | Prog.TypeDecl dd ->
          Sig.extend_type sig_ dd
      in
      let* (final_sig, rest') = check_decls sig' rest in
      Ok (final_sig, d' :: rest')
  in
  let* (final_sig, decls') = check_decls initial_sig p.decls in
  let* main' = check final_sig Context.empty p.main unit_ty Effect.Impure in
  Ok { Prog.decls = decls'; main = main'; loc = p.loc }
