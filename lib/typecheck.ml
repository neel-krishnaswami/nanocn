let err_at pos msg =
  Error (Format.asprintf "@[%a:@ %s@]" SourcePos.print pos msg)

let err_at_f pos fmt =
  Format.kasprintf (fun msg -> err_at pos msg) fmt

(* Result monad helpers *)
let ( let* ) = Result.bind

let typ_equal (a : Typ.ty) (b : Typ.ty) = Typ.compare a b = 0

(** Look up a label in a sum type's case list. *)
let find_label l cases =
  let rec go = function
    | [] -> None
    | (l', a) :: rest ->
      if Label.compare l l' = 0 then Some a
      else go rest
  in
  go cases

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
  let pair_int = mk_ty (Typ.Record [int_ty; int_ty]) in
  let unit_ty = mk_ty (Typ.Record []) in
  match p with
  | Add -> (pair_int, int_ty, Effect.Pure)
  | Sub -> (pair_int, int_ty, Effect.Pure)
  | Mul -> (pair_int, int_ty, Effect.Pure)
  | Div -> (pair_int, int_ty, Effect.Effectful)
  | New a -> (a, mk_ty (Typ.Ptr a), Effect.Effectful)
  | Del a -> (mk_ty (Typ.Ptr a), unit_ty, Effect.Effectful)
  | Get a -> (mk_ty (Typ.Ptr a), a, Effect.Effectful)
  | Set a -> (mk_ty (Typ.Record [mk_ty (Typ.Ptr a); a]), unit_ty, Effect.Effectful)

(** Synthesize: Σ; Γ ⊢ e ⇒ A[ϵ] *)
let rec synth sig_ ctx (Expr.In (shape, info) as _e) =
  let pos = info#loc in
  match shape with
  | Expr.Var x ->
    (match Context.lookup x ctx with
     | Some a -> Ok (mk ctx pos a Effect.Pure (Expr.Var x))
     | None -> err_at_f pos "unbound variable %a" Var.print x)

  | Expr.IntLit n ->
    Ok (mk ctx pos (mk_ty Typ.Int) Effect.Pure (Expr.IntLit n))

  | Expr.App (p, arg) ->
    let (arg_ty, ret_ty, eff) = prim_signature p in
    let* arg' = check sig_ ctx arg arg_ty Effect.Pure in
    Ok (mk ctx pos ret_ty eff (Expr.App (p, arg')))

  | Expr.Call (name, arg) ->
    (match Sig.lookup name sig_ with
     | Some entry ->
       let* arg' = check sig_ ctx arg entry.Sig.arg Effect.Pure in
       Ok (mk ctx pos entry.ret entry.eff (Expr.Call (name, arg')))
     | None -> err_at_f pos "unknown function %a" Var.print name)

  | Expr.Annot (e, ty, eff) ->
    let* e' = check sig_ ctx e ty eff in
    Ok (mk ctx pos ty eff (Expr.Annot (e', ty, eff)))

  | Expr.Tuple _ | Expr.Let _ | Expr.LetTuple _ | Expr.Inject _
  | Expr.Case _ | Expr.Iter _ ->
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
      let ctx' = Context.extend x a ctx in
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
           let ctx' = Context.extend_list bindings ctx in
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
     | Typ.Sum cases ->
       (match find_label l cases with
        | Some a ->
          let* e_inner' = check sig_ ctx e_inner a eff in
          Ok (mk ctx pos ty eff (Expr.Inject (l, e_inner')))
        | None -> err_at_f pos "label %a not found in sum type" Label.print l)
     | _ -> err_at pos "injection: expected sum type")

  | Expr.Case (scrut, branches) ->
    let* scrut' = synth sig_ ctx scrut in
    let scrut_ty = (Expr.extract scrut')#typ in
    let eff1 = (Expr.extract scrut')#eff in
    if not (Effect.sub eff1 eff) then
      err_at pos "effect of case scrutinee exceeds allowed effect"
    else
      (match Typ.shape scrut_ty with
       | Typ.Sum cases ->
         if List.compare_lengths branches cases <> 0 then
           err_at pos "case: number of branches does not match number of labels"
         else
           let* branches' = check_branches sig_ ctx branches cases ty eff pos in
           Ok (mk ctx pos ty eff (Expr.Case (scrut', branches')))
       | _ -> err_at pos "case: scrutinee must have sum type")

  | Expr.Iter (x, e1, body) ->
    let* e1' = synth sig_ ctx e1 in
    let a = (Expr.extract e1')#typ in
    if not (Effect.sub Effect.Effectful eff) then
      err_at pos "iter requires effectful context"
    else
      let next_label = match Label.of_string "Next" with Ok l -> l | Error _ -> failwith "impossible" in
      let done_label = match Label.of_string "Done" with Ok l -> l | Error _ -> failwith "impossible" in
      let iter_ty = mk_ty (Typ.Sum [(next_label, a); (done_label, ty)]) in
      let ctx' = Context.extend x a ctx in
      let* body' = check sig_ ctx' body iter_ty Effect.Effectful in
      Ok (mk ctx pos ty eff (Expr.Iter (x, e1', body')))

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

and check_branches sig_ ctx branches cases ty eff pos =
  match branches with
  | [] -> Ok []
  | (l, x, body) :: rest ->
    (match find_label l cases with
     | Some a ->
       let ctx' = Context.extend x a ctx in
       let* body' = check sig_ ctx' body ty eff in
       let* rest' = check_branches sig_ ctx rest cases ty eff pos in
       Ok ((l, x, body') :: rest')
     | None ->
       err_at_f pos "branch label %a not in sum type" Label.print l)

let check_decl sig_ (d : Expr.expr Prog.decl) : (typed_expr Prog.decl, string) result =
  let entry = { Sig.arg = d.arg_ty; ret = d.ret_ty; eff = d.eff } in
  let sig' = Sig.extend d.name entry sig_ in
  let ctx = Context.extend d.param d.arg_ty Context.empty in
  let* body' = check sig' ctx d.body d.ret_ty d.eff in
  Ok { d with body = body' }

let check_prog (p : Expr.expr Prog.t) : (typed_expr Prog.t, string) result =
  let unit_ty = mk_ty (Typ.Record []) in
  let rec check_decls sig_ = function
    | [] -> Ok (sig_, [])
    | (d : Expr.expr Prog.decl) :: rest ->
      let* d' = check_decl sig_ d in
      let entry = { Sig.arg = d.arg_ty; ret = d.ret_ty; eff = d.eff } in
      let sig' = Sig.extend d.name entry sig_ in
      let* (final_sig, rest') = check_decls sig' rest in
      Ok (final_sig, d' :: rest')
  in
  let* (final_sig, decls') = check_decls Sig.empty p.decls in
  let* main' = check final_sig Context.empty p.main unit_ty Effect.Effectful in
  Ok { Prog.decls = decls'; main = main'; loc = p.loc }
