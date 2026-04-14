type config = { position_trace : bool }

let default_config = { position_trace = true }

let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

let rec map_result f = function
  | [] -> Ok []
  | x :: xs ->
    let* y = f x in
    let* ys = map_result f xs in
    Ok (y :: ys)

(* Collect oks, concatenating their lists. *)
let rec concat_map_result f = function
  | [] -> Ok []
  | x :: xs ->
    let* ys = f x in
    let* rest = concat_map_result f xs in
    Ok (ys @ rest)

(* ---------- Sexp builder helpers ---------- *)

let d = SourcePos.dummy
let info loc = object method loc = loc end
let sym loc s = SmtSexp.symbol (info loc) s
let num loc n = SmtSexp.numeral_s (info loc) (string_of_int n)
let list_of loc xs = SmtSexp.list (info loc) xs
let res loc r = SmtSexp.reserved (info loc) r
let kw loc s = SmtSexp.keyword (info loc) s

(* ---------- Logic + built-in datatypes ---------- *)

let set_logic_all =
  list_of d [res d SmtAtom.R_set_logic; sym d "ALL"]

(* Position tracing prelude declarations: Pos record and PosList. *)
let position_trace_decls =
  let pos_decl =
    list_of d [
      res d SmtAtom.R_declare_datatype;
      sym d "Pos";
      list_of d [list_of d [
        sym d "mk-pos";
        list_of d [sym d "pos-file"; sym d "String"];
        list_of d [sym d "pos-line"; sym d "Int"];
        list_of d [sym d "pos-col";  sym d "Int"];
      ]];
    ]
  in
  let poslist_decl =
    list_of d [
      res d SmtAtom.R_declare_datatypes;
      list_of d [list_of d [sym d "PosList"; num d 1]];
      list_of d [list_of d [
        res d SmtAtom.R_par;
        list_of d [sym d "A"];
        list_of d [
          list_of d [sym d "pnil"];
          list_of d [
            sym d "pcons";
            list_of d [sym d "phead"; sym d "A"];
            list_of d [sym d "ptail"; list_of d [sym d "PosList"; sym d "A"]];
          ];
        ];
      ]];
    ]
  in
  [pos_decl; poslist_decl]

(* (declare-datatype Ptr (par (A) ((loc (addr Int))))) *)
let ptr_decl =
  list_of d [
    res d SmtAtom.R_declare_datatype;
    sym d "Ptr";
    list_of d [
      res d SmtAtom.R_par;
      list_of d [sym d "A"];
      list_of d [list_of d [
        sym d "loc";
        list_of d [sym d "addr"; sym d "Int"];
      ]];
    ];
  ]

(* (declare-datatype Tuple-n (par (T1 ... Tn) ((tuple-n (prj-n-1 T1) ... (prj-n-n Tn))))) *)
let tuple_decl n =
  if n = 0 then
    list_of d [
      res d SmtAtom.R_declare_datatype;
      sym d (SmtSym.tuple_sort 0);
      list_of d [list_of d [sym d (SmtSym.tuple_ctor 0)]];
    ]
  else begin
    let params =
      List.init n (fun i -> Printf.sprintf "T%d" (i + 1))
    in
    let selectors =
      List.mapi (fun i pname ->
        list_of d [sym d (SmtSym.tuple_proj n (i + 1)); sym d pname])
        params
    in
    list_of d [
      res d SmtAtom.R_declare_datatype;
      sym d (SmtSym.tuple_sort n);
      list_of d [
        res d SmtAtom.R_par;
        list_of d (List.map (sym d) params);
        list_of d [list_of d (sym d (SmtSym.tuple_ctor n) :: selectors)];
      ];
    ]
  end

let tuple_decls =
  tuple_decl 0 ::
  List.init 15 (fun i -> tuple_decl (i + 2))

let pred_decl =
  list_of d [res d SmtAtom.R_declare_sort; sym d "Pred"; num d 1]

(* ---------- User datatype decls ---------- *)

(* Render a single constructor declaration. The constructor and selector
   names are datatype-prefixed per [doc/smt-encoding.md] §Datatype
   declarations so that labels shared between datatypes (e.g.
   [Seq.Nil] and [List.Nil]) don't collide in SMT-LIB's single global
   constructor namespace.
   - nullary payload [Record []]  →  (D-L)
   - any other payload            →  (D-L (get-D-L <sort>)) *)
let ctor_decl dsort (label, payload_sort) =
  let l_sym = sym d (SmtSym.ctor_name dsort label) in
  match Sort.shape payload_sort with
  | Sort.Record [] ->
    list_of d [l_sym]
  | _ ->
    let sel = sym d (SmtSym.ctor_selector dsort label) in
    list_of d [l_sym; list_of d [sel; SmtExpr.of_sort payload_sort]]

let dsort_datatype_decl (d_decl : DsortDecl.t) =
  let name_sym = sym d (SmtSym.of_dsort d_decl.DsortDecl.name) in
  let ctors = list_of d (List.map (ctor_decl d_decl.DsortDecl.name) d_decl.DsortDecl.ctors) in
  let body =
    match d_decl.DsortDecl.params with
    | [] -> ctors
    | params ->
      list_of d [
        res d SmtAtom.R_par;
        list_of d (List.map (fun a -> sym d (SmtSym.of_tvar a)) params);
        ctors;
      ]
  in
  list_of d [res d SmtAtom.R_declare_datatype; name_sym; body]

let dtype_datatype_decl (d_decl : DtypeDecl.t) =
  let name_sym = sym d (SmtSym.of_dsort d_decl.DtypeDecl.name) in
  let ctors = list_of d (List.map (ctor_decl d_decl.DtypeDecl.name) d_decl.DtypeDecl.ctors) in
  let body =
    match d_decl.DtypeDecl.params with
    | [] -> ctors
    | params ->
      list_of d [
        res d SmtAtom.R_par;
        list_of d (List.map (fun a -> sym d (SmtSym.of_tvar a)) params);
        ctors;
      ]
  in
  list_of d [res d SmtAtom.R_declare_datatype; name_sym; body]

(* ---------- Monad ops: per-sort declarations ---------- *)

let fail_decl tau =
  list_of d [
    res d SmtAtom.R_declare_const;
    sym d (SmtSym.fail_sym tau);
    list_of d [sym d "Pred"; SmtExpr.of_sort tau];
  ]

let return_decl tau =
  list_of d [
    res d SmtAtom.R_declare_fun;
    sym d (SmtSym.return_sym tau);
    list_of d [SmtExpr.of_sort tau];
    list_of d [sym d "Pred"; SmtExpr.of_sort tau];
  ]

(* (declare-fun bind-τ-σ ((Pred τ) (-> τ (Pred σ))) (Pred σ)) *)
let bind_decl tau sigma =
  let pred_tau = list_of d [sym d "Pred"; SmtExpr.of_sort tau] in
  let pred_sigma = list_of d [sym d "Pred"; SmtExpr.of_sort sigma] in
  let arrow =
    list_of d [sym d "->"; SmtExpr.of_sort tau; pred_sigma]
  in
  list_of d [
    res d SmtAtom.R_declare_fun;
    sym d (SmtSym.bind_sym tau sigma);
    list_of d [pred_tau; arrow];
    pred_sigma;
  ]

(* (declare-fun own-τ ((Ptr τ)) (Pred τ)) *)
let own_decl tau =
  let ptr_tau  = list_of d [sym d "Ptr";  SmtExpr.of_sort tau] in
  let pred_tau = list_of d [sym d "Pred"; SmtExpr.of_sort tau] in
  list_of d [
    res d SmtAtom.R_declare_fun;
    sym d (SmtSym.own_sym tau);
    list_of d [ptr_tau];
    pred_tau;
  ]

(* ---------- Monad-law axioms ---------- *)

let with_pattern body pattern =
  list_of d [res d SmtAtom.R_bang; body; kw d "pattern"; pattern]

let assert_forall binders body_with_pat =
  list_of d [
    res d SmtAtom.R_assert;
    list_of d [
      res d SmtAtom.R_forall;
      list_of d binders;
      body_with_pat;
    ];
  ]

(* Right-unit: (bind-τ-τ m return-τ) = m *)
let right_unit_axiom tau =
  let m = sym d "m" in
  let pred_tau = list_of d [sym d "Pred"; SmtExpr.of_sort tau] in
  let binder = list_of d [sym d "m"; pred_tau] in
  let app =
    list_of d [
      sym d (SmtSym.bind_sym tau tau);
      m;
      sym d (SmtSym.return_sym tau);
    ]
  in
  let eq = list_of d [sym d "="; app; m] in
  assert_forall [binder] (with_pattern eq app)

(* Left-unit: (bind-τ-σ (return-τ a) f) = (f a) *)
let left_unit_axiom tau sigma =
  let a = sym d "a" in
  let f = sym d "f" in
  let pred_sigma = list_of d [sym d "Pred"; SmtExpr.of_sort sigma] in
  let arrow_sort = list_of d [sym d "->"; SmtExpr.of_sort tau; pred_sigma] in
  let binders = [
    list_of d [a; SmtExpr.of_sort tau];
    list_of d [f; arrow_sort];
  ] in
  let ret_a = list_of d [sym d (SmtSym.return_sym tau); a] in
  let app = list_of d [
    sym d (SmtSym.bind_sym tau sigma);
    ret_a;
    f;
  ] in
  let fa = list_of d [f; a] in
  let eq = list_of d [sym d "="; app; fa] in
  assert_forall binders (with_pattern eq app)

(* Assoc: (bind-σ-ρ (bind-τ-σ m f) g) = (bind-τ-ρ m (lambda ((x τ)) (bind-σ-ρ (f x) g))) *)
let assoc_axiom tau sigma rho =
  let m = sym d "m" in
  let f = sym d "f" in
  let g = sym d "g" in
  let x = sym d "x" in
  let pred_tau = list_of d [sym d "Pred"; SmtExpr.of_sort tau] in
  let pred_sigma = list_of d [sym d "Pred"; SmtExpr.of_sort sigma] in
  let pred_rho = list_of d [sym d "Pred"; SmtExpr.of_sort rho] in
  let tau_to_pred_sigma =
    list_of d [sym d "->"; SmtExpr.of_sort tau; pred_sigma]
  in
  let sigma_to_pred_rho =
    list_of d [sym d "->"; SmtExpr.of_sort sigma; pred_rho]
  in
  let binders = [
    list_of d [m; pred_tau];
    list_of d [f; tau_to_pred_sigma];
    list_of d [g; sigma_to_pred_rho];
  ] in
  let inner_bind =
    list_of d [sym d (SmtSym.bind_sym tau sigma); m; f]
  in
  let lhs =
    list_of d [sym d (SmtSym.bind_sym sigma rho); inner_bind; g]
  in
  let fx = list_of d [f; x] in
  let bind_fx_g =
    list_of d [sym d (SmtSym.bind_sym sigma rho); fx; g]
  in
  let inner_lambda =
    list_of d [
      res d SmtAtom.R_lambda;
      list_of d [list_of d [x; SmtExpr.of_sort tau]];
      bind_fx_g;
    ]
  in
  let rhs =
    list_of d [sym d (SmtSym.bind_sym tau rho); m; inner_lambda]
  in
  let eq = list_of d [sym d "="; lhs; rhs] in
  assert_forall binders (with_pattern eq lhs)

(* Right-fail: (bind-τ-σ m (lambda ((x τ)) fail-σ)) = fail-σ *)
let right_fail_axiom tau sigma =
  let m = sym d "m" in
  let x = sym d "x" in
  let pred_tau = list_of d [sym d "Pred"; SmtExpr.of_sort tau] in
  let binders = [list_of d [m; pred_tau]] in
  let lambda =
    list_of d [
      res d SmtAtom.R_lambda;
      list_of d [list_of d [x; SmtExpr.of_sort tau]];
      sym d (SmtSym.fail_sym sigma);
    ]
  in
  let lhs =
    list_of d [sym d (SmtSym.bind_sym tau sigma); m; lambda]
  in
  let rhs = sym d (SmtSym.fail_sym sigma) in
  let eq = list_of d [sym d "="; lhs; rhs] in
  assert_forall binders (with_pattern eq lhs)

(* Left-fail: (bind-τ-σ fail-τ f) = fail-σ *)
let left_fail_axiom tau sigma =
  let f = sym d "f" in
  let pred_sigma = list_of d [sym d "Pred"; SmtExpr.of_sort sigma] in
  let arrow = list_of d [sym d "->"; SmtExpr.of_sort tau; pred_sigma] in
  let binders = [list_of d [f; arrow]] in
  let lhs =
    list_of d [
      sym d (SmtSym.bind_sym tau sigma);
      sym d (SmtSym.fail_sym tau);
      f;
    ]
  in
  let rhs = sym d (SmtSym.fail_sym sigma) in
  let eq = list_of d [sym d "="; lhs; rhs] in
  assert_forall binders (with_pattern eq lhs)

(* ---------- Function emission ---------- *)

let define_fun name ~param ~arg ~ret ~body =
  list_of d [
    res d SmtAtom.R_define_fun;
    sym d (SmtSym.of_funname name);
    list_of d [list_of d [sym d (SmtSym.of_var param); SmtExpr.of_sort arg]];
    SmtExpr.of_sort ret;
    body;
  ]

let declare_fun name ~arg ~ret =
  list_of d [
    res d SmtAtom.R_declare_fun;
    sym d (SmtSym.of_funname name);
    list_of d [SmtExpr.of_sort arg];
    SmtExpr.of_sort ret;
  ]

(* ---------- Top-level assembly ---------- *)

let build ?(config = default_config) rsig ms =
  let trace_prelude =
    if config.position_trace then position_trace_decls else []
  in
  let user_type_decls =
    List.filter_map (function
      | RSig.LSort d_decl -> Some (dsort_datatype_decl d_decl)
      | RSig.LType d_decl -> Some (dtype_datatype_decl d_decl)
      | RSig.LFun _ -> None)
      (RSig.entries rsig)
  in
  let s_list = SmtMonadSorts.SortSet.elements ms.SmtMonadSorts.s in
  (* Bind declarations cover the full S × S cartesian product — not just
     the [Take]-collected [pairs] — because the monad-law axioms below
     reference [bind-τ-σ] for every (τ, σ) ∈ S². *)
  let fail_return_decls =
    List.concat_map (fun t -> [fail_decl t; return_decl t]) s_list
  in
  let bind_decls =
    List.concat_map (fun t ->
      List.map (fun s -> bind_decl t s) s_list)
      s_list
  in
  let own_list =
    SmtMonadSorts.SortSet.elements ms.SmtMonadSorts.own_sorts
  in
  let own_decls = List.map own_decl own_list in

  (* Monad laws. *)
  let right_unit  = List.map right_unit_axiom s_list in
  let left_unit   =
    List.concat_map (fun t ->
      List.map (fun s -> left_unit_axiom t s) s_list)
      s_list
  in
  let assoc =
    List.concat_map (fun t ->
      List.concat_map (fun s ->
        List.map (fun r -> assoc_axiom t s r) s_list)
        s_list)
      s_list
  in
  let right_fail =
    List.concat_map (fun t ->
      List.map (fun s -> right_fail_axiom t s) s_list)
      s_list
  in
  let left_fail =
    List.concat_map (fun t ->
      List.map (fun s -> left_fail_axiom t s) s_list)
      s_list
  in

  (* Pure function defs + spec function decls. *)
  let* fun_cmds =
    concat_map_result (function
      | RSig.LFun (name, RSig.FunDef { param; arg; ret; eff = Effect.Pure; body }) ->
        let* body_sexp = SmtExpr.of_ce body in
        Ok [define_fun name ~param ~arg ~ret ~body:body_sexp]
      | RSig.LFun (name, RSig.FunDef { arg; ret; eff = Effect.Spec; _ }) ->
        Ok [declare_fun name ~arg ~ret]
      | RSig.LFun (name, RSig.FunSig { arg; ret; eff = Effect.Spec }) ->
        Ok [declare_fun name ~arg ~ret]
      | RSig.LFun (_, RSig.FunSig { eff = Effect.Impure | Effect.Pure; _ })
      | RSig.LFun (_, RSig.FunDef { eff = Effect.Impure; _ })
      | RSig.LFun (_, RSig.RFunSig _)
      | RSig.LFun (_, RSig.SortDecl _)
      | RSig.LFun (_, RSig.TypeDecl _)
      | RSig.LSort _ | RSig.LType _ ->
        Ok [])
      (RSig.entries rsig)
  in

  Ok (
    [set_logic_all]
    @ trace_prelude
    @ [ptr_decl]
    @ tuple_decls
    @ user_type_decls
    @ [pred_decl]
    @ fail_return_decls
    @ bind_decls
    @ right_unit @ left_unit @ assoc @ right_fail @ left_fail
    @ own_decls
    @ fun_cmds
  )

(* Suppress unused-value warning for [map_result] — reserved for
   future use and symmetrical to [concat_map_result]. *)
let _ = map_result
