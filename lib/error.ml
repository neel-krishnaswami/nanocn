type branch_merge_failure =
  | Mf_length_mismatch of { lhs : int; rhs : int }
  | Mf_entry_kind_mismatch
  | Mf_usage_incompatible of Var.t
  | Mf_empty_list

type kind =
  | K_parse_error of { msg : string }
  | K_duplicate_pat_var of { name : string }
  | K_sort_mismatch of
      { expected : Sort.sort
      ; actual : Sort.sort
      ; diff : SortDiff.shape_compare }
  | K_annotation_disagrees of
      { inner_sort : Sort.sort
      ; annot : Sort.sort
      ; diff : SortDiff.shape_compare }
  | K_unbound_var of Var.t
  | K_unbound_name of string
  | K_unbound_ctor of Label.t
  | K_unbound_sort of Dsort.t
  | K_unbound_tvar of Tvar.t
  | K_unknown_function of { name : string }
  | K_log_var_not_found of { name : Var.t }
  | K_var_effect_mismatch of
      { var : Var.t
      ; declared : Effect.t
      ; required : Effect.t }
  | K_prim_effect_mismatch of
      { prim : Prim.t
      ; declared : Effect.t
      ; required : Effect.t }
  | K_fun_effect_mismatch of
      { name : string
      ; declared : Effect.t
      ; required : Effect.t }
  | K_iter_requires_impure of { actual : Effect.t }
  | K_eq_not_equality_type of { got : Sort.sort }
  | K_construct_sort_mismatch of
      { construct : string
      ; expected_shape : string
      ; got : Sort.sort }
  | K_tuple_arity_mismatch of
      { construct : string
      ; expected : int
      ; actual : int }
  | K_scrutinee_not_data of { got : Sort.sort }
  | K_not_spec_type of { construct : string; got : Sort.sort }
  | K_spec_context_required of { construct : string }
  | K_cannot_synthesize of { construct : string }
  | K_subst_arity_mismatch of { expected : int; actual : int }
  | K_resource_not_found of { name : Var.t }
  | K_resource_already_used of { name : Var.t }
  | K_branch_merge_failure of { reason : branch_merge_failure }
  | K_dep_res_not_pred of { got : Sort.sort }
  | K_ctor_sig_inconsistent of { label : Label.t; where : string }
  | K_tvar_kind_mismatch of
      { tvar : Tvar.t; got : Kind.t; expected : Kind.t }
  | K_dsort_arity_mismatch of
      { dsort : Dsort.t; expected : int; actual : int }
  | K_pred_misuse of { context : string }
  | K_unguarded_recursion of { dsort : Dsort.t }
  | K_empty_decl of { name : string; is_type : bool }
  | K_duplicate_ctor_in_decl of
      { label : Label.t; decl_name : string; is_type : bool }
  | K_non_exhaustive of { witness : PatWitness.t }
  | K_wrong_pred_shape of
      { construct : string
      ; expected_shape : string
      ; got : string }
  | K_unfold_not_spec of { name : string }
  | K_unfold_not_fundef of { name : string }
  | K_resource_leak of { name : Var.t option }
  | K_let_pattern_resource_leak of { leftovers : string list }
  | K_internal_invariant of { rule : string; invariant : string }

type t =
  | Structured of { loc : SourcePos.t option; kind : kind }
  (* [loc] is optional only for [K_parse_error] from the lexer's
     catch-all [Failure] handler; every other kind builder supplies
     a real position. *)

let structured ~loc kind = Structured { loc = Some loc; kind }

let structured_nopos kind = Structured { loc = None; kind }

let parse_error ~loc ~msg =
  match loc with
  | Some loc -> structured ~loc (K_parse_error { msg })
  | None -> structured_nopos (K_parse_error { msg })

let duplicate_pat_var ~loc ~name =
  structured ~loc (K_duplicate_pat_var { name })

let sort_mismatch ~loc ~expected ~actual =
  let diff = SortDiff.diff expected actual in
  structured ~loc (K_sort_mismatch { expected; actual; diff })

let annotation_disagrees ~loc ~inner ~annot =
  let diff = SortDiff.diff inner annot in
  structured ~loc
    (K_annotation_disagrees { inner_sort = inner; annot; diff })

let unbound_var ~loc v = structured ~loc (K_unbound_var v)
let unbound_name ~loc n = structured ~loc (K_unbound_name n)
let unbound_ctor ~loc l = structured ~loc (K_unbound_ctor l)
let unbound_sort ~loc d = structured ~loc (K_unbound_sort d)
let unbound_tvar ~loc t = structured ~loc (K_unbound_tvar t)

let unknown_function ~loc ~name =
  structured ~loc (K_unknown_function { name })

let log_var_not_found ~loc ~name =
  structured ~loc (K_log_var_not_found { name })

let var_effect_mismatch ~loc ~var ~declared ~required =
  structured ~loc
    (K_var_effect_mismatch { var; declared; required })

let prim_effect_mismatch ~loc ~prim ~declared ~required =
  structured ~loc
    (K_prim_effect_mismatch { prim; declared; required })

let fun_effect_mismatch ~loc ~name ~declared ~required =
  structured ~loc
    (K_fun_effect_mismatch { name; declared; required })

let scrutinee_not_data ~loc ~got =
  structured ~loc (K_scrutinee_not_data { got })

let not_spec_type ~loc ~construct ~got =
  structured ~loc (K_not_spec_type { construct; got })

let spec_context_required ~loc ~construct =
  structured ~loc (K_spec_context_required { construct })

let cannot_synthesize ~loc ~construct =
  structured ~loc (K_cannot_synthesize { construct })

let eq_not_equality_type ~loc ~got =
  structured ~loc (K_eq_not_equality_type { got })

let construct_sort_mismatch ~loc ~construct ~expected_shape ~got =
  structured ~loc
    (K_construct_sort_mismatch { construct; expected_shape; got })

let tuple_arity_mismatch ~loc ~construct ~expected ~actual =
  structured ~loc
    (K_tuple_arity_mismatch { construct; expected; actual })

let subst_arity_mismatch ~loc ~expected ~actual =
  structured ~loc (K_subst_arity_mismatch { expected; actual })

let resource_not_found ~loc ~name =
  structured ~loc (K_resource_not_found { name })

let resource_already_used ~loc ~name =
  structured ~loc (K_resource_already_used { name })

let branch_merge_failure ~loc ~reason =
  structured ~loc (K_branch_merge_failure { reason })

let dep_res_not_pred ~loc ~got =
  structured ~loc (K_dep_res_not_pred { got })

let ctor_sig_inconsistent ~loc ~label ~where =
  structured ~loc (K_ctor_sig_inconsistent { label; where })

let at ~loc r =
  Result.map_error (structured ~loc) r

let tvar_kind_mismatch ~loc ~tvar ~got ~expected =
  structured ~loc (K_tvar_kind_mismatch { tvar; got; expected })

let dsort_arity_mismatch ~loc ~dsort ~expected ~actual =
  structured ~loc (K_dsort_arity_mismatch { dsort; expected; actual })

let pred_misuse ~loc ~context =
  structured ~loc (K_pred_misuse { context })

let unguarded_recursion ~loc ~dsort =
  structured ~loc (K_unguarded_recursion { dsort })

let empty_decl ~loc ~name ~is_type =
  structured ~loc (K_empty_decl { name; is_type })

let duplicate_ctor_in_decl ~loc ~label ~decl_name ~is_type =
  structured ~loc
    (K_duplicate_ctor_in_decl { label; decl_name; is_type })

let non_exhaustive ~loc ~witness =
  structured ~loc (K_non_exhaustive { witness })

let wrong_pred_shape ~loc ~construct ~expected_shape ~got =
  structured ~loc
    (K_wrong_pred_shape { construct; expected_shape; got })

let unfold_not_spec ~loc ~name =
  structured ~loc (K_unfold_not_spec { name })

let unfold_not_fundef ~loc ~name =
  structured ~loc (K_unfold_not_fundef { name })

let resource_leak ~loc ~name =
  structured ~loc (K_resource_leak { name })

let let_pattern_resource_leak ~loc ~leftovers =
  structured ~loc (K_let_pattern_resource_leak { leftovers })

let iter_requires_impure ~loc ~actual =
  structured ~loc (K_iter_requires_impure { actual })

let internal_invariant ~loc ~rule ~invariant =
  structured ~loc (K_internal_invariant { rule; invariant })

let loc = function
  | Structured { loc; _ } -> loc

(* Tag names are ocolor tag names (see Ocolor_format.mli): once the
   formatter has been prettified via [ErrorRender.configure_formatter],
   these render as ANSI styles; on non-tty output they become no-ops
   and the text prints plain. *)
let print_header fmt s =
  Format.pp_open_stag fmt (Format.String_tag "red;bold");
  Format.pp_print_string fmt s;
  Format.pp_close_stag fmt ()

(* A format-style printer wrapper: given a printer [pp] for ['a],
   [print_emph pp fmt x] prints [x] with a red-bold semantic tag. *)
let print_emph pp fmt x =
  Format.pp_open_stag fmt (Format.String_tag "red;bold");
  pp fmt x;
  Format.pp_close_stag fmt ()

let print_to_buffer f =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  f fmt;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let print_location reg fmt pos =
  Format.fprintf fmt "@ at %a:@ " SourcePos.print pos;
  SourceExcerpt.excerpt reg ~context:1 pos fmt;
  Format.pp_print_cut fmt ()

let print_kind fmt = function
  | K_parse_error { msg } ->
    (* Preserve the Menhir-derived message layout — it already
       contains its own newlines and hanging indentation. *)
    Format.fprintf fmt "  %s" msg
  | K_duplicate_pat_var { name } ->
    Format.fprintf fmt "  duplicate variable %a in pattern"
      (print_emph Format.pp_print_string) name
  | K_sort_mismatch { diff; _ } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  expected: @[%a@]" SortDiff.print_left diff;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  actual:   @[%a@]" SortDiff.print_right diff;
    Format.fprintf fmt "@]"
  | K_annotation_disagrees { diff; _ } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  the annotation disagrees with the expression's sort.";
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  annotation: @[%a@]" SortDiff.print_right diff;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  expression: @[%a@]" SortDiff.print_left diff;
    Format.fprintf fmt "@]"
  | K_unbound_var v ->
    Format.fprintf fmt "  unbound variable %a" (print_emph Var.print) v
  | K_unbound_name s ->
    Format.fprintf fmt "  unbound variable %a"
      (print_emph Format.pp_print_string) s
  | K_unbound_ctor l ->
    Format.fprintf fmt "  unknown constructor %a" (print_emph Label.print) l
  | K_unbound_sort d ->
    Format.fprintf fmt "  unknown sort or type %a"
      (print_emph Dsort.print) d
  | K_unbound_tvar a ->
    Format.fprintf fmt "  unbound type variable %a" (print_emph Tvar.print) a
  | K_unknown_function { name } ->
    Format.fprintf fmt "  unknown function %a"
      (print_emph Format.pp_print_string) name
  | K_log_var_not_found { name } ->
    Format.fprintf fmt "  unbound log variable %a"
      (print_emph Var.print) name
  | K_var_effect_mismatch { var; declared; required } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  variable %a was bound with effect %a,"
      (print_emph Var.print) var
      (print_emph Effect.print) declared;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  but the current context only allows effect %a."
      (print_emph Effect.print) required;
    Format.fprintf fmt "@]"
  | K_prim_effect_mismatch { prim; declared; required } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  primitive %a requires effect %a,"
      (print_emph Prim.print) prim
      (print_emph Effect.print) declared;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  but the current context only allows effect %a."
      (print_emph Effect.print) required;
    Format.fprintf fmt "@]"
  | K_fun_effect_mismatch { name; declared; required } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  function %s was declared with effect %a,"
      name (print_emph Effect.print) declared;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  but the current context only allows effect %a."
      (print_emph Effect.print) required;
    Format.fprintf fmt "@]"
  | K_eq_not_equality_type { got } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  @[<hov 2>==@]@ requires an equality type,";
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  but got sort %a." (print_emph Sort.print) got;
    Format.fprintf fmt "@]"
  | K_construct_sort_mismatch { construct; expected_shape; got } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  %s expects a sort of shape %a,"
      construct
      (print_emph Format.pp_print_string) expected_shape;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  but got sort %a." (print_emph Sort.print) got;
    Format.fprintf fmt "@]"
  | K_tuple_arity_mismatch { construct; expected; actual } ->
    Format.fprintf fmt
      "  %s: expected %d component%s, got %d"
      construct
      expected (if expected = 1 then "" else "s")
      actual
  | K_scrutinee_not_data { got } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  case scrutinee must have a data-sort or data-type,";
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  but got sort %a." (print_emph Sort.print) got;
    Format.fprintf fmt "@]"
  | K_not_spec_type { construct; got } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  %s requires a spec type (no @[<hov 2>Pred@ _@]),"
      construct;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  but got sort %a." (print_emph Sort.print) got;
    Format.fprintf fmt "@]"
  | K_spec_context_required { construct } ->
    Format.fprintf fmt
      "  %s is only legal in a @[<hov 2>[spec]@] context." construct
  | K_cannot_synthesize { construct } ->
    Format.fprintf fmt
      "  cannot synthesize %s; add a type annotation." construct
  | K_subst_arity_mismatch { expected; actual } ->
    Format.fprintf fmt
      "  type-argument arity mismatch: expected %d, got %d"
      expected actual
  | K_resource_not_found { name } ->
    Format.fprintf fmt "  resource %a is not in scope"
      (print_emph Var.print) name
  | K_resource_already_used { name } ->
    Format.fprintf fmt "  resource %a has already been consumed"
      (print_emph Var.print) name
  | K_branch_merge_failure { reason } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt
      "  cannot merge refined contexts across branches:";
    Format.pp_print_cut fmt ();
    (match reason with
     | Mf_length_mismatch { lhs; rhs } ->
       Format.fprintf fmt
         "  branch contexts have different lengths (%d vs %d)." lhs rhs
     | Mf_entry_kind_mismatch ->
       Format.fprintf fmt
         "  branches disagree on the kind of entry at some position \
          (comp/log/res mismatch)."
     | Mf_usage_incompatible name ->
       Format.fprintf fmt
         "  resource %a has incompatible usages across branches."
         (print_emph Var.print) name
     | Mf_empty_list ->
       Format.fprintf fmt "  no branches to merge.");
    Format.fprintf fmt "@]"
  | K_dep_res_not_pred { got } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  a @[<hov 2>dep-res@] entry's predicate must have \
                        sort @[<hov 2>Pred _@],";
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  but got sort %a." (print_emph Sort.print) got;
    Format.fprintf fmt "@]"
  | K_ctor_sig_inconsistent { label; where } ->
    Format.fprintf fmt
      "  constructor %a is indexed in the signature but missing from \
       its %s declaration (internal inconsistency)."
      (print_emph Label.print) label where
  | K_tvar_kind_mismatch { tvar; got; expected } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  type variable %a has kind %a,"
      (print_emph Tvar.print) tvar
      (print_emph Kind.print) got;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  but the context requires kind %a."
      (print_emph Kind.print) expected;
    Format.fprintf fmt "@]"
  | K_dsort_arity_mismatch { dsort; expected; actual } ->
    Format.fprintf fmt
      "  %a expects %d argument%s, got %d"
      (print_emph Dsort.print) dsort
      expected (if expected = 1 then "" else "s")
      actual
  | K_pred_misuse { context } ->
    Format.fprintf fmt
      "  @[<hov 2>Pred _@] is not allowed in %s." context
  | K_unguarded_recursion { dsort } ->
    Format.fprintf fmt
      "  recursive reference to %a must go through a @[<hov 2>Ptr _@]."
      (print_emph Dsort.print) dsort
  | K_empty_decl { name; is_type } ->
    Format.fprintf fmt
      "  %s %a declaration must have at least one constructor."
      (if is_type then "type" else "sort")
      (print_emph Format.pp_print_string) name
  | K_duplicate_ctor_in_decl { label; decl_name; is_type } ->
    Format.fprintf fmt
      "  duplicate constructor %a in %s declaration %a."
      (print_emph Label.print) label
      (if is_type then "type" else "sort")
      (print_emph Format.pp_print_string) decl_name
  | K_non_exhaustive { witness } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  pattern match does not cover all cases.";
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  missing case: @[%a@]" PatWitness.print witness;
    Format.fprintf fmt "@]"
  | K_wrong_pred_shape { construct; expected_shape; got } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  %s expects a predicate of shape %a,"
      construct
      (print_emph Format.pp_print_string) expected_shape;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  but got @[%a@]" (print_emph Format.pp_print_string) got;
    Format.fprintf fmt "@]"
  | K_unfold_not_spec { name } ->
    Format.fprintf fmt
      "  unfold: function %a is not a @[<hov 2>[spec]@] function."
      (print_emph Format.pp_print_string) name
  | K_unfold_not_fundef { name } ->
    Format.fprintf fmt
      "  unfold: %a is not an elaborated function definition."
      (print_emph Format.pp_print_string) name
  | K_let_pattern_resource_leak { leftovers } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  let-pattern's resources were not fully consumed.";
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  unconsumed leftovers:";
    List.iter (fun entry ->
      Format.pp_print_cut fmt ();
      Format.fprintf fmt "    %s" entry)
      leftovers;
    Format.fprintf fmt "@]"
  | K_resource_leak { name } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  a resource binding must be consumed before going out of scope.";
    Format.pp_print_cut fmt ();
    (match name with
     | Some v ->
       Format.fprintf fmt "  unconsumed resource: %a"
         (print_emph Var.print) v
     | None ->
       Format.fprintf fmt "  (unconsumed resource in this block)");
    Format.fprintf fmt "@]"
  | K_iter_requires_impure { actual } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  iter requires an @[<hov 2>[impure]@] ambient effect,";
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  but the current effect is %a."
      (print_emph Effect.print) actual;
    Format.fprintf fmt "@]"
  | K_internal_invariant { rule; invariant } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  internal invariant failed in rule %a:"
      (print_emph Format.pp_print_string) rule;
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  %s" invariant;
    Format.fprintf fmt "@]"

let kind_header = function
  | K_parse_error _ -> "Parse error"
  | K_duplicate_pat_var _ -> "Type error: duplicate pattern variable"
  | K_sort_mismatch _ -> "Type error: sort mismatch"
  | K_annotation_disagrees _ -> "Type error: annotation mismatch"
  | K_unbound_var _ | K_unbound_name _ -> "Type error: unbound variable"
  | K_unbound_ctor _ -> "Type error: unknown constructor"
  | K_unbound_sort _ -> "Type error: unknown sort/type"
  | K_unbound_tvar _ -> "Type error: unbound type variable"
  | K_unknown_function _ -> "Type error: unknown function"
  | K_log_var_not_found _ -> "Type error: unbound log variable"
  | K_var_effect_mismatch _
  | K_prim_effect_mismatch _
  | K_fun_effect_mismatch _ -> "Type error: effect mismatch"
  | K_iter_requires_impure _ -> "Type error: wrong effect context"
  | K_eq_not_equality_type _ -> "Type error: not an equality type"
  | K_construct_sort_mismatch _ -> "Type error: wrong sort shape"
  | K_tuple_arity_mismatch _ -> "Type error: arity mismatch"
  | K_scrutinee_not_data _ -> "Type error: bad scrutinee"
  | K_not_spec_type _ -> "Type error: not a spec type"
  | K_spec_context_required _ -> "Type error: wrong effect context"
  | K_cannot_synthesize _ -> "Type error: missing annotation"
  | K_subst_arity_mismatch _ -> "Type error: arity mismatch"
  | K_resource_not_found _ -> "Type error: unknown resource"
  | K_resource_already_used _ -> "Type error: resource reuse"
  | K_branch_merge_failure _ -> "Type error: branch merge failure"
  | K_dep_res_not_pred _ -> "Type error: wrong sort shape"
  | K_ctor_sig_inconsistent _ -> "Internal error: signature inconsistency"
  | K_tvar_kind_mismatch _ -> "Type error: kind mismatch"
  | K_dsort_arity_mismatch _ -> "Type error: sort arity mismatch"
  | K_pred_misuse _ -> "Type error: pred in wrong context"
  | K_unguarded_recursion _ -> "Type error: unguarded recursion"
  | K_empty_decl _ -> "Type error: empty declaration"
  | K_duplicate_ctor_in_decl _ -> "Type error: duplicate constructor"
  | K_non_exhaustive _ -> "Type error: non-exhaustive pattern match"
  | K_wrong_pred_shape _ -> "Type error: wrong predicate shape"
  | K_unfold_not_spec _
  | K_unfold_not_fundef _ -> "Type error: cannot unfold"
  | K_resource_leak _
  | K_let_pattern_resource_leak _ -> "Type error: unconsumed resource"
  | K_internal_invariant _ -> "Internal error: invariant failed"

let rec to_string e =
  print_to_buffer (fun fmt -> print (SourceExcerpt.create ()) fmt e)

and print reg fmt = function
  | Structured { loc; kind } ->
    Format.fprintf fmt "@[<v>";
    print_header fmt (kind_header kind);
    (match loc with
     | Some pos -> print_location reg fmt pos
     | None -> Format.pp_print_cut fmt ());
    print_kind fmt kind;
    Format.fprintf fmt "@]"

module Test = struct
  let contains_substring s sub =
    let n = String.length s and m = String.length sub in
    let rec aux i = i + m <= n && (String.sub s i m = sub || aux (i+1)) in
    m = 0 || (n >= m && aux 0)

  let test_parse_error_preserves_message =
    QCheck.Test.make ~name:"Error: parse_error preserves its message"
      ~count:1
      QCheck.unit
      (fun () ->
        let e = parse_error ~loc:None ~msg:"hello" in
        contains_substring (to_string e) "hello")

  let test_sort_mismatch_mentions_both =
    QCheck.Test.make
      ~name:"Error: sort_mismatch printer mentions expected and actual"
      ~count:1
      QCheck.unit
      (fun () ->
        let mk s =
          Sort.mk (object method loc = SourcePos.dummy end) s in
        let int_s = mk Sort.Int in
        let bool_s = mk Sort.Bool in
        let e = sort_mismatch ~loc:SourcePos.dummy
                  ~expected:int_s ~actual:bool_s in
        let s = to_string e in
        contains_substring s "sort mismatch"
        && contains_substring s "expected"
        && contains_substring s "actual")

  let test_unbound_var_mentions_name =
    QCheck.Test.make
      ~name:"Error: unbound_var printer mentions the variable"
      ~count:1
      QCheck.unit
      (fun () ->
        let (v, _) = Var.mk "xyzzy" SourcePos.dummy Var.empty_supply in
        let e = unbound_var ~loc:SourcePos.dummy v in
        let s = to_string e in
        contains_substring s "unbound variable"
        && contains_substring s "xyzzy")

  let test = [
    test_parse_error_preserves_message;
    test_sort_mismatch_mentions_both;
    test_unbound_var_mentions_name;
  ]
end
