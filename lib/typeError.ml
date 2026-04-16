type kind =
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
  | K_scrutinee_not_data of { got : Sort.sort }
  | K_not_spec_type of { construct : string; got : Sort.sort }
  | K_spec_context_required of { construct : string }
  | K_non_exhaustive of { witness : PatWitness.t }
  | K_resource_leak of { name : Var.t option }
  | K_iter_requires_impure of { actual : Effect.t }

type t =
  | Legacy of SourcePos.t option * string
  | Structured of { loc : SourcePos.t; kind : kind }

let legacy pos msg = Legacy (pos, msg)
let structured ~loc kind = Structured { loc; kind }

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

let non_exhaustive ~loc ~witness =
  structured ~loc (K_non_exhaustive { witness })

let resource_leak ~loc ~name =
  structured ~loc (K_resource_leak { name })

let iter_requires_impure ~loc ~actual =
  structured ~loc (K_iter_requires_impure { actual })

let loc = function
  | Legacy (pos, _) -> pos
  | Structured { loc; _ } -> Some loc

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
  | K_non_exhaustive { witness } ->
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "  pattern match does not cover all cases.";
    Format.pp_print_cut fmt ();
    Format.fprintf fmt "  missing case: @[%a@]" PatWitness.print witness;
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

let kind_header = function
  | K_sort_mismatch _ -> "Type error: sort mismatch"
  | K_annotation_disagrees _ -> "Type error: annotation mismatch"
  | K_unbound_var _ | K_unbound_name _ -> "Type error: unbound variable"
  | K_unbound_ctor _ -> "Type error: unknown constructor"
  | K_unbound_sort _ -> "Type error: unknown sort/type"
  | K_unbound_tvar _ -> "Type error: unbound type variable"
  | K_var_effect_mismatch _
  | K_prim_effect_mismatch _
  | K_fun_effect_mismatch _ -> "Type error: effect mismatch"
  | K_scrutinee_not_data _ -> "Type error: bad scrutinee"
  | K_not_spec_type _ -> "Type error: not a spec type"
  | K_spec_context_required _ -> "Type error: wrong effect context"
  | K_non_exhaustive _ -> "Type error: non-exhaustive pattern match"
  | K_resource_leak _ -> "Type error: unconsumed resource"
  | K_iter_requires_impure _ -> "Type error: wrong effect context"

let rec to_string e =
  print_to_buffer (fun fmt -> print (SourceExcerpt.create ()) fmt e)

and print reg fmt = function
  | Legacy (pos_opt, msg) ->
    Format.fprintf fmt "@[<v>";
    print_header fmt "Error";
    (match pos_opt with
     | Some pos -> print_location reg fmt pos
     | None -> Format.pp_print_cut fmt ());
    Format.fprintf fmt "  %s" msg;
    Format.fprintf fmt "@]"
  | Structured { loc; kind } ->
    Format.fprintf fmt "@[<v>";
    print_header fmt (kind_header kind);
    print_location reg fmt loc;
    print_kind fmt kind;
    Format.fprintf fmt "@]"

module Test = struct
  let contains_substring s sub =
    let n = String.length s and m = String.length sub in
    let rec aux i = i + m <= n && (String.sub s i m = sub || aux (i+1)) in
    m = 0 || (n >= m && aux 0)

  let test_legacy_preserves_message =
    QCheck.Test.make ~name:"TypeError: Legacy preserves its message"
      ~count:1
      QCheck.unit
      (fun () ->
        let e = legacy None "hello" in
        contains_substring (to_string e) "hello")

  let test_sort_mismatch_mentions_both =
    QCheck.Test.make
      ~name:"TypeError: sort_mismatch printer mentions expected and actual"
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
      ~name:"TypeError: unbound_var printer mentions the variable"
      ~count:1
      QCheck.unit
      (fun () ->
        let (v, _) = Var.mk "xyzzy" SourcePos.dummy Var.empty_supply in
        let e = unbound_var ~loc:SourcePos.dummy v in
        let s = to_string e in
        contains_substring s "unbound variable"
        && contains_substring s "xyzzy")

  let test = [
    test_legacy_preserves_message;
    test_sort_mismatch_mentions_both;
    test_unbound_var_mentions_name;
  ]
end
