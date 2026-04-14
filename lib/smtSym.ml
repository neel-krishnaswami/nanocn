let of_var v = Var.to_string v
let of_label l = Label.to_string l
let of_dsort d = Dsort.to_string d
let of_tvar a = Tvar.to_string a
let of_funname name = name

let ctor_name d l = Dsort.to_string d ^ "-" ^ Label.to_string l
let ctor_selector d l = "get-" ^ Dsort.to_string d ^ "-" ^ Label.to_string l
let tuple_sort n = Printf.sprintf "Tuple-%d" n
let tuple_ctor n = Printf.sprintf "tuple-%d" n
let tuple_proj n k = Printf.sprintf "prj-%d-%d" n k

(* [sort_tag] is a structural recursion on [Sort.shape]. It produces a
   flat legal SMT simple-symbol string; the output uses only letters,
   digits, underscore, and hyphen, all of which are permitted in a
   simple_symbol per SMT-LIB 2.7 §3.1.

   The construction matches [SmtExpr.of_sort] shape-by-shape, so the
   QCheck invariant relating the two holds by structural induction. *)
let rec sort_tag s =
  match Sort.shape s with
  | Sort.Int -> "Int"
  | Sort.Bool -> "Bool"
  | Sort.Ptr t -> "Ptr_" ^ sort_tag t
  | Sort.Record [] -> tuple_sort 0
  | Sort.Record [t] -> sort_tag t
  | Sort.Record ts ->
    let n = List.length ts in
    let parts = List.map sort_tag ts in
    tuple_sort n ^ "_" ^ String.concat "_" parts
  | Sort.App (d, []) -> Dsort.to_string d
  | Sort.App (d, ts) ->
    let parts = List.map sort_tag ts in
    Dsort.to_string d ^ "_" ^ String.concat "_" parts
  | Sort.Pred t -> "Pred_" ^ sort_tag t
  | Sort.TVar a -> Tvar.to_string a

let return_sym t = "return-" ^ sort_tag t
let fail_sym t   = "fail-"   ^ sort_tag t
let bind_sym t s = "bind-"   ^ sort_tag t ^ "-" ^ sort_tag s
let own_sym t    = "own-"    ^ sort_tag t

module Test = struct
  open QCheck.Gen

  (* Legal simple-symbol character set per §3.1. *)
  let is_simple_symbol_char c =
    let code = Char.code c in
    (Char.compare 'a' c <= 0 && Char.compare c 'z' <= 0)
    || (Char.compare 'A' c <= 0 && Char.compare c 'Z' <= 0)
    || (Char.compare '0' c <= 0 && Char.compare c '9' <= 0)
    || Char.equal c '_' || Char.equal c '-'
    (* the full set allows more punctuation but we don't emit any *)
    || code < 0 (* unreachable, placate exhaustiveness *)

  let is_legal_simple_symbol s =
    String.length s >= 1
    && not (Char.compare '0' s.[0] <= 0 && Char.compare s.[0] '9' <= 0)
    && String.for_all is_simple_symbol_char s

  let test = [
    QCheck.Test.make ~name:"smtSym sort_tag stability under Sort.compare"
      ~count:200
      (QCheck.make (pair Sort.Test.gen Sort.Test.gen))
      (fun (s1, s2) ->
         if Sort.compare s1 s2 = 0
         then String.equal (sort_tag s1) (sort_tag s2)
         else true);

    QCheck.Test.make ~name:"smtSym sort_tag is a legal simple_symbol"
      ~count:200
      (QCheck.make Sort.Test.gen)
      (fun s -> is_legal_simple_symbol (sort_tag s));

    QCheck.Test.make ~name:"smtSym return/fail/bind_sym are legal simple_symbols"
      ~count:100
      (QCheck.make (pair Sort.Test.gen Sort.Test.gen))
      (fun (t, s) ->
         is_legal_simple_symbol (return_sym t)
         && is_legal_simple_symbol (fail_sym t)
         && is_legal_simple_symbol (bind_sym t s));

    QCheck.Test.make ~name:"smtSym tuple_sort/ctor/proj are legal simple_symbols"
      ~count:20
      (QCheck.make (int_bound 16))
      (fun n ->
         is_legal_simple_symbol (tuple_sort n)
         && is_legal_simple_symbol (tuple_ctor n)
         && is_legal_simple_symbol (tuple_proj n (max 1 n)));
  ]
end
