type reserved =
  | R_BINARY | R_DECIMAL | R_HEXADECIMAL | R_NUMERAL | R_STRING
  | R_underscore | R_bang
  | R_as | R_lambda | R_let | R_exists | R_forall | R_match | R_par
  | R_assert | R_check_sat | R_check_sat_assuming
  | R_declare_const | R_declare_datatype | R_declare_datatypes
  | R_declare_fun | R_declare_sort | R_declare_sort_parameter
  | R_define_const | R_define_fun | R_define_fun_rec | R_define_funs_rec
  | R_define_sort
  | R_echo | R_exit
  | R_get_assertions | R_get_assignment | R_get_info | R_get_model
  | R_get_option | R_get_proof | R_get_unsat_assumptions
  | R_get_unsat_core | R_get_value
  | R_pop | R_push | R_reset | R_reset_assertions
  | R_set_info | R_set_logic | R_set_option

(* Shared spelling table. One entry per [reserved] constructor; used
   for both [reserved_of_string] (lexer classification) and
   [reserved_to_string] (printer). Keeping the two functions driven
   by one source avoids drift. *)
let reserved_table : (string * reserved) list = [
  "BINARY", R_BINARY;
  "DECIMAL", R_DECIMAL;
  "HEXADECIMAL", R_HEXADECIMAL;
  "NUMERAL", R_NUMERAL;
  "STRING", R_STRING;
  "_", R_underscore;
  "!", R_bang;
  "as", R_as;
  "lambda", R_lambda;
  "let", R_let;
  "exists", R_exists;
  "forall", R_forall;
  "match", R_match;
  "par", R_par;
  "assert", R_assert;
  "check-sat", R_check_sat;
  "check-sat-assuming", R_check_sat_assuming;
  "declare-const", R_declare_const;
  "declare-datatype", R_declare_datatype;
  "declare-datatypes", R_declare_datatypes;
  "declare-fun", R_declare_fun;
  "declare-sort", R_declare_sort;
  "declare-sort-parameter", R_declare_sort_parameter;
  "define-const", R_define_const;
  "define-fun", R_define_fun;
  "define-fun-rec", R_define_fun_rec;
  "define-funs-rec", R_define_funs_rec;
  "define-sort", R_define_sort;
  "echo", R_echo;
  "exit", R_exit;
  "get-assertions", R_get_assertions;
  "get-assignment", R_get_assignment;
  "get-info", R_get_info;
  "get-model", R_get_model;
  "get-option", R_get_option;
  "get-proof", R_get_proof;
  "get-unsat-assumptions", R_get_unsat_assumptions;
  "get-unsat-core", R_get_unsat_core;
  "get-value", R_get_value;
  "pop", R_pop;
  "push", R_push;
  "reset", R_reset;
  "reset-assertions", R_reset_assertions;
  "set-info", R_set_info;
  "set-logic", R_set_logic;
  "set-option", R_set_option;
]

let reserved_of_string s =
  let rec loop = function
    | [] -> None
    | (str, r) :: _ when String.equal str s -> Some r
    | _ :: rest -> loop rest
  in
  loop reserved_table

let reserved_to_string = function
  | R_BINARY -> "BINARY"
  | R_DECIMAL -> "DECIMAL"
  | R_HEXADECIMAL -> "HEXADECIMAL"
  | R_NUMERAL -> "NUMERAL"
  | R_STRING -> "STRING"
  | R_underscore -> "_"
  | R_bang -> "!"
  | R_as -> "as"
  | R_lambda -> "lambda"
  | R_let -> "let"
  | R_exists -> "exists"
  | R_forall -> "forall"
  | R_match -> "match"
  | R_par -> "par"
  | R_assert -> "assert"
  | R_check_sat -> "check-sat"
  | R_check_sat_assuming -> "check-sat-assuming"
  | R_declare_const -> "declare-const"
  | R_declare_datatype -> "declare-datatype"
  | R_declare_datatypes -> "declare-datatypes"
  | R_declare_fun -> "declare-fun"
  | R_declare_sort -> "declare-sort"
  | R_declare_sort_parameter -> "declare-sort-parameter"
  | R_define_const -> "define-const"
  | R_define_fun -> "define-fun"
  | R_define_fun_rec -> "define-fun-rec"
  | R_define_funs_rec -> "define-funs-rec"
  | R_define_sort -> "define-sort"
  | R_echo -> "echo"
  | R_exit -> "exit"
  | R_get_assertions -> "get-assertions"
  | R_get_assignment -> "get-assignment"
  | R_get_info -> "get-info"
  | R_get_model -> "get-model"
  | R_get_option -> "get-option"
  | R_get_proof -> "get-proof"
  | R_get_unsat_assumptions -> "get-unsat-assumptions"
  | R_get_unsat_core -> "get-unsat-core"
  | R_get_value -> "get-value"
  | R_pop -> "pop"
  | R_push -> "push"
  | R_reset -> "reset"
  | R_reset_assertions -> "reset-assertions"
  | R_set_info -> "set-info"
  | R_set_logic -> "set-logic"
  | R_set_option -> "set-option"

(* Assign a total order on [reserved] constructors. The specific
   ordering is arbitrary but must be total and consistent. *)
let reserved_ordinal = function
  | R_BINARY -> 0   | R_DECIMAL -> 1   | R_HEXADECIMAL -> 2
  | R_NUMERAL -> 3  | R_STRING -> 4
  | R_underscore -> 5 | R_bang -> 6
  | R_as -> 7       | R_lambda -> 8    | R_let -> 9
  | R_exists -> 10  | R_forall -> 11   | R_match -> 12  | R_par -> 13
  | R_assert -> 14  | R_check_sat -> 15 | R_check_sat_assuming -> 16
  | R_declare_const -> 17 | R_declare_datatype -> 18
  | R_declare_datatypes -> 19 | R_declare_fun -> 20
  | R_declare_sort -> 21 | R_declare_sort_parameter -> 22
  | R_define_const -> 23 | R_define_fun -> 24
  | R_define_fun_rec -> 25 | R_define_funs_rec -> 26
  | R_define_sort -> 27
  | R_echo -> 28    | R_exit -> 29
  | R_get_assertions -> 30 | R_get_assignment -> 31
  | R_get_info -> 32 | R_get_model -> 33
  | R_get_option -> 34 | R_get_proof -> 35
  | R_get_unsat_assumptions -> 36 | R_get_unsat_core -> 37
  | R_get_value -> 38
  | R_pop -> 39 | R_push -> 40 | R_reset -> 41 | R_reset_assertions -> 42
  | R_set_info -> 43 | R_set_logic -> 44 | R_set_option -> 45

let reserved_compare a b = Int.compare (reserved_ordinal a) (reserved_ordinal b)

type t =
  | Numeral     of string
  | Decimal     of string
  | Hexadecimal of string
  | Binary      of string
  | String      of string
  | Symbol      of string
  | Keyword     of string
  | Reserved    of reserved

let tag_ordinal = function
  | Numeral _     -> 0
  | Decimal _     -> 1
  | Hexadecimal _ -> 2
  | Binary _      -> 3
  | String _      -> 4
  | Symbol _      -> 5
  | Keyword _     -> 6
  | Reserved _    -> 7

let compare a b =
  match a, b with
  | Numeral x,     Numeral y     -> String.compare x y
  | Decimal x,     Decimal y     -> String.compare x y
  | Hexadecimal x, Hexadecimal y -> String.compare x y
  | Binary x,      Binary y      -> String.compare x y
  | String x,      String y      -> String.compare x y
  | Symbol x,      Symbol y      -> String.compare x y
  | Keyword x,     Keyword y     -> String.compare x y
  | Reserved x,    Reserved y    -> reserved_compare x y
  | _ -> Int.compare (tag_ordinal a) (tag_ordinal b)

(* §3.1 simple-symbol character class. [letter | digit | sym_punct];
   additionally the first character must not be a digit and the string
   must not be empty. *)
let is_sym_punct = function
  | '~' | '!' | '@' | '$' | '%' | '^' | '&' | '*' | '_' | '+' | '='
  | '<' | '>' | '.' | '?' | '/' | '-' -> true
  | _ -> false

let is_letter = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_sym_char c = is_letter c || is_digit c || is_sym_punct c

let looks_like_simple_symbol s =
  (* Legal [simple_symbol]: non-empty, every char in [sym_char], first
     char not a digit. Reserved-word check is handled separately. *)
  String.length s >= 1
  && not (is_digit s.[0])
  && String.for_all is_sym_char s

let is_valid_symbol s =
  String.for_all (fun c -> not (Char.equal c '|') && not (Char.equal c '\\')) s

(* Print a symbol's contents: bare if it's a legal simple-symbol and
   not a reserved-word spelling; otherwise as a [|...|] quoted symbol.
   The precondition [is_valid_symbol s] is required for the quoted
   form to be well-formed; it is documented in the .mli. *)
let print_symbol fmt s =
  let can_be_simple =
    looks_like_simple_symbol s
    && (match reserved_of_string s with Some _ -> false | None -> true)
  in
  if can_be_simple then Format.pp_print_string fmt s
  else begin
    Format.pp_print_char fmt '|';
    Format.pp_print_string fmt s;
    Format.pp_print_char fmt '|'
  end

(* Print a string literal: wrap in double quotes, double any embedded
   quote character. Raw newlines and other bytes are emitted verbatim
   per §3.1 (no backslash escapes in SMT-LIB strings). *)
let print_string_literal fmt s =
  Format.pp_print_char fmt '"';
  String.iter (fun c ->
    if Char.equal c '"' then begin
      Format.pp_print_char fmt '"';
      Format.pp_print_char fmt '"'
    end else
      Format.pp_print_char fmt c
  ) s;
  Format.pp_print_char fmt '"'

let print fmt = function
  | Numeral n -> Format.pp_print_string fmt n
  | Decimal d -> Format.pp_print_string fmt d
  | Hexadecimal h -> Format.fprintf fmt "#x%s" h
  | Binary b -> Format.fprintf fmt "#b%s" b
  | String s -> print_string_literal fmt s
  | Symbol s -> print_symbol fmt s
  | Keyword k -> Format.fprintf fmt ":%s" k
  | Reserved r -> Format.pp_print_string fmt (reserved_to_string r)

let to_string a = Format.asprintf "%a" print a

module Test = struct
  open QCheck.Gen

  let gen_reserved =
    oneof_list [
      R_BINARY; R_DECIMAL; R_HEXADECIMAL; R_NUMERAL; R_STRING;
      R_underscore; R_bang;
      R_as; R_lambda; R_let; R_exists; R_forall; R_match; R_par;
      R_assert; R_check_sat; R_check_sat_assuming;
      R_declare_const; R_declare_datatype; R_declare_datatypes;
      R_declare_fun; R_declare_sort; R_declare_sort_parameter;
      R_define_const; R_define_fun; R_define_fun_rec; R_define_funs_rec;
      R_define_sort;
      R_echo; R_exit;
      R_get_assertions; R_get_assignment; R_get_info; R_get_model;
      R_get_option; R_get_proof; R_get_unsat_assumptions;
      R_get_unsat_core; R_get_value;
      R_pop; R_push; R_reset; R_reset_assertions;
      R_set_info; R_set_logic; R_set_option;
    ]

  (* Generate a legal simple-symbol that is not a reserved-word
     spelling. Used both for [Symbol] atoms and (indirectly via
     [Keyword]) for keyword contents. *)
  let gen_simple_symbol_string =
    let letter_or_punct = oneof [
      map Char.chr (97 -- 122);
      map Char.chr (65 -- 90);
      oneof_list ['~';'!';'@';'$';'%';'^';'&';'*';'_';'+';'=';
              '<';'>';'.';'?';'/';'-'];
    ] in
    let sym_char = oneof [
      letter_or_punct;
      map Char.chr (48 -- 57);
    ] in
    let rec loop () =
      let* first = letter_or_punct in
      let* rest = list_size (0 -- 6) sym_char in
      let s = String.init (1 + List.length rest) (fun i ->
        if i = 0 then first else List.nth rest (i - 1))
      in
      match reserved_of_string s with
      | Some _ -> loop ()
      | None -> pure s
    in
    loop ()

  let gen_numeral_string =
    oneof [
      pure "0";
      (let* d = 1 -- 9 in
       let* rest = list_size (0 -- 5) (0 -- 9) in
       pure (String.concat "" (string_of_int d :: List.map string_of_int rest)));
    ]

  let gen_decimal_string =
    let* n = gen_numeral_string in
    let* frac = list_size (1 -- 4) (0 -- 9) in
    pure (n ^ "." ^ String.concat "" (List.map string_of_int frac))

  let gen_hex_string =
    let hex_digit = oneof_list
      ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
       'a';'b';'c';'d';'e';'f';'A';'B';'C';'D';'E';'F'] in
    let* cs = list_size (1 -- 6) hex_digit in
    pure (String.init (List.length cs) (fun i -> List.nth cs i))

  let gen_bin_string =
    let bit = oneof_list ['0'; '1'] in
    let* cs = list_size (1 -- 8) bit in
    pure (String.init (List.length cs) (fun i -> List.nth cs i))

  (* SMT-LIB strings: any printable ASCII. We include embedded quote
     characters to stress the doubling in the printer. *)
  let gen_string_contents =
    let char_gen = oneof [
      map Char.chr (32 -- 126);
      pure '"';
    ] in
    let* cs = list_size (0 -- 8) char_gen in
    pure (String.init (List.length cs) (fun i -> List.nth cs i))

  let gen =
    oneof [
      map (fun s -> Numeral s)     gen_numeral_string;
      map (fun s -> Decimal s)     gen_decimal_string;
      map (fun s -> Hexadecimal s) gen_hex_string;
      map (fun s -> Binary s)      gen_bin_string;
      map (fun s -> String s)      gen_string_contents;
      map (fun s -> Symbol s)      gen_simple_symbol_string;
      map (fun s -> Keyword s)     gen_simple_symbol_string;
      map (fun r -> Reserved r)    gen_reserved;
    ]

  (* One case per [reserved] constructor: exercises the bijection
     [reserved_of_string (reserved_to_string r) = Some r]. *)
  let all_reserved = [
    R_BINARY; R_DECIMAL; R_HEXADECIMAL; R_NUMERAL; R_STRING;
    R_underscore; R_bang;
    R_as; R_lambda; R_let; R_exists; R_forall; R_match; R_par;
    R_assert; R_check_sat; R_check_sat_assuming;
    R_declare_const; R_declare_datatype; R_declare_datatypes;
    R_declare_fun; R_declare_sort; R_declare_sort_parameter;
    R_define_const; R_define_fun; R_define_fun_rec; R_define_funs_rec;
    R_define_sort;
    R_echo; R_exit;
    R_get_assertions; R_get_assignment; R_get_info; R_get_model;
    R_get_option; R_get_proof; R_get_unsat_assumptions;
    R_get_unsat_core; R_get_value;
    R_pop; R_push; R_reset; R_reset_assertions;
    R_set_info; R_set_logic; R_set_option;
  ]

  let test = [
    QCheck.Test.make ~name:"smtAtom compare is reflexive"
      ~count:100
      (QCheck.make gen)
      (fun a -> compare a a = 0);

    QCheck.Test.make ~name:"smtAtom compare is antisymmetric"
      ~count:100
      (QCheck.make (pair gen gen))
      (fun (a, b) ->
         let x = compare a b in
         let y = compare b a in
         (x = 0 && y = 0) || (x > 0 && y < 0) || (x < 0 && y > 0));

    QCheck.Test.make ~name:"smtAtom compare is transitive"
      ~count:200
      (QCheck.make (triple gen gen gen))
      (fun (a, b, c) ->
         let ab = compare a b and bc = compare b c and ac = compare a c in
         if ab <= 0 && bc <= 0 then ac <= 0
         else if ab >= 0 && bc >= 0 then ac >= 0
         else true);

    QCheck.Test.make ~name:"smtAtom print never raises"
      ~count:200
      (QCheck.make gen)
      (fun a -> let _ = to_string a in true);

    QCheck.Test.make ~name:"smtAtom reserved bijection"
      ~count:(List.length all_reserved)
      (QCheck.make (oneof_list all_reserved))
      (fun r ->
         match reserved_of_string (reserved_to_string r) with
         | Some r' -> reserved_compare r r' = 0
         | None -> false);

    QCheck.Test.make ~name:"smtAtom generated symbols are valid"
      ~count:100
      (QCheck.make gen_simple_symbol_string)
      (fun s -> is_valid_symbol s);
  ]
end
