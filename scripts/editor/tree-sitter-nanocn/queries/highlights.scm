; highlights.scm — font-lock captures for nanoCN
;
; Feature levels (matching Emacs treesit-font-lock-feature-list):
;   Level 1: comment, definition
;   Level 2: keyword, string, type
;   Level 3: constant, number, function, builtin
;   Level 4: operator, delimiter, bracket

; === Comments ===

(comment) @comment

; === Definition names (Level 1) ===

(fun_decl name: (lower_ident) @function)
(rfun_decl name: (lower_ident) @function)
(sort_decl name: (upper_label) @type.definition)
(type_decl name: (upper_label) @type.definition)

; === Keywords (Level 2) ===

; Keywords that appear as anonymous tokens inside seq() rules.
; Bare-string rules (fail_expr, crt_exfalso, lpf_auto) are handled
; as @constant.builtin below; eff_level is its own named node.
[
  "fun"
  "rfun"
  "sort"
  "type"
  "main"
  "let"
  "case"
  "of"
  "iter"
  "if"
  "then"
  "else"
  "take"
  "do"
  "return"
  "unfold"
  "core"
  "log"
  "res"
  "not"
  "open-ret"
  "open-take"
  "make-ret"
  "make-take"
] @keyword

(eff_level) @keyword

; === Type names (Level 2) ===

(int_sort) @type.builtin
(bool_sort) @type.builtin
"Pred" @type.builtin
"Ptr" @type.builtin

(app_sort name: (upper_label) @type)
(name_sort (upper_label) @type)

; === Constructor labels (Level 2) ===

(ctor_decl label: (upper_label) @constructor)
(ctor_pat label: (upper_label) @constructor)
(inject_expr label: (upper_label) @constructor)
(crt_case_branch label: (upper_label) @constructor)
(case_branch pat: (ctor_pat label: (upper_label) @constructor))

; === Constants and numbers (Level 3) ===

(int_lit) @number
(bool_lit) @constant.builtin
(unit_expr) @constant.builtin
(unit_pat) @constant.builtin
(fail_expr) @constant.builtin
(crt_exfalso) @constant.builtin
(lpf_auto) @constant.builtin
(hole_expr) @variable.special

; === Function references (Level 3) ===

(fun_call func: (lower_ident) @function.call)
(crt_call func: (lower_ident) @function.call)
(lpf_unfold func: (lower_ident) @function.call)

; === Built-in state primitives (Level 3) ===

(state_prim) @function.builtin

; === Operators (Level 4) ===

(or_expr  op: _ @operator)
(and_expr op: _ @operator)
(eq_expr  op: _ @operator)
(cmp_expr op: _ @operator)
(add_expr op: _ @operator)
(mul_expr op: _ @operator)
"@" @operator
"->" @operator
":" @operator

; === Delimiters (Level 4) ===

"," @punctuation.delimiter
";" @punctuation.delimiter
"|" @punctuation.delimiter
"=" @punctuation.delimiter

; === Brackets (Level 4) ===

"(" @punctuation.bracket
")" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
