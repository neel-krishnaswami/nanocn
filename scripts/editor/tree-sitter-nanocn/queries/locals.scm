; locals.scm — scope and reference hints for nanoCN
;
; Minimal: real scope resolution belongs to the LSP server. These
; hints tell tree-sitter which nodes introduce scopes and which
; identifiers are definitions vs references. This enables basic
; local-variable highlighting in editors that support it.

; === Scopes ===

(fun_decl) @local.scope
(rfun_decl) @local.scope
(branch) @local.scope
(case_branch) @local.scope
(crt_case_branch) @local.scope
(let_expr) @local.scope
(take_expr) @local.scope
(iter_expr) @local.scope
(crt_let) @local.scope
(crt_let_log) @local.scope
(crt_let_log_annot) @local.scope
(crt_let_res) @local.scope
(crt_let_res_annot) @local.scope
(crt_let_core) @local.scope
(crt_iter) @local.scope
(crt_if) @local.scope

; === Definitions (pattern variables and binders) ===

(var_pat (lower_ident) @local.definition)
(fun_decl param: (lower_ident) @local.definition)
(crt_let_log var: (lower_ident) @local.definition)
(crt_let_log_annot var: (lower_ident) @local.definition)
(crt_let_res var: (lower_ident) @local.definition)
(crt_let_res_annot var: (lower_ident) @local.definition)
(crt_let_core proof: (lower_ident) @local.definition)
(crt_if witness: (lower_ident) @local.definition)
(crt_case witness: (lower_ident) @local.definition)
(crt_case_branch var: (lower_ident) @local.definition)

; === References ===

(var_expr (lower_ident) @local.reference)
