# parser.mly → grammar.js nonterminal mapping

When `parser.mly` changes, find the corresponding `grammar.js` rule(s) below and update them.

## Sorts

| parser.mly | grammar.js |
|---|---|
| `sort` | `_sort`, `_atomic_sort` |
| `sort` (Pred/Ptr) | `pred_sort`, `ptr_sort` |
| `sort` (Record) | `tuple_sort`, `unit_sort`, `paren_sort` |
| `sort` (App) | `app_sort`, `name_sort` |

## Patterns (surface)

| parser.mly | grammar.js |
|---|---|
| `pat` | `_pat`, `_atomic_pat` |
| `pat` (Ctor) | `ctor_pat` |
| `pat` (Tuple) | `tuple_pat`, `unit_pat`, `paren_pat`, `var_pat` |

## Expressions (surface)

| parser.mly | grammar.js |
|---|---|
| `expr` | `_expr`, `annot_expr` |
| `seq_expr` | `_seq_expr`, `let_expr`, `take_expr`, `return_expr`, `fail_expr`, `case_expr`, `iter_expr`, `if_expr` |
| `or_expr` .. `mul_expr` | `_or_expr` .. `_mul_expr`, `or_expr` .. `mul_expr` |
| `app_expr` | `_app_expr`, `inject_expr`, `state_prim_app`, `not_expr`, `fun_call` |
| `simple_expr` | `_simple_expr`, `var_expr`, `int_lit`, `bool_lit`, `unit_expr`, `paren_expr`, `tuple_expr` |

## Declarations

| parser.mly | grammar.js |
|---|---|
| `decl` (fun) | `fun_decl` |
| `decl` (sort/type) | `sort_decl`, `type_decl` |
| `rdecl` (rfun) | `rfun_decl` |
| `main_decl` | `main_decl` |

## Proof sorts

| parser.mly | grammar.js |
|---|---|
| `pf_sort` | `pf_sort` |
| `pf_entry` | `pf_entry` |
| `pf_domain` | `pf_domain` |
| `pf_domain_entry` | `pf_domain_entry` |

## Refined patterns (Phase 1)

| parser.mly | grammar.js |
|---|---|
| `rpat` | `rpat` |
| `rpat_elem` | `rpat_elem` |
| `rpat_res` | `rpat_res` |
| `cpat_inner` | `cpat_inner` |
| `lpat_inner` | `lpat_inner` |

## Core refined terms

| parser.mly | grammar.js |
|---|---|
| `crt_expr` | `_crt_expr`, `crt_annot` |
| `crt_seq_expr` (let) | `crt_let` |
| `crt_seq_expr` (let log) | `crt_let_log`, `crt_let_log_annot` |
| `crt_seq_expr` (let res) | `crt_let_res`, `crt_let_res_annot` |
| `crt_seq_expr` (let core) | `crt_let_core` |
| `crt_seq_expr` (iter) | `crt_iter` |
| `crt_seq_expr` (if) | `crt_if` |
| `crt_seq_expr` (case) | `crt_case` |
| `crt_case_branch` | `crt_case_branch` |
| `crt_seq_expr` (exfalso) | `crt_exfalso` |
| `crt_seq_expr` (open-take) | `crt_open_take` |
| `spine`, `spine_arg` | `spine`, `spine_arg` |

## Proof facts

| parser.mly | grammar.js |
|---|---|
| `lpf_expr` | `lpf_expr`, `lpf_annot` |
| `lpf_atom_expr` | `_lpf_atom`, `lpf_var`, `lpf_auto`, `lpf_unfold`, `lpf_open_ret` |
| `rpf_expr` | `rpf_expr`, `rpf_annot` |
| `rpf_atom_expr` | `_rpf_atom`, `rpf_var`, `rpf_make_ret`, `rpf_make_take` |
