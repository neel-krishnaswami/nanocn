# Form index — nanoCN syntactic forms and their canonical baselines

One-page map of the syntactic forms in the parser, the number of error states each walks through, and the file under `examples/` that exercises it.

Run `scripts/classify_states.sh --summary` for the live count. The numbers below are snapshots at the time of writing and may drift as the grammar evolves.

## Surface (simply-typed) fragment

| Form | States | Canonical baseline | Notes |
|---|---|---|---|
| `fun <n> : τ → σ [eff] = { <branches> }` | 11 | `examples/fun_simple.cn` | Branch body is `{ <pat> -> <body> | ... }` |
| `sort D[(params)] = { <ctors> }` | 8 | `examples/sort_enum.cn`, `examples/spec_list.cn` | Plus 4 ctor-list states (60, 61) and 4 ctor-decl states (55, 56) |
| `type D[(params)] = { <ctors> }` | 8 | `examples/type_record_like.cn` | Same shape as sort, different keyword |
| `main : τ [eff] = <body>` | 8 | any file | One per file |
| `let <pat> [: τ] = <rhs>; <body>` | 10 | `examples/let_plain.cn`, `let_annotated.cn`, `let_tuple.cn` | State 195 unreachable |
| `take <pat> [: τ] = <spec-rhs>; <body>` | 10 | `examples/spec_take.cn`, `spec_take_annotated.cn` | Only legal in `[spec]` |
| `if <cond> then <t> else <e>` | 5 | any file with `if` | State 179 unreachable |
| `case <e> of { <branches> }` | 4+2 | any `sum_types` / `mixed` | Plus case_branch states |
| `iter (<pat> = <init>) { <body> }` | 8 | `examples/gcd.cn`, `fibonacci.cn`, `listsum.cn`, `factorial.cn` | Body returns `Step(α, σ)` |
| `return <e>` | 1 | `spec_take.cn` | Only in spec context |
| `<prim>[τ] <arg>` (New/Del/Get/Set/Own/Eq) | 3+1 | `examples/state.cn`, `factorial.cn` | State 122 handled; 123-125 via surface |
| Patterns (tuple, ctor, parenthesised) | 6 | exercised everywhere | States 81, 83, 86, 88, 91, 92 |
| Sort forms (`Pred s`, `Ptr s`, record, parametric) | 7 | exercised everywhere | States 13, 14, 16, 17, 18, 29, 30 |
| Tuple/paren expression `(e)`, `(e,e)`, `(e,…,e)` | 3+1 | any | States 107, 198, 200, 204 |

## Refined fragment

| Form | States | Canonical baseline | Notes |
|---|---|---|---|
| `rfun <n> (<dom>) -> <cod> [eff] = <crt>` | 9 | `examples/listlength.rcn`, `incr.rcn`, `refined_add.rcn`, `simple_refined.rcn` | |
| `rmain : <pf_sort> [eff] = <crt>` | 8 | any `.rcn` file | One per file |
| `fun <n>(<param>) -> σ [eff] = <expr>` (inside `.rcn`) | 13 | `examples/refined_add.rcn`, `listlength.rcn` | Different shape from surface `fun` — one parameter, expression body |
| `sort`/`type` inside `.rcn` | 8 + 8 | `examples/listlength.rcn` | Same grammar as surface |
| **Refined term (`crt_expr`) forms:** | | | |
| `let <rpat> = <crt>; <crt>` | 5 | `examples/refined_add.rcn` | |
| `let core[a] <binder> = <expr>; <crt>` | 13 | `examples/listlength.rcn` | Two forms: single binder, or tuple of names |
| `let log <n> [: σ] = <lpf>; <crt>` | 10 | `examples/listlength.rcn` | |
| `let res <n> [: π @ v] = <rpf>; <crt>` | 12 | `examples/listlength.rcn`, `incr.rcn` | |
| `case[<log-var>] <e> of { <rbranches> }` | 7+5 | `examples/listlength.rcn` | Branches are `<Ctor> <var> -> <crt>` — single-binder only |
| `if[<log-var>] <cond> then <crt> else <crt>` | 8 | `examples/incr.rcn` | |
| `iter [<res>] (<rpat> = <init>) { <body> }` | 11 | `examples/listlength.rcn` | Prefix `[<res>]` is the loop's resource invariant |
| `open_take(<rpf>)` | 3 | `examples/listlength.rcn` | |
| `make_take(<crt>)` | 3 | — | |
| `make_ret(<lpf>)` | 2 | — | |
| **Proof terms:** | | | |
| `rpf_expr` (resource proof) | 8 | `examples/listlength.rcn` | |
| `lpf_expr` (logical proof) | 3 | `examples/listlength.rcn` | |
| `unfold <f>(<e>)` | 4 | `examples/listlength.rcn` | |
| `open_ret(<rpf>)` | 2 | `examples/listlength.rcn` | |
| **Refined patterns:** | | | |
| `rpat` (top-level) | 1 | everywhere | |
| `rpat_elem` (list element) | 7 | `examples/listlength.rcn` | Includes DepRes pair form `(<log>, <res>)` |
| **Call spine `f(<args>)`:** | | | |
| `spine_arg` (single argument shape) | 4 | `examples/listlength.rcn` | Core expr, `log <x>`, `res <r>` |
| `crt_spine_expr` | 1 | everywhere | |
| **Refined-function signature internals:** | | | |
| `pf_domain_entry` (each domain entry) | 28 | `examples/listlength.rcn` | Value, log, res, DepRes variants |
| `pf_entry` (each codomain entry) | 24 | `examples/listlength.rcn` | Same shape, slightly different grammar |
| `pf_domain` / `pf_sort` framing | 1+3 | everywhere | |

## How to use this index

When triaging a new PLACEHOLDER state:

1. Run `scripts/classify_states.sh --state <N>` — it prints the form.
2. Look up the form in this table — find the canonical baseline.
3. The baseline's body gives you a working example of the form; derive your mutant from it.

When the user changes the grammar:

1. Run `scripts/diff_states.sh` — see what moved.
2. For each new state, `scripts/classify_states.sh --state <N>` to see the form.
3. If the form isn't in this table, it's a truly new form — you need to write a new baseline. Add it to this table when done.

## Canonical baseline conventions

- Every baseline should parse AND typecheck. Use `verify_baselines.sh --typecheck` to check.
- Prefer real-feeling programs over toy `a b c` examples. `gcd`, `fibonacci`, `listsum` are canonical because they exercise iter + arithmetic + tuple state + control flow.
- When a form has several legal sub-forms (e.g., `let` has annotated and unannotated variants), write one baseline per sub-form: `let_plain.cn`, `let_annotated.cn`, `let_tuple.cn`.
- Baselines live directly in `examples/`. Mutants live in `examples/errors/parsing/`. Don't mix them.
