# `ElabM.fail` survivors in `lib/rCheck.ml`

Catalogue of every `ElabM.fail` site that remained in `lib/rCheck.ml`
after Phase E of the rpf / pattern-matching refresh shipped (commit
`e33a8ae`, 226/226 tests passing).  Each site is classified as one
of:

- **defensive guard** — fail-fast at a module boundary on errors the
  upstream pass already attached to a typed AST node.  These are
  intentional and should *not* be migrated; converting them removes
  the boundary's invariant.
- **pattern dispatch** — a fail-fast in the new q_match / cpat_match /
  rpat_match dispatch.  Migration target: attach errors to the
  qbase / pattern node's `info#answer` and continue.
- **CIter holdout** — pre-existing CIter user-error halt.  Multi-error
  rollout deferred CIter; no change.
- **pf_eq holdout** — pre-existing pf_eq user-error halt.  pf_eq
  returns a bare `Constraint.typed_ct`, so attach-and-continue
  doesn't fit naturally without restructuring the result type.
- **RFunDecl holdout** — top-of-decl resource leak; relies on
  per-decl driver capture.

Total surviving sites: **20**.  Three (the q_match dispatcher
helpers at lines 1983, 1994, 2050) are pattern-dispatch holdouts that
the Phase D / E plan flagged for migration but did not migrate in
this slice.

## Inventory

### `elab_se` / `elab_se_check` — defensive guards

| Line | Site | Category |
| ---: | ---- | -------- |
|  146 | `elab_se`: forwards `(CoreExpr.info ce)#answer = Error e` | defensive guard |
|  149 | `elab_se`: forwards the first entry in `subterm_errors` after `annotate_subterm_errors` | defensive guard |
|  159 | `elab_se_check`: forwards the first `subterm_errors` entry after `annotate_subterm_errors` | defensive guard |

These three sites are the boundary between the surface-expression
elaborator (`Elaborate`) and the refined typechecker (`rCheck`).  When
the elaborator returns a typed_ce that already has an answer-level
error or any subterm error, `elab_se` / `elab_se_check` propagate the
*first* such error into the refined typechecker's monad.  The per-decl
driver (`compileFile.compile_rfile`) catches it, records the
diagnostic, and keeps elaborating later declarations.

The reason these stay as `ElabM.fail`: the refined typechecker's
clauses assume the typed_ce they receive is well-formed enough to
read its sort / shape via `CoreExpr.sort_of_info`, `CoreExprView`,
etc.  Stripping the guard would let an Error-tainted typed_ce reach
atom-guards and shape-extractors that expect Ok answers; those would
fall back to placeholders, producing nonsense follow-on diagnostics.
Phase C.0's project-memory note ("Adds `Util.Invariant_failure` …
defensive guards in `elab_se` / `elab_se_check` / `elab_fundecl_body`
… should stay") classified these as keep-forever.

### `CIter` — pre-existing CIter holdouts

| Line | Site | Error kind | Category |
| ---: | ---- | ---------- | -------- |
| 1125 | `CIter` body: effect must subsume Impure | `iter_requires_impure` | CIter holdout |
| 1181 | `CIter` body: pattern bindings must be consumed by the iter step | `resource_leak` | CIter holdout |
| 1207 | `CIter` pattern dispatch: QLog where QCore expected | `iter_pattern_shape ~got:"a logical pattern"` | CIter holdout |
| 1211 | `CIter` pattern dispatch: QRes/QDepRes where QCore expected | `iter_pattern_shape ~got:"a resource pattern"` | CIter holdout |
| 1215 | `CIter` pattern dispatch: empty qbase list | `iter_pattern_shape ~got:"an empty pattern"` | CIter holdout |

`CIter` (`iter[ce](pat = init) { body }`) is a pure-then-impure
control flow primitive.  Its checking rule has tightly woven effect
constraints (init is pure, body is impure, the iter binder is a
specific cpat shape) and the failure modes here all break that
weaving in ways that make attach-and-continue tricky.  For example,
`iter_requires_impure` fires before any of the body has been
elaborated, so there's no typed body to attach an error to; the
existing fail keeps the per-decl driver's handling clean.

`iter_pattern_shape` could in principle attach to the qbase node's
info, but the iter rule's downstream code reads `x_pat` (the iter
binder) by `RPatGet.get_cvar`, and the alternative shapes leave no
sensible binder to substitute through.

The multi-error rollout's project memory marks these as "C.3
holdouts" — deferred until iter's control-flow restructure is done.

### `pf_eq` — pre-existing structural-equality holdouts

| Line | Site | Error kind | Category |
| ---: | ---- | ---------- | -------- |
| 1659 | Comp/Comp entry: sort mismatch | `sort_mismatch` | pf_eq holdout |
| 1662 | Comp/Comp entry: effect mismatch | `pf_effect_mismatch` | pf_eq holdout |
| 1697 | Two non-empty entries with mismatched kinds | `pf_structure_mismatch` | pf_eq holdout |
| 1702 | One list empty, the other not | `pf_structure_mismatch` | pf_eq holdout |

`pf_eq` is structural equality of two proof sorts.  It returns just
a `Constraint.typed_ct` — no typed AST node — so attach-and-continue
doesn't fit naturally: there's no info field where the error could
ride along.  The per-decl driver's catch is the existing safety net.

A future refactor would either give `pf_eq` a typed-result return
(adding a wrapper node carrying answer/subterm_errors) or reshape
the synth-vs-expected check at the call sites so the mismatches
attach to the synthesizing node's info.  Multi-error rollout flagged
these as "C.4 holdouts."

### `cpat_match` (CTuple component-length mismatch) — pattern dispatch

| Line | Site | Error kind | Category |
| ---: | ---- | ---------- | -------- |
| 1757 | CTuple loop: cpat list length disagrees with sort component list length | `K_rpat_length_mismatch` | pattern dispatch |

`cpat_match`'s `CTuple` clause uses `view_get_record_sorts` to obtain
a list of `(Sort.sort, Error.kind) result` per component.  The
helper always returns *exactly* `n` entries (so a sort-shape
mismatch surfaces as `n` `Error _` results, not a length disagreement).
The fail at line 1757 fires only if the inner `loop` runs out of
component sorts before consuming all cpats, or vice versa — which
would mean `view_get_record_sorts`'s `~n` argument disagreed with the
cpat list length.  In practice this is unreachable from valid
parser output (parser guarantees `n = List.length cps`), so the fail
acts as a defensive guard against an arity desync between caller
and the View.  Could be migrated to `Util.raise_invariant` if we
want to mark it explicitly as compiler-bug-only.

### `rpat_match` (RCase) — pattern dispatch

| Line | Site | Error kind | Category |
| ---: | ---- | ---------- | -------- |
| 1896 | RCase: user-supplied label not in the predicate's case branches | `rcase_label_not_in_branches` | pattern dispatch |
| 1933 | RCase: predicate isn't a `case _ of { … }` form | `K_wrong_pred_shape` | pattern dispatch |

These are user-facing pattern errors that should attach to the rpat
node's `info#answer` so the multi-error pipeline shows them next to
the offending pattern.  The current implementation fails the monad,
which lets the per-decl driver capture the error but loses the
fine-grained pattern-node attachment.

Migration target: attach the error to the rpat node, build a
placeholder typed_rp (with `RPat.RAnnot` pointing at a sub-rpat with
its own placeholder), and continue elaboration.  Compatible with the
attach-and-continue pattern but requires synthesizing reasonable
placeholders for the recursive substructure (typed_lp, typed_cp,
typed_inner) — non-trivial.  Phase D introduced these alongside the
new four-judgement split; the migration is a follow-up slice.

### `q_match` (dispatcher) — pattern dispatch

| Line | Site | Error kind | Category |
| ---: | ---- | ---------- | -------- |
| 1983 | `fail_length` helper: qbase list length ≠ proof-sort list length | `K_rpat_length_mismatch` | pattern dispatch |
| 1994 | `fail_kind` helper: qbase head kind ≠ proof-sort entry kind | `K_rpat_kind_mismatch` | pattern dispatch |
| 2050 | QDepRes against a non-`Pred` predicate sort | `K_dep_res_not_pred` | pattern dispatch |

These three are the q_match outer dispatcher's fail-fast paths, new
in Phase D.  Migration would attach errors to the qbase node's
`info#answer` and continue with the remaining list.  Subtleties:

- For `fail_length`: when the qbase list is shorter than the proof
  sort, the trailing pf entries have no qbase to attach against; we
  could attach the diagnostic to the last consumed qbase (or the
  q itself).  When the qbase list is longer, the trailing qbases
  attach individually.
- For `fail_kind`: attach to the offending qbase, then either skip
  (advance both lists by one) or commit to one and attach a "skipped"
  diagnostic.  The latter risks cascading diagnostics; the former
  may misalign substitution chains for downstream entries.
- For `K_dep_res_not_pred`: attach to the qbase, build a placeholder
  typed_rp, continue.

The plan's "Eliminate ElabM.fail" section explicitly flagged these
three as Phase D / E migration targets; not migrated in the
implementation slice because the partial-traversal output's
recursive substructure (typed_q with placeholders for skipped
elements) is non-trivial to construct without introducing a new
"placeholder qbase" constructor or significantly restructuring
q_match's accumulator shape.

### `elab_fundecl_body` — defensive guard

| Line | Site | Error kind | Category |
| ---: | ---- | ---------- | -------- |
| 2069 | `elab_fundecl_body`: forwards the first `subterm_errors` entry after `annotate_subterm_errors` | defensive guard |

Sibling of `elab_se` / `elab_se_check`.  `elab_fundecl_body` is the
boundary between surface elaboration and the refined typechecker
for *function declaration bodies* (the `body` field of a
`FunDecl`).  Same rationale: don't let Error-tainted typed_ce reach
the refined-checker's atom-guards.

### `check_rdecl` (RFunDecl resource-leak) — RFunDecl holdout

| Line | Site | Error kind | Category |
| ---: | ---- | ---------- | -------- |
| 2110 | `RFunDecl` body: pattern bindings must be consumed at the end of the function | `let_pattern_resource_leak` | RFunDecl holdout |

Top-of-decl: after checking an `rfun` body, the output context
must have all linear resources consumed.  If not, fail with
`let_pattern_resource_leak` listing leftovers.  Could attach to the
declaration's typed node, but the typed_decl is built downstream of
this check; restructuring would require threading a "leaked
diagnostic" down through `check_rdecl`'s `RFunDecl` arm.  Per-decl
driver catches the error and lets later declarations elaborate, so
the multi-error story is preserved at file granularity.

## Summary

| Category | Count | Action |
| -------- | ----: | ------ |
| defensive guard | 4 | keep |
| CIter holdout | 5 | follow-up (multi-error rollout C.3) |
| pf_eq holdout | 4 | follow-up (multi-error rollout C.4) |
| pattern dispatch | 6 | follow-up — new in Phase D / E |
| RFunDecl holdout | 1 | follow-up |
| **Total** | **20** | |

The 6 "pattern dispatch" sites are the substantive new survivors
introduced by the Phase D / E refresh.  The plan's verification
step expected these to be migrated; the migration was deferred
because the partial-traversal placeholder output is non-trivial
without a "skipped" pattern variant or restructured accumulator.
That migration is a tracked follow-up slice alongside the C.3 / C.4
holdouts already on the multi-error rollout backlog.
