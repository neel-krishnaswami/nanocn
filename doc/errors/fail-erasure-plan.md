# Eliminating `ElabM.fail` — implementation plan

## Goal

Drop `ElabM.fail` from the elaboration monad's API and simplify the
type to `'a t = state -> 'a * state` (just a fresh-name supply +
warnings — no error short-circuiting).

After this work:

- Elaboration always produces a fully-elaborated typed AST. The tree
  may carry `Error _` annotations on individual nodes' `info#answer`
  and aggregated `subterm_errors`, but the monad itself never
  short-circuits.
- LSP services see complete diagnostics for type-incorrect programs
  rather than losing all info past the first error.
- The per-decl driver (`compileFile`, `bin/nanocn_lsp.ml`) walks the
  typed AST for diagnostics rather than catching monadic failures.

## Why this works

The surface elaborator (`lib/elaborate.ml`) already never calls
`ElabM.fail` — every error attaches to the typed AST it builds.  The
20 surviving `fail` sites in `lib/rCheck.ml` (catalogued in
`failure-erasure.md`) plus a tail of `ElabM.lift_at`-induced fails
are all migratable to attach-and-continue.

The Phase B/C/D/E refactors already established the patterns:
View modules (option-typed extractors), per-call-site option→result
wrappers, `&&&` linearization for conditional gates, and result-typed
`info#answer` fields.  This plan applies those patterns
exhaustively.

## Commit plan

Nine commits, each leaving the test suite green.  Earlier commits
build foundations the later ones depend on.

### C1 — `RPat.t` restructure

`RPat.t` is currently `'b * qbase list`.  Replace with a proper
shape-functor + knot-tied fixpoint.  Merges `qbaseF` and the new
sequence shape into a single five-case sum (mirroring `spineF`):

```ocaml
type ('cpat, 'lpat, 'rpat, 'q) q_shapeF =
  | QNil
  | QCore   of 'cpat * 'q
  | QLog    of 'lpat * 'q
  | QRes    of 'rpat * 'q
  | QDepRes of 'cpat * 'rpat * 'q

type ('b, 'var) t = TIn of 'b *
  (('b, 'var) cpat, ('b, 'var) lpat, ('b, 'var) rpat, ('b, 'var) t) q_shapeF
```

Removed: `qbase`, `qbaseF`, `mk_qbase`, `qbase_info`, `qbase_shape`,
`map_qbaseF`, `elems`.  Added: `q_shapeF`, `map_q_shapeF`, `shape`.
`mk` changes signature from `'b -> qbase list -> t` to
`'b -> q_shapeF -> t`.

Per-qbase position info is lost; each head element's info (on cpat /
lpat / rpat) is sufficient.

**Caller updates** (mechanical, no semantic shift):
- `lib/parser.mly` — grammar rules build `('q -> q_shapeF)`
  closures, top-level rule folds them right-to-left into a `t`.
- `lib/resolve.ml` — `resolve_qbase_with_entry` and `resolve_qbase`
  inline into a single 5-case walk.
- `lib/hoverIndex.ml` — walks `RPat.shape`.
- `lib/rCheck.ml` — pattern matches and singleton constructions
  translate to shape/QCons-QNil.

No fail sites eliminated; this is the structural foundation.

### C2 — ProofSortView + q_match + signature changes for inner pattern matchers

Add `lib/proofSortView.{ml,mli}`, mirroring `SortView`:

```ocaml
type 'a t = 'a option

module Get : sig
  val nil    : ('e, 'b, 'var) ProofSort.t t -> unit t
  val comp   : ('e, 'b, 'var) ProofSort.t t ->
    'var t * Sort.sort t * Effect.t t * ('e, 'b, 'var) ProofSort.t t
  val log    : ('e, 'b, 'var) ProofSort.t t ->
    'e t * ('e, 'b, 'var) ProofSort.t t
  val res    : ('e, 'b, 'var) ProofSort.t t ->
    'e t * 'e t * ('e, 'b, 'var) ProofSort.t t
  val depres : ('e, 'b, 'var) ProofSort.t t ->
    'var t * 'e t * ('e, 'b, 'var) ProofSort.t t
end

module Build : sig
  (* analogous: each takes 'b, option-typed components, option-typed tail *)
end
```

Invariants per SortView: `Get.h (Some pf)` is all-`Some` iff `pf`
starts with head `h`, else all-`None`.  `Build.h info comps` is
`Some _` iff every component is `Some`.

**Goal**: q_match accesses `ProofSort.t` *only* via this view.  No
direct `match pf with [] | _ :: _` outside the view module itself.

**New error kinds**: `K_pf_expected_nil`, `K_pf_kind_mismatch
{ expected; got }` in `lib/error.{ml,mli}`.

**New rCheck.ml wrappers** (option→result):
`view_get_pf_nil`, `view_get_pf_comp`, `view_get_pf_log`,
`view_get_pf_res`, `view_get_pf_depres`.  Mirrors the existing
`view_get_pred_sort` pattern.

**New rCheck.ml `extend_*_opt` helpers** — option-typed `var`,
result-typed payloads:

```ocaml
let extend_comp_opt (var : Var.t option)
    (sort : (Sort.sort, Error.kind) result)
    (eff : (Effect.t, Error.kind) result)
    (delta : RCtx.t) : RCtx.t =
  match var, sort, eff with
  | None, _, _ -> delta
  | Some v, Ok s, Ok e -> RCtx.extend_comp v s e delta
  | Some v, _, _ -> RCtx.extend_unknown v delta

(* analogous extend_log_opt, extend_res_opt *)
```

`var = None` arises in q_match (when `Get.comp pf` returns `None`
for the entry's var because the head wasn't `Comp`); `payload =
Error` arises when destructure fails or when the payload itself is a
result-typed input.

**Signature changes** to inner pattern matchers:
- `cpat_match : ... -> (Effect.t, Error.kind) result -> ... cpat -> (Sort.sort, Error.kind) result -> ...`
- `lpat_match : ... -> ... lpat -> (CoreExpr.typed_ce, Error.kind) result -> ...`
- `rpat_match : ... -> Effect.t -> ... rpat -> (CoreExpr.typed_ce, Error.kind) result -> (CoreExpr.typed_ce, Error.kind) result -> ...`

**`cpat_match` CTuple cleanup**: the explicit length-check loop
becomes a single-list walk via `List.combine cps component_sort_results`
(both lists guaranteed equal length by `view_get_record_sorts`'s
contract). The `| _ ->` arm vanishes.

**lpat_match LAuto with Error prop**: `Constraint.atom` requires a
typed_ce; when prop is `Error`, emit `Constraint.top` instead.

**`rpat_match` minimal compatibility**: signature change only.
Internal lift_at-fails stay until C3.

**New `q_match`** dispatches via `RPat.shape` and `ProofSortView.Get.*`:

```ocaml
let rec go pat pf_opt delta =
  match RPat.shape pat with
  | QNil -> ...
  | QCore (cp, rest_pat) ->
    let (var_o, sort_r, eff_r, tail_o) = view_get_pf_comp ~loc pf_opt in
    let* (typed_cp, delta', ce_w) = cpat_match rs delta eff_r cp sort_r in
    let tail' = match var_o with
      | Some v -> Option.map (ProofSort.subst v ce_w) tail_o
      | None   -> tail_o in
    let* (typed_rest, delta'', ct_rest) = go rest_pat tail' delta' in
    return (RPat.mk info (QCore (typed_cp, typed_rest)), delta'', ct_rest)
  (* analogous QLog, QRes, QDepRes *)
```

Both recursions structural; no length checks; no fall-through arm.

**Eliminates**: 1757 (cpat_match CTuple length), 1983 (q_match
length), 1994 (q_match kind), 2050 (q_match DepRes-not-Pred).

### C3 — rpat_match whole-body refactor

Migrate every clause of rpat_match to result-flow.  Drops both the
direct fail sites (1896, 1933 — RCase) and the ~11 hidden
`lift_at`-induced fails in other clauses (RReturn, RTake, RFail,
RLet, RIfTrue, RIfFalse, RUnfold).

**Per-clause pattern**:
1. Extract via View → result-typed components.
2. Derive intermediates via `Result.map` / `Result.bind`.
3. Recurse into sub-patterns with result-typed inputs.
4. Default constraint to `Constraint.top` when structural view fails.
5. Set rinfo's `answer` to first Error from the structural checks.
6. Build typed_rp.

**RFail decision** (from discussion): when the predicate isn't `fail`,
mark the rpat with the error and emit `Constraint.top` (not
`Constraint.bot` — bot is reserved for the case where the structural
check passed and the pattern truly is the unsatisfiable fail).

**New helpers**:
- `view_get_case_ce` — option→result wrapper for `CoreExprView.Get.case`.
- `find_case_branch ~label branches_r` — propagates
  `K_rcase_label_not_in_branches` on miss.
- `mk_rinfo_with_answer pos delta sort eff answer` — variant of
  `mk_rinfo` taking an explicit answer.
- `mk_info_at pos sort` — variant of `mk_info` for synthesized
  intermediate typed_ce's at a known position.
- `result_map2`, `result_map3` if the multi-arg `Result.bind` pattern
  recurs more than 2-3 times.

**Eliminates**: 1896, 1933 (direct), plus ~11 lift_at-fails inside
rpat_match.

### C4 — CIter

Migrate the CIter clause's effect check, sort destructures, leak
check, and pattern shape dispatch.

**Sites migrated**:
- 1125 (effect check) → result-typed gate via `check_pred`.
- 1131, 1133 (lift_at: pred-sort, app-extraction) → `view_get_pred_sort`,
  `view_get_app_sort` chains.
- 1152, 1153 (lift_at: ctor lookups) → result-bind chains.
- 1181 (resource leak) → result-typed gate.
- 1198 (lift_at: get_cvar) → new `view_get_cvar` wrapper.
- 1207, 1211, 1215 (pattern shape) → `RPat.shape` dispatch with
  `view_get_cvar`.

**Pattern-shape collapse** (decided): the existing three error
strings ("a logical pattern" / "a resource pattern" / "an empty
pattern") collapse into a single "a non-core pattern" diagnostic for
the QLog/QRes/QDepRes cases plus a separate "an empty pattern" for
QNil.  Marginal info loss, simpler code.

**Forall on placeholder binder** (decided): when the iter binder
extraction fails (no QCore CVar), drop the forall entirely — emit
`ct'` directly.  Don't introduce a fresh placeholder Var.t.

**Placeholder cascade**: when a sort destructure fails, fall back to
`bool_sort` placeholder.  q_match etc. continue with the placeholder
and may emit cascading diagnostics; user fixes the primary error and
re-runs.  Same trade-off as C2's q_match cascade.

**`mk_rinfo_with_answer` extension**: takes an explicit
`subterm_errors` list (not just `[]`).  Lets CIter aggregate all
its check errors (effect, pred, inner_sort, app, ctor lookups, leak,
pattern shape) onto the rinfo.

**Eliminates**: 1125, 1181, 1207, 1211, 1215 (catalogued) plus
~5 lift_at-fails.

### C5 — RFunDecl resource leak

Migrate the RFunDecl arm of `check_rdecl`:

- 2099 (lift_at: `ProofSort.bind`) → result-flow with `gamma'`
  fallback to the unbound gamma.
- 2110 (resource leak fail) → result-typed leak check.

**Decl-level errors attached to body's outer rinfo's
`subterm_errors`** via a new `prepend_subterm_errors` helper.  No
new info field on `RProg.RFunDecl` — adding info to declarations is
deferred to the elaboration-info-refactor (where it lands alongside
the per-judgement answer-type rethink — see
`doc/errors/elaboration-info-refactor.md`).

**Cascade**: when `gamma'_r = Error`, codomain is elaborated against
the unbound gamma; references to domain-bound vars surface as
"variable not found".  Acceptable per the precedent.

**Eliminates**: 2110 (catalogued) plus 2099 (lift_at).

### C6 — pf_eq

Change `pf_eq`'s return type to
`(Constraint.typed_ct * Error.t list) ElabM.t`.  On success the error
list is `[]`.  On structural mismatch, emit `Constraint.top pos` for
the local contribution and accumulate the error.

**Single call site** (`check_crt_impl` at line 1456) folds the error
list into the synthesizing crt's outer rinfo via
`prepend_subterm_errors`.

**Comp/Comp now reports both sort and effect mismatches** (today
only the first surfaces because the sort fail short-circuits the eff
check).  Strict improvement.

**Length-mismatch handling**: when one list is fully consumed and
the other isn't, emit a single trailing diagnostic per remaining
entry on the longer side.  When the lists are mismatched in the
middle, the kind mismatch cascade applies (substitution chain breaks
for downstream entries; user fixes one error and re-runs).

**DepRes pred-not-Pred**: today's `lift_at`-fail becomes a recorded
error via `view_get_pred_sort`.  Same diagnostic, different reporting
path.

**Eliminates**: 1659, 1662, 1697, 1702.

### C7a — Live `subterm_errors` aggregation

Currently every node constructor (`mk_typed`, `mk` in elaborate.ml;
`mk_rinfo`, `mk_rinfo_err`, `mk_info` in rCheck.ml) sets
`subterm_errors = []`.  The field is populated by a separate
post-pass `Typecheck.annotate_subterm_errors`.

This commit moves the aggregation into the constructors:

```ocaml
let collect_subterm_errors_from_shape shape =
  let collect_one ce =
    let info = CoreExpr.info ce in
    let own = match info#answer with Ok _ -> [] | Error e -> [e] in
    own @ info#subterm_errors in
  (* iterate over shape's typed sub-trees, concat each via collect_one *)
  ...

let mk_typed ctx pos sort eff shape : typed_ce =
  CoreExpr.mk (object
    method loc = pos
    ...
    method subterm_errors = collect_subterm_errors_from_shape shape
  end) shape
```

Same pattern for the rinfo constructors — they aggregate from typed
sub-trees on construction.

**`Typecheck.annotate_subterm_errors` removal**: delete from
`lib/typecheck.{ml,mli}`.  The .mli removal forces any straggler
caller to surface as a compile error.  Update doc references at
`lib/typecheck.mli:34` that mention it as a precondition.

**Why before C7**: C5/C6's `prepend_subterm_errors` and C7's drop of
the boundary `annotate_subterm_errors` calls both assume
subterm_errors is built live.  Without C7a, the post-pass would
clobber prepended cross-cutting errors.

No fail sites eliminated directly; structural prerequisite for C5,
C6, and C7.

### C7 — Defensive guards

Drop the four boundary fail sites in `elab_se`, `elab_se_check`,
`elab_fundecl_body`:

```ocaml
(* Before: *)
match (CoreExpr.info ce)#subterm_errors with
| e :: _ -> ElabM.fail e
| [] -> return ce

(* After: *)
return ce
```

`elab_se`'s return type changes from
`(typed_ce * Sort.sort) ElabM.t` to
`(typed_ce * (Sort.sort, Error.kind) result) ElabM.t` — sort becomes
result-typed since it might be absent.  Callers thread the
result-typed sort through downstream view wrappers (eliminating
their own `lift_at` calls along the way).

**`CoreExpr.sort_of_info` audit**: the existing helper does
`Result.get_ok info#answer` — assertion-fails on Error.  After C7,
more typed_ce's flow through with Error answers.  Audit each call
site:
- Genuine invariants ("answer must be Ok by construction") leave
  as-is.
- Sites that rely on the boundary switch to reading `info#answer`
  directly (result-typed) and threading through view wrappers.

Probably ~30+ call sites in `rCheck.ml`.  Mostly mechanical.

**Driver-side update**: `compileFile.compile_rfile` and
`bin/nanocn_lsp.ml` walk the typed AST for diagnostics rather than
catching monad failures (which no longer exist for user errors).

**Eliminates**: 146, 149, 159, 2069.

### C8 — API change: drop `ElabM.fail`, simplify `'a t`

After C1–C7 land, `ElabM.fail` has zero call sites.  This commit
locks in the win:

```ocaml
(* lib/elabM.{ml,mli} *)

(* Before *)
type 'a t = state -> ('a * state, Error.t) result
val fail    : Error.t -> 'a t
val lift    : ('a, Error.t) result -> 'a t
val lift_at : SourcePos.t -> ('a, Error.kind) result -> 'a t
val run     : Var.supply -> 'a t -> ('a * Var.supply, Error.t) result
val run_full : Var.supply -> 'a t ->
  ('a * Var.supply * Warning.t list, Error.t) result

(* After *)
type 'a t = state -> 'a * state
(* fail, lift, lift_at all removed *)
val run      : Var.supply -> 'a t -> 'a * Var.supply
val run_full : Var.supply -> 'a t -> 'a * Var.supply * Warning.t list
```

`return`, `let*`, `fresh`, `mk_var`, `record_warning`, `sequence`
unchanged.  The `Test` test that exercises `fail` is removed.

**Compile-error driven cleanup**: removing `fail`/`lift`/`lift_at`
surfaces every remaining caller as a type error.  That's the *point*
of doing this last — it's the forcing function that catches
stragglers from earlier categories.

**`run`'s Error case removal in callers**: anywhere
`match ElabM.run … with Ok x -> … | Error e -> …` exists, the Error
arm becomes unreachable.  Mechanical fix → strictly cleaner.

**Internal-bug channel**: `Util.raise_invariant` (and
`invariant_at` via that) raises an exception caught at the driver
boundary.  Existing sites unaffected.  Verify no leftover
`ElabM.fail` is being used as a "compiler bug" channel rather than a
user-error channel.

**Tests**: any test that checked monadic-fail semantics needs
updating to check `info#answer` Error semantics instead.

## Cross-cutting design decisions

These were settled during planning and apply across multiple
commits.

### Views are option-typed; consumers wrap to result-typed

`SortView`, `CoreExprView`, and the new `ProofSortView` all use
`'a t = 'a option`.  Local wrappers in `rCheck.ml`
(`view_get_pred_sort`, `view_get_pf_comp`, etc.) do the
`Option.to_result` conversion with the call-site's appropriate
error kind.

This keeps the View modules error-vocabulary-free and lets each
clause attach its own contextual `K_construct_sort_mismatch` /
`K_pf_kind_mismatch` etc.

### Cascade is acceptable

When dispatching pattern destructures fail (e.g., qbase shape
mismatch in q_match, predicate shape mismatch in rpat_match), the
View returns all-None and downstream clauses see Error inputs.
This produces typed AST with Error annotations through the affected
subtree — cascade.

User fixes the primary error and re-runs.  Bounded blast radius;
acceptable per repeated discussion.

### Cross-cutting errors attach via `prepend_subterm_errors`

For errors that don't correspond to a specific node's
`info#answer` (e.g., RFunDecl resource leak, pf_eq structural
mismatches, CIter's effect/leak checks), use the helper
`prepend_subterm_errors errs ce` to add them to the outer node's
`subterm_errors` list.

Requires C7a (live `subterm_errors`) to land first.

### Decl-level info deferred

Adding an `info` field to `RProg.FunDecl` / `RProg.RFunDecl` is the
right long-term shape but is out of scope here.  Belongs with the
elaboration-info-refactor (see
`doc/errors/elaboration-info-refactor.md`) since both involve
rethinking what the per-node "answer" carries.

## File index

Files materially touched across all commits:

- `lib/rPat.{ml,mli}` — C1
- `lib/parser.mly`, `lib/resolve.ml`, `lib/hoverIndex.ml` — C1
- `lib/proofSortView.{ml,mli}` — C2 (new)
- `lib/error.{ml,mli}` — C2 (new error kinds), possibly C7 / C8
- `lib/rCheck.{ml,mli}` — C2, C3, C4, C5, C6, C7a (rinfo constructors), C7
- `lib/elaborate.{ml,mli}` — C7a (typed_info constructors)
- `lib/typecheck.{ml,mli}` — C7a (remove `annotate_subterm_errors`)
- `lib/coreExpr.{ml,mli}` — C7 (audit `sort_of_info` callers)
- `lib/elabM.{ml,mli}` — C8
- `lib/compileFile.ml`, `bin/nanocn_lsp.ml` — C7, C8 (driver-side
  AST walk)
- Tests across the above modules.

## Related documents

- `doc/errors/multiple-errors.md` — original multi-error design.
- `doc/errors/failure-erasure.md` — catalogue of the 20 cataloged
  surviving fail sites this plan eliminates.
- `doc/errors/fine-grained-errors-status.md` — companion status doc.
- `doc/errors/elaboration-info-refactor.md` — separate refactor of
  what `info#answer` carries per judgement.  Sequenced after this
  plan completes.
