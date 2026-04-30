# Multi-error typechecking pipeline rollout

## Context

NanoCN's typecheckers currently bail at the first error: `synth`/`check` return `(_, Error.t) result`, and a single error halts elaboration of the enclosing function. This makes the LSP nearly useless on a function with even one error — the user sees one diagnostic and a totally untyped function body. The design doc `doc/errors/multiple-errors.md` (now the source of truth) prescribes a refactor following Zhao et al.'s "Total Type Error Localization and Recovery with Holes": typecheck/elaborate produces a *partially-typed tree* whose nodes carry either a sort or an error, and continues past errors so all problems in a function are surfaced together.

The user has dictated this ordering, overriding the design doc's claim that refinement is easier than elaboration:

  **Phase A — `lib/typecheck.ml`** (628 lines, `Result` monad, no fresh vars). Establish all shared infrastructure here.
  **Phase B — `lib/elaborate.ml`** (710 lines, `ElabM` monad with `Var.supply`, contains pattern compilation). Apply lessons from A.
  **Phase C — `lib/rCheck.ml`** (1671 lines, `ElabM`, produces SMT constraint trees). Apply lessons from A and B.

Each phase is its own commit (or a small series), reviewed against the design doc before the next phase begins.

## Cross-cutting design decisions

- **D1.** `typed_info` in `lib/coreExpr.mli` migrates **in lockstep**: `< loc; ctx; sort : Sort.sort; eff : Effect.t >` → `< loc; ctx; answer : (Sort.sort, Error.t) result; eff : Effect.t >`. `eff` is preserved as a non-result field (it's the ambient effect, not derived from sort). During Phase A, every constructor site in `elaborate.ml` and `rCheck.ml` wraps the existing sort in `Ok` (`answer = Ok s`); reads of `info#answer` inside those files use `Result.get_ok` (a partial extractor — *not* a branch on the result; safe because Phase A guarantees `answer` is always `Ok`).
- **D2.** `typecheck.ml`'s `synth`/`check` collapse to identity — no `result`, no monad — because they allocate no fresh `Var`. Errors live on `info#answer` of every offending node. **`check` takes `(Sort.sort, Error.kind) result`** (not `Sort.sort option`) for its expected-sort argument: when `synth` invokes `check` on an unsynthesizable subterm, it passes `Error (K_cannot_synthesize ...)` so the *reason* is preserved at the call site instead of erased to `None`.
- **D3.** Linearization combinator is named `&&&` (per user preference). Defined in typecheck.ml as `let (&&&) gate x = match gate with Ok () -> x | Error e -> Error e`. Companion `let check_pred b err = if b then Ok () else Error err`.
- **D4.** New error kind `K_unknown_var_type of { var : Var.t }` — distinct from `K_unbound_var`, fired when a variable is in scope but its binding is `Unknown` (the prior let-RHS errored). Plus `K_cannot_synthesize of { construct : string }` for D2's `Error` argument to `check` from unsynthesizable synth cases.
- **D5.** `SortView` lives at `lib/sortView.ml/.mli` as a standalone module (not a submodule of `Sort`). **The View is polymorphic over the info parameter `'info`** — `Get` consumes `'info Sort.t t`, `Build` takes an `'info` and produces `'info Sort.t t`. This matches the design doc's signature literally and lets the same View serve both `Sort.sort` (a particular instantiation) and the typed-info-bearing sorts that elaboration produces. The future `RPatView` is a parallel module.
- **D6.** Phase C's constraint-tree-on-error strategy is **deferred** to the start of Phase C. We'll decide once we have lessons from A and B.
- **D7.** **No code in the typecheckers branches on a `result`** (`match r with Ok ... | Error ...`). That defeats the View pattern. Results are threaded through `&&&`, `SortView.Get`/`Build`, `Result.map`, and View functions; they are unwrapped only at the boundaries (`info#answer` writes, the final `mk` call). The `Result.get_ok` shim in elaborate.ml/rCheck.ml during Phase A is a *partial extractor*, not a branch — those sites get rewritten in View style during Phases B/C.

## Phase A — `lib/typecheck.ml`

### A.0 Ott alignment check (CLAUDE.md rule 0)
Before any code change, confirm `doc/syntax.ott` already requires Case branches to cover the full ctor set of the scrutinee's datatype. If yes (expected), no Ott change. If the rule is missing, update Ott **first**, separate commit.

### A.1 Shared infrastructure (no behavior change yet)
Build in this order; each step compiles cleanly before the next.

1. **`lib/util.mli` + `lib/util.ml`** — add `val result_list : ('a, 'e) result list -> ('a list, 'e) result` (returns first `Error`). Plus `Util.Test` with QCheck round-trip property.
2. **`lib/error.mli` + `lib/error.ml` + error printer** — add `K_unknown_var_type of { var : Var.t }`, smart constructor `unknown_var_type ~loc ~var`, render arm in the printer per `doc/errors/type-errors.md` style.
3. **`lib/context.mli` + `lib/context.ml`**:
   - Add `| Unknown of Var.t` to `binding`.
   - Add `val extend_unknown : Var.t -> t -> t`.
   - **Replace** `val lookup : Var.t -> t -> (Sort.sort * Effect.t) option` with `val lookup : Var.t -> t -> ((Sort.sort * Effect.t), Error.kind) result` (returns `K_unbound_var x` for missing, `K_unknown_var_type {var=x}` for `Unknown`).
   - Update `Context.Test` generator to produce `Unknown` bindings and assert the new lookup behavior.
   - **Audit**: `grep -rn "Context.lookup" lib/` — every caller updates. Mostly typecheck.ml but elaborate.ml and rCheck.ml will compile under the new sig too (they consume option results — small mechanical fix).
4. **`lib/sortView.mli`** — `.mli` first per CLAUDE.md rule 3, **polymorphic in the info parameter `'info`** (D5):
   ```ocaml
   type 'a t = ('a, Error.kind) result
   module Get : sig
     val int    : 'info Sort.t t -> unit t
     val bool   : 'info Sort.t t -> unit t
     val ptr    : 'info Sort.t t -> 'info Sort.t t
     val pred   : construct:string -> 'info Sort.t t -> 'info Sort.t t
     val record : int -> 'info Sort.t t -> 'info Sort.t t list
     val app    : construct:string -> 'info Sort.t t -> Dsort.t t * 'info Sort.t t list
     val tvar   : 'info Sort.t t -> Tvar.t t
   end
   module Build : sig
     val int    : 'info -> unit t -> 'info Sort.t t
     val bool   : 'info -> unit t -> 'info Sort.t t
     val ptr    : 'info -> 'info Sort.t t -> 'info Sort.t t
     val pred   : 'info -> 'info Sort.t t -> 'info Sort.t t
     val record : 'info -> int -> 'info Sort.t t list -> 'info Sort.t t
     val app    : 'info -> Dsort.t t -> 'info Sort.t t list -> 'info Sort.t t
     val tvar   : 'info -> Tvar.t t -> 'info Sort.t t
   end
   module Test : sig val test : QCheck.Test.t list end
   ```
   Build's first arg is the full `'info` value (not a `SourcePos.t`); the caller supplies whatever info object their context demands. `Sort.sort = < loc : SourcePos.t > Sort.t` is one instantiation; typed sub-sorts (in elaborate/rCheck) are another.
5. **`lib/sortView.ml`** — implement per the design doc's pseudo-code (`doc/errors/multiple-errors.md:198-270`). `Get.record n` always returns a list of length `n`, padding with `Error K_construct_sort_mismatch` on shape/arity mismatch. `Build.record n` succeeds iff length matches and all sub-results are `Ok`. QCheck properties (per `doc/errors/multiple-errors.md:186-194`): round-trip, failure propagation, `Get` on wrong shape returns `Error`.
6. **Old `SortGet`** (`lib/sortGet.ml/.mli`) — keep alive in this commit; delete once typecheck.ml stops using it (within Phase A). Elaborate/rCheck still depend on it at this stage; they migrate in Phases B/C.

### A.2 `typed_info` migration (D1, lockstep)
1. Update `lib/coreExpr.mli:46` typed_info shape to add `answer : (Sort.sort, Error.t) result` and remove `sort`.
2. Update `lib/typecheck.ml:17` typed_info accordingly.
3. **Patch elaborate.ml + rCheck.ml in this commit**: every `method sort = s` becomes `method answer = Ok s`; every read of `info#sort` becomes `Result.get_ok info#answer` (commented `(* TODO Phase B/C *)`). These shims are safe because Phases B/C haven't touched the elaborator yet — every answer is Ok.
4. Update `mk` (`typecheck.ml:20-26`) signature: third arg becomes `(Sort.sort, Error.t) result`.
5. Update `lift_sort` (`typecheck.ml:39-46`) filler: `answer = Ok s` so Hover/SortDiff stay correct.

### A.3 Linearization helpers (in typecheck.ml, near the top)
```ocaml
let check_pred (b : bool) (err : Error.t) : (unit, Error.t) result =
  if b then Ok () else Error err
let ( &&& ) (gate : (unit, Error.t) result) (x : ('a, Error.t) result) : ('a, Error.t) result =
  match gate with Ok () -> x | Error e -> Error e
```

### A.4 Per-clause migration of `synth` (typecheck.ml:81-160)
Each clause now ends with `mk ctx pos answer eff0 shape` where `answer : (Sort.sort, Error.t) result`. Recurse into all subterms unconditionally; never short-circuit. Per D7, no `match … with Ok | Error` inside clauses — results are threaded through `&&&` / View / `Result.map`.

| Clause | Recipe |
| --- | --- |
| Var (:84) | `answer = Context.lookup x ctx \|> Result.map fst \|> Error.at ~loc:pos`, gated through effect-check via `&&&`. |
| IntLit, BoolLit (:96-100) | `answer = Ok (mk_sort Int / Bool)`. |
| Eq (:102) | Always recurse on ce1 (synth) and ce2 (check). `is_spec_type` becomes a gate. Pass `(info ce1')#answer` (a result) directly to the inner `check`. |
| And, Not (:114-123) | Recurse unconditionally; `answer = Ok bool_sort`. |
| App (:125) | Effect-check is a gate; arg checked unconditionally. |
| Call (:141) | `Sig.lookup_fun` failure goes into `answer`; arg checked against `Ok arg_sort` if lookup ok, else the lookup's `Error`. |
| Annot (:153) | Pass `Ok s` to `check`. |
| Unsynthesizable cases (:157-160) | Recurse with `check ... (Error (K_cannot_synthesize {construct})) ...` so subterms are still elaborated *and* the reason for the missing expected type is preserved at the call site (D2). Node's own `answer = Error (cannot_synthesize ...)`. |

### A.5 Per-clause migration of `check` (typecheck.ml:163-295)
`check` now takes `(Sort.sort, Error.kind) result` for its expected-sort argument (D2). An `Error` arg means the caller couldn't synthesize a type; the reason rides along.

Per D7, no clause `match`es on the expected-sort result. Instead, the result feeds directly into `SortView.Get.*`, which propagates errors through to sub-positions; the same View functions accept and return results, threading them through.

| Clause | Recipe |
| --- | --- |
| Return / Fail (:166-180) | `Effect.sub Effect.Spec eff0` → gate; `SortView.Get.pred ~construct:"return" sort` (where `sort` is the result-typed expected) feeds the inner check directly. |
| Take (:181) | Synth e1 unconditionally; let `bound = SortView.Get.pred (info e1')#answer`; bind `x` via `Context.extend_or_unknown x bound` (single helper that does the right thing on Ok/Error without callers branching). Recurse on ce2 with the new ctx. |
| Let (:195) | Same `extend_or_unknown` against `(info ce1')#answer`. |
| LetTuple (:204) | `SortView.Get.record (List.length xs) sort` returns exactly-n result-sorts; pair `(x, s_result)`, bind via `extend_or_unknown`. Arity-check vanishes. |
| Tuple (:230) | `SortView.Get.record n sort` returns exactly-n results; pair `(e, s_result)` and check `e` against `s_result` directly. Branch-free. |
| Inject (:241) | Per design doc :362-370. `let (d, args) = SortView.Get.app sort`; `let tp = Error.at ~loc:pos (CtorLookup.lookup sig_ d l args)`; recurse `check sig_ ctx e_inner tp eff0'`. |
| Case (:248) | See A.6. |
| Iter (:256) | `Effect.sub Effect.Impure eff0` → gate; recurse on body with the iter-sort result threaded directly. |
| If (:269) | Recurse on all three; pass `sort` (result) to then/else. |
| Annot (:277) | If mismatch, `answer = Error annotation_disagrees`; still elaborate inner. |
| Hole (:285) | `answer = sort` (the expected-sort result, passed straight through). |
| fallback (:288) | Synth then compare; mismatch → `Error sort_mismatch`. |

To support the table without forcing callers to branch, add a small helper in `lib/context.ml`:
```ocaml
val extend_or_unknown : Var.t -> (Sort.sort, _) result -> Effect.t -> t -> t
(* Ok s  → Context.extend x s eff;  Error _ → Context.extend_unknown x. *)
```
This is the only place the result is *consumed* by case-analysis, hidden inside Context. Per D7, callers never see the `match`.

### A.6 Case completeness fix (typecheck.ml:248-336)
Two new pieces:

1. **`CtorLookup.lookup_all`** in `lib/ctorLookup.mli`/`.ml`:
   ```ocaml
   val lookup_all :
     'a Sig.t -> Dsort.t -> Sort.sort list ->
     ((Label.t * Sort.sort) list, Error.kind) result
   ```
   Returns the full ctor set with type args substituted. Reuses `CtorLookup.lookup` internals.

2. **`merge_branches`** local to typecheck.ml. The declared-ctor argument is *itself a result* so failures from `lookup_all` propagate without the caller branching:
   ```ocaml
   type merged_branch =
     | M_present    of Label.t * Var.t * CoreExpr.ce * Sort.sort
     | M_missing    of Label.t * Sort.sort                 (* fill body with Hole *)
     | M_redundant  of Label.t * Var.t * CoreExpr.ce * Sort.sort
       (* second+ occurrence of label; sort is the known expected type from the
          declaration so we still typecheck the body and avoid spurious errors *)
     | M_unknown_label of Label.t * Var.t * CoreExpr.ce
   val merge_branches :
     (Label.t * Var.t * CoreExpr.ce * 'b) list ->
     ((Label.t * Sort.sort) list, Error.kind) result ->
     merged_branch list * Error.t list
   ```
   - When the declared-ctor list is `Error _` (e.g., the scrutinee's dsort lookup failed), `merge_branches` returns each given branch as `M_unknown_label` (we have no type to check it against), and the single error from the lookup is the only entry in the error list.
   - When the declared-ctor list is `Ok lts`, each missing/redundant/unknown ctor → its own `Error.t` (new kinds: `K_missing_ctor`, `K_redundant_ctor`; existing `K_ctor_not_in_decl` covers unknown).
   - **`M_redundant` carries the type** (per user feedback): a duplicate branch is still typechecked against the correct expected ctor type, so its body doesn't cascade spurious errors. The redundancy itself is the only error reported for that branch.
   - For `M_missing`, the produced typed branch's body is `CoreExpr.Hole "missing-case-<label>"`; its `answer` carries the missing-ctor error.
   - The first error of the list is *also* surfaced on the enclosing Case node so root-level scanners see it.

### A.7 Cleanups
- Delete `check_list` (typecheck.ml:297-312) — no longer needed; the `K_internal_invariant` fallback there is genuinely unreachable post-View (CLAUDE.md rule 9).
- Delete `lib/sortGet.ml/.mli` once unreferenced (after typecheck.ml + elaborate/rCheck shims migrate, which happens at the end of Phase A for typecheck and during Phases B/C for the others — keep `sortGet` alive across Phase A).

### A.8 `lib/typecheck.mli` updates
```ocaml
val synth : _ Sig.t -> Context.t -> Effect.t -> CoreExpr.ce -> typed_ce
val check : _ Sig.t -> Context.t -> CoreExpr.ce -> (Sort.sort, Error.kind) result -> Effect.t -> typed_ce
```
`check_decl` / `check_prog` (driver-level) keep `(_, Error.t) result` shape **for now** (preserve `compileFile.ml` callers). Add `val collect_errors : typed_ce -> Error.t list` and have the driver call it; on non-empty list, return `Error first_err` for backward compat, `Ok prog` otherwise. A follow-up commit can change the driver to return `(typed_ce_prog * Error.t list)` once consumers are ready.

### A.9 QCheck tests (CLAUDE.md rule 3)
- `Util.Test`: `result_list` round-trip + first-error properties.
- `SortView.Test`: round-trip, failure-propagation, Get-on-wrong-shape (per design doc invariants).
- `Context.Test`: extended generator + `lookup` behavior on `Unknown`.
- New `Typecheck.Test`: synthetic Case with missing/redundant ctors → expected `merge_branches` output.

### A.10 Phase A regression risk
| Severity | Risk | Mitigation |
| --- | --- | --- |
| Critical | `coreExpr.mli typed_info` change forces elaborate.ml + rCheck.ml recompile | The `Ok s` shim (D1); spot-check `dune build` after the lockstep change before clause migration begins. |
| High | `Context.lookup` API change is invasive | `grep -rn "Context.lookup" lib/` audit; mechanical migration. |
| Medium | `lift_sort` filler | Filler must be `Ok s` so Hover/SortDiff stay correct. |
| Medium | `merge_branches` produces synthetic branches that downstream rCheck might choke on | Add a "is well-formed branch" predicate that rCheck consumers can use. |
| Low | `K_internal_invariant` removal in `check_list` | Branch is genuinely unreachable post-View. |

### A.11 Phase A verification
- `dune build` after each substep.
- `dune runtest` — all existing QCheck + parser-message tests pass; new `SortView.Test`, `Util.Test`, `Typecheck.Test` cases added.
- Examples that exercise the migrated clauses:
  - `examples/sum_types.cn`, `examples/pattern_match.cn` (Inject + Case + completeness fix)
  - `examples/spec_list.cn`, `examples/spec_tree.cn` (Take/Return/Pred)
  - `examples/iter.cn`, `examples/listsum.cn` (Iter)
  - `examples/let_tuple.cn`, `examples/tuples.cn` (LetTuple — branch-free path)
  - `examples/factorial.cn`, `examples/fibonacci.cn` (recursion + If)
- New fixtures under `examples/errors/typing/` (create dir): each file deliberately contains ≥3 errors per function; assert via `test/resilient/test_resilient.ml` harness that all are reported. Golden expected outputs.
- Manual LSP smoke test: open one of the multi-error fixtures via `bin/nanocn_lsp.ml`, confirm multiple diagnostics appear and Hover still works (via `Result.get_ok` shim).
- `git diff doc/syntax.ott` — empty.

### A.12 History snapshot (CLAUDE.md rule 0.5)
Copy this plan to `doc/history/multi-error-typecheck-phase-a.md` (or similar) at the start of Phase A so new sessions can read it.

---

## Phase B — `lib/elaborate.ml`

**Do not start until Phase A is merged.** The point of Phase A's `Result.get_ok` shim is to keep elaborate.ml building; Phase B is when we actually migrate it.

### B.1 ElabM monad becomes non-failing (`lib/elabM.mli`/`.ml`)
```ocaml
type 'a t = Var.supply -> 'a * Var.supply
val return : 'a -> 'a t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val fresh : SourcePos.t -> Var.t t
val mk_var : ...
val sequence : 'a t list -> 'a list t
val run : Var.supply -> 'a t -> 'a * Var.supply
(* removed: fail, lift, lift_at, from_supply *)
```

~57 `ElabM.fail` callsites across the codebase. Categorize each by the rubric below; this is the bulk of the work.

| Failure category | Migration |
| --- | --- |
| Unbound/unknown name (elaborate.ml:278, 437; rCheck deferred to C) | Emit typed node with `answer = Error (...)` plus `CoreExpr.Hole` placeholder if there's no sensible elaboration target. |
| Cannot synthesize (elaborate.ml:347) | Emit `Hole` + attach error; the design doc's "General design issue: Elaboration" prescribes this. |
| Effect mismatch (elaborate.ml:273, 320, 332, 455) | Linearize via `&&&`; continue elaborating. |
| Sort/shape mismatch from view (elaborate.ml:415, 497) | Replace with View.Get/Build pattern from Phase A. |
| Coverage non-exhaustive (elaborate.ml:548, in `coverage_check`) | See B.3. |
| Internal invariant (elaborate.ml:40 etc.) | Keep — genuinely unreachable (CLAUDE.md rule 9). |

### B.2 Drop `Result.get_ok` shims in elaborate.ml
Per D7, the migration is **not** "every `Result.get_ok info#answer` site becomes a branch on the result" — that defeats the View pattern. Instead, each shim site is rewritten in View style: feed the `info#answer` result directly into a `SortView.Get.*` (or the analogous `PatView` / `CoreExprView`) call, which threads the result through; eventually it lands on a `mk` answer field or in a `&&&` gate without anyone pattern-matching on it.

The mechanical pass:
1. `grep -n "Result.get_ok" lib/elaborate.ml` — list every site.
2. For each, identify the View call (existing or new) that should consume the result.
3. Where the View doesn't yet cover the destructure, extend the View.

### B.3 Pattern-match coverage (`coverage_check`, elaborate.ml:532-700)
This is Phase B's hardest part. Today, malformed pattern matrices `ElabM.fail`. New behavior:

- **Non-exhaustive matrix**: produce a synthesized `Case` whose missing branches contain `CoreExpr.Hole "missing-coverage"`; attach `K_non_exhaustive` to the case node's `answer`. Continue.
- **Cov_var when scrutinee sort can't be looked up**: bind via `Context.extend_unknown`; continue.
- **Cov_con when scrutinee sort isn't `Sort.App`**: emit a Hole at the case position with `K_scrutinee_not_data` on its answer; do NOT recurse into branches (would cascade nonsense).
- **Cov_con when leading ctor isn't in the declaration's ctor set**: keep the row in the matrix, mark its body's `answer` with `K_ctor_not_in_decl`, skip its contribution to the partition.

### B.4 PatView (optional)
`grep "Pat.shape" lib/elaborate.ml` first. If ≥5 sites, build `lib/patView.ml/.mli` mirroring SortView. If fewer, skip — the gain is marginal.

### B.5 Surface-language Holes
Confirm `lib/parseResilient.ml` already emits `SurfExpr.Hole "<reason>"` on incomplete syntax. (`SurfExpr.Hole` exists per `lib/surfExpr.mli:31`.) If yes, no parser change. If not, add it as a separate sub-task.

### B.6 Elaborate.synth/check signatures
```ocaml
val synth : _ Sig.t -> Context.t -> Effect.t -> SurfExpr.se -> typed_ce ElabM.t
val check : _ Sig.t -> Context.t -> SurfExpr.se -> (Sort.sort, Error.kind) result -> Effect.t -> typed_ce ElabM.t
```
(Sort lives on `info#answer` — drop the trailing `* Sort.sort`.) Mirrors Phase A's D2: `check`'s expected-sort argument is a result, not an option. ElabM is still needed because coverage allocates fresh Vars.

### B.7 Phase B regression risk
| Severity | Risk | Mitigation |
| --- | --- | --- |
| Critical | `coverage_check` Hole-emission wrong → rCheck typechecks nonsense | Golden tests in `examples/errors/typing/` for non-exhaustive matches. |
| High | Each `ElabM.fail` migration is a "where to attach this error" decision | Code-review every site; default to attaching to the smallest enclosing typed node. |
| Medium | `ElabM.run` callers in `compileFile.ml`, `typecheck.ml:494-542` | New return type — mechanical fix once monad changes. |

### B.8 Phase B verification
- All Phase A examples still elaborate identically when error-free.
- `examples/sum_types.cn` and `examples/pattern_match.cn` exercise coverage.
- New `examples/errors/typing/multiple_errors_per_fun.cn` — a function with ≥3 errors — assert ≥3 reported.
- `dune runtest` green.

---

## Phase C — `lib/rCheck.ml`

**Do not start until Phase B is merged.** Constraint-tree handling decision (D6) gets resolved at the start of this phase.

### C.0 Resolve D6 (constraint-tree-on-error)
Choose between **skip-SMT-on-error** (mark `Constraint.typed_ct` as a sentinel "skipped" if any error fired in the function) vs. **hole-aware constraints** (add `Constraint.Hole` variant; SMT driver treats hole regions as `True` for soundness-of-error-reporting, NOT verification). Recommend skip-SMT for the first cut. Document in `doc/history/multi-error-rcheck-phase-c.md` once chosen.

### C.1 RCtx changes (`lib/rCtx.mli`/`.ml`)
- Add `| Unknown of { var : Var.t }` to `entry`.
- Add `val extend_unknown : Var.t -> t -> t`.
- **Replace** `lookup_comp : Var.t -> t -> (Sort.sort * Effect.t) option` with `... -> ((Sort.sort * Effect.t), Error.kind) result`.
- **Replace** `lookup_log : Var.t -> t -> CoreExpr.typed_ce option` with `... -> (CoreExpr.typed_ce, Error.kind) result`.
- `use_resource`: extend with new failure mode `K_unknown_resource of { var : Var.t }` for `Unknown` entries. **Crucially**: `Unknown` entries report the error but do NOT consume a usage flag (otherwise we mis-track linearity).
- `erase` (`rCtx.mli:33`): convert `Unknown` to `Context.Unknown`.
- `merge`/`merge_n`/`lattice_merge`: clauses for `Unknown` — two `Unknown`s with same var merge to `Unknown`; `Unknown` × `Comp` mismatches as `Mf_entry_kind_mismatch`.

### C.2 RPatView (`lib/rPatView.ml/.mli`)
Parallel to SortView. Get/Build for `cvar`, `ctup`, `lvar`, `rvar`, `rpair`, `rcase`. Keep `lib/rPatGet.ml` alive during transition; migrate sites individually; delete once unreferenced.

### C.3 ElabM.fail migration in rCheck.ml
~40 sites. Categories analogous to Phase B:
- "Cannot synthesize proof sort" (rCheck.ml:530-536) → emit `RefinedExpr.CHole`, attach error.
- "Spine tag mismatch" (rCheck.ml:1218, 1228, 1232, 1259) → emit `LHole`/`RHole` placeholder, attach error.
- "Resource leak" (rCheck.ml:862) → keep error, route into constraint tree as non-fatal annotation (or skip — depends on D6).
- "Non-exhaustive case_branches" (rCheck.ml:1123) → analogous to Phase A's `merge_branches`; build `merge_rbranches` (sharing `CtorLookup.lookup_all`).
- Internal invariants (rCheck.ml:25, 27 etc.) → keep.

### C.4 rCheck Case audit
Two case-checking entry points: `rCheck.ml:889` (refined `CCase`) and `rCheck.ml:1104` (`check_case_branches`). Both need merge-branches treatment, with refined patterns' resource-pattern structure preserved.

### C.5 Phase C regression risk
| Severity | Risk | Mitigation |
| --- | --- | --- |
| Critical | Constraint-tree corruption → SMT confusion | D6 decision (skip-SMT recommended for first cut). |
| Critical | RCtx linearity mistracking with `Unknown` | `Unknown` reports error but doesn't consume usage. |
| High | `ElabM.lift_at` callsites (dozens) — each is a sort/pattern view that stays in result land | Hand-migrate; each is a small, local change. |
| Medium | Doc's "easier than elaboration" prediction may not hold | Per CLAUDE.md rule 0/3, consult user if rCheck redesign exceeds expected scope. |

### C.6 Phase C verification
- All `.rcn` examples (`incr.rcn`, `listlength.rcn`, `pat_*.rcn`, `refined_*.rcn`, `simple_refined.rcn` — 12 files) produce identical `*-constraints.smt` output. Diff as a regression suite.
- New `examples/errors/refined/` with multi-error refined functions.
- Constraint tests in `test/test_main.ml` green.

---

## Critical files to modify

**Phase A**
- `lib/util.mli` + `lib/util.ml` (new `result_list`)
- `lib/error.mli` + `lib/error.ml` + `lib/errorRender.ml` (new `K_unknown_var_type` + others)
- `lib/context.mli` + `lib/context.ml` (`Unknown` binding, lookup → result)
- `lib/sortView.mli` + `lib/sortView.ml` (new module)
- `lib/coreExpr.mli` (typed_info → answer)
- `lib/typecheck.mli` + `lib/typecheck.ml` (per-clause migration, Case fix, drop result wrapper)
- `lib/ctorLookup.mli` + `lib/ctorLookup.ml` (`lookup_all`)
- `lib/elaborate.ml`, `lib/rCheck.ml` (Ok-shim only — minimal)

**Phase B**
- `lib/elabM.mli` + `lib/elabM.ml` (non-failing monad)
- `lib/elaborate.mli` + `lib/elaborate.ml` (full migration, `coverage_check` Hole emission)
- `lib/parseResilient.ml` (only if surface Holes aren't already emitted)
- Possibly `lib/patView.ml/.mli` (only if needed)

**Phase C**
- `lib/rCtx.mli` + `lib/rCtx.ml` (`Unknown` entry, lookups → result)
- `lib/rPatView.mli` + `lib/rPatView.ml` (new module)
- `lib/rCheck.mli` + `lib/rCheck.ml` (full migration, two case-checking entry points)
- `lib/constraint.mli` (only if D6 chooses hole-aware)

## Functions / utilities to reuse

- `Sort.mk : 'b -> ('b t, 'b) sortF -> 'b t` (`lib/sort.mli:23`) — use inside `SortView.Build`.
- `Sort.shape : 'b t -> ('b t, 'b) sortF` (`lib/sort.mli:25`) — use inside `SortView.Get`.
- `Error.at`, `Error.structured`, `Error.construct_sort_mismatch` (`lib/error.ml`) — preserve patterns for new view errors.
- `CoreExpr.Hole of string` (`lib/coreExpr.mli:9-29`) — already present; reuse for Phase A `M_missing` and Phase B coverage.
- `RefinedExpr.{CHole, LHole, RHole}` (`lib/refinedExpr.mli`) — Phase C placeholders.
- `Context.extend`, `Context.extend_list`, `Context.extend_tvar` (`lib/context.mli`) — pair `extend_unknown` with these.
- `bin/nanocn_lsp.ml` — already exposes diagnostics; the multi-error refactor strengthens it without changing its API.

## Verification (full pipeline)

After each phase:
1. `dune build` clean.
2. `dune runtest` green — all existing tests + new QCheck tests.
3. Hand-craft 2-3 multi-error fixtures per phase under `examples/errors/typing/` (Phase A/B) and `examples/errors/refined/` (Phase C); assert via `test/resilient/test_resilient.ml` that all errors per function are reported.
4. Re-run all `.cn` examples in `examples/` (and `.rcn` examples for Phase C); compare to a baseline of pre-refactor expected output. Constraint files (`*-constraints.smt`) must match byte-for-byte after Phase C.
5. Manual LSP smoke test: open a multi-error fixture in an LSP-capable editor; confirm multiple diagnostics appear simultaneously and Hover/Goto-Def still work.
6. `git diff doc/syntax.ott` — empty (no Ott changes expected; if any, those landed in a separate commit per CLAUDE.md rule 0).
7. Each phase's design captured in `doc/history/` per CLAUDE.md rule 0.5 before implementation begins.
