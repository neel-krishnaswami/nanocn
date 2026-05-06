# Plan: Implementation refresh for the new resource-term and pattern-matching syntax

## Context

`doc/syntax.ott` (commit `60f943b`) now reflects the new resource-term and
pattern-matching design from `doc/extended-resource-terms.md`:

- **`rpf` grammar** went from 6 productions (Var, MakeRet, MakeTake, Annot,
  Unfold, Paren) to 11 (Var, Annot, Return, Take, Fail, Let, Case, IfTrue,
  IfFalse, Unfold, AnnotStrip). Synth is restricted to `Var` and `Annot`;
  every other form is checking-only.
- **`rpat` `case`** form changed to `case [ lpat ] L cpat ; rpat`.
- **`q` (refined patterns)** now wraps `rpb` (resource pattern bases =
  `do cpat = rpat | rpat`) inside its `Res` constructor; `QDepRes` is gone
  as a top-level qbase.
- **`crt` let-family** binders take patterns: `let log lpat = lpf`, `let res
  rpat = rpf`, `let core[lpat] cpat = ce`. The single-variable `let core[w] x`
  form is gone; `LetCoreTup` is gone (subsumed by `LetCore` with tuple `cpat`).
- **Four new pattern-matching judgements** replace the old monolithic
  `rpat_match`: `cpat_match` (sort-driven), `lpat_match` (proposition-driven),
  `rpat_match` (standalone, predicate-driven), `q_match` (refined-pattern,
  proof-sort-driven, dispatches to the other three).
- **`rpf_check`** has 10 rules + sub: return, take, fail, let, case, iftrue,
  iffalse, unfold, annot, sub.
- **`rcrt_check` let-family** rules updated to use the four new pattern
  judgements with explicit context-discharge (`len Δ0 = len Δ3`, `zero Δ''`,
  `close Δ'' (Ct1 ∧ Ct2)`).

This plan covers the implementation slice that propagates these spec
changes into `lib/`, the parser, the tree-sitter grammar, examples, and
baselines, while keeping the multi-error attach-and-continue invariants
that Phases A/B/C of `doc/history/multi-error-typechecking-rollout.md`
established.

## Architectural decisions

### 1. View modules are option-typed, not result-typed

Today `SortView` uses `type 'a t = ('a, Error.kind) result` and every
`Get` extractor takes a `~construct:string` so it can build an
appropriate `K_*` error when the shape doesn't match. That couples the
View module to the typechecker's error vocabulary and to the calling
context's notion of "what is being constructed."

Switch every View module's `'a t` to `'a option`. `Get` extractors
become unconditional shape-projections: `Some x` if the shape matches,
`None` otherwise; no `~construct:` argument. `Build` constructors
become unconditional shape-injections.

Each typechecker module that uses a View (`typecheck.ml`, `elaborate.ml`,
`rCheck.ml`) defines small *local* wrapper helpers that:

- Wrap `View.Get.foo` with `Option.to_result` plus the right
  `K_appropriate_error { construct = "..." }` to lift the option into
  the result-threading domain (`('a, Error.kind) result`).
- Wrap `View.Build.foo` with `Result.to_option` going the other way
  when needed.

The wrappers are local because the choice of error kind and construct
string is a property of the call site, not of the view. This restores the
View pattern's invariant — clauses still don't `match` on results
themselves; the Option→Result lift happens inside the wrapper, then `&&&`
/ `Result.map` carries the result-typed value through.

Apply this to **`SortView` (refactor)**, **`CoreExprView` (new)**, and
**`RPatView` (new)**.

### 2. Two new View modules: `CoreExprView` and `RPatView`

Following the (now-option-typed) precedent of `SortView` and the rule
captured in `feedback_view_no_branching.md`, we add:

- **`CoreExprView`** (`lib/coreExprView.{ml,mli}`) — wraps
  `CoreExpr.typed_ce` in `type 'a t = 'a option`, with `Get` extractors
  (`return`, `take`, `let_`, `if_`, `call`, `fail`, `case`, `inject`,
  `eq`) and `Build` constructors. Each `Get` operation takes an
  optioned typed_ce and returns optioned sub-components.
- **`RPatView`** (`lib/rPatView.{ml,mli}`) — wraps `RPat.cpat`,
  `RPat.lpat`, `RPat.rpat`, `RPat.qbase` similarly. Provides
  answer-bearing `Build` constructors that take an `'info` and an
  optioned answer, and `Get` extractors for shape destructuring inside
  `q_match`.

`CoreExprGet` (existing) and the new `CoreExprView` coexist:
`CoreExprView` calls into `CoreExprGet`'s `(_, Error.kind) result`
primitives and discards the error tag (`Result.to_option`). Local
wrappers in `rCheck.ml` lift the option back to a typechecker-specific
result. Same relationship as `SortGet` ↔ `SortView`.

`RPatView` is needed because the four pattern-matching judgements
produce typed patterns whose `info#answer` field can carry per-node
errors. Without the View wrappers, attach-and-continue on patterns
devolves to ad-hoc `match`-on-`result` blocks scattered across
`cpat_match`, `lpat_match`, and `rpat_match` — the pattern
`feedback_view_no_branching.md` rules out.

### 3. Pattern-matching judgements: shape-then-view

Every `_match` judgement follows the same outer skeleton:

1. **Pattern-match on the shape of the pattern** (`cpat_shape`,
   `lpat_shape`, `rpat_shape`, `qbase_shape`) — vanilla OCaml dispatch.
2. **Use a View to check the expected shape of the input context** —
   `SortView` for cpat against a sort, `CoreExprView` for lpat against
   a proposition or rpat against a predicate. `q_match` doesn't need a
   ProofSortView: the proof-sort entry is already a discriminated
   union (`Comp | Log | Res | DepRes`) constructed by `elab_pf_entry`,
   so direct OCaml pattern-matching on it is fine — kind/length
   mismatches are the only failure modes and they're handled in
   q_match's outer dispatch.
3. **Recurse on sub-patterns** with the spec-prescribed expectations.
4. **Build the typed pattern node** via `RPatView.Build`, with
   `info#answer` derived from the View result via the local
   option→result wrapper plus `Result.map` / `&&&`.

For step 2, if the input shape doesn't match the pattern's shape (e.g.
`RReturn lpat` against a non-`return` predicate), the local wrapper
turns the `None` into an `Error K_*` and that flows onto the pattern
node's `info#answer`. Recursion still proceeds against placeholders so
sub-pattern errors collect.

#### Output context in `info` for IDE hover

When each `_match` function builds the `RProg.typed_rinfo` it attaches
to its returned typed pattern, the `ctx` / `rctx` fields use the
**output** delta (after binding the pattern's vars), not the input
delta. Reason: when the cursor in the IDE hovers over a bound variable
in the source, hover lookup walks up to the nearest typed node and
reads `info#ctx` — only the output context contains the binding the
user is asking about. This applies symmetrically to `q_match` and to
`crt`-let-family rules where the body's typed_rinfo should reflect the
post-binding context.

### 4. Pattern-matching judgements: split into four

Replace the single `rpat_match` (`lib/rCheck.ml:1436-1729`) with:

- `cpat_match : RSig.t -> RCtx.t -> Effect.t -> RPat.cpat -> Sort.sort
   -> (typed_cpat * RCtx.t * CoreExpr.typed_ce) ElabM.t`
- `lpat_match : RSig.t -> RCtx.t -> RPat.lpat -> CoreExpr.typed_ce
   -> (typed_lpat * RCtx.t * Constraint.typed_ct) ElabM.t`
- `rpat_match : RSig.t -> RCtx.t -> Effect.t -> RPat.rpat
   -> CoreExpr.typed_ce -> CoreExpr.typed_ce
   -> (typed_rpat * RCtx.t * Constraint.typed_ct) ElabM.t`
- `q_match : RSig.t -> RCtx.t -> Effect.t -> RPat.t
   -> (CoreExpr.typed_ce, _, Var.t) ProofSort.t
   -> (typed_q * RCtx.t * Constraint.typed_ct) ElabM.t`

(Where `typed_cpat = (RProg.typed_rinfo, Var.t) RPat.cpat` etc.)

`q_match`'s body is the residual pattern-matching dispatcher: walk the
qbase list against the proof-sort list, calling `cpat_match` for QCore,
`lpat_match` for QLog, `rpat_match` for QRes (delegating to the wrapper
that understands `rpb = do cpat = rpat | rpat`).

`rpat_match` is now genuinely recursive: `RTake (cpat, rp1, rp2)` calls
`cpat_match` and two `rpat_match` recursive calls — no need for the
"expand into qbase list and prepend" trick the current implementation uses.

### 5. AST shape: update `rpfF`, keep `RHole` for placeholders

In `lib/refinedExpr.{ml,mli}`, the new `rpfF` is:

```ocaml
type ('crt, 'lpf, 'rpf, 'spine, 'e, 'b, 'var) rpfF =
  | RVar       of 'var
  | RAnnot     of 'rpf * 'e * 'e
  | RReturn    of 'lpf
  | RTake      of 'rpf * 'rpf
  | RFail      of 'lpf
  | RLet       of ('b, 'var) RPat.lpat * ('b, 'var) RPat.cpat * 'rpf
  | RCase      of ('b, 'var) RPat.lpat * Label.t * ('b, 'var) RPat.cpat * 'rpf
  | RIfTrue    of 'rpf
  | RIfFalse   of 'rpf
  | RUnfold    of 'rpf
  | RAnnotStrip of 'rpf
  | RHole      of string
```

`RAnnot` (ascription `rpf : ce @ ce'`) and `RAnnotStrip` (the strip form
`annot ; rpf`) are distinct constructors per the existing decision in the
prior plan. `RHole` survives as the multi-error placeholder.

`'b` flows in via `RPat.lpat` and `RPat.cpat` field types — the same trick
already used by `crtF`'s `CLet` / `CLetLog` / `CLetRes` / `CLetCore`.

### 6. `synth_rpf` shrinks; everything else is check-only

Per the spec, only `Var` and `Annot` synth. Every other rpf form in synth
position is unsynthesizable and emits a `cannot_synthesize` error via
attach-and-continue (placeholder predicates and value, RHole or echoed
sub-tree as the typed-rpf body). Mirrors the existing pattern at
`rCheck.ml:609-686` — generalize it.

### 7. Continue past errors via `info#answer`

Each new clause in `check_rpf` follows the established pattern:

1. Extract sub-pieces from `pred = strip_annots ce1` via `CoreExprView.Get`.
   Result type: `'sub typed_ce CoreExprView.t`.
2. Recurse on subterms (lpat/cpat/rpf) with the *intended* expected sort,
   feeding through a `view_or_hole` helper that returns either the extracted
   typed_ce or a typed `CHole` placeholder. Helper hides the `match` so the
   clause stays branch-free.
3. Emit the typed rpf node with `info#answer` set from the View result via
   `Result.map_error (Error.at ~loc:pos)` and `Result.map (fun _ -> bool_sort)`
   (the rpf sort is a placeholder bool — its real semantic content is in the
   pred/value pair stored in `goal`).
4. Pass through the constraint produced by the recursive call(s), `cand`-ed
   with any side-condition constraints (e.g. `Ct_ce ce` for iftrue, `is ce L`
   for case).

Errors from sub-trees ride along on `info#answer` and are collected in O(n)
by the existing `Typecheck.annotate_subterm_errors` and
`RCheck.collect_errors_rprog` walkers (the latter at `rCheck.ml:1827-1932`).

The walker `collect_errors_rprog` already covers `lpf`, `rpf`, `crt`, `spine`,
and `RPat`. We update the `rpf` case to walk the new constructors. We also
extend the rpat sub-walker to read `info#answer` on cpat/lpat/rpat/qbase
nodes (they may now carry errors from the new pattern judgements).

## Module-by-module design

### `lib/sortView.{ml,mli}` (refactor)

Switch `'a t` from `('a, Error.kind) result` to `'a option`. Drop the
`~construct:string` arg from every `Get.*`. `Get.record` now returns
`('info Sort.t list) option` (just `None` on shape/arity mismatch);
callers that need a fixed-length list still get one because the local
result-domain wrapper supplies the per-call default error and length
behavior.

Update every consumer of `SortView` (`lib/typecheck.ml`,
`lib/elaborate.ml`, `lib/rCheck.ml`) to call a *local* wrapper that
lifts `Get`'s `'a option` to `('a, Error.kind) result` with the
appropriate `K_*` error kind. The wrappers stay terse — usually one
line each — and live alongside the clauses that need them. No clause
touches a `match … with Some … | None …` directly.

### New: `lib/coreExprView.mli`

```ocaml
type 'a t = 'a option

module Get : sig
  val return : CoreExpr.typed_ce t -> CoreExpr.typed_ce t
  val take   : CoreExpr.typed_ce t
            -> (Var.t * CoreExpr.typed_ce * CoreExpr.typed_ce) t
  val let_   : CoreExpr.typed_ce t
            -> (Var.t * CoreExpr.typed_ce * CoreExpr.typed_ce) t
  val if_    : CoreExpr.typed_ce t
            -> (CoreExpr.typed_ce * CoreExpr.typed_ce * CoreExpr.typed_ce) t
  val call   : CoreExpr.typed_ce t -> (string * CoreExpr.typed_ce) t
  val fail   : CoreExpr.typed_ce t -> unit t
  val case   : CoreExpr.typed_ce t
            -> (CoreExpr.typed_ce
                * (Label.t * CoreExpr.typed_info * Var.t * CoreExpr.typed_ce) list) t
  val inject : CoreExpr.typed_ce t -> (Label.t * CoreExpr.typed_ce) t
end

module Build : sig
  val return : CoreExpr.typed_info -> CoreExpr.typed_ce t -> CoreExpr.typed_ce t
  val eq     : CoreExpr.typed_info -> CoreExpr.typed_ce t -> CoreExpr.typed_ce t
            -> CoreExpr.typed_ce t
  (* Plus mirrors of every Get extractor that the new check_rpf rules need. *)
end

module Test : sig val test : QCheck.Test.t list end
```

The `Get` shapes match what `rCheck.ml`'s current implementation pulls
through `CoreExprGet`. Implementation: each `Get.foo` calls
`CoreExprGet.get_foo` and applies `Result.to_option`.

A typical local wrapper in `rCheck.ml` looks like:

```ocaml
let view_get_return ~construct (ce : CoreExpr.typed_ce option)
    : (CoreExpr.typed_ce, Error.kind) result =
  Option.to_result ~none:(Error.K_wrong_pred_shape
                            { construct; expected_shape = "return _";
                              got = "<expression>" })
    (CoreExprView.Get.return ce)
```

### New: `lib/rPatView.mli`

```ocaml
type 'a t = 'a option

(** Constructors that bake an [info#answer] (an [option]) into the node.
    The local wrapper at the call site lifts to result before passing in. *)
module Build : sig
  val cpat : RProg.typed_rinfo
          -> ((RProg.typed_rinfo, Var.t) RPat.cpat, Var.t) RPat.cpatF t
          -> (RProg.typed_rinfo, Var.t) RPat.cpat
  val lpat : RProg.typed_rinfo
          -> Var.t RPat.lpatF t
          -> (RProg.typed_rinfo, Var.t) RPat.lpat
  val rpat : RProg.typed_rinfo
          -> ((RProg.typed_rinfo, Var.t) RPat.cpat,
              (RProg.typed_rinfo, Var.t) RPat.lpat,
              (RProg.typed_rinfo, Var.t) RPat.rpat,
              Var.t) RPat.rpatF t
          -> (RProg.typed_rinfo, Var.t) RPat.rpat
  val qbase : RProg.typed_rinfo
           -> ((RProg.typed_rinfo, Var.t) RPat.cpat,
               (RProg.typed_rinfo, Var.t) RPat.lpat,
               (RProg.typed_rinfo, Var.t) RPat.rpat) RPat.qbaseF t
           -> (RProg.typed_rinfo, Var.t) RPat.qbase
end

(** Shape extractors for the pattern-matching judgements. *)
module Get : sig
  val cpat_var   : _ RPat.cpat t -> Var.t t
  val cpat_tuple : _ RPat.cpat t -> (_ RPat.cpat list) t
  val lpat_var   : _ RPat.lpat t -> Var.t t
  (* Etc — only what the four pattern judgements actually destructure. *)
end

module Test : sig val test : QCheck.Test.t list end
```

The `Build` constructors take an optioned shape (the answer) plus an
info, and emit a typed pattern node. The call site supplies a local
wrapper to convert any error-bearing computation into the optioned
input; the answer field on the node is whatever the wrapper produced.

### `lib/refinedExpr.{ml,mli}`

- Replace the `rpfF` declaration (mli line 46-52, ml line 27-33) with the
  12-constructor version above.
- Update `map_rpfF` (ml line 78-89): drop `RMakeRet`/`RMakeTake`/`RUnfold`
  legacy arms; add arms for new constructors. The `RLet`, `RCase` arms
  use `RPat.map_info_lpat`/`RPat.map_info_cpat` to walk pattern info,
  driven by `m.info`.
- Update `print_gen_rpf` (ml around line 217-219) to render new
  constructors. New keywords: `return`, `take`, `fail`, `let`, `case`,
  `iftrue`, `iffalse`, `unfold;`, `annot;`.
- Add `rpf` mapper-record-walking helpers if absent — `map_rpf_pat` to
  rewrite the embedded `RPat.lpat`/`RPat.cpat` sub-trees when info type
  changes (parsed_rpf → checked_rpf).

The mli's `parsed_rpf` and `located_rpf` definitions don't change; the
shape change is internal. `RPat.lpat`/`RPat.cpat` sub-fields use the same
`'b` info parameter as the surrounding rpf, so the four-way knot stays
consistent.

### `lib/parser.mly`

Replace `rpf_atom_expr` and `rpf_expr` (lines 605-623) and remove the
`MAKE_RET`/`MAKE_TAKE` tokens (line 51). New shape:

```menhir
rpf_expr:
  | r = rpf_atom_expr; COLON; e1 = simple_expr; AT; e2 = simple_expr
    { RefinedExpr.mk_rpf (loc_obj $startpos $endpos) (RefinedExpr.RAnnot (r, e1, e2)) }
  | r = rpf_atom_expr { r }

rpf_atom_expr:
  | x = ident_var
    { RefinedExpr.mk_rpf (loc) (RefinedExpr.RVar x) }
  | RETURN; LPAREN; l = lpf_expr; RPAREN
    { RefinedExpr.mk_rpf (loc) (RefinedExpr.RReturn l) }
  | TAKE; LPAREN; r1 = rpf_expr; COMMA; r2 = rpf_expr; RPAREN
    { RefinedExpr.mk_rpf (loc) (RefinedExpr.RTake (r1, r2)) }
  | FAIL; LBRACKET; l = lpf_expr; RBRACKET
    { RefinedExpr.mk_rpf (loc) (RefinedExpr.RFail l) }
  | LET; LBRACKET; lp = lpat_inner; RBRACKET; cp = cpat_inner; SEMICOLON; r = rpf_atom_expr
    { RefinedExpr.mk_rpf (loc) (RefinedExpr.RLet (lp, cp, r)) }
  | CASE; LBRACKET; lp = lpat_inner; RBRACKET; l = LABEL; cp = cpat_inner; SEMICOLON; r = rpf_atom_expr
    { RefinedExpr.mk_rpf (loc) (RefinedExpr.RCase (lp, label l, cp, r)) }
  | IFTRUE;  SEMICOLON; r = rpf_atom_expr  { mk_rpf (RIfTrue r) }
  | IFFALSE; SEMICOLON; r = rpf_atom_expr  { mk_rpf (RIfFalse r) }
  | UNFOLD;  SEMICOLON; r = rpf_atom_expr  { mk_rpf (RUnfold r) }
  | ANNOT;   SEMICOLON; r = rpf_atom_expr  { mk_rpf (RAnnotStrip r) }
  | LPAREN; r = rpf_expr; RPAREN { r }
  | h = HOLE { mk_rpf (RHole h) }
```

All the keyword tokens (`RETURN`, `TAKE`, `FAIL`, `LET`, `CASE`, `IFTRUE`,
`IFFALSE`, `UNFOLD`, `ANNOT`, `LBRACKET`, `RBRACKET`, `COMMA`, `SEMICOLON`,
`AT`, `COLON`, `LABEL`) already exist (used in `rpat_res` lines 517-537).
We delete `MAKE_RET` and `MAKE_TAKE` from the token list and from
`lib/lexer.mll`'s keyword table.

Note: the `RETURN` keyword does not yet appear in lpf or rpf. Verify it's
already a token (used by rpat). If not, add it.

Per `feedback_parse_recovery_sync_tokens.md`: leave error-recovery
behavior alone — the recovery layer splits on reserved keywords; the new
keywords are already reserved via `terminals` block in syntax.ott.

### `lib/lexer.mll`

Drop `make-ret` and `make-take` from the keyword table. All other new
rpf keywords (`return`, `take`, `fail`, `iftrue`, `iffalse`, `annot`)
should already be present (used by rpat). Verify and add if missing.

### `lib/resolve.ml`

Lines 560-565 (RMakeRet/RMakeTake handling) are replaced by handling for
the 10 new constructors. Each one is a structural recursion that walks
into sub-rpf / sub-lpf / sub-cpat / sub-lpat / sub-crt fields, applying
`resolve_var` to all binders and references. Pattern-side resolution uses
the existing `RPat`-walker helpers in `resolve.ml`.

### `lib/hoverIndex.ml`

Lines 174-175 collect the embedded lpf/crt for hover info. New rpf
constructors with embedded sub-trees: `RReturn lpf`, `RTake (rpf, rpf)`,
`RFail lpf`, `RLet (lpat, cpat, rpf)`, `RCase (lpat, _, cpat, rpf)`,
`RIfTrue rpf`, `RIfFalse rpf`, `RUnfold rpf`, `RAnnotStrip rpf`.
Add a `collect_lpat`/`collect_cpat` helper if not present.

### `lib/rCheck.ml`

This is the largest edit. Sections:

#### Context discharge by input-length split

Several spec rules — every `rcrt_check` let-family rule and the
`rpf_check :: let` / `:: case` rules — share a context-discharge
shape. The recursive premise produces an output context of the form
`Δ3 , Δ''` (the body's context plus extra bindings introduced by the
pattern that need to go out of scope). The conclusion's output is
just `Δ3`; the constraint is `close Δ'' (Ct1 ∧ Ct2)`.

The rule's side conditions `len Δ0 = len Δ3` and `zero Δ''` are how
the implementation discovers the split: we know the conclusion's
output context must have the same length as the rule's input context
`Δ0`, so split at length `|Δ0|` to get `(Δ3, Δ'')`. Then
`zero Δ''` checks that all linear resources Δ'' tracks have been
consumed.

Every implementation site of these rules:

```ocaml
let n0 = RCtx.length delta0 in
let* (full_delta, ct_body) = check_rpf rs delta2 rpf' ce2_subst ce3 in
let (delta3, delta_pop) = RCtx.split_at n0 full_delta in
let* () = RCtx.assert_zero delta_pop in
let ct = Constraint.close delta_pop (Constraint.cand ct1 ct_body) in
```

`RCtx.split_at` and `RCtx.assert_zero` are the helpers we need. Audit
`rCtx.mli`; add them if missing (probably present as
`RCtx.length`/`affinize`-adjacent operations).

Sections:

#### `synth_rpf` (lines 582-686)

Reduces to two productive cases:

- `RVar x`: unchanged.
- `RAnnot (rpf', se1, se2)`: unchanged.

All other 9 cases (`RReturn`, `RTake`, `RFail`, `RLet`, `RCase`,
`RIfTrue`, `RIfFalse`, `RUnfold`, `RAnnotStrip`) emit a placeholder pred
(`CHole "rpf-<form>-unsynth-pred"`), placeholder value (`CHole
"...-value"`), an info with `answer = Error (cannot_synthesize ~construct:
"<form> rpf (add a : pred @ value annotation)")`, and a typed-rpf body
that recursively calls `synth_rpf` on every sub-rpf / sub-lpf / etc. with
checked-but-error-tagged contexts so subterm errors still collect.
`RHole h` keeps its existing behavior.

A helper `synth_unsynthesizable rpf_shape ~construct ~loc ~delta` builds
the placeholder typed-rpf node with the right error. Single call site per
clause.

#### `check_rpf` (lines 688-769)





Replace the whole body with one clause per new check rule:

- **`RReturn lpat`** (`return lpat r<== return ce1 @ ce2`):
  ```
  pred = strip_annots ce1
  ret_view = CoreExprView.Get.return ~construct:"return rpf" (Ok pred)
  -- ret_view : typed_ce CoreExprView.t
  ce_a = view_or_hole_ce loc bool_sort "return-pred-shape" ret_view
  eq_prop = CoreExpr.eq pos ce_a ce2
  (lp_typed, delta', ct) = lpat_match rs delta lpat eq_prop
  answer = ret_view |> Result.map (fun _ -> bool_sort)
                    |> Result.map_error (Error.at ~loc:pos)
  rinfo = mk_rinfo_with_answer ~goal:(RpfGoal (ce1, ce2)) pos delta answer Spec
  rpf' = mk_rpf rinfo (RReturn lp_typed)
  return (rpf', delta', ct)
  ```
  
- **`RTake (rpf1, rpf2)`** (`take(rpf1, rpf2) r<== take x = ce1; ce2 @ ce3`):
  Use `CoreExprView.Get.take` to extract `(x, ce1, ce2)`. Synthesize
  `rpf1` to get `(rpf1', ce1', ce_witness, delta1, ct1)` and check
  consistency with `ce1`. Substitute the witness into `ce2` to get
  `ce2_subst`. Check `rpf2` against `ce2_subst @ ce3`. Build the typed
  rpf node.

- **`RFail lpat`** (`fail[lpat] r<== fail @ ce`):
  `CoreExprView.Get.fail` checks that pred is `fail`. Then check `lpat`
  against `false` (using `lpat_match`). Affinize delta. Constraint: ct.

- **`RLet (lpat, cpat, rpf')`** (`let[lpat] cpat; rpf r<== let x = ce1; ce2 @ ce3`):
  Use `CoreExprView.Get.let_` to extract `(x, ce1, ce2)`. Synthesize
  the sort of `ce1` via `Elaborate.synth` (already typed). Run
  `cpat_match` to bind `cpat`'s vars at sort τ, producing witness `ce`
  and extended `delta1`. Run `lpat_match` to check `lpat : ce == ce1`,
  producing `delta2` and `ct1`. Substitute `(ce : τ) / x` into `ce2` to
  get `ce2_subst`. Check `rpf'` against `ce2_subst @ ce3` to get
  `delta3`, `ct2`. Discharge the freshly-bound delta via the
  `len Δ0 = len Δ3` and `zero Δ''` checks; close into `ct = Δ'' ⇒
  (ct1 ∧ ct2)`. Build typed node.

- **`RCase (lpat, label, cpat, rpf')`**: same pattern with case-branch
  extraction (`case_branch L x ce'''`) — the existing rpat_match `RCase`
  case at lines 1655-1697 has the exact constructor lookup and equation
  building; reuse the structure.

- **`RIfTrue rpf'`** / **`RIfFalse rpf'`**: `CoreExprView.Get.if_`,
  recurse on the appropriate branch, AND the discriminator (or its
  negation) onto the constraint.

- **`RUnfold rpf'`**: `CoreExprView.Get.call` to get `(f, ce_arg)`. Look
  up `f` in `RSig.comp rs`; check it's pure. Substitute and recurse.
  Mirrors the existing `RUnfold` clause at lines 727-763.

- **`RAnnotStrip rpf'`**: `strip_annots ce1` and recurse on `rpf'` against
  the stripped pred; pass-through.

- **`RHole h`**: affinize delta, attach hole error, return.

- **fall-through (var, ascription)**: synth-then-unify, like the existing
  `_` case at lines 765-769.

For the View-pattern compliance, define a helper:

```ocaml
let view_or_hole_ce (pos : SourcePos.t) (sort : Sort.sort) (name : string)
    (v : CoreExpr.typed_ce CoreExprView.t) : CoreExpr.typed_ce =
  Result.value v
    ~default:(CoreExpr.mk (mk_info sort) (CoreExpr.Hole name))
```

This hides the `match`. Clauses thread results through, then call
`view_or_hole_ce` once where a real typed_ce is needed for substitution
or recursive calls.

For the answer field, each clause builds it with `Result.map` /
`Result.map_error` chained from the `view_or_hole_ce` result and any
sub-rpf / lpat / cpat answers — never via explicit `match`.

#### Pattern-matching split

Replace `rpat_match` (lines 1436-1729) with four new functions:

- **`cpat_match`** ~80 lines: dispatch on `RPat.cpat_shape`. CVar:
  `RCtx.extend_comp x sort eff delta`, witness `ce_of_var x sort`. CTuple:
  `SortView.Get.record` then map over component sorts, recursively call
  `cpat_match` on each component, build tuple witness.

- **`lpat_match`** ~30 lines: dispatch on `RPat.lpat_shape`. LVar:
  `RCtx.extend_log x prop delta`. LAuto: `Constraint.atom pos prop`.

- **`rpat_match`** (standalone) ~250 lines: dispatch on
  `RPat.rpat_shape` × pred shape. Each rule mirrors the spec at
  `syntax.ott:1750-1803`. Recursion: `RTake (cpat, rp1, rp2)` calls
  `cpat_match cpat τ`, then recursively `rpat_match rp1` and `rpat_match
  rp2`. No more "expand into qbase list" trick.

- **`q_match`** ~120 lines: walk the qbase list against the proof-sort
  list. For each element, dispatch on `qbase_shape × proof_sort_entry`:
  - `(QCore cp, Comp { var = y; sort; eff })` → `cpat_match cp sort`,
    then substitute the resulting witness into rest of pf, recurse.
  - `(QLog lp, Log { prop })` → `lpat_match lp prop`, recurse.
  - `(QRes (Plain rp), Res { pred; value })` → `rpat_match rp pred value`,
    recurse.
  - `(QRes (Do (cp, rp)), DepRes { bound_var = z; pred })` → fresh `x`,
    `cpat_match cp sort`, then `rpat_match rp pred[x/z] x`, recurse.
  - Length / kind mismatches: emit error onto qbase node's
    `info#answer`, build placeholder, continue.

  q_match also handles the `q` grammar's `QRes rpb` wrapper: `rpb_shape`
  is `Plain rp | Do (cp, rp)`, dispatched as above.

All four functions return typed pattern nodes. Errors at the pattern
level (sort mismatch, kind mismatch, length mismatch, predicate-shape
mismatch) attach to the appropriate pattern node's `info#answer` via
`RPatView.Build`.

#### Update consumers of `rpat_match`




- **`CLet (q, crt1, crt2)`** at line 973: replace `rpat_match` call with
  `q_match` call. The current `eff_pat = purify eff` logic survives, but
  the spec says q invocation uses `[eff]` directly (per the syntax.ott
  `:: let` rule, which now passes `eff` into q_match without purification).
  Audit the code site against the spec line 1896:
  `RS ; Delta1 |- [ eff ] q : Pf' -| Delta2 ~~> Ct1`.

- **`CLetLog (lpat, lpf, body)`** at lines 1006-1022: instead of building
  a fake one-element qbase list, call `lpat_match` directly with the
  synthesized prop. Then close over the inner Δ as the spec letlog rule
  prescribes.

- **`CLetRes (rpat, rpf, body)`** at lines 1024-1060: call `rpat_match`
  directly.

- **`CLetCore (lpat, cpat, ce, body)`** at lines 1140-1168: synthesize
  `ce` (already in code), then call `cpat_match cpat τ` for the witness,
  then `lpat_match lpat (eq witness ce)` for the equation.

- **`CIter`** at line 883: keep `q_match` (CIter still uses a q-pattern).

- **`RFunDecl`** at line 1773: keep `q_match` (function-parameter pattern
  is a q).

#### `collect_errors_rprog` walker (lines 1827-1932)

- Update the rpf walker to cover new constructors.
- Extend the rpat / lpat / cpat / qbase sub-walkers to read
  `info#answer` (they may now carry errors attached by the new pattern
  judgements). Mirror the existing `rinfo_error` accessor.

### `scripts/editor/tree-sitter-nanocn/grammar.js`

Lines 755-790 (rpf grammar) — drop `make-ret`/`make-take`/`unfold rpf`
forms; add the 10 new rpf shapes. Keywords already present in the grammar
(used by the rpat productions). Bump grammar version, regenerate parser
via `npx tree-sitter generate` (per `feedback_treesitter_npx.md`).

### Examples and baselines

- `examples/listlength.rcn` lines 32, 60, 65, 79, 81, 89, 107: rewrite
  `make-ret` / `make-take` / `unfold rpf` uses to the new forms.
  Concretely:
  - `make-ret(auto)` ⇒ `return auto`
  - `make-take((v0, res own2, res branch_res))` ⇒ a sequence of
    `take(v0_rpf, take(own2_rpf, branch_res_rpf))` or, more idiomatically,
    a `take(...)` constructor matching the spec's `take(rpf1, rpf2)`.
  - `unfold make-ret(auto)` ⇒ `unfold; return auto`.

  This is a careful semantic rewrite, not mechanical.

- `examples/errors/parsing/listlength.rcn`: parsing-error fixture; should
  remain a parse-error case, but possibly with a different error now.
  Update or replace.

- `test/test_main.ml` lines 1575-1601: update the `unfold rpf` /
  `make-ret` test fixtures to new syntax.

### LSP

`bin/nanocn_lsp.ml` consumes errors via the multi-error pipeline. No
direct rpf knowledge — the rinfo `subterm_errors` collection is enough.
Verify by running the LSP smoke test.

## Slice plan (commits)

Six phases. Each is a single commit; tests pass at the end of each.

### Phase A — AST + Parser + Resolve + HoverIndex

- Edit `lib/refinedExpr.{ml,mli}` to the new `rpfF` shape.
- Edit `lib/parser.mly` rpf grammar; remove `MAKE_RET`/`MAKE_TAKE` tokens.
- Edit `lib/lexer.mll` to drop legacy keywords.
- Edit `lib/resolve.ml` rpf walker.
- Edit `lib/hoverIndex.ml` rpf walker.
- Stub `lib/rCheck.ml` synth_rpf and check_rpf cases for new constructors
  to RHole-with-error placeholders so the project compiles. Keep the
  existing rpat_match unchanged in this phase.

After Phase A: tests that don't touch rpf still pass. Programs using new
rpf shapes parse and elaborate to a typed AST; rCheck produces
`cannot_implement` errors uniformly. Programs using old shapes (make-ret,
make-take, unfold rpf) get parser errors.

### Phase B — Tree-sitter grammar

- Update `scripts/editor/tree-sitter-nanocn/grammar.js`.
- Regenerate via `npx tree-sitter generate`.

### Phase C — `SortView` refactor + new `CoreExprView`

- Refactor `lib/sortView.{ml,mli}`: switch `'a t` from
  `('a, Error.kind) result` to `'a option`; drop `~construct:` from
  `Get.*`; adjust `Build.*` accordingly. Existing tests in
  `SortView.Test` updated to the option contract.
- Update every existing `SortView` consumer in `lib/typecheck.ml` and
  `lib/elaborate.ml` to call a local `option→result` wrapper. The
  clause bodies otherwise stay identical (same `&&&` / `Result.map`
  pipeline downstream).
- Add `lib/coreExprView.{ml,mli}` with option-typed Get/Build modules
  layered on top of `CoreExprGet`.
- Migrate the rpf-related call sites in `rCheck.ml` (RAnnot's predicate
  extraction, the existing `make-ret` clause's `CoreExprGet.get_return`)
  to use `CoreExprView.Get` plus a local wrapper as a proof-of-concept.
  Other `CoreExprGet` call sites elsewhere in the file stay until
  they're naturally touched in Phase E.

### Phase D — `RPatView` + pattern judgement split

- New `lib/rPatView.{ml,mli}` with Build/Get modules.
- Replace `rpat_match` (lines 1436-1729) in `rCheck.ml` with
  `cpat_match`, `lpat_match`, `rpat_match` (standalone), `q_match`. The
  four functions are mutually recursive (`q_match` calls all three;
  `rpat_match` recursive on sub-rpat/sub-cpat/sub-lpat).
- Update existing callers (`CLet`, `CLetLog`, `CLetRes`, `CLetCore`,
  `CIter`, `RFunDecl`) to the new judgements.
- Update the rcrt_check let-family rules to match the new spec
  (size-equality and zero-Δ context discharge per syntax.ott:1893-1931).

### Phase E — `synth_rpf` + `check_rpf`

- Cut `synth_rpf` to Var + Annot + the unsynthesizable fallback helper.
- Rewrite `check_rpf` with one clause per new spec rule, using
  `CoreExprView.Get` for predicate-shape extraction and the new pattern
  judgements for sub-pattern checking.
- Update `collect_errors_rprog`'s rpf walker.

### Phase F — Examples, baselines, tests

- Rewrite `examples/listlength.rcn` to the new syntax.
- Update `test/test_main.ml` test fixtures.
- Refresh baselines (`dune runtest --auto-promote` or whatever the
  promotion command is).
- Add a couple of new unit tests for the four pattern judgements
  (length mismatch on q, predicate-shape mismatch on rpat, sort
  mismatch on cpat) verifying errors attach to the right node.

## Files to edit

- `lib/refinedExpr.ml`, `lib/refinedExpr.mli`
- `lib/parser.mly`, `lib/lexer.mll`
- `lib/resolve.ml`
- `lib/hoverIndex.ml`
- `lib/rCheck.ml` (Phases A, C, D, E)
- `lib/sortView.ml`, `lib/sortView.mli` (refactor: result→option) (Phase C)
- `lib/typecheck.ml`, `lib/elaborate.ml` (downstream SortView consumers
  get local option→result wrappers; clauses unchanged otherwise)
- `lib/coreExprView.ml`, `lib/coreExprView.mli` (new in Phase C)
- `lib/rPatView.ml`, `lib/rPatView.mli` (new in Phase D)
- `lib/rCtx.ml`, `lib/rCtx.mli` (add `split_at`, `assert_zero` if absent)
- `scripts/editor/tree-sitter-nanocn/grammar.js`
- `examples/listlength.rcn`
- `examples/errors/parsing/listlength.rcn` (review)
- `test/test_main.ml`
- Baselines under `test/`

## Existing functions / utilities to reuse

- `Typecheck.( &&& )`, `Typecheck.unsynth`, `Typecheck.check_pred`,
  `Typecheck.annotate_subterm_errors` (`lib/typecheck.ml`).
- `RCtx.extend_comp`, `RCtx.extend_log`, `RCtx.extend_res`,
  `RCtx.use_resource`, `RCtx.affinize`, `RCtx.erase` (`lib/rCtx.mli`).
- `Subst.apply_ce`, `Subst.extend_var` (`lib/subst.mli`).
- `Sig.lookup_fundef`, `CtorLookup.lookup`, `RSig.comp` (existing).
- `SortView.Get.pred`, `SortView.Get.record`, `SortView.Get.app`
  (`lib/sortView.mli`) — note: refactored in this slice from
  result-typed to option-typed; existing call sites in
  `typecheck.ml` / `elaborate.ml` get small local wrappers.
- `CoreExprGet.get_*` — wrapped (via `Result.to_option`) by new
  `CoreExprView`; keep the underlying module.
- `RProg.typed_rinfo`, `mk_rinfo`, `mk_rinfo_err` (`lib/rProg.mli` and
  rCheck.ml's helpers).
- `Util.raise_invariant` for impossible-in-well-formed-code branches
  only (per the rule in CLAUDE.md and `feedback_view_no_branching.md`).

## Verification

End-to-end:

1. **Compile**: `dune build` after each phase. Phase A passes when all
   stubs are in place.
2. **Tests**: `dune runtest`. After Phase F, all 221 tests pass plus the
   new pattern-judgement tests.
3. **Examples**: `./_build/default/bin/main.exe check examples/listlength.rcn`
   runs and produces the expected number of errors (zero if the example
   typechecks under the new design).
4. **Multi-error fixture**: a refined program with 2 broken rpf forms
   should produce 2 diagnostics in one pass (validates attach-and-continue).
5. **LSP smoke**: open a refined-typed file with new rpf syntax in the
   LSP; confirm hover info, diagnostics, and code lens all populate.
6. **Tree-sitter**: highlight a sample file with new keywords; confirm
   `return`, `take`, `fail`, etc. highlight correctly.

## Open design questions surfaced for discussion

- **`q_match`'s `eff` parameter** — spec uses `[eff]` directly (no
  `purify eff`). Current `CLet` consumer at `rCheck.ml:973` already
  passes `eff_pat = purify eff`. Audit whether this matches the new
  spec's rcrt_check `:: let` rule (which itself uses `[ eff ]` per
  syntax.ott:1896).
- **`RTake` payload shape** — spec says `take(rpf1, rpf2)` with no
  binder. The implementation extracts the binder `x` from the predicate
  `take x = ce1; ce2`, so `RTake of rpf * rpf` is correct (no AST-level
  binder).
- **Local-wrapper consolidation** — each typechecker module ends up
  with a small bundle of `option→result` wrappers (one per `Get`
  shape, parameterized by `~construct`). They live alongside the
  clauses that use them. Worth considering a shared helper module
  later (e.g., `Diagnostic.lift_kind`) once the pattern stabilizes;
  for this slice, keep them local.
- **`view_or_hole_ce` placement** — local helper in `rCheck.ml`,
  not exposed from `CoreExprView`, to avoid coupling `CoreExprView`
  to `mk_info` and to the typechecker's notion of the bool sort
  placeholder.

## Eliminate `ElabM.fail` from the touched code paths

The new attach-and-continue clauses for `synth_rpf`, `check_rpf`,
`cpat_match`, `lpat_match`, `rpat_match`, and `q_match` should drive
**every** former user-error `ElabM.fail` site within these functions to
use info-attached errors instead. The only remaining exceptions are
`Util.raise_invariant` for genuinely unreachable code (per CLAUDE.md
rule 9).

After Phase E the project memory's count of `ElabM.fail` sites in
`lib/rCheck.ml` should drop further from the current ~21 — each of:

- the 5 cpat/lpat/rpat-related sites in the old `rpat_match`
  (around lines 1444, 1454, 1485, 1545, 1664)
- the 4 `RPat.QDepRes` / `fail_pred_shape` sites
- the 9 sites attached to specific rpf check clauses

should turn into per-node answer attachments via the View pattern.

**Verification step**: after Phase E, `grep -c "ElabM\.fail" lib/rCheck.ml`
should yield only

1. defensive-guard sites in `elab_se` / `elab_se_check` /
   `elab_fundecl_body` (added in Phase C.0 to prevent Error-tainted
   typed_ce from reaching atom-guards), and
2. *non-rpf, non-pattern* user-error halts already deferred in the
   multi-error rollout (CIter holdouts, pf_eq holdouts).

**Notify the user of any survivor** outside those two sets. Each one is
a candidate for a follow-up slice — surface them, don't silently work
around them.

## Out of scope for this slice

- Phase 2 of `doc/extended-resource-terms.md` (effect-sensitive cpat,
  the `L cpat` core-pattern extension).
- Per-subterm answer fields on cpat/lpat/rpat *beyond* what the new
  pattern matchers naturally produce — i.e., we don't add new
  `subterm_errors` fields to RPat info if the existing
  `RProg.typed_rinfo` already covers them.
- Drop ElabM's result wrapper / migration of the remaining non-rpf,
  non-pattern `ElabM.fail` sites (Phase C.6 in the multi-error
  rollout).
