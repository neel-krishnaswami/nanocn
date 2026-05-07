# Refactoring `info#answer`: a typed answer per judgement

## Status

Deferred design note.  Not blocking the `ElabM.fail` elimination
(`failure-erasure.md`) — to be tackled as a separate task once the
fail-erasure refactor lands.

## Problem

Every elaborated AST node in nanocn carries an `info` object that
records what the typechecker concluded about that node.  Today the
two info shapes used during elaboration are:

```ocaml
(* lib/coreExpr.mli, used by all CoreExpr nodes *)
type typed_info = <
  loc            : SourcePos.t;
  ctx            : Context.t;
  answer         : (Sort.sort, Error.t) result;
  eff            : Effect.t;
  subterm_errors : Error.t list;
>

(* lib/rProg.mli, used by all RefinedExpr / RPat nodes *)
type typed_rinfo = <
  loc            : SourcePos.t;
  ctx            : Context.t;
  rctx           : RCtx.t;
  sort           : Sort.sort;
  eff            : Effect.t;
  goal           : goal;
  answer         : (Sort.sort, Error.t) result;
  subterm_errors : Error.t list;
>
```

Both share `answer : (Sort.sort, Error.t) result`.  This is a
historical artefact of the multi-error refactor: when `answer` was
introduced (Phase A of `multiple-errors.md`), the only judgements
needing per-node success/failure tracking were sort-level
(`synth`/`check` in `typecheck.ml` and `elaborate.ml`).  As later
phases extended attach-and-continue to the refined typechecker and
the four pattern judgements, every new judgement was forced to fit
its conclusion into the same `Sort.sort` slot — which only really
fits two of them.

The conclusions that the various judgements *actually* produce:

| Node category                   | Judgement                       | Conclusion (what "type-correct" means) |
| ------------------------------- | ------------------------------- | -------------------------------------- |
| Core expression                 | `synth` / `check`               | A sort `τ`                             |
| Core pattern                    | `cpat_match`                    | A sort `τ`                             |
| Logical expression              | `synth_lpf` / `check_lpf`       | A proposition `φ` (a `typed_ce` of bool sort) |
| Logical pattern                 | `lpat_match`                    | A proposition `φ`                      |
| Resource expression             | `synth_rpf` / `check_rpf`       | A resource type `ce@ce'`               |
| Resource pattern                | `rpat_match`                    | A resource type `ce@ce'`               |
| Refined term                    | `synth_crt` / `check_crt`       | A proof sort `Pf`                      |
| Refined pattern (qbase / q)     | `q_match`                       | A proof sort entry `Pf`                |
| Spine                           | `check_spine`                   | A proof sort `Pf` (the residual)       |
| Sort (in declarations)          | sort well-formedness            | (nothing — yes/no)                     |
| Proof sort (in declarations)    | proof-sort well-formedness      | (nothing — yes/no)                     |

There are also auxiliary well-formedness checks woven into the
refined judgements:

- Resource expression `ce' => τ` (predicate) and `ce <= Pred τ` (value) — these are sort-checks on the constituent core expressions, but in the spec they're named first-class judgements.
- Logical expression `φ <= bool` — sort-check on the proposition.

Currently, when the refined typechecker stores `answer = Ok sort` on
an rpf node, it's storing the *underlying core sort* of the resource
value (or whatever surrogate fits), not the resource type
`ce@ce'`.  The `goal` field carries the expected proof sort
separately.  Hover / inspector consumers reading `info#answer` see a
plausible sort, but it isn't really the answer to the question the
refined judgement was asking.

The same problem appears in slightly different guises:

- For lpf nodes, the answer "should be" the proposition `φ`; instead it's some bool sort.
- For crt nodes, the answer "should be" the proof sort `Pf`; instead it's a sort.
- For pattern nodes, `cpat_match` sees a sort but `rpat_match` sees a resource type — so reusing one info shape across pattern categories has the same mismatch.

## Conceptual model

Each AST node should record:

1. **Context** (Γ for core, Δ for refined) — the environment the judgement ran in.
2. **Judgement-specific success info** — what the judgement concluded if it succeeded.  Differs per node category (sort / proposition / resource type / proof sort / well-formedness witness).
3. **Error** — a structured diagnostic if the judgement failed.

Today's single-arm `(Sort.sort, Error.t) result` collapses points 2
and 3 onto a single shape that only fits sort-shaped judgements.

## Proposed shape

A unified `Answer.t` sum type, used uniformly in every info object:

```ocaml
(* lib/answer.mli *)

type t =
  | A_sort     of Sort.sort
      (** Core expression / core pattern: judgement concluded with sort τ. *)
  | A_prop     of CoreExpr.typed_ce
      (** Logical expression / logical pattern: concluded proposition φ
          (a typed core expression of bool sort). *)
  | A_resource of { pred : CoreExpr.typed_ce; value : CoreExpr.typed_ce }
      (** Resource expression / resource pattern: concluded resource
          type ce'@ce (predicate and value). *)
  | A_proof    of (CoreExpr.typed_ce, typed_rinfo, Var.t) ProofSort.t
      (** Refined term / refined pattern / spine: concluded proof
          sort Pf. *)
  | A_wf
      (** Sort / proof-sort well-formedness (no payload — the
          judgement is yes/no). *)
  | A_error    of Error.t
      (** This node's judgement attached an error and continued. *)
```

The two info shapes simplify to:

```ocaml
(* shared across CoreExpr and RefinedExpr / RPat *)
type 'extra info_base = <
  loc            : SourcePos.t;
  answer         : Answer.t;
  subterm_errors : Error.t list;
  ..
> as 'extra
```

with the per-category extra fields layered on (e.g. RProg's adds
`rctx`, `goal`).

**Design choices to flag:**

- *Single sum vs. per-node-kind variants.*  OCaml without GADTs in info
  positions makes the per-node-kind option awkward (every traversal
  becomes pattern-match-on-info-shape).  A single sum keeps the
  uniform-info story while letting each clause use the right case.
  Mismatched cases (e.g. an `A_prop` showing up on a core expr node)
  would be a compiler bug — invariant-checked, not user-facing.
- *Where does sort live for hover?*  The current `typed_rinfo.sort`
  field is a hover convenience.  Once `answer` is properly typed,
  hover queries can read `Answer.t` directly; the redundant `sort`
  field can go.  Same for the redundant `eff` mirror.
- *What does an error case carry per judgement?*  Today every
  judgement uses the same `Error.t`.  We could parameterize errors
  by judgement (`Error.sort_err`, `Error.prop_err`, …) but that's a
  separate refactor — `Error.t` already discriminates kinds.

## Knock-on changes

1. **`info#answer` readers.**  Every consumer (LSP services, hover,
   inspector, error-collection traversals in `rCheck.ml` lines
   2157–2358, `Typecheck.collect_errors`) needs updating to pattern-
   match on `Answer.t` instead of result-bind on the sort case.
2. **Error-collection.**  `collect_errors_*` traversals become
   simpler: a single `A_error e` case at every node, no per-node-
   kind branching.
3. **View wrappers.**  `view_get_pred_sort` / `view_get_record_sorts`
   / `view_get_app_sort` and the `view_get_*_ce` family in
   `rCheck.ml` lines 51–130 currently consume `(Sort.sort, Error.kind)
   result`.  They'd be re-keyed to take `Answer.t` (or to project
   whatever case fits their construct).
4. **Pretty printers.**  `RefinedExpr.print`, `RPat.print` etc. that
   currently inspect `info#sort` for their hover output would shift
   to inspecting `Answer.t`.

## Related but out of scope

- The `subterm_errors` field stays as `Error.t list`.  Its job is
  unchanged: a denormalised summary of every `A_error` in the
  subtree, populated by an annotation pass after elaboration.
- The `goal` field on `typed_rinfo` (CrtGoal / PatGoal / SpineGoal /
  NoGoal) stays.  It captures the *expected* proof sort (judgement
  input), distinct from what the judgement *concluded* (which is
  the `Answer.t` payload).
- Whether `typed_info` and `typed_rinfo` should merge into a single
  shape after this refactor is an open question.  The remaining
  difference is `rctx` and `goal`; if those become optional fields
  on a unified info, the two types fold together.

## Why it's deferred

The fail-erasure refactor (`failure-erasure.md`) needs to land
first.  Trying to do both at once means simultaneously:

- Restructuring every `ElabM.fail` site (20 sites in `rCheck.ml`)
- Restructuring every `info#answer` reader and writer (dozens of sites across `lib/elaborate.ml`, `lib/rCheck.ml`, `lib/typecheck.ml`, plus every consumer)

The two changes are independent: fail-erasure is about *control flow*
(stop halting the monad on user errors), while this refactor is about
the *data shape* of the per-node answer.  Sequenced, each lands as a
focused diff with its own test surface.

After the fail-erasure refactor:

1. `ElabM.fail` is gone from the API; the monad is `state -> 'a *
   state`.
2. Every clause attaches errors to `info#answer` (currently as
   `Error e`).
3. The number of writers of `info#answer` is finalised — making this
   refactor's blast radius easy to enumerate.

That's the right moment to upgrade `answer` from
`(Sort.sort, Error.t) result` to the proper per-judgement sum.
