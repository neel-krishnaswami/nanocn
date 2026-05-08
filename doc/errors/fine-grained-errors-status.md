## Obstacles to a per-subterm multi-error migration of `rpat_match`

(Notes from the 2026-05-01 Phase C work on multi-error refined
typechecking.  Captured here so the pattern-rule refactor can keep
them in mind.)

The current `rpat_match` in `lib/rCheck.ml:1436+` fails fast on the
first user-error site (length mismatch, kind mismatch, predicate-
shape mismatch).  Migrating it to attach errors at the precise
offending sub-pattern — the design choice agreed on for slice C.5 —
runs into five concrete obstacles in the function as written today.

### 1. The output signature doesn't carry a typed RPat

```
rpat_match : RSig.t -> RCtx.t -> Effect.t ->
             (parsed RPat) -> (typed ProofSort) ->
             (RCtx.t * Constraint.typed_ct) ElabM.t
```

The function returns only the updated context and the constraint;
it does not return a typed pattern.  Each caller — `CLet`,
`CLetLog`, `CLetRes`, `CIter` (in `check_crt_impl`), and the
`RFunDecl` branch of `check_rdecl` — builds the typed pattern
*separately* via `RPat.map_info` after `rpat_match` returns:

```ocaml
let* (delta', ct_pat) = rpat_match ... in
let typed_pat = RPat.map_info (fun b -> mk_rinfo b#loc ...) pat in
```

Per-subterm errors require attaching to specific RPat sub-nodes,
so the typed RPat has to be constructed *inside* `rpat_match` —
which means changing the return type to
`(typed RPat * RCtx.t * Constraint.typed_ct) ElabM.t` and updating
every caller.

### 2. The internal `go` loop has nowhere to insert typed nodes

The recursion at `lib/rCheck.ml:1462+` processes
`(qbase elements × proof_sort entries)` pairs, threading `RCtx` and
accumulating `Constraint.typed_ct`.  It never builds a typed
cpat / lpat / rpat / qbase value.  Each recursive arm would need a
parallel "and emit the typed sub-tree" step, which is an
architectural shift rather than a localized edit.

### 3. Resource-pattern destructuring synthesizes sub-patterns that aren't in the input tree

`RTake`, `RLet`, `RCase`, etc. expand by *prepending* freshly
synthesized sub-patterns to the work list and continuing.  Those
expansion fragments don't correspond to anything in the user's
source RPat — they're auxiliary work created during elaboration.

A typed-tree output that mirrors the *original* user-written
pattern needs the loop to track a parallel "current position in
the source tree" cursor alongside the expansion work list, so
errors can be attached to the right user-visible sub-pattern.

### 4. Length / kind mismatches don't have a clear sub-pattern home

When the pattern has 3 qbase elements but the proof sort has 2,
where should "length mismatch" land — the whole pattern, the
third (extraneous) element, or the position past the second?  The
fine-grained choice is the third element, but reaching it requires
walking past the matched prefix and emitting error-bearing typed
nodes for each extra element.  Today the function bails on the
first mismatch and never visits the rest.

### 5. Predicate-shape failures fire deep in nested resource patterns

`fail_pred_shape` (`lib/rCheck.ml:1457`) triggers when a resource
pattern's destructure target — `Take` / `Return` / `Fail` / `Let` /
`Case` — doesn't match the embedded `CoreExpr`'s shape.  These can
fire several layers into an `RLet` / `RCase` chain.  The error
wants to land on the *specific* sub-pattern whose predicate
disagreed, which means each nested resource-pattern recursion
needs its own typed-tree construction with its own rinfo, and
errors flow up through that tree.  Today the function `ElabM.fail`s
and the bookkeeping never gets built.

### Summary of the migration work

If the pattern-rule refactor lands, the slice reduces to:

a. Extend `rpat_match`'s return type to include the typed RPat.
b. Thread typed-tree construction through the `go` loop.
c. Maintain a "current position in the source tree" cursor
   alongside the expansion work list, so errors attach to the
   right user-visible sub-pattern.
d. Replace each `ElabM.fail` site with `mk_rinfo_err` + a
   placeholder typed sub-pattern + `Constraint.top` for that
   sub-tree's contribution.
e. Update the five callers to use the returned typed RPat
   directly, dropping their `RPat.map_info` calls.

Roughly a day of work in `lib/rCheck.ml`: ~150 lines in
`rpat_match`, ~30 lines in callers.

The pragmatic shortcut, if per-subterm precision is dropped: have
`rpat_match` accumulate errors via a separate `Error.t list`
channel returned alongside the existing output, and attach to the
surrounding crt's rinfo.  This trades fine-grained location
information for a much smaller refactor (~30 lines total), at the
cost of LSP hover pointing at the enclosing let-pattern / iter
binder rather than the precise broken sub-pattern.

