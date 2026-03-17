# Refactor Syntax Trees to Doubly-Parameterized Style

## Summary

Refactored all 5 syntax tree types (Typ, Sort, Pat, CoreExpr, SurfExpr) from
single-parameter `'a tF` shapes with exposed `In` constructor to doubly-parameterized
`('a, 'b) tF` shapes with abstract tree type.

## Key Design Decisions

### New Interface Pattern

Each syntax tree module now exposes:
- `('a, 'b) tF` — shape with `'a` = recursive positions, `'b` = auxiliary info
- `map_shape` — map over recursive positions
- `map_info` — map over info positions in the shape
- Abstract `'b t` with `mk`/`info`/`shape` accessors
- `map : ('b -> 'c) -> 'b t -> 'c t` — recursive tree map

### Phantom vs Non-Phantom `'b`

- **Typ, Sort, Pat**: `'b` is phantom (no constructors carry `'b`). `map_info` reconstructs without change.
- **CoreExpr**: `'b` is non-phantom. `Annot` and `Own` embed `'b Sort.t`. `map_info` calls `Sort.map`.
- **SurfExpr**: `'b` is non-phantom. Embeds `'b Sort.t` and `'b Pat.t`. `map_info` calls both `Sort.map` and `Pat.map`.

### Typechecker `lift_sort`

The typechecker produces `typed_info CoreExpr.t` where `typed_info` includes ctx/sort/eff.
Since `CoreExpr.Own` and `CoreExpr.Annot` embed `'b Sort.t`, constructing the typed output
requires converting `Sort.sort` to `typed_info Sort.t`. A `lift_sort` helper does this by
mapping sort nodes to carry `typed_info` (with filler values for ctx/sort/eff, which no
client inspects on sort nodes).

## Files Modified

**Defining modules (10 files):**
- `lib/typ.mli`, `lib/typ.ml`
- `lib/sort.mli`, `lib/sort.ml`
- `lib/pat.mli`, `lib/pat.ml`
- `lib/coreExpr.mli`, `lib/coreExpr.ml`
- `lib/surfExpr.mli`, `lib/surfExpr.ml`

**Client modules (10 files):**
- `lib/elaborate.ml`, `lib/typecheck.ml`, `lib/evalCtx.ml`
- `lib/subst.ml`, `lib/typSubst.ml`
- `lib/dsortDecl.ml`, `lib/dtypeDecl.ml`
- `lib/prim.ml`, `lib/parser.mly`
- `test/test_main.ml`

## Naming Conventions

- `extract` → `info` (extract auxiliary info from tree node)
- `map` (old, shape-level) → `map_shape`
- `map` (new, tree-level) — recursive map over whole tree
- `In` constructor hidden; replaced by `mk`/`info`/`shape`

## New QCheck Tests

Each of Typ, Sort, Pat gained:
- `mk`/`info`/`shape` round-trip: `mk (info t) (shape t) ≡ t`
- `map Fun.id ≡ id`

## Verification

- `dune build` — compiles cleanly
- `dune test` — all 130 tests pass
- All 17 `.cn` examples pass
