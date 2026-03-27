# Unique Variable IDs via Type-Parameterized ASTs and Scope Resolution

## Summary

Every variable now gets a globally unique integer ID so that SMT constraint
generation can produce unambiguous variable names. The parser produces
string-named trees (pure, no monadic threading), and a separate scope
resolution pass (`Resolve`) assigns unique IDs by walking the AST monadically.

## Architecture

```
Parser (pure)          Scope Resolution (ElabM.t)      Elaboration / RCheck
─────────────          ──────────────────────────       ────────────────────
string trees    ──▶    Var.t trees (unique IDs)   ──▶   CoreExpr / constraints
SurfExpr.parsed_se     SurfExpr.se                      (ID-only comparison)
Pat.parsed_pat         Pat.pat
```

## Key Design Decisions

### 1. `Var.t` uses `User of string * int` with ID-only comparison

- `Var.mk : string -> SourcePos.t -> supply -> t * supply` creates unique-ID variables
- `Var.of_string : string -> SourcePos.t -> t` creates sentinel-ID (-1) variables (backward compat)
- `Var.compare` compares by ID only (ignoring names and positions)
- `Var.fresh` (for generated variables) also uses the same supply

### 2. Function names are plain strings, not `Var.t`

Function names are resolved against the signature (`Sig`/`RSig`), not lexically.
They use `string` throughout:
- `SurfExpr.Call of string * 'a`
- `CoreExpr.Call of string * 'a`
- `RefinedExpr.CCall of string * 'spine`
- `RefinedExpr.LUnfold of string * 'e`
- `Sig.extend : string -> ...`, `Sig.lookup_fun : string -> ...`
- `RSig.extend : string -> ...`, etc.
- `Prog.FunDecl.name : string`, `Prog.CoreFunDecl.name : string`
- `RProg.FunDecl.name : string`, `RProg.RFunDecl.name : string`

### 3. AST types parameterized by `'var`

All AST types that carry variable bindings are parameterized:
- `Pat.t` → `('b, 'var) Pat.t` with `pat = (info, Var.t) t`, `parsed_pat = (info, string) t`
- `SurfExpr.t` → `('b, 'var) SurfExpr.t` with `se`/`parsed_se` aliases
- `ProofSort.entry/t` → `('e, 'var) entry/t`
- `RPat.t` → `'var RPat.t`
- `RefinedExpr.crt/lpf/rpf/spine` → `('e, 'b, 'var) crt/lpf/rpf/spine`
- `RProg.decl/t` → `('e, 'var) decl/t`
- `Prog.decl/t` → `('a, 'b, 'var) decl/t`
- `RFunType.t` → `('e, 'var) RFunType.t`

### 4. Parser is pure (non-monadic)

The parser produces string trees directly — no `ElabM.t` threading.
Start symbols return concrete types:
- `program` → `SurfExpr.parsed_se`
- `prog_eof` → `(SurfExpr.parsed_se, SourcePos.t, string) Prog.t`
- `rprog_eof` → `RProg.raw_parsed`

### 5. `Parse` module exposes both raw and resolved APIs

- `parse_expr_raw` etc. — pure, returns `(parsed_se, string) result`
- `parse_expr` etc. — monadic, composes parse + resolve

### 6. Binding vs use sites in `Resolve`

Determined by the Ott specification:
- **Binding sites** (create fresh Var.t, extend env): pattern variables, `CIf [x]`,
  `CCase [y]`, case branch `xi`, `CLet q`, `CIter q`, RPat elements, ProofSort entries
- **Use sites** (look up in env): `SurfExpr.Var`, `LVar`, `RVar`
- **Function names** (not resolved): `Call`, `CCall`, `LUnfold` — stay as strings

## Files Changed

| File | Change |
|------|--------|
| `lib/var.ml/mli` | `User of string * int`, ID-only compare, `mk` |
| `lib/pat.ml/mli` | `('b, 'var) t`, `map_var`, `parsed_pat` |
| `lib/surfExpr.ml/mli` | `('b, 'var) t`, `Call of string`, `parsed_se` |
| `lib/coreExpr.ml/mli` | `Call of string` |
| `lib/proofSort.ml/mli` | `('e, 'var) entry/t`, `map_var` |
| `lib/rPat.ml/mli` | `'var pat_elem`, `'var t`, `map_var` |
| `lib/refinedExpr.ml/mli` | `('e, 'b, 'var) crt/lpf/rpf/spine`, `CCall/LUnfold of string` |
| `lib/rProg.ml/mli` | `('e, 'var) decl/t`, `raw_parsed`, `name : string` |
| `lib/prog.ml/mli` | `('a, 'b, 'var) decl/t`, `name : string` |
| `lib/rFunType.ml/mli` | `('e, 'var) t` |
| `lib/sig.ml/mli` | `string` keys for function names |
| `lib/rSig.ml/mli` | `string` keys for function names |
| `lib/resolve.ml/mli` | **NEW** — scope resolution pass |
| `lib/parser.mly` | Pure (non-monadic), string-based |
| `lib/parse.ml/mli` | Raw + resolved APIs |
| `lib/elabM.ml/mli` | `mk_var` creates `User(name, id)` |
| `lib/elaborate.ml` | `Call` name is string |
| `lib/typecheck.ml/mli` | Updated type parameters |
| `lib/rCheck.ml/mli` | Updated type parameters, string function names |
| `bin/main.ml` | Updated for string function names |
| `test/test_main.ml` | Parse-only tests use `parse_expr_raw` |
