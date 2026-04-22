# Plan: Adding Typed Holes to nanoCN

## Context

Writing refined programs from scratch is difficult because we want to write programs incrementally. Typed holes (`$name`) let the typechecker operate on incomplete programs — they are checking forms that always succeed, so the user can leave parts unfinished and still get type feedback on the rest. This is explicitly an implementation-only feature (not in syntax.ott).

## Design Decisions

1. **Hole names are plain strings, not variables.** They are not subject to scope resolution, alpha-equivalence, or substitution. They follow the pattern of `Fail` in the codebase.

2. **Holes in refined expressions.** The spec defines holes for crt (core refined terms). I also add them to lpf and rpf positions, since a user may not yet know which logical fact or resource proof to provide. NOT added to spines — a spine's structure is determined by the function signature.

3. **Constraint output.** Core/surface holes produce no constraint. Refined holes produce `Constraint.top` (already exists in the constraint module). This contrasts with `CExfalso` which produces `Constraint.bot`.

4. **LSP hole reporting.** Emit each hole as an Information-severity diagnostic showing the expected type. This is the simplest approach and gives immediate visibility.

## Implementation Order

Each step maintains a compilable state.

### Step 1: Lexer — add `$name` token
- **`lib/lexer.ml`**: Add `hole` sedlex regexp (`'$', lower, Star ident_rest`). Add match arm producing `Parser.HOLE` (strip leading `$`).
- **`lib/parser.mly`**: Add `%token <string> HOLE` declaration only (no grammar rules yet).

### Step 2: Surface AST — add `Hole of string` variant
- **`lib/surfExpr.mli`**: Add `| Hole of string` to `seF`.
- **`lib/surfExpr.ml`**: Add variant + cases in `map_shape`, `map_info`, `print` (format as `$name`), `json`, QCheck gen.

### Step 3: Core AST — add `Hole of string` variant
- **`lib/coreExpr.mli`**: Add `| Hole of string` to `ceF`.
- **`lib/coreExpr.ml`**: Add variant + cases in `map_shape`, `map_info`, `print_gen` (format as `$name`), `json`, QCheck gen.

### Step 4: Refined AST — add `CHole`/`LHole`/`RHole`
- **`lib/refinedExpr.mli`**: Add `| CHole of string` to `crtF`, `| LHole of string` to `lpfF`, `| RHole of string` to `rpfF`.
- **`lib/refinedExpr.ml`**: Add variants + cases in `map_crtF`, `map_lpfF`, `map_rpfF`, and all four `print_gen_*` functions.

### Step 5: Substitution
- **`lib/subst.ml`**: In `apply_ce`, add `| CoreExpr.Hole _ -> e` (inert under substitution, same as `Fail`).

### Step 6: Scope resolution
- **`lib/resolve.ml`**: In `resolve_expr`, add `| SurfExpr.Hole h -> mk (SurfExpr.Hole h)` (pass through, same as `Fail`). Add similar pass-through cases for `CHole`, `LHole`, `RHole` in `resolve_crt`, `resolve_lpf`, `resolve_rpf`.

### Step 7: Parser grammar rules
- **`lib/parser.mly`**: Add `HOLE` productions to:
  - `simple_expr` → `SurfExpr.Hole`
  - `crt_seq_expr` (or appropriate crt level) → `RefinedExpr.CHole`
  - `lpf_atom_expr` → `RefinedExpr.LHole`
  - `rpf_atom_expr` → `RefinedExpr.RHole`

### Step 8: Core typechecker
- **`lib/typecheck.ml`**:
  - `synth`: Add `CoreExpr.Hole _` to the cannot-synthesize arm (line 159).
  - `check`: Add case — always succeeds, no effect/sort restriction:
    ```ocaml
    | CoreExpr.Hole h ->
      Ok (mk ctx pos sort eff0 (CoreExpr.Hole h))
    ```

### Step 9: Surface typechecker
- **`lib/elaborate.ml`**:
  - `synth`: Add `SurfExpr.Hole _` to the cannot-synthesize arm (line 346).
  - `check`: Add case — elaborates to core hole:
    ```ocaml
    | SurfExpr.Hole h ->
      ElabM.return (mk_typed ctx pos sort eff0 (CoreExpr.Hole h))
    ```

### Step 10: Refined typechecker
- **`lib/rCheck.ml`**:
  - `check_crt`: Add case modeled on `CExfalso` (line 898) but with `Constraint.top` instead of `bot`:
    ```ocaml
    | RefinedExpr.CHole h ->
      let delta' = RCtx.affinize delta in
      let rinfo = mk_rinfo pos delta (ProofSort.comp pf) eff in
      let checked = RefinedExpr.mk_crt rinfo (RefinedExpr.CHole h) in
      return (checked, delta', Constraint.top pos)
    ```
  - `synth_crt`: Add `CHole` to cannot-synthesize handling.
  - `check_lpf`: Add case (delta unchanged, constraint = top).
  - `synth_lpf`: Add `LHole` to cannot-synthesize handling.
  - `check_rpf`: Add case (affinize delta, constraint = top).
  - `synth_rpf`: Add `RHole` to cannot-synthesize handling.

### Step 11: Hover index
- **`lib/hoverIndex.ml`**: Add leaf cases for `Hole _`, `CHole _`, `LHole _`, `RHole _` alongside existing `Fail`/`Var`/`IntLit` leaf patterns.

### Step 12: Parser error messages
- **`lib/parser.messages`**: After grammar changes, run `menhir --list-errors` to discover new error states and provide messages. Use the `nanocn-parse-errors` skill.

### Step 13: Tree-sitter grammar
- **`scripts/editor/tree-sitter-nanocn/grammar.js`**:
  - Add `hole_expr: _ => /\$[a-z][a-zA-Z0-9_']*/`
  - Add `$.hole_expr` to `_simple_expr`, crt atom, `_lpf_atom`, `_rpf_atom`.
- **`scripts/editor/tree-sitter-nanocn/queries/highlights.scm`**: Add `(hole_expr) @variable.special`.

### Step 14: Emacs highlighting
- **`scripts/editor/emacs/nanocn-ts-mode.el`**:
  - Define `nanocn-hole-face` with yellow background.
  - Add tree-sitter font-lock rule mapping `hole_expr` to the face.
  - Add `hole` to `treesit-font-lock-feature-list`.

### Step 15: LSP hole listing
- **`bin/nanocn_lsp.ml`**: After compilation, walk the typed tree to collect holes (name, location, expected sort, context). Emit each as an Information-severity diagnostic: `"Hole $name : <sort>"`. This gives immediate visibility in the editor's problems panel and in-buffer.

## Verification

1. **Unit test**: Write a `.cn` file with holes at various positions (let binding body, function body, tuple element, case branch). Confirm it typechecks successfully.
2. **Refined test**: Write a `.rcn` file with holes in crt, lpf, and rpf positions. Confirm it typechecks with `Constraint.top`.
3. **Build**: `dune build` succeeds at each step.
4. **Parser messages**: Run the parse-errors skill to verify no PLACEHOLDER messages.
5. **Tree-sitter**: `tree-sitter generate && tree-sitter test` in the grammar directory.
6. **LSP**: Open a file with holes in Emacs, verify yellow highlighting and information diagnostics.
