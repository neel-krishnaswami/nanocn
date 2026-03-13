# Surface computation expressions with pattern matching

## Summary

Added a surface computation language (`se`) with pattern matching that elaborates
to core computations (`ce`), following the same pattern as the assertion elaboration.

## Changes to `doc/syntax.ott`

### Step 1: Rename pass

| Old | New | Meaning |
|-----|-----|---------|
| `se` / `SE_` | `st` / `ST_` | surface assertion (spec) terms |
| `ce` / `CE_` | `ct` / `CT_` | core assertion terms |
| `e` / `E_` | `ce` / `CE_` | core computation expressions |

### Step 2: New grammar nonterminals

- `se :: 'SE_'` — surface computation expressions (mirrors core `ce` but with patterns)
- `cbinds :: 'CBinds_'` — computation pattern bindings (like `binds` but with types `A`)
- `CEC :: 'CECtx_'` — computation evaluation contexts
- `cmbranches :: 'CMB_'` — computation match branches

### Step 3: New judgement sections

- `Jsurf` — surface computation elaboration (`surf_synth`, `surf_check`)
- `Jcompmatch` — computation pattern matching (`comp_has_con`, `comp_has_tup`,
  `comp_strip_var`, `comp_con`, `comp_expand_tup`, `comp_coverage`, `comp_fill_ctx`)

### Step 4: Updated prog rules

- `fun f (pat : A) -> B [eff] { se }` — function declarations now take patterns
  and surface computation bodies, elaborated via `comp_coverage`
- `spec` declarations use renamed `st`/`ct`
- `main = se` uses surface computation

## Verification

- Ott: 160 good rules, 0 bad rules
- PDF: builds successfully (17 pages)
