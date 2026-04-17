# tree-sitter-nanocn

Tree-sitter grammar for nanoCN (`.cn` surface programs and `.rcn`
refined programs). Used by the Emacs `nanocn-ts-mode` for syntax
highlighting, indentation, Imenu, and sexp navigation.

This is a scaffold. The grammar currently accepts only the minimum
file (`main : () [pure|impure|spec] = ()`). The full translation of
`lib/parser.mly` is the next task; see
`doc/history/editor-support-plan.md` and
`/home/nk480/.claude/plans/humming-hopping-pascal.md`.

## Building

```sh
# from this directory:
tree-sitter generate        # produces src/parser.c and headers
tree-sitter build           # produces a loadable .so (Linux/macOS)
tree-sitter test            # runs test/corpus/ against the grammar
tree-sitter parse PATH      # parses a file and prints its tree
```

The generated `src/parser.c` is committed so consumers without
`tree-sitter-cli` can still build the parser with a C compiler.
Other generated artifacts (`src/grammar.json`,
`src/node-types.json`, `build/`, `.so`) are gitignored.

## Dual-update invariant

Whenever `lib/parser.mly` changes, this grammar must change too.
See `doc/instructions/grammar-change.md`.
