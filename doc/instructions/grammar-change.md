# Dual-update: parser.mly and tree-sitter-nanocn

Whenever you change `lib/parser.mly` (add a production, rename a
token, change the precedence cascade, etc.), you **must** also
update the tree-sitter grammar:

1. Edit `scripts/editor/tree-sitter-nanocn/grammar.js` to mirror
   the parser.mly change.

2. Run `tree-sitter generate && tree-sitter test` in that
   directory and fix any failures.

3. Run `tree-sitter parse` on all examples:
   ```sh
   tree-sitter parse examples/*.cn examples/*.rcn
   ```
   Verify zero `ERROR` / `MISSING` nodes on positive examples.

4. If the change introduces a new node type, update the queries:
   - `queries/highlights.scm` — add font-lock captures.
   - `queries/tags.scm` — add Imenu entries if it's a new decl.
   - `queries/locals.scm` — add scope / definition if it binds.

5. If the Emacs mode references node types explicitly (e.g. in
   `treesit-defun-type-regexp` or `nanocn-ts-defun-name`), update
   `scripts/editor/emacs/nanocn-ts-mode.el`.

The ordering is: **Ott first** (`doc/syntax.ott`), then
`lib/parser.mly`, then `tree-sitter-nanocn/grammar.js`. See
`CLAUDE.md` rule 0.1 for the Ott-first requirement.
