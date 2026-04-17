# Editor support for nanoCN

nanoCN ships with Emacs integration via a tree-sitter major mode
(`nanocn-ts-mode`). An LSP server (`nanocn-lsp`) providing
diagnostics, hover, and go-to-definition is planned but not yet
built.

## Requirements

- **Emacs 29.1+** compiled with tree-sitter support.
- The **nanocn tree-sitter grammar** (a compiled `.so`/`.dylib`).

## Installing the grammar

From Emacs:

```
M-x treesit-install-language-grammar RET nanocn RET
```

This downloads and compiles the grammar automatically, provided
the grammar recipe is registered (it is, if you load
`nanocn-ts-mode`).

Or manually:

```sh
cd scripts/editor/tree-sitter-nanocn
tree-sitter generate
tree-sitter build
# tree-sitter build produces nanocn.so (or .dylib on macOS).
# Emacs expects the libtree-sitter- prefix, so rename on copy:
cp nanocn.so ~/.emacs.d/tree-sitter/libtree-sitter-nanocn.so
```

## Loading the mode

Add to your Emacs init file:

```elisp
(add-to-list 'load-path "/path/to/nanocn/scripts/editor/emacs")
(require 'nanocn-ts-mode)
```

Or if you use `use-package`:

```elisp
(use-package nanocn-ts-mode
  :load-path "/path/to/nanocn/scripts/editor/emacs")
```

## Features

- **Syntax highlighting** (tree-sitter-backed):
  keywords, types, constructors, function names, numbers,
  booleans, operators, delimiters, brackets, comments.
  Four feature levels matching the Emacs-29 convention.

- **Indentation** via `treesit-simple-indent-rules`.

- **Imenu** / `which-function-mode`: lists all `fun`, `rfun`,
  `sort`, `type` declarations.

- **Defun navigation**: `C-M-a` / `C-M-e` jump between
  top-level declarations.

- **Comment support**: `M-;` inserts `//` comments.

## LSP support (planned — Phase 2)

When `nanocn-lsp` is built and on `$PATH`, Eglot will
automatically connect:

```elisp
;; Already registered by nanocn-ts-mode:
;; (add-to-list 'eglot-server-programs
;;              '(nanocn-ts-mode . ("nanocn-lsp" "--stdio")))
M-x eglot
```

Planned features: diagnostics, hover with typing context,
go-to-definition, document symbols, asynchronous SMT checking.
