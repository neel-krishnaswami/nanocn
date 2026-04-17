This is a small implementation of a CN+Fulminate style system. 

1. The core language syntax is based on Jad Ghalayini's functional syntax for SSA, with the idea that this will make it easy to eventually convert it to work over real LLVM/MLIR IR, while letting us work in a language with sane scoping and substitution principles. 
2. The refinement layer is inspired by Dhruv Makwana's PhD thesis. This has multiple levels:
   * Pure ghost terms, corresponding to SMT terms and formulas, including recursive functions
   * Predicate expressions, which are monadically-typed ghost terms, embodying separation logic formulas, including recursive predicates
   * Resource terms, which are proof terms for resource predicates
   * Commands, which manipulate the ghost state (eg, unfolding recursive predicates and functions, extracting and inserting elements of arrays, applying lemmas).
   * Finally, primitive calls in the core language are extended to allow function calls with ghost arguments/returns.
   * Commands and ghost args/returns are extensions of the core, but are eraseable. 
3. There is a minimal SMT-based typechecker for the language. Many of CN's quality-of-life features (such as automatic unfolding of predicates) will be omitted, because the plan is to get Claude to write the annotations (and this implementation, for that matter). Probably we will also require annotations at all control-flow mergers, too, so that we don't have to separately recheck each control-flow path the way CN does now.  
4. There is an interpreter for this language, which has two modes.
   * In mode 1, it just ignores the ghost state and runs the program.
   * In mode 2, all the ghost state is evaluated at runtime alongside the program a la Fulminate.
   
Another difference from Fulminate is that we will track *all* of the abstract resources of the program (i.e., the resource context) symbolically. In Fulminate, each byte of memory is owned by a stack frame. Here, each byte of memory will be owned by a *predicate call `P(t1,...,tn)`*, where the `P` is a predicate name and the `ti` are concrete ground terms. 

The reason for this is that we want all of the proof-manipulating commands to have an operational semantics, so that we can turn static errors into runtime errors, and use  concrete program states to help figure out what is going wrong with a proof, using ideas from CEGAR/CEGIS. The idea is that the results of these analyses can be invoked and studied both by programmers and by LLMs like Claude (maybe as a "skill"), and my conjecture is that informational mechanisms which are good for humans will also be good for machines. 

## Building

### Prerequisites

- OCaml 5.0+
- dune 3.0+
- opam packages: `sedlex`, `menhir`, `qcheck`, `qcheck-alcotest`, `lsp`, `jsonrpc`, `linenoise`

Install dependencies via opam:

```sh
opam install sedlex menhir qcheck qcheck-alcotest lsp jsonrpc linenoise
```

### Build

```sh
dune build
```

This produces two executables:

- `nanocn` -- the compiler and REPL (`dune exec nanocn`)
- `nanocn-lsp` -- the LSP server for editor integration

### Test

```sh
dune runtest
```

### Install

```sh
dune install
```

This places `nanocn` and `nanocn-lsp` on your PATH.

## Emacs mode

nanoCN ships with an Emacs tree-sitter major mode and an LSP server.
Requires **Emacs 29.1+** with tree-sitter support.

### 1. Install the tree-sitter grammar

From Emacs:

```
M-x treesit-install-language-grammar RET nanocn RET
```

Or manually:

```sh
cd scripts/editor/tree-sitter-nanocn
tree-sitter generate && tree-sitter build
cp nanocn.so ~/.emacs.d/tree-sitter/libtree-sitter-nanocn.so
```

### 2. Load the mode

Add to your Emacs init file:

```elisp
(add-to-list 'load-path "/path/to/nanocn/scripts/editor/emacs")
(require 'nanocn-ts-mode)
```

This provides syntax highlighting, indentation, Imenu, and defun
navigation for `.cn` and `.rcn` files.

### 3. Enable LSP (hover, diagnostics, SMT checking)

Ensure `nanocn-lsp` is on your PATH (via `dune install` or
`dune exec nanocn-lsp`), then:

```
M-x eglot
```

Eglot registration is already configured by `nanocn-ts-mode`.
The LSP server provides:

- **Diagnostics** via Flymake (parse errors, type errors)
- **Hover** showing the typing context, sort, and effect at point
  (including refined contexts with logical facts and resources)
- **Document symbols** for navigation
- **Async SMT checking** on save for `.rcn` files (requires Z3)

See `doc/editor-support.md` for full details.

