---
name: code-search
description: Use the precomputed nanocn searcher index (locations.txt and callsites.txt under _build/default/info/) to find where a toplevel function is defined, what it calls, and which functions call it. Use this whenever the user asks "where is X defined?", "what does X call?", "who calls X?", or any similar navigation question about nanocn's OCaml code — and prefer it over a fresh Grep across lib/.
---

# nanocn code-search

A static index of every toplevel definition and call site in the nanocn
library. The searcher tool walks the OCaml typed-tree (`.cmt` files) and
emits two text files that are kept in sync with the build via the
`info/dune` rule, so a plain `dune build` keeps them up to date.

Prefer this skill over a raw `Grep` across `lib/` when answering
navigation questions: it gives you fully-qualified names (including
across submodules), exact source ranges, and resolved call targets that
a textual grep would miss (e.g., a call to `is_valid` is recorded as
`Dsort.is_valid` even though the source just says `is_valid`).

## Index files

The two files live under the build directory:

- **`_build/default/info/locations.txt`** — every toplevel definition.
  Format (one per line):
  ```
  Module.func : file:line:col : file:line:col
  ```
  The two positions are the start and end of the binding (the `let`
  keyword to the end of the body). Submodules are namespaced:
  `Dsort.Test.gen` is the `gen` binding inside `module Test = struct ... end`
  inside `Dsort`.

- **`_build/default/info/callsites.txt`** — every call from one toplevel
  binding to another function. Format (one per line):
  ```
  Caller.qname Callee.qname file:line:col
  ```
  The position is the location of the called identifier (not the whole
  application). The caller is always a toplevel binding in nanocn; the
  callee can be a nanocn function, a Stdlib function, or any external
  library function. Operators (`Stdlib.&&`, `Stdlib.>=`, etc.) are
  included — grep them out with `grep -v ' Stdlib\.[^A-Za-z]'` if you
  only want named function calls.

## Keeping the index fresh

The index is built by `dune build`. The `info/dune` rule depends on
`(source_tree ../lib)` and `(alias_rec ../lib/check)`, so editing any
file under `lib/` automatically rebuilds the index on the next build.
If the files don't exist or look stale, run:

```
dune build
```

Don't try to regenerate the files manually — let dune do it.

## How to use the index

### 1. Find where a definition is

Grep `locations.txt` for the qualified name. If you only know the bare
function name (e.g., `is_valid`), search for ` is_valid ` with the
trailing space, or use the regex anchor `\.is_valid `:

```
grep '\.is_valid ' _build/default/info/locations.txt
```

If you know the module, search by qualified name:

```
grep '^Dsort\.is_valid ' _build/default/info/locations.txt
```

The output gives you the source range you can pass to `Read` to look at
the definition. Example:

```
Dsort.is_valid : lib/dsort.ml:3:0 : lib/dsort.ml:12:34
```

means `is_valid` is defined in `lib/dsort.ml` from line 3 column 0 to
line 12 column 34. Use `Read` with `offset: 3` and `limit: 10` to view
it.

### 2. Find what a toplevel function calls

Grep `callsites.txt` for the function as the **first column** (the
caller). Always anchor with `^` and end with a space so a substring
match doesn't pick up the wrong name:

```
grep '^Dsort\.is_valid ' _build/default/info/callsites.txt
```

This lists every callee and the location of each call. To filter out
operators and Stdlib noise and just see internal calls:

```
grep '^Dsort\.is_valid ' _build/default/info/callsites.txt | grep -v ' Stdlib\.'
```

To get a unique sorted list of callees:

```
grep '^Dsort\.is_valid ' _build/default/info/callsites.txt | awk '{print $2}' | sort -u
```

### 3. Find what calls a toplevel function

Grep `callsites.txt` for the function as the **second column** (the
callee). Anchor with leading and trailing space so the match is exact:

```
grep ' Dsort\.is_valid ' _build/default/info/callsites.txt
```

Each line tells you the caller and the call site. To get unique callers:

```
grep ' Dsort\.is_valid ' _build/default/info/callsites.txt | awk '{print $1}' | sort -u
```

This is the "find all references" / "find all callers" operation.

## Tips and gotchas

- **Always run the grep against the build-dir paths**, not source paths.
  `_build/default/info/locations.txt` is the canonical location.
- **The Grep tool works fine** — pass `path: "_build/default/info/locations.txt"`
  (or `callsites.txt`) and your pattern. You don't need to shell out.
- **Anchor your patterns**. `Foo` will match `Foo.bar`, `FooBar.baz`,
  `Stdlib.Foo`, etc. Use `^Foo\.` for callers / def names, ` Foo\.bar `
  (leading and trailing space) for callees.
- **Module-qualified names follow OCaml rules**. The submodule `Test`
  inside `module Test = struct ... end` becomes `Dsort.Test.gen`, not
  `Dsort_Test.gen`.
- **Constructors and types are not indexed.** Only `let` bindings with a
  simple variable pattern (`Tpat_var`) are recorded. `module M = struct
  ... end` and `type t = ...` don't appear as definitions.
- **Calls only count `Texp_apply` of a `Texp_ident`**. Calls through a
  local variable (`let f = String.length in f s`) don't get attributed
  to `String.length` — they're attributed to the local `f`, which the
  searcher then drops as a "Local" reference. Direct partial-application
  chains like `(f x) y` only record the inner `f`.
- **`let xs = [M.f 1; M.f 2]` records two calls** with `xs` as the
  caller, even though `xs` is a list, not a function. The searcher
  doesn't filter callers by type — every toplevel binding can appear as
  a "caller" if its initializer contains function applications.
- **Operators are recorded** (`Stdlib.&&`, `Stdlib.+`, etc.). Filter
  them out if they're noise: `grep -v ' Stdlib\.[^A-Za-z]'`.

## Quick command reference

| Task | Command |
|------|---------|
| Find a definition by qualified name | `grep '^Dsort\.foo ' _build/default/info/locations.txt` |
| Find a definition by bare name | `grep '\.foo ' _build/default/info/locations.txt` |
| List all functions in a module | `grep '^Dsort\.' _build/default/info/locations.txt` |
| What does `Dsort.foo` call? | `grep '^Dsort\.foo ' _build/default/info/callsites.txt` |
| Who calls `Dsort.foo`? | `grep ' Dsort\.foo ' _build/default/info/callsites.txt` |
| Unique callees of `Dsort.foo` | `grep '^Dsort\.foo ' _build/default/info/callsites.txt \| awk '{print $2}' \| sort -u` |
| Unique callers of `Dsort.foo` | `grep ' Dsort\.foo ' _build/default/info/callsites.txt \| awk '{print $1}' \| sort -u` |
| Hide operator/Stdlib noise | append ` \| grep -v ' Stdlib\.'` |
