# SMT encoding — implementation plan (Phase 1)

## Context

`doc/smt-encoding.md` specifies how to translate a typechecked refined
nanoCN program and its verification constraints into SMT-LIB 2.7, feed
the result to Z3, and report the verdict. This plan operationalises
that design into concrete OCaml modules, naming conventions, file
output, and a `smt-check` subcommand.

Phase 1 scope:

- **One-shot file mode.** Write `<basename>-constraints.smt` and invoke
  `z3 <basename>-constraints.smt`. The pipe-based driver from
  `doc/smt-via-pipes.md` is out of scope (no interactive querying).
  `(push)`/`(pop)` still appear in the emitted file because the
  constraint-translation rules for `∀x:τ.C` require them, but Z3
  handles them from a file the same way it would over a pipe.
- **Z3 only.** CVC5 compatibility deferred.
- **No `(get-model)` / counter-model extraction.** Phase 1 just
  classifies `sat` / `unsat` / `unknown` / `error`.

Preconditions already met:

- `Constraint.t` matches the grammar `⊤ | ⊥ | C₁ ∧ C₂ | ∀x:τ.C | ce ⇒ C
  | ce` exactly (shape functor in `lib/constraint.mli`).
- `RCheck.check_rprog : RProg.parsed → (RSig.t * Constraint.typed_ct)
  ElabM.t` already returns a typed constraint tree alongside the
  refined signature.
- `CoreExpr.typed_ce` carries `sort` at every node, so the translator
  never has to re-infer types.
- The SMT s-expression library (`SmtAtom`, `SmtSexp`, `SmtLexer`,
  `SmtParse`) is in place, including `SmtParse.parse_sexps` for
  response parsing.
- Pure/spec discrimination is available on every function entry via
  `Effect.t` (`Pure | Impure | Spec`) — so no `is_pure` flag needs
  adding to `RSig.entry`.

## Two distinct things: `of_sort` vs. `sort_tag`

The translation handles two things that look superficially similar
but are structurally different:

1. **Sort translation** (`SmtExpr.of_sort : Sort.sort → SmtSexp.sexp`)
   produces a **structured S-expression** suitable for any position
   that expects an SMT sort term — argument/return sorts in
   `declare-fun` / `define-fun`, the sort of a `declare-const`, the
   sort in `(as e τ)`, parameters of `Pred τ`, etc. Examples:

   ```
   Int                →  Int
   Bool               →  Bool
   Ptr τ              →  (Ptr  <of_sort τ>)
   Pred τ             →  (Pred <of_sort τ>)
   Record []          →  Tuple-0
   Record [τ]         →  <of_sort τ>                (n=1: unboxed)
   Record [τ₁;…;τₙ]   →  (Tuple-n <of_sort τ₁> … <of_sort τₙ>)
   App (D, [])        →  D
   App (D, [τ₁;…;τₙ]) →  (D <of_sort τ₁> … <of_sort τₙ>)
   TVar a             →  a                          (sort variable)
   ```

   `Ptr`, `Pred`, and `Tuple-n` are **type constructors** in the
   prelude (`declare-datatype Ptr (par (A) …)`, `declare-sort Pred 1`,
   `declare-datatype Tuple-n (par (T1…Tn) …)`) — they are applied to
   their arguments as proper SMT sort applications, never flattened
   into a single identifier.

2. **Name mangling** (`SmtSym.sort_tag : Sort.sort → string`) produces
   a **flat SMT simple-symbol string** used only when a sort has to
   be embedded inside a larger identifier — specifically the
   per-sort monad operations the design doc monomorphises over S:
   `fail-<tag τ>`, `return-<tag τ>`, `bind-<tag τ>-<tag σ>`. The
   contents of these identifiers must themselves be legal SMT simple
   symbols, so flattening is unavoidable.

   `sort_tag` is derived by rendering `of_sort s` through
   `SmtSexp.to_string` and substituting characters so the result is
   a legal simple symbol:

   ```
   ( ) →  drop
   space →  _
   ```

   Examples:

   ```
   Int                →  "Int"
   Ptr Int            →  "Ptr_Int"
   Pred (Tuple-2 Int Bool)
                      →  "Pred_Tuple-2_Int_Bool"
   Record [Int;Bool]  →  "Tuple-2_Int_Bool"
   Record [τ]         →  sort_tag τ
   Record []          →  "Tuple-0"
   App (List, [Int])  →  "List_Int"
   ```

   Invariant: `Sort.compare s1 s2 = 0` iff `String.equal (sort_tag s1)
   (sort_tag s2)`. Tested by QCheck.

The rest of this plan treats `of_sort` as the canonical sort
translator; `sort_tag` appears only inside `SmtSym.{return,fail,bind}_sym`
and inside the prelude's monad decl/axiom loop.

## Module decomposition

Seven new modules in `lib/` plus small edits to `lib/rSig.*` and
`bin/main.ml`. `SmtExpr` merges sort and expression translation
because `CoreExpr.Annot` embeds a `Sort.sort` into an expression,
inducing cross-recursion (CLAUDE.md rule 1 prefers one module over
two coupled by callbacks).

### `lib/smtSym.mli` — name mangling for symbol-shaped identifiers

```ocaml
(** Mangling of nanoCN names into SMT-LIB simple symbols.
    Produces *flat* names for use as identifiers in declarations and
    references. Structured sort terms (e.g. [(Tuple-2 Int Bool)]) are
    built by [SmtExpr.of_sort], not here. *)

val of_var      : Var.t -> string   (* uses Var.print_unique *)
val of_label    : Label.t -> string
val of_dsort    : Dsort.t -> string
val of_tvar     : Tvar.t -> string
val of_funname  : string -> string

val ctor_selector : Label.t -> string      (* "get-L" per the doc *)
val tuple_sort    : int -> string          (* "Tuple-n" *)
val tuple_ctor    : int -> string          (* "tuple-n" *)
val tuple_proj    : int -> int -> string   (* "prj-n-k" *)

val sort_tag : Sort.sort -> string
(** Flat identifier derived from a sort, for embedding inside per-sort
    monad-op symbol names. See the "Two distinct things" section. *)

val return_sym : Sort.sort -> string               (* "return-<tag>" *)
val fail_sym   : Sort.sort -> string               (* "fail-<tag>"   *)
val bind_sym   : Sort.sort -> Sort.sort -> string  (* "bind-<τ>-<σ>" *)

module Test : sig val test : QCheck.Test.t list end
```

### `lib/smtExpr.mli` — sort + expression translation

```ocaml
(** Translate core sorts and typed core expressions to SMT
    s-expressions. Sort translation shares this module because
    [CoreExpr.Annot] embeds a [Sort.sort], producing mutual recursion
    with [of_ce]. *)

val of_sort : Sort.sort -> SmtSexp.sexp
(** Structured translation per the table in the "Two distinct things"
    section. Applied in every position that expects an SMT sort term. *)

val of_ce : CoreExpr.typed_ce -> (SmtSexp.sexp, string) result
(** Per the SMT(ce) rules of [doc/smt-encoding.md]. Returns [Error]
    for unsupported forms: [Iter], state primitives
    [New/Del/Get/Set/Own], and any primitive whose argument shape
    violates the expected tuple convention. *)

module Test : sig val test : QCheck.Test.t list end
```

### `lib/smtMonadSorts.mli` — collector pass

```ocaml
(** Collect the sets R, F, T, S from a refined signature and the
    constraint tree. S is the set of all monadic payload sorts used;
    the prelude emits per-sort [return]/[fail] and per-pair [bind]
    declarations for these sets. *)

module SortSet     : Set.S with type elt = Sort.sort
module SortPairSet : Set.S with type elt = Sort.sort * Sort.sort

type t = { s : SortSet.t; pairs : SortPairSet.t }

val collect : RSig.t -> Constraint.typed_ct -> t

module Test : sig val test : QCheck.Test.t list end
```

### `lib/smtPrelude.mli` — prelude assembly

```ocaml
(** Build the full SMT-LIB prelude for a program:
    (set-logic ALL), [Ptr] datatype, Tuple-0 and Tuple-2..16,
    user datatype / sort declarations in source order, [Pred] sort,
    per-sort monad operations and axioms, then pure-function defs
    and spec-function declarations.

    Per [Effect.t]: [FunDef] entries with [Effect.Pure] emit
    [(define-fun ...)]; entries with [Effect.Spec] emit
    [(declare-fun ...)]. Impure functions are skipped. *)

val build : RSig.t -> SmtMonadSorts.t -> (SmtSexp.sexp list, string) result

module Test : sig val test : QCheck.Test.t list end
```

### `lib/smtConstraint.mli` — constraint translation

```ocaml
(** Translate a typed constraint tree to a flat list of top-level
    SMT commands per SMT(C) rules. Each entry carries the source
    position of the originating constraint node so the file writer
    can emit a preceding comment. *)

type located_cmd = { pos : SourcePos.t; cmd : SmtSexp.sexp }

val of_ct : Constraint.typed_ct -> (located_cmd list, string) result

module Test : sig val test : QCheck.Test.t list end
```

### `lib/smtEncode.mli` — orchestrator

```ocaml
(** End-to-end: build prelude + constraints and write them to a file,
    annotating each constraint command with a source-position comment. *)

val encode :
  RSig.t -> Constraint.typed_ct ->
  (SmtSexp.sexp list * SmtConstraint.located_cmd list, string) result

val write_file :
  out_channel ->
  prelude:SmtSexp.sexp list ->
  constraints:SmtConstraint.located_cmd list ->
  unit
```

### `lib/solverInvoke.mli` — one-shot Z3 driver

```ocaml
(** One-shot file-based solver invocation. Spawns [exe <smt_path>],
    captures stdout, parses via [SmtParse.parse_sexps], and classifies
    each top-level form. *)

type answer = Sat | Unsat | Unknown | Error of string

val run_z3 : exe:string -> smt_path:string -> (answer list, string) result

module Test : sig val test : QCheck.Test.t list end
```

One `answer` per `(check-sat)` command in the file, in order. No
pipes, no sentinel framing, no timeout — Z3 runs to completion.

## Monad-sort collection

`SmtMonadSorts.collect rsig ct` performs two structural recursions:

1. **Constraint walk.** Pattern match on `Constraint.shape ct`:
   - `Top`/`Bot` → empty.
   - `And (a,b)` → combine `walk_ct a` and `walk_ct b`.
   - `Forall (_,_,a)` → `walk_ct a`.
   - `Impl (e,a)` → `walk_ce e` ⊎ `walk_ct a`.
   - `Atom e` → `walk_ce e`.

2. **Expression walk.** Pattern match on `CoreExpr.shape ce`:
   - `Return child` → add `(info child)#sort` to R and S; recurse
     into `child`. (The child's own sort is the payload τ of the
     surrounding `Pred τ`.)
   - `Fail` → destructure `(info ce)#sort` as `Pred τ`; add τ to F and
     S.
   - `Take ((_,_), bound, body)` → destructure `(info bound)#sort` as
     `Pred τ` and `(info body)#sort` as `Pred σ`; add `(τ,σ)` to
     pairs, `τ`/`σ` to S; recurse into both.
   - Every other node: recurse into its immediate `CoreExpr`
     children.

Per CLAUDE.md rule 4 ("avoid folds"), each walk is an explicit
`let rec walk_ce ce acc = ...`; the list-of-children case is also
explicit recursion, not `List.fold_left`.

Descent into the signature needs a small addition:

```ocaml
(* in lib/rSig.mli *)
type listed_entry =
  | LFun  of string * entry
  | LSort of DsortDecl.t
  | LType of DtypeDecl.t

val entries : t -> listed_entry list
(** Source-order listing of every entry in the signature. *)
```

`collect` walks only `LFun (_, FunDef { body; _ })` bodies. `FunSig`
entries carry no body; `RFunSig` entries' refined types may mention
`Pred τ` in domain/codomain sorts — handled via `of_sort` later, not
by `collect`.

## Constraint translation

Direct rendering of the six cases (`SmtConstraint.of_ct`):

| Constraint shape | Emitted commands |
|------------------|------------------|
| `Top`            | `[]`             |
| `Bot`            | `(check-sat)`    |
| `And (a,b)`      | `of_ct a @ of_ct b` |
| `Impl (e,a)`     | `(assert <SMT e>)` then `of_ct a` |
| `Forall (x,τ,a)` | `(push 1)` `(declare-const x <of_sort τ>)` `of_ct a` `(pop 1)` |
| `Atom e`         | `(assert (not <SMT e>))` `(check-sat)` |

`Forall` emits `(push 1)` / `(pop 1)` with explicit level; Z3 accepts
bare `(push)` / `(pop)` too, but the explicit numeral is portable.
The output is a flat command stream — commands cannot be nested
inside asserts because declarations must be top-level.

Shadowing across nested `Forall` binders is unproblematic: `Var.t`
carries a unique counter, and `SmtSym.of_var` uses
`Var.print_unique`, so every `x` from every `Forall` has a distinct
mangled symbol.

When position tracing is enabled (see "Position tracking" below),
each `Impl`, `Forall`, and `Atom` also emits the three
position-chain commands (`declare-const pos-N`, two `assert`s) on
scope entry, and `Atom`/`Bot` additionally emit
`(assert (= (ptail pos-N) pnil))` immediately before the
`(check-sat)`.

## Prelude assembly

Order, top of file to bottom:

1. `(set-logic ALL)` — most permissive.
1b. (Position tracing only) `Pos` and `PosList` datatypes plus the
    `pos-0` root constant with its head-fixing assertion. See
    "Position tracking" below for the exact three declarations.
2. Ptr datatype — verbatim per the design doc:
   `(declare-datatype Ptr (par (A) ((loc (addr Int)))))`. The sort
   parameter `A` is phantom (unused in the body) but is kept so that
   `(Ptr τ₁)` and `(Ptr τ₂)` are distinct SMT sorts — this catches
   pointer-type confusion in SMT-encoded terms. `of_sort (Ptr τ)`
   accordingly emits `(Ptr <of_sort τ>)`.
3. Tuple datatypes: `Tuple-0`, then `Tuple-n` for `n = 2..16`. Skip
   `n = 1` — matches the "declare nothing" rule and consistent with
   `of_sort (Record [τ]) = of_sort τ`.
4. User datatype/sort declarations from `RSig.entries`, in source
   order. Each `SortDecl { name; params; ctors; _ }` / `TypeDecl …`
   emits
   `(declare-datatype <name> (par (<params>)
      ((<L1> (<get-L1> <of_sort τ1>)) … (<Lk> (<get-Lk> <of_sort τk>)))))`.
   Nullary constructors (payload `Record []`) emit `((Lᵢ))` with no
   selector; see "Resolved design decisions".
5. `(declare-sort Pred 1)`.
6. For each τ ∈ `S` (iteration order given by `SortSet`):
   - `(declare-const <fail_sym τ> <of_sort (Pred τ)>)`
   - `(declare-fun <return_sym τ> (<of_sort τ>) <of_sort (Pred τ)>)`
7. For each `(τ,σ) ∈ S × S` (`SortPairSet` order):
   - `(declare-fun <bind_sym τ σ>
        (<of_sort (Pred τ)> (-> <of_sort τ> <of_sort (Pred σ)>))
        <of_sort (Pred σ)>)`
8. Monad laws — one `(assert (forall … (! (= … …) :pattern …)))` per
   member of the appropriate S-power:
   - Right-unit (|S|): `(<bind τ τ> m <return τ>) = m`.
   - Left-unit (|S|²): `(<bind τ σ> (<return τ> a) f) = (f a)`.
   - Associativity (|S|³).
   - Right-fail (|S|²): `(<bind τ σ> m (λx. <fail σ>)) = <fail σ>`.
   - Left-fail (|S|²): `(<bind τ σ> <fail τ> f) = <fail σ>`.
   The design doc has typos in these laws (missing `forall`
   binders, bare `return` / `bind` names); the doc is fixed in
   step 0 of the implementation order.
9. Pure function definitions. For each `LFun (name, FunDef { param;
   arg; ret; eff = Pure; body; _ })`:
   `(define-fun <name> ((<param> <of_sort arg>)) <of_sort ret> <SMT body>)`.
10. Spec function declarations. For each `LFun (name, FunDef { arg;
    ret; eff = Spec; _ })` or `LFun (name, FunSig { arg; ret; eff =
    Spec; _ })`:
    `(declare-fun <name> (<of_sort arg>) <of_sort ret>)`. Body is
    deliberately hidden (spec functions are recursive and must be
    explicitly unfolded by user-written equations).
11. Impure functions (`eff = Impure`): skip entirely — not expressible
    in pure SMT.

The prelude is cheap to recompute per query; no caching.

## Expression translation — the SMT(ce) rules

The design doc lists explicit rules for `Var`, `IntLit`, `BoolLit`,
`Let`, `Tuple`, `LetTuple`, `Inject`, `Case`, `If`, `App (prim, ce)`,
`Call`, `Eq`, `Return`, `Take`, `Fail`. Gaps and extensions:

- **`Annot (ce, τ)`** — not in doc. Emit `(as <SMT ce> <of_sort τ>)`.
  Needed when a polymorphic constructor is used without enough
  context (e.g. `nil : List Int`). `as` is a 2.7 feature; see
  smt-lib-2.7-summary.md §7.
- **`And (e1, e2)`** — not in doc. Emit `(and <SMT e1> <SMT e2>)`.
- **`Not e`** — emit `(not <SMT e>)`.
- **`Iter _`** — refuse with `Error "iter is not expressible in SMT"`.
  Iteration is unbounded and belongs in imperative code, not
  refinements.
- **State primitives** `New` / `Del` / `Get` / `Set` / `Own` — refuse
  with a structured error. They should never reach the translator
  because typechecking guarantees spec/refinement contexts are pure,
  but the guard is cheap.

`App (prim, arg)` argument shape: binary primitives (`Add`, `Sub`,
`Mul`, `Div`, `Lt`, `Le`, `Gt`, `Ge`, `And`, `Or`, `Eq _`) expect
`arg` to be a `Tuple [e1; e2]` per the typechecker's convention;
unary primitives (`Not`) expect `arg` plain. The translator
destructures accordingly and emits `(<op> <SMT e1> <SMT e2>)` or
`(<op> <SMT arg>)`. If the shape is wrong, return `Error` (defensive
— would indicate a typechecker bug).

Primitive symbol table:

```
Add → +   Sub → -   Mul → *   Div → div
Lt  → <   Le  → <=  Gt  → >   Ge  → >=
And → and  Or → or  Not → not
Eq _ → =
```

`Call (f, arg)`: if `arg` is `Tuple [a₁;…;aₙ]`, emit `(<f> <SMT a₁> …
<SMT aₙ>)`; otherwise `(<f> <SMT arg>)`.

`Case (ce, branches)`: requires `(info ce)#sort` to be
`App (D, _)`; emits
`(match <SMT ce> ((<L₁> <x₁>) <SMT ce₁>) … ((<Lₖ> <xₖ>) <SMT ceₖ>))`.
Pattern variables `xᵢ` are `Var.t`s; mangle via `SmtSym.of_var`.

`Return ce`: emit `(<return_sym τ> <SMT ce>)` where τ is
`(info ce)#sort`. Similarly for `Take` (uses `bind_sym`) and `Fail`
(uses `fail_sym`).

## File output and solver invocation

`SmtEncode.write_file` emits, in order:

```
<prelude cmd 1>
<prelude cmd 2>
...
; <file>:<line>:<col>
<constraint cmd 1>
; <file>:<line>:<col>
<constraint cmd 2>
...
```

One blank line between distinct prelude commands for readability; a
single-line `; ...` comment immediately precedes each constraint
command (no blank line between comment and command, so Z3's error
line numbers stay pointed at the command). Comments are emitted at
the writer level (plain `output_string oc "; …\n"`); `SmtSexp` is
not extended with a comment variant.

`SolverInvoke.run_z3 ~exe ~smt_path`:

```ocaml
let cmd = Filename.quote_command exe [smt_path] in
let ic = Unix.open_process_in cmd in
let buf = Buffer.create 4096 in
(try
   while true do
     Buffer.add_string buf (input_line ic);
     Buffer.add_char buf '\n'
   done
 with End_of_file -> ());
let status = Unix.close_process_in ic in
match status with
| WEXITED 0 ->
  (match SmtParse.parse_sexps (Buffer.contents buf) ~file:smt_path with
   | Ok sexps -> Ok (List.map classify_answer sexps)
   | Error msg -> Error msg)
| _ -> Error "z3 exited abnormally"
```

`classify_answer` pattern matches `SmtSexp.shape`:
- `Atom (Symbol "sat")` → `Sat`
- `Atom (Symbol "unsat")` → `Unsat`
- `Atom (Symbol "unknown")` → `Unknown`
- `List [Atom (Symbol "error"); Atom (String msg); …]` → `Error msg`
- anything else → `Error "unparsed solver output"`.

## `bin/main.ml` hook

Add a new `smt-check` subcommand — leaves `check-refined` behaviour
untouched:

```ocaml
let smt_check_file filename =
  let input = read_file filename in
  match ElabM.run Var.empty_supply (
    let open ElabM in
    let* rprog = Parse.parse_rprog input ~file:filename in
    RCheck.check_rprog rprog
  ) with
  | Error msg -> Format.eprintf "Error: %s@." msg; exit 1
  | Ok ((rsig, ct), _supply) ->
    match SmtEncode.encode rsig ct with
    | Error msg -> Format.eprintf "SMT encode error: %s@." msg; exit 1
    | Ok (prelude, constraints) ->
      let smt_path =
        (Filename.remove_extension filename) ^ "-constraints.smt"
      in
      let oc = Out_channel.open_text smt_path in
      SmtEncode.write_file oc ~prelude ~constraints;
      Out_channel.close oc;
      let z3 = Option.value (Sys.getenv_opt "Z3") ~default:"z3" in
      match SolverInvoke.run_z3 ~exe:z3 ~smt_path with
      | Error msg -> Format.eprintf "Solver error: %s@." msg; exit 1
      | Ok answers ->
        List.iteri (fun i a ->
          let pos = (List.nth constraints i).SmtConstraint.pos in
          Format.printf "%a: %a@."
            SourcePos.print pos SolverInvoke.print_answer a
        ) answers
```

Also extend `help` / `usage` at `bin/main.ml:194–203`.

## Tests

Per CLAUDE.md every module has `module Test`. Concrete properties:

- **`SmtSym`** — `sort_tag` stability: `Sort.compare s1 s2 = 0 ⇒
  String.equal (sort_tag s1) (sort_tag s2)` (QCheck with
  `Sort.Test.gen`, 200 cases). `sort_tag` produces a string that
  parses as a legal `simple_symbol` (no spaces, no parens, etc.).
- **`SmtExpr`** — tabular: hand-built `typed_ce` for each ce shape,
  check the emitted sexp against a canonical string. Roundtrip
  through `SmtParse.parse_sexp`. `of_sort` on compound sorts
  produces the structured forms described in "Two distinct things"
  (not flat identifiers).
- **`SmtMonadSorts`** — hand-built typed constraint / ce trees;
  assert expected `{ s; pairs }`.
- **`SmtConstraint`** — one test per constraint shape, asserting the
  emitted command sequence matches.
- **`SmtPrelude`** — structural: every emitted sexp re-parses via
  `SmtParse.parse_sexps` without error. Not a full semantic test —
  just catches malformed commands.
- **`SolverInvoke`** — an integration test gated on `Z3` env var:
  write `(set-logic QF_LIA) (assert true) (check-sat)` to a temp
  file, invoke, assert `[Sat]`. Skip if the env var is unset.
- **End-to-end** (`test/test_main.ml`): a new Alcotest group
  `smt-encode` running `smt_check_file` on `examples/incr.rcn` (or
  whichever `.rcn` rcheck already tests) under `Z3=install/bin/z3` if
  available; assert every constraint yields `Unsat` (verification
  success).

## Files touched

New (each `.mli` written first per CLAUDE.md rule 2):

- `lib/smtSym.{mli,ml}`
- `lib/smtExpr.{mli,ml}`
- `lib/smtMonadSorts.{mli,ml}`
- `lib/smtPrelude.{mli,ml}`
- `lib/smtConstraint.{mli,ml}`
- `lib/smtEncode.{mli,ml}`
- `lib/solverInvoke.{mli,ml}`

Edits:

- `doc/smt-encoding.md` — apply the typo fixes described under
  "Resolved design decisions" (monad-law naming + missing `forall`
  binders). Step 0 of the implementation order.
- `lib/rSig.mli` / `.ml` — add `listed_entry` and `entries : t ->
  listed_entry list`.
- `lib/dune` — add `unix` to `libraries`.
- `bin/main.ml` — new `smt_check_file`; new argv branch
  `[| _; "smt-check"; file |]`; updated `help` / `usage`.
- `test/test_main.ml` — register each new module's `Test.test` in
  `qcheck_tests`; add `smt-encode` Alcotest group at the bottom.

No edits to `Constraint`, `CoreExpr`, `Sort`, `RCheck`, `Sig`,
`DsortDecl`, `DtypeDecl`, or any parser/lexer module.

## Implementation order

0. Fix the typos in `doc/smt-encoding.md` (naming drift in monad
   laws, missing `forall` binders in fail laws).
1. `SmtSym` (no deps; pure mangling). Tests first.
2. `RSig.entries` minimal edit.
3. `SmtExpr` (depends on `SmtSym`, `SmtSexp`, `Sort`, `CoreExpr`,
   `Prim`). Most of the real work. Tests with hand-built typed CEs.
4. `SmtConstraint` (depends on `SmtExpr`). Independent of
   `SmtMonadSorts` / `SmtPrelude`. Includes position-trace threading.
5. `SmtMonadSorts` (depends on `RSig.entries`, structural walks).
6. `SmtPrelude` (depends on `SmtSym`, `SmtExpr`, `SmtMonadSorts`,
   `RSig.entries`). The large one. Includes `Pos` / `PosList`
   datatypes and initial `pos-0` constant when position tracing is
   enabled.
7. `SmtEncode` — glue; exposes the `Position_trace` gate.
8. `SolverInvoke` — can be built any time; no deps beyond `SmtParse`
   and `unix`.
9. `bin/main.ml` hook and end-to-end tests.

Steps 1–2 unblock 3; 4 and 5 proceed in parallel after 3; 6 depends
on 3+5; 7 after 6; 8 is independent.

## Resolved design decisions

- **Monad ops are monomorphised.** SMT-LIB 2.7 formally specifies
  `declare-sort-parameter`, but neither Z3 nor CVC5 supports it. We
  emit |S| `fail`/`return` decls, |S|² `bind` decls, and the
  |S| + |S|² + |S|³ + 2·|S|² axiom families in full. `sort_tag` and
  the `{return,fail,bind}_sym` helpers in `SmtSym` are therefore
  load-bearing, not speculative.

- **`doc/smt-encoding.md` will be updated** before implementation to
  fix the monad/fail law typos:
  - `(bind-τ-τ m return)` → `(bind-τ-τ m return-τ)`.
  - `:pattern (bind (ret a) f)` → `:pattern (bind-τ-σ (return-τ a) f)`.
  - Wrap the two fail-law asserts with the missing `forall` binder.

  Fixing the doc is step 0 of the implementation order.

- **Nullary constructors emit `((L))`** with no selector. Downstream:
  `Inject (L, e)` whose `e : Record []` emits bare `L` (not
  `(L <…>)`); a `Case` branch on `L` binds no variable (the
  SMT-LIB `match` pattern is just the symbol `L`, no application).

- **Keep the `Ptr` sort parameter.** Prelude keeps
  `(declare-datatype Ptr (par (A) ((loc (addr Int)))))` verbatim —
  the parameter is phantom in the constructor body but distinguishes
  `(Ptr τ₁)` from `(Ptr τ₂)` at the SMT type level, which is useful
  for catching pointer-type confusion in SMT-encoded terms.
  Consequently `of_sort (Ptr τ)` emits `(Ptr <of_sort τ>)`, not bare
  `Ptr`.

- **Source positions are emitted as comments AND as in-SMT
  tracking** (see next section). The comment form keeps `SmtSexp`
  pure; the tracking form threads a position chain through the model
  so that failing queries can be located.

## Position tracking (provisional, isolable)

In addition to the `; file:line:col` comment emitted before each
constraint command, the translator maintains a position chain as a
SMT-LIB constant so the model of a failing `(check-sat)` reveals the
full nested context of the failing constraint. The feature is
**provisional**: gated behind a single flag in `SmtEncode` so it can
be disabled with no residue in the output.

### Prelude additions

```
(declare-datatype Pos
  ((mk-pos (pos-file String) (pos-line Int) (pos-col Int))))

(declare-datatypes ((PosList 1))
  ((par (A) ((pnil) (pcons (phead A) (ptail (PosList A))))) ))

(declare-const pos-0 (PosList Pos))
(assert (= (phead pos-0) (mk-pos "<root-file>" <line> <col>)))
;; (ptail pos-0) is deliberately unconstrained at this point.
```

The `pos-0` chain's head is the source position of the top of the
constraint tree (the whole program being verified). Its tail starts
free and is successively tightened as the translator descends.

### Scope-entry emission

When the constraint translator enters any new scope node — an
`Impl`, a `Forall`, or an `Atom` — it increments an internal counter
`N`, emits:

```
(declare-const pos-N (PosList Pos))
(assert (= (ptail pos-{N-1}) pos-N))
(assert (= (phead pos-N) (mk-pos "<file>" <line> <col>)))
```

where `pos-{N-1}` is the immediately enclosing scope. This forces
the previous scope's tail to `pos-N` and fixes `pos-N`'s head to the
current position, leaving `(ptail pos-N)` free for the next level.

### Before `(check-sat)`

Immediately before each emitted `(check-sat)` (i.e. each `Atom` or
`Bot` case), the translator closes the tail:

```
(assert (= (ptail pos-N) pnil))
```

If the query is `sat` (verification failure), the solver's model of
`pos-0` is a finite cons-chain whose heads are the positions of
every enclosing `Forall`/`Impl`/`Atom` from root to the failing atom.

### State threading

`SmtConstraint.of_ct` grows internal state — a counter `next_scope :
int ref` or, preferably, a monadic state — to thread the current
`pos-N` name through the recursion. Keep state as plain threaded
integers (no `ref`) per CLAUDE.md rule 3.

### Gating

`SmtEncode` exposes a `Position_trace of bool` configuration that
controls whether the prelude additions and per-scope emissions are
generated. Default `true` for Phase 1, but callers can set it
`false` for a clean diff against the design doc. The implementation
localises all position-trace code in one branch of each relevant
function so "undo" is a single-commit revert.


## Out of scope

- Pipe-based driver (`doc/smt-via-pipes.md`), sentinel framing,
  `push`/`pop` interaction from the OCaml side.
- CVC5 backend.
- Polymorphic monad declarations via `declare-sort-parameter` (not
  supported by Z3 or CVC5).
- Automatic `(get-model)` extraction on `sat` (the position-trace
  chain is populated regardless, but Phase 1 does not surface the
  model to the user).
- Solver timeouts and cancellation.
- Caching the emitted `.smt` file / incremental re-verification.
- Translating `Iter` or the state primitives (they are refused at
  encode time).
- Unsat-core extraction per constraint.
