# Implementation Plan: Assertion Language

This plan extends nanocn with the assertion language specified in `syntax.ott`.
The implementation follows the project conventions in CLAUDE.md: small modules,
.mli-first design, shape functors with location info, result types instead of
exceptions, and QCheck property tests.

## Overview of new features

1. **Assertion sorts** (τ): int, bool, loc, tuples, datasort applications, pred τ
2. **Datasort declarations**: `sort D(a1,...,an) = { C1 : τ1 | ... | Cn : τn }`
3. **Spec function declarations**: clausal (`spec f : τ → τ' = { ... }`) and simple (`spec f : τ = e`)
4. **Patterns**: variables, constructor patterns, tuple patterns
5. **Surface assertion expressions** (se): with pattern matching in let/take/case
6. **Core assertion expressions** (ce): patterns compiled away
7. **Elaboration**: surface → core via matrix-style pattern match compilation
8. **Bidirectional typechecking** for both core and surface assertion expressions

## Phase 1: Foundation types

### 1a. `Dsort` module — datasort names

Datasort names like `list`, `tree`. Same structure as `Label` (validated strings),
but with a different naming convention — datasort names start with a lowercase
letter (like type names), while labels start with uppercase (like constructors).

```ocaml
(* dsort.mli *)
type t
val of_string : string -> (t, string) result
val to_string : t -> string
val compare : t -> t -> int
val print : Format.formatter -> t -> unit
```

### 1b. Extend `Var` module — binding sites, generated variables, and fresh supply

Variables are extended to track:
- Whether they are user-written or generated
- Their **binding site** (the source position where they were introduced)

The internal representation changes from `string` to a richer type:

```ocaml
(* var.ml *)
type name =
  | User of string
  | Generated of int

type t = {
  name : name;
  binding_site : SourcePos.t;
}

(* var.mli — new operations added to existing interface *)
val is_generated : t -> bool
val binding_site : t -> SourcePos.t

type supply
val empty_supply : supply
val fresh : SourcePos.t -> supply -> t * supply
```

`of_string` becomes `of_string : string -> SourcePos.t -> t`, taking
the binding site (i.e., where the variable is bound in source code).

`fresh` takes a `SourcePos.t` — for variables generated during pattern
match compilation, this is the location of the subpattern they originate
from. This ensures error messages can point back to the relevant pattern.

`empty_supply` is the initial supply. `fresh` returns a `Generated n`
variable and the incremented supply. Purely functional — no mutation.

User-written variables print as their name (`x`, `xs`). Generated
variables print with a prefix (`_v0`, `_v1`). `compare` compares
by name only (ignoring binding site) — two variables with the same
name are equal regardless of where they were bound.

**Error messages** should report both the **use site** (from the
enclosing AST node's location annotation) and the **binding site**
(from `Var.binding_site`). For example:

```
Error at line 5, col 3: variable x has type int, expected bool
  x was bound at line 2, col 10
```

For generated variables originating from pattern compilation:

```
Error at line 8, col 5: type mismatch in pattern match
  generated from pattern at line 3, col 7
```

### 1c. `TVar` module — type variables

Type variables `a`, `b`, etc. used in parameterized datasort declarations.
Simple string wrapper like `Var`.

```ocaml
(* tvar.mli *)
type t
val of_string : string -> t
val to_string : t -> string
val compare : t -> t -> int
val print : Format.formatter -> t -> unit
```

### 1d. `Sort` module — assertion sorts

The assertion sort type τ. Follows the shape functor pattern.

```ocaml
(* sort.mli *)
type 'a sortF =
  | Int
  | Bool
  | Loc
  | Record of 'a list
  | App of Dsort.t * 'a list
  | Pred of 'a

type 'b t = In of 'b t sortF * 'b
type sort = < loc : SourcePos.t > t

val extract : 'b t -> 'b
val shape : 'b t -> 'b t sortF
val map : ('a -> 'b) -> 'a sortF -> 'b sortF
val compare : sort -> sort -> int
val print : Format.formatter -> sort -> unit

(* Check whether a sort is a spec type (no pred) *)
val is_spec_type : sort -> bool
```

The `is_spec_type` function enforces the `stau <:: tau` subrule: spec types
are sorts that don't contain `pred` anywhere.


## Phase 2: Datasort infrastructure

### 2a. `DsortDecl` module — datasort declarations

A datasort declaration `sort D(a1,...,an) = { C1 : τ1 | ... | Cn : τn }`.

```ocaml
(* dsortDecl.mli *)
type t = {
  name : Dsort.t;
  params : TVar.t list;
  ctors : (Label.t * Sort.sort) list;
  loc : SourcePos.t;
}

val lookup_ctor : t -> Label.t -> Sort.sort option
val ctor_labels : t -> Label.t list
val print : Format.formatter -> t -> unit
```

### 2b. `Subst` module — type variable substitution

Substitution of sorts for type variables, needed for instantiating
parameterized datasort constructors.

```ocaml
(* subst.mli *)
type t

val empty : t
val extend : t -> TVar.t -> Sort.sort -> t
val apply : t -> Sort.sort -> Sort.sort
val of_lists : TVar.t list -> Sort.sort list -> (t, string) result
```


## Phase 3: Assertion expressions

### 3a. `Pat` module — patterns

Patterns for the surface assertion language.

```ocaml
(* pat.mli *)
type 'a patF =
  | Var of Var.t
  | Con of Label.t * 'a
  | Tuple of 'a list

type 'b t = In of 'b t patF * 'b
type pat = < loc : SourcePos.t > t

val extract : 'b t -> 'b
val shape : 'b t -> 'b t patF
val map : ('a -> 'b) -> 'a patF -> 'b patF
val print : Format.formatter -> pat -> unit

(* Check that no variable appears twice *)
val linear_check : pat -> (unit, string) result

(* Collect all variables bound by a pattern *)
val vars : pat -> Var.t list
```

### 3b. `CoreExpr` module — core assertion expressions

Core assertion expressions have no complex patterns — only variable bindings.

```ocaml
(* coreExpr.mli *)
type 'a ceF =
  | Var of Var.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Own of Sort.sort
  | Take of Var.t * 'a * 'a
  | Return of 'a
  | Con of Label.t * 'a
  | Case of 'a * (Label.t * Var.t * 'a) list
  | Tuple of 'a list
  | LetTuple of Var.t list * 'a * 'a
  | Call of Var.t * 'a
  | Const of Var.t
  | Let of Var.t * 'a * 'a
  | If of 'a * 'a * 'a
  | Annot of 'a * Sort.sort

type 'b t = In of 'b t ceF * 'b
type ce = < loc : SourcePos.t > t

val extract : 'b t -> 'b
val shape : 'b t -> 'b t ceF
val map : ('a -> 'b) -> 'a ceF -> 'b ceF
val print : Format.formatter -> ce -> unit
```

### 3c. `SurfExpr` module — surface assertion expressions

Surface expressions have patterns in let, take, and case.

```ocaml
(* surfExpr.mli *)
type 'a seF =
  | Var of Var.t
  | Eq of 'a * 'a
  | And of 'a * 'a
  | Not of 'a
  | Take of Pat.pat * 'a * 'a
  | Return of 'a
  | Let of Pat.pat * 'a * 'a
  | Tuple of 'a list
  | Inject of Label.t * 'a
  | Case of 'a * (Pat.pat * 'a) list
  | Call of Var.t * 'a
  | Const of Var.t
  | If of 'a * 'a * 'a
  | Annot of 'a * Sort.sort

type 'b t = In of 'b t seF * 'b
type se = < loc : SourcePos.t > t

val extract : 'b t -> 'b
val shape : 'b t -> 'b t seF
val map : ('a -> 'b) -> 'a seF -> 'b seF
val print : Format.formatter -> se -> unit
```

### 3d. `EvalCtx` module — evaluation contexts

Evaluation contexts accumulate let-bindings during pattern match compilation.

```ocaml
(* evalCtx.mli *)
type t =
  | Hole
  | Let of Var.t * CoreExpr.ce * t

val fill : t -> CoreExpr.ce -> CoreExpr.ce
val extend : t -> Var.t -> CoreExpr.ce -> t
val print : Format.formatter -> t -> unit
```

`fill` applies the context to a core expression (the Fill judgement in the spec).
`extend` appends a let-binding at the innermost position of the context.


## Phase 4: Context and signature extensions

### 4a. Extend `Context` — spec variable entries

The context Γ must now distinguish computational and spec variable bindings.

**Option A (recommended):** Replace the entry type with a tagged union:

```ocaml
type tag = Comp | Spec

type entry = { tag : tag; typ : ... }
```

Where computational variables carry `Typ.ty` and spec variables carry `Sort.sort`.
Since computational types and assertion sorts are different types, we need a
sum type for the stored type:

```ocaml
type binding =
  | CompVar of Var.t * Typ.ty
  | SpecVar of Var.t * Sort.sort

type t  (* abstract *)

val extend_comp : t -> Var.t -> Typ.ty -> t
val extend_spec : t -> Var.t -> Sort.sort -> t
val lookup_comp : t -> Var.t -> Typ.ty option
val lookup_spec : t -> Var.t -> Sort.sort option
```

The existing `extend` and `lookup` become `extend_comp` and `lookup_comp`.
All current call sites use computational variables, so this is a straightforward
rename.

### 4b. Extend `Sig` — spec entries and datasort declarations

The signature Σ must now also contain:

- Spec function types: `spec f : τ → τ'`
- Spec value types: `spec f : τ`
- Datasort declarations: `sort D(a1,...,an) = { C1 : τ1 | ... | Cn : τn }`

```ocaml
type entry =
  | FunSig of { arg : Typ.ty; ret : Typ.ty; eff : Effect.t }
  | SpecFun of { arg : Sort.sort; ret : Sort.sort }
  | SpecVal of { sort : Sort.sort }
  | SortDecl of DsortDecl.t

val extend_spec_fun : t -> Var.t -> Sort.sort -> Sort.sort -> t
val extend_spec_val : t -> Var.t -> Sort.sort -> t
val extend_sort : t -> DsortDecl.t -> t

val lookup_spec_fun : t -> Var.t -> (Sort.sort * Sort.sort) option
val lookup_spec_val : t -> Var.t -> Sort.sort option
val lookup_sort : t -> Dsort.t -> DsortDecl.t option
val lookup_ctor : t -> Label.t -> (Dsort.t * DsortDecl.t) option
```

`lookup_ctor` searches all datasort declarations for a constructor label.
This is used during typechecking to resolve constructor labels to their
datasort and type.


## Phase 5: Typechecking

### 5a. `ElabM` module — elaboration monad

Elaboration threads a fresh variable supply and may fail with an error.
Rather than passing and returning the supply explicitly through every
function, we use a state+error monad.

```ocaml
(* elabM.mli *)
type 'a t

val return : 'a -> 'a t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val fail : string -> 'a t

val fresh : SourcePos.t -> Var.t t

val run : 'a t -> ('a, string) result
```

Internally, `'a t = Var.supply -> ('a * Var.supply, string) result`.
`fresh` calls `Var.fresh` on the threaded supply. `run` starts with
`Var.empty_supply`.

This monad is used by Coverage, Elaborate, and the spec typechecking
of toplevel declarations. The existing computational typechecker
(`Typecheck`) does not need it — it has no need for fresh variables.

### 5b. `CtorLookup` module — constructor resolution

Implements the `L : τ in D(τ1,...,τi) in S` judgement from the Ott spec.
Given a label L, a signature S, and the desired datasort instantiation types,
resolve the constructor's argument type with type variables substituted.

```ocaml
(* ctorLookup.mli *)
val lookup : Sig.t -> Label.t -> Sort.sort list ->
  (Sort.sort, string) result
```

### 5c. `SpecTypecheck` module — core assertion typechecking

Implements the `core_synth` and `core_check` judgements. Bidirectional
typechecking for core assertion expressions.

```ocaml
(* specTypecheck.mli *)
type typed_info = < loc : SourcePos.t; sort : Sort.sort >
type typed_ce = typed_info CoreExpr.t

val synth : Sig.t -> Context.t -> CoreExpr.ce -> (typed_ce, string) result
val check : Sig.t -> Context.t -> CoreExpr.ce -> Sort.sort -> (typed_ce, string) result
```

### 5d. `Elaborate` module — surface elaboration and pattern match compilation

This single module implements both the surface→core elaboration
(`spec_synth`/`spec_check`) and the coverage/pattern match compilation
judgement (`coverage_check`). These are mutually recursive: elaboration
calls coverage for `case`/`let`/`take` with patterns, and coverage calls
elaboration at `Cov_done` to elaborate the branch body. Placing them in
the same module lets us use OCaml's `and` for mutual recursion, avoiding
callbacks or recursive modules.

Key coverage data types:

```ocaml
(** A single binding: pattern with its sort *)
type binding = Pat.pat * Sort.sort

(** A match branch *)
type branch = {
  bindings : binding list;
  ctx : Context.t;        (* accumulated spec variable bindings G' *)
  ectx : EvalCtx.t;       (* accumulated let-bindings E *)
  body : SurfExpr.se;
}

(** Scrutinee variable list *)
type scrutinees = Var.t list
```

Coverage dispatches on the form of the leading pattern column:
- All variables → `strip_var` then recurse
- Has constructor patterns → `spec_con` to group by constructor, emit `case`
- Has tuple patterns → `expand_tup` to flatten, emit `let (...) = y`
- Empty scrutinees, single branch → `done`: elaborate body, apply eval context

Each case uses `ElabM.fresh` to generate fresh scrutinee and binding
variables as needed.

Elaboration interface:

```ocaml
(* elaborate.mli *)
val synth : Sig.t -> Context.t -> SurfExpr.se ->
  (CoreExpr.ce * Sort.sort) ElabM.t

val check : Sig.t -> Context.t -> SurfExpr.se -> Sort.sort ->
  CoreExpr.ce ElabM.t
```

The elaboration of `case`, `let`, and `take` calls `coverage_check`.
For example, `case se of { pat1 -> se1 | ... | patn -> sen }`:
1. Synthesize the scrutinee type τ'
2. Build the match matrix: `mk τ' { pat1 --> se1 || ... || patn --> sen }`
3. `let* y = ElabM.fresh` for the scrutinee variable
4. Call `coverage_check` to get the body `ce`
5. Return `let y = ce'; ce`


## Phase 6: Parsing

### 6a. Lexer extensions

New keywords:
- `spec`, `sort`, `take`, `return`, `pred`, `loc`, `case`, `of`, `Own`

New tokens:
- `==` (assertion equality, distinct from computational if needed)

Note: `case`, `not`, `if`/`then`/`else`, `let`, `true`, `false` are already
keywords. Check for conflicts.

The lexer must also handle datasort names (lowercase identifiers that resolve
to datasort declarations) and labels (uppercase identifiers that resolve to
constructors). Currently labels are already supported. Datasort names can
share the same lexical class as variables — disambiguation happens during
typechecking by checking whether a name is in the signature as a datasort.

### 6b. Parser extensions

New grammar productions:

**Sorts:**
```
sort:
  | INT                          { Sort.Int }
  | BOOL                         { Sort.Bool }
  | LOC                          { Sort.Loc }
  | PRED sort                    { Sort.Pred $2 }
  | LPAREN sort_list RPAREN      { Sort.Record $2 }
  | IDENT LPAREN sort_list RPAREN { Sort.App (Dsort.of_string $1, $3) }
  | IDENT                        { Sort.App (Dsort.of_string $1, []) }
```

**Patterns:**
```
pat:
  | IDENT                        { Pat.Var }
  | LABEL pat                    { Pat.Con ($1, $2) }
  | LPAREN pat_list RPAREN       { Pat.Tuple $2 }
```

**Surface expressions** (assertion language):
```
spec_expr:
  | spec_expr == spec_expr       { SE.Eq }
  | spec_expr && spec_expr       { SE.And }
  | NOT spec_expr                { SE.Not }
  | TAKE pat = spec_expr ; spec_expr  { SE.Take }
  | RETURN spec_expr             { SE.Return }
  | LET pat = spec_expr ; spec_expr   { SE.Let }
  | LPAREN spec_expr_list RPAREN { SE.Tuple }
  | LABEL spec_expr              { SE.Inject }
  | CASE spec_expr OF LBRACE spec_branches RBRACE  { SE.Case }
  | IDENT spec_expr              { SE.Call }
  | IDENT                        { SE.Const / SE.Var }
  | IF spec_expr THEN spec_expr ELSE spec_expr  { SE.If }
  | spec_expr : sort             { SE.Annot }
```

**Toplevel declarations:**
```
  | SPEC IDENT COLON sort ARROW sort EQ LBRACE spec_branches RBRACE prog
      { Prog.SpecFun ... }
  | SPEC IDENT COLON sort EQ spec_expr prog
      { Prog.SpecDef ... }
  | SORT IDENT LPAREN tvar_list RPAREN EQ ctor_decls prog
      { DsortDecl ... }
```

### 6c. Disambiguation: variables vs. spec constants vs. function calls

In the surface expression grammar, `f` could be a variable, a spec constant,
or a spec function applied to an argument. The parser can't distinguish these.

**Strategy:** Parse `IDENT` as a variable. Parse `IDENT spec_expr` as a
function call. During elaboration, look up the name in the context and
signature to determine which it is:
- If `spec x : τ ∈ Γ` → variable
- If `spec f : τ ∈ Σ` → constant
- If `spec f : τ → τ' ∈ Σ` → function (with argument)


## Phase 7: Prog extension

### 7a. Extend `Prog` with spec declarations

The program type needs new declaration forms:

```ocaml
type 'a decl =
  | FunDecl of { ... (* existing *) }
  | SpecFunDecl of {
      name : Var.t;
      arg_sort : Sort.sort;
      ret_sort : Sort.sort;
      branches : (Pat.pat * SurfExpr.se) list;
      loc : SourcePos.t;
    }
  | SpecDefDecl of {
      name : Var.t;
      sort : Sort.sort;
      body : SurfExpr.se;
      loc : SourcePos.t;
    }
  | SortDecl of DsortDecl.t
```

### 7b. Extend `Typecheck.check_prog`

The program typechecker must handle the new declaration forms:

- **SortDecl**: Validate the datasort declaration (no duplicate constructors,
  at least one non-recursive constructor, all referenced datasorts already
  declared). Add to the signature.

- **SpecFunDecl**: Build the match matrix from branches, invoke
  `Coverage.check` to elaborate and typecheck. Add `spec f : τ → τ'` to
  the signature (including for recursive calls within the body).

- **SpecDefDecl**: Invoke `Elaborate.check` on the body at the declared sort.
  Add `spec f : τ` to the signature.


## Phase 8: REPL integration

Extend the REPL to accept:

```
>>> sort list(a) = { Nil : () | Cons : (a, list(a)) }
sort list(a)

>>> spec length : list(int) -> int = { Nil () -> 0 | Cons (x, xs) -> 1 + length xs }
spec length : list(int) -> int

>>> spec zero : int = 0
spec zero : int
```

This requires new `parse_sort_decl`, `parse_spec_fun`, `parse_spec_def`
entry points in the parser, and corresponding dispatch in the REPL loop.


## Implementation order

The phases above are listed in dependency order. Within each phase, write
the .mli file first, then the .ml implementation, then QCheck tests.

1. **Phase 1** (Sort, Dsort, TVar) — no dependencies on existing code
2. **Phase 2** (DsortDecl, Subst) — depends on Phase 1
3. **Phase 3** (Pat, CoreExpr, SurfExpr, EvalCtx) — depends on Phases 1-2
4. **Phase 4** (Context/Sig extensions) — depends on Phases 1-2, touches existing code
5. **Phase 5** (SpecTypecheck, Elaborate) — depends on all above
6. **Phase 6** (Lexer/Parser) — depends on Phases 1-3
7. **Phase 7** (Prog extension) — depends on Phases 5-6
8. **Phase 8** (REPL) — depends on Phase 7

Phases 1-3 and Phase 6 can be developed somewhat in parallel since they
are mostly independent (Phase 6 just needs the types from 1-3).

## Key design decisions

1. **Assertion sorts are separate from computational types.** `Sort.sort` and
   `Typ.ty` are different OCaml types. There is no implicit coercion between
   them. The `pred τ` sort has no computational counterpart.

2. **Context tags.** The context distinguishes comp and spec variables to
   enforce the separation: assertion expressions can only use spec variables,
   and computational expressions can only use comp variables.

3. **Pattern match compilation is done during elaboration.** The Elaborate
   module contains both surface→core elaboration and the matrix decomposition
   algorithm from the Ott spec, as mutually recursive functions. Surface
   patterns are compiled into core case/let expressions. These live in one
   module because elaboration calls coverage (for case/let/take with patterns)
   and coverage calls elaboration (at Cov_done to elaborate branch bodies).

4. **Fresh variable generation.** Coverage needs fresh variables. The `Var`
   module has a purely functional `supply` type with
   `fresh : SourcePos.t -> supply -> t * supply`. Variables track whether
   they are user-written or generated, and always carry their binding site.
   For generated variables, the binding site is the source location of the
   subpattern they originate from. The supply is threaded via `ElabM`, a
   state+error monad that also handles failure. `ElabM.fresh` takes a
   `SourcePos.t` so that every generated variable remembers its origin.
   Error messages report both the use site and binding site of variables.

5. **Spec type enforcement.** Equality (`==`) is restricted to spec types
   (σ, no `pred`). This is checked during typechecking by calling
   `Sort.is_spec_type` on the synthesized sort.

6. **Constructor disambiguation.** Labels (uppercase) are constructors in
   both the computational and assertion languages. In assertion contexts,
   they resolve to datasort constructors. In computational contexts, they
   resolve to sum type labels. The parser doesn't need to distinguish — the
   typechecker handles this based on the expected type.
