# FAQ — Questions and Answers about the nanocn codebase

## Phase 1: After reading doc/design.md and doc/syntax.ott

### Q1: What is nanocn?
**A:** A small implementation of a CN+Fulminate-style verification system. It has a computational core language (SSA-inspired, from Jad Ghalayini), a refinement/assertion layer (inspired by Dhruv Makwana's PhD), and a minimal SMT-based typechecker. The refinement layer has multiple levels: pure ghost terms, predicate expressions (separation logic), resource terms, and commands. There is also an interpreter with two modes (ignoring ghost state, or evaluating it alongside the program a la Fulminate).

### Q2: What is the relationship between the computational language and the assertion language?
**A:** They are separate but connected. The computational language has types `A, B, C` (int, bool, ptr, records, sums) with effects (pure/impure). The assertion language has sorts `τ` (int, bool, loc, tuples, datasort applications, pred τ). Spec types `σ` are the subset of sorts without `pred`. The two type systems share some base types (int, bool) but are structurally distinct — `Typ.ty` and `Sort.sort` are different OCaml types.

### Q3: What is the `stau <:: tau` subrule in the Ott spec?
**A:** It defines spec types (`stau`/`σ`) as the subset of assertion sorts that exclude `pred`. Concretely, spec types are: int, bool, loc, tuples of spec types, and datasort applications of spec types. Equality (`==`) in the assertion language is only defined on spec types — you cannot compare predicates for equality.

### Q4: How does the pattern match compilation work for spec expressions?
**A:** Surface assertion expressions (`se`) have patterns in `let`, `take`, and `case`. These are compiled to core assertion expressions (`ce`) which only have variable bindings. The compilation uses a matrix-style algorithm (the `Jmatch` judgements: `coverage`, `strip_var`, `spec_con`, `expand_tup`, `has_con`, `has_tup`). The key data structure is `mbranches` — a list of branches, each carrying pattern bindings, an accumulated context (`G'`), an evaluation context (`E`), and a body expression. The algorithm dispatches on the form of the leading pattern: all-variables, constructor patterns, or tuple patterns.

### Q5: What are evaluation contexts (`E`) used for?
**A:** Evaluation contexts accumulate `let`-bindings during pattern compilation. When a variable pattern is processed, a `let x = y` binding is added to the eval context. When compilation finishes (the `done` case), the eval context is "filled" around the body expression — essentially wrapping it in all the accumulated let-bindings.

### Q6: How do `take` and `return` relate to separation logic?
**A:** `return ce` introduces a predicate resource (checking rule: `ce <== tau` implies `return ce <== pred tau`). `take x = ce1; ce2` binds the result of a predicate expression (synthesizing `pred tau`) to a variable `x : tau`, then continues with `ce2` in the extended context. This is a monadic bind for the predicate sort — the `pred` type constructor has monadic structure.

### Q7: How are spec functions checked at the top level?
**A:** Two forms: (1) Clausal spec functions `spec f : τ → τ' = { pat1 → se1 | ... }` — the branches are assembled into a match matrix (`mk τ { ... }`), then coverage-checked against the return sort `τ'`. The function name is added to the signature *before* checking the body (for recursion). (2) Simple spec definitions `spec f : τ = se` — the body `se` is checked against `τ`, and `spec f : τ` is added to the signature.

### Q8: What is the `mk` operation on match branches?
**A:** `mk tau { pat1 --> se1 || ... || patn --> sen }` initializes the match matrix. Each branch `pati --> sei` becomes `pati : tau, nobinds --> [empty][hole] sei` — a branch with a single binding (the pattern at its declared sort), no accumulated context, a trivial evaluation context (just a hole), and the surface body expression.

### Q9: How does constructor lookup work with parameterized datasorts?
**A:** The `Ctor_lookup` rule: given `L : τ in D(τ1,...,τi) in S`, look up the datasort declaration `sort D(a1,...,ai)`, find the raw constructor type `τ'` for label `L`, then substitute the actual type arguments `τ1/a1,...,τi/ai` into `τ'` to get the instantiated type `τ`.

### Q10: What is the scoping rule for pure vs. impure functions?
**A:** Pure functions do NOT have access to their own binding during typechecking (rule `Prog_purefun`: the function `f` is NOT in `S` when checking the body). Impure functions DO have access (rule `Prog_impurefun`: `f`'s signature is in `S` when checking the body). This prevents unbounded recursion in the pure fragment.

### Q11: What is the `fill` operation on evaluation contexts?
**A:** `fill E ce` plugs `ce` into the hole of evaluation context `E`. `fill hole ce = ce`, and `fill (let x = ce''; E) ce = let x = ce''; fill E ce`. It's used in the `done` case of coverage to wrap the compiled body in accumulated let-bindings.


## Phase 2: After reading .mli files in lib/

### Q12: How do syntax trees carry location information?
**A:** Using the shape functor pattern. For each syntax tree, there's a parameterized shape type (e.g., `'a exprF`), a recursive wrapper `'b t = In of 'b t exprF * 'b`, and a concrete type `expr = < loc : SourcePos.t > t`. The `'b` parameter is an object with a `loc` method. Typed trees extend this: `typed_info = < loc : SourcePos.t; ctx : Context.t; typ : Typ.ty; eff : Effect.t >`.

### Q13: Why do `Dsort` and `Label` both validate strings, but in different ways?
**A:** Labels must start with an uppercase letter (constructors: `Done`, `Next`, `Cons`). Datasort names must start with a lowercase letter (type names: `list`, `tree`). Both require at least some characters and restrict to alphanumeric + `_` + `'`. They are separate types to enforce the distinction at the type level.

### Q14: Why does `TVar.of_string` not return a result type?
**A:** Unlike `Dsort.of_string` and `Label.of_string`, `TVar.of_string` does no validation — it's just `fun s -> s`. Type variables are simple lowercase identifiers and the validation is presumably done at the parser level.

### Q15: How does `Context` currently work, and what changes for spec variables?
**A:** Currently `Context.t = (Var.t * Typ.ty) list` — it only stores computational variables. The assertion-implementation plan calls for extending it with tagged entries: `CompVar of Var.t * Typ.ty | SpecVar of Var.t * Sort.sort`, with separate `extend_comp`/`lookup_comp` and `extend_spec`/`lookup_spec` operations.

### Q16: How does `Sig` currently work, and what changes for spec functions/datasorts?
**A:** Currently `Sig.t = (Var.t * entry) list` where `entry = { arg : Typ.ty; ret : Typ.ty; eff : Effect.t }` — only computational function signatures. The plan calls for extending it to also store spec function types, spec value types, and datasort declarations.

### Q17: Why does `Prog` use `'a decl` and `'a t` parameterized over the body type?
**A:** To allow the same program structure to hold both untyped (`Expr.expr`) and typed (`typed_expr`) expressions. After typechecking, `Prog.map` transforms an `Expr.expr Prog.t` into a `typed_expr Prog.t`.

### Q18: Is there an interpreter yet?
**A:** No. The design document mentions two interpreter modes (ignoring ghost state, evaluating it), but only parsing and typechecking are implemented so far.

### Q19: What parser technology is used?
**A:** Menhir for the parser (.mly), sedlex for the lexer (.ml with `[%sedlex.regexp? ...]` PPX). They are connected via the `Parse` module which uses `MenhirLib.Convert.Simplified.traditional2revised` to bridge the two.

### Q20: How is `Var.fresh` used in practice?
**A:** `Var.fresh : SourcePos.t -> supply -> t * supply` creates a `Generated n` variable. The supply is a simple `int` counter. This is threaded through the `ElabM` monad (not yet implemented) during pattern match compilation. Generated variables print as `_v0`, `_v1`, etc.


## Phase 3: After reading .ml implementations

### Q21: Why does `Typecheck.check` handle `Annot` in both synth and check mode?
**A:** In synth mode (`synth`), `Annot(e, ty, eff)` checks `e` against `ty` at `eff` and returns `ty, eff`. In check mode (`check`), `Annot(e, ty, eff)` checks `e` against the annotation's `ty` and `eff`, then verifies the annotation matches the expected type/effect. This provides redundant but consistent handling for both directions.

### Q22: How does the typechecker enforce the pure/impure function scoping rule?
**A:** In `check_decl`: for `Impure` functions, the function's own signature is added to `sig_for_body`; for `Pure` functions, it is NOT added. This means a pure function body can't call itself (it's not in scope), preventing recursion in the pure fragment.

### Q23: How are binary operators parsed?
**A:** The parser desugars infix operators to primitive applications. E.g., `x + y` becomes `App(Add, Tuple [x; y])`. The printer reverses this: `App(Add, In(Tuple [e1; e2], _))` prints as `e1 + e2`.

### Q24: Why does `Sort.compare` ignore the location annotation?
**A:** `compare_sort (In (s1, _)) (In (s2, _))` discards the `'b` annotation and compares only the shape `s1` and `s2`. This is by design — sorts should be compared structurally, not by source location. Same pattern is used in `Typ.compare`.

### Q25: What is the current state of implementation vs. the plan?
**A:** Implemented: Phase 1 foundation types (`Dsort`, `TVar`, `Sort` modules) and the extended `Var` module with binding sites and fresh variable generation. The computational language is fully implemented: parsing, typechecking, REPL. Not yet implemented: Phases 2-8 (DsortDecl, Subst, Pat, CoreExpr, SurfExpr, EvalCtx, Context/Sig extensions, SpecTypecheck, Coverage, Elaborate, parser extensions for assertion language, Prog extension, REPL extensions, ElabM).

### Q26: How does the parser distinguish function calls from other expressions?
**A:** In `app_expr`, the rule `f = ident_var; e = simple_expr` matches a lowercase identifier followed by a simple expression, producing `Expr.Call(f, e)`. Labels (uppercase) followed by an expression produce `Expr.Inject`. State primitives (`Set`, `Get`, `New`, `Del`) are matched specially with type brackets.

### Q27: What's the relationship between the `dune` build files and the module structure?
**A:** Not yet examined (dune files were not read in detail), but the project uses: `lib/` for the core library modules, `bin/` for the executable (`main.ml`), and `test/` for tests (`test_main.ml`). The parser needs `menhir` and the lexer needs `sedlex` as build dependencies.

### Q28: Are there any discrepancies between the Ott spec and the implementation?
**A:** A few observations:
- The Ott spec includes `sort D(a1,...,ai) = {L1:τ1 | ... | Ln:τn} in S` in the signature, but no `SortDecl` entry exists in `Sig.t` yet.
- The Ott spec includes `spec x : τ in G` and `spec f : χ in S`, but `Context` and `Sig` don't yet have spec entries.
- The Ott spec has `Prog_specfun` and `Prog_specdef` rules, but `Prog.t` only has `FunDecl`-style declarations.
- These are all consistent with the plan being only partially implemented (Phase 1 complete, Phases 2-8 pending).

### Q29: What does `check_branches` in the typechecker actually verify?
**A:** It checks that each branch label exists in the sum type, extends the context with the bound variable at the label's type, and checks the branch body against the target type and effect. It does NOT verify exhaustiveness — it relies on a length check (`List.compare_lengths branches cases <> 0`) done before the call.

### Q30: How does the REPL distinguish between declarations, let-bindings, and expressions?
**A:** By prefix matching on the input string: `starts_with_prefix line "fun "` → declaration, `starts_with_prefix line "let "` → let-binding, otherwise → expression. This is simple but fragile — it wouldn't handle leading whitespace or comments before keywords.
