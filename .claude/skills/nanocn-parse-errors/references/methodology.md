# The five principles

## 1. What you had to look up is what the message should have said

Every time writing a mutant or a message makes you open `lib/parser.mly`, `lib/surfExpr.ml`, `lib/refinedExpr.ml`, or any other source file, ask: *what could the message have told the user to make this unnecessary?* The answer is almost always the next sentence of the message.

**Worked example.** A user writes `Done r : Step(Int, Int) [impure]` inside a function body. This is wrong because the grammar's expression-annotation rule (`parser.mly:183`) is `expr -> seq_expr COLON sort` — no `[eff]` suffix. Before understanding this, I had to open the grammar to confirm. The fix isn't just "close the bracket"; it's to add a *caveat* to the message for state 218 (after a function branch):

> "...expected `|` to begin another branch, or `}` to end the function definition. (Note: expression-level sort annotations `e : <sort>` take a sort only — no `[<eff>]` suffix — since an expression's effect is inferred from context.)"

That caveat saves every future user the same lookup.

Other examples from the existing corpus:
- State 48 (`SORT .` awaiting name) says "expected a constructor-style name beginning with an uppercase letter" because the #1 question users had was "why can't I say `sort list(a)`?"
- State 194 (after `LET pat COLON sort EQUAL`) says "expected the right-hand side expression" AND notes that `fun` is a declaration keyword, not a lambda form, because several users wrote `let f = fun x -> ...`.
- State 111 (after `LET pat`) shows both `let x = 5;` and `let x : Int = 5;` because users stumbled over whether the annotation was optional.

## 2. A mutant that doesn't hit the target state is diagnostic

LALR(1) performs spurious reductions: when the parser reads a lookahead token, it reduces up through the precedence chain (`mul_expr → add_expr → cmp_expr → ... → seq_expr → expr`) *before* the error fires. If the lookahead is not in the FOLLOW set of `expr` in the current context, the error fires at an inner state like 148 (`add_expr -> mul_expr .`), not at the outer "expected RPAREN" or "expected RBRACE" state.

**Worked example.** I wrote `iter ((a, b) = ((x, y) : (Int * Int)) { ... }` expecting state 184 (`ITER LPAREN pat EQUAL expr . RPAREN LBRACE ... RBRACE`). It hit state 148 instead ("Expression complete: expected an operator or delimiter"). Why? After the inner `)` of the annotation, the parser is mid-expression; `{` is not in FOLLOW of expr at that position, so the error fires at state 148 before the outer state-machine position can catch up.

**The fix** is not to rewrite state 184's message; it's to pick a mutation whose lookahead token forces full reduction. `,`/`;`/`|` and various `RPAREN` tokens in inner contexts trigger the reduction chain. For state 184 specifically, `iter (a = x, b = y) { ... }` (comma-separated multi-arg, which looks like a plausible user mistake — C-style `for (int i = 0, j = 0; ...)`) does hit state 184.

**General rule.** If your mutant lands at state 148 or similar precedence-chain state, change the trailing token to `,`/`;`/`|` or wrap the context so the lookahead is something expected in the outer rule's FOLLOW set.

See `spurious-reductions.md` for the full table.

## 3. Not every state needs a message

Some error states are not reachable via user-plausible mistakes:

- **Expression-precedence scaffolding** (states 143, 144, 147, 149, 151, 155, 159–172, 175, 203, 236, 246): every one of these is inside `add_expr/mul_expr/cmp_expr/...`. Users never hit them directly because state 148's generic "Expression complete" message fires for the lookahead token that would trigger them.
- **Spurious-reduction duplicates** (e.g. states 195, 208, 214, 230): these are "expression complete inside iter/let/take" states. A user who reaches them has committed the kind of mistake that fires state 148 first.
- **REPL-only states** (e.g. 240, 244, 245, 499, 551): reachable from `repl_decl`/`repl_let`/`sort_eof` start symbols, not from parsing source files.

Leave these with PLACEHOLDER. They are sinks, not user-facing. Don't waste effort on them.

## 4. Messages are prose, not telegrams

A good message has three parts:

**Shape** (always). What was just parsed, what's expected next.

**Example** (almost always). A minimal concrete program fragment. Not formal BNF — actual source a user would type.

**Caveat** (when you learned something). A common confusion, a cross-reference to a related form, or a rule that sounds like it should apply but doesn't.

Bad:
> After `LABEL COLON`, an unexpected token appeared.

Good:
> After `<Ctor> :` in a `sort { ... }` body: expected the constructor's payload sort (e.g. `: a`, `: (a * List(a))`, `: ()` for a nullary constructor). Every constructor needs a payload — use `: ()` for ones with no arguments.

Bad messages leak grammar internals (tokens named after terminals). Good messages speak the user's language ("constructor", "payload sort", "`: ()` for nullary").

See `message-patterns.md` for more examples.

## 5. If a mistake recurs, fix the grammar, not the message

During the take-binding session, I noticed that the annotation `take x : Int = incP n` was elaborating into `Annot(incP n, Int)` — but `incP n` has sort `Pred Int`, not `Int`. The grammar was wrong: it should wrap the annotation in `Pred τ` so the user can write `Int` and have it mean "the payload".

I fixed `parser.mly`: the `take ... COLON sort EQUAL ...` rule now wraps `s` in `Sort.Pred s` before attaching the annotation. That one change enabled every future user to write the natural form, and the message for state 211 could honestly say "the annotation is the payload type".

**Rule.** If writing a message requires you to say "the natural form is wrong; you have to write this unnatural workaround instead", the grammar is probably the thing to fix. Papering over the grammar with a message is a code smell.

## A note on incremental pace

When sweeping a form:
- Write baselines first. If a baseline doesn't typecheck, you don't have a reliable stand-in for "the user who meant well".
- Write all the mutants for the form before any of the messages. You'll notice patterns — "every missing-`;` state falls through to 148", "every missing-`)` state hits the parent" — and the messages become easier to write consistently.
- After writing messages, run `verify_mutants.sh` to confirm each one lands on the new prose.
- Commit per form, not per state. A single commit "hand-craft all the `iter` states" is a unit of work someone can review.
