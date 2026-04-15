# Spurious reductions — why your mutant hits the wrong state

## The phenomenon

When the parser reads a lookahead token and it's time to decide what to do next, it might:

1. **Shift** the token into the stack, or
2. **Reduce** the top of the stack via some rule that has the token in its FOLLOW set.

For expression parsing, the stack often contains `mul_expr`, `add_expr`, `cmp_expr`, `eq_expr`, `and_expr`, `or_expr`, `seq_expr`, `expr` nested across many rule positions. When a bad token arrives, the parser reduces up through these chain rules — *before* it errors — because each intermediate nonterminal's FOLLOW set is broader than the outer rule's.

The result: you expected the error to fire at state N (say, "after iter's init expr, expecting RPAREN"), but it fires at state 148 ("after mul_expr, expecting operator or delimiter") because the lookahead token triggered reduction up to `mul_expr` but no further.

## Diagnostic pattern

If you wrote a mutant expecting state N and got state 148 (or another precedence state like 203):

1. Look at state N's item: what token is expected?
2. If the expected token is `RPAREN`, `RBRACE`, `RBRACKET`, `SEMICOLON`, `COMMA`, or `BAR`, the lookahead you provided wasn't in FOLLOW-of-expr at the parser's current position.
3. Change the trailing bad token to one that **is** in FOLLOW-of-expr: `,`, `;`, `|`, or the right closing bracket for an inner context.

## Tokens that force full reduction

Roughly:

| Token | Forces reduction to... | Hits state... |
|---|---|---|
| `,` (in tuple/list context) | `expr` or `pat` | Whatever expects `,` or closer |
| `;` (in let/take/ seq) | `expr` then `SEMICOLON seq_expr` | The expected state after `;` |
| `)` (closing paren) | `expr` then `RPAREN` | Outer expecting state |
| `}` (closing brace) | `expr` then `RBRACE` | Outer expecting state |
| `|` (branch separator) | `expr` then `BAR` | Branch-list state |

If you need to trigger state N specifically, pick a mutation where the lookahead token is something state N explicitly expects (or explicitly rejects) — not a mid-chain operator or arbitrary identifier.

## Worked examples

### Missing closing paren inside iter — state 184 vs state 148

**Bad mutation** (hits state 148):
```
iter ((a, b) = ((x, y) : (Int * Int)) { ... }
```
After `Int * Int))`, we're mid-expression. The `{` triggers mul_expr-level error.

**Good mutation** (hits state 184):
```
iter (a = x, b = y) { ... }
```
After `0` reduces to expr, comma forces the reduction chain all the way up, then `,` is rejected at state 184 (which wants RPAREN).

### Missing closing brace inside iter body — state 187 vs state 218

**Bad mutation** (hits wrong state):
```
iter (...) {
  body

}  <- the `}` closes the iter; next `}` closes the fun, next token from next decl confuses branch list
```
If you omit just the iter's `}`, the outer function's `}` absorbs it; the error fires later on an unrelated token.

**Good mutation** (hits state 187):
```
iter (...) { body; }
```
The stray `;` after `body` is rejected at state 187 (which wants RBRACE only).

### Missing `;` after a `let` — state 189 vs state 148

**Bad mutation** (hits state 148):
```
let x = 5 let y = 6;
```
After `5`, the parser could reduce to mul_expr, then sees `let`. `let` is not in FOLLOW of mul_expr (LET is a starter of seq_expr, not a continuation), so state 148 fires. The user is told "expected an operator or delimiter" — which is *accurate* but less specific than "expected `;`".

**Better alternative**: for missing-`;` cases, either accept state 148's generic message (which is good enough), or mutate to something that forces reduction to seq_expr. In practice, state 189 is only reliably hit from `let x = 5;` at end of function (so the lookahead is `}`) — which is a different mistake.

## Reachability as a category

Categorise each state as:
- **Reachable via direct mistake**: a specific simple typo lands here. Always write a message.
- **Reachable only via spurious reduction**: fires after reducing through precedence chain. Often state 148 or similar catches these first.
- **Unreachable from source files**: REPL-only or dead states.

`classify_states.sh` + `verify_mutants.sh` together let you triage. Running `verify_mutants.sh` after writing a mutant tells you immediately whether the state you expected actually fires.

## When to leave a state with PLACEHOLDER

Leave it alone if:
- The state is in the precedence chain (`add_expr/mul_expr/cmp_expr/and_expr/or_expr/eq_expr`) AND state 148 handles the most plausible lookahead.
- The state is only reachable via spurious reduction after a complete inner expression (`WARNING: This example involves spurious reductions` in the `.messages` trace).
- The state is REPL-only (`repl_decl`, `repl_let`, `sort_eof` start symbols).

Don't waste effort writing messages no user will see. Focus on the ~40% of states that are reachable via real mistakes.
