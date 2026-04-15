# Writing good messages

## The three-part template

```
After <shape>: expected <what>. <Example>. (<Caveat>.)
```

- **Shape**: what was just parsed. Use user-language (`<pat>`, `<sort>`, `<ctor>`), not grammar terminals (`LABEL`, `IDENT`, `pat`).
- **Expected**: list of valid continuations. If there are several, group them by shape.
- **Example**: at least one concrete program fragment showing the correct syntax. Prefer real user code over toy `a` / `b`.
- **Caveat** (optional): a cross-reference or common confusion.

## Examples from the nanoCN corpus

### Good: state 189 (after `let <pat> = <rhs> ;`)

```
After `let <pat> = <rhs>;`: expected the *body* expression —
`let ... ;` is a sequencer, so a following expression is required
(typically another `let`, or the value returned by the block). If
this was the last binding, return `()` or the value you meant.
```

Why it works: explains *why* `;` requires a body ("it's a sequencer"), flags the exact mistake the user is likely making ("I meant to end the function"), and gives them two outs.

### Good: state 218 (after a function branch)

```
After a function branch `<pat> -> <body>`: expected `|` to begin
another branch, or `}` to end the function definition. (Note:
expression-level sort annotations `e : <sort>` take a sort only —
no `[<eff>]` suffix — since an expression's effect is inferred
from context.)
```

Why it works: the shape is clear, the two valid continuations are listed, and the caveat addresses the single most common mistake (putting `[impure]` on an expr annotation) that was discovered from the corpus.

### Good: state 83 (after a LABEL in a pattern)

```
After a constructor name in a pattern: expected an argument pattern
— an identifier (`x`), unit (`()`), or a parenthesised pattern
(`(x, y)`). Nullary constructors must be written `L ()`, not bare
`L`.
```

Why it works: three concrete examples of what a valid argument pattern looks like, plus the specific rule that catches beginners.

### Good: state 69 (after `FUN IDENT COLON`)

```
After `fun <name> :`: expected the argument sort, e.g. `Int`,
`Bool`, `(Int * Bool)`, `List(Int)`. Functions are unary; for
multiple arguments, use a tuple sort (e.g. `(Int * Int) -> Int`).
```

Why it works: four concrete sorts, and the unary-arity rule that trips up everyone coming from a curried language.

### Bad: literal PLACEHOLDER

```
PLACEHOLDER: after `TYPE LABEL LPAREN <type-params> RPAREN`, an
unexpected token appeared.
```

Why it fails: grammar terminals leak through (`TYPE LABEL LPAREN`, `<type-params>`). The message doesn't say what *should* come next. There's no example. It's grammar-verbose but user-vague.

### Bad: too terse

```
After `=`: expected `{`.
```

Why it fails: works for the easy case, useless for the confused case. No example, no guidance about what `{ ... }` contains. The user might wonder "why `{`?" — answer that.

### Bad: leaks Menhir internals

```
After `separated_nonempty_list(COMMA, sort) -> sort .`, expected
COMMA or RPAREN.
```

Why it fails: users don't know what a `separated_nonempty_list` is. They know what "a list of sort arguments" is.

## Prose patterns that work

- **"After X: expected Y."** Minimal opener, universal shape. Lead with it.
- **"X is a sequencer"** / **"the body follows"** — cross-reference the role of tokens when they're ambiguous. `;` is a huge source of confusion in nanoCN; explicitly calling out that it sequences (rather than terminates) pays off repeatedly.
- **"Use `: ()` for nullary"** — every time you need `: ()`, mention it. Users don't discover this idiom without being told.
- **"<form> is unary; use a tuple for multiple arguments"** — mention it every time a function signature is involved, because the mistake recurs across `fun`, `rfun`, `take`, iter binder, etc.
- **"`[<eff>]` is not allowed here"** — mention it whenever a sort annotation is involved at the expression level.
- **"`<form>` has shape `<literal-program>`"** — for unfamiliar forms (refined `iter`, `let core[a]`, DepRes binder `[res] x : (do y = ...)`), show the full shape before discussing error details. Users have no template to compare against otherwise.

## Prose patterns to avoid

- Terminal names (`LABEL`, `IDENT`, `SEMICOLON`, ...) except when they are themselves user-visible (`=`, `:`, `,`). Translate `LABEL` → "constructor-style name", `IDENT` → "lowercase identifier", `COLON` → "`:`".
- All-caps "MUST", "NEVER" without explanation. If the rule has a reason, state it. If not, restate it in lowercase.
- Linking to documentation ("see doc/syntax.ott"). The user is reading this in a terminal in the middle of debugging — they won't navigate.
- Multiple paragraphs. Aim for one paragraph; two if there's a significant caveat.
- Jokes, personality, emoji. The message shows up when something's wrong. Keep it factual.

## Anchoring messages in parser.messages

Each state's entry in `lib/parser.messages` has the shape:

```
<test sentence that hits this state>
##
## Ends in an error in state: <N>.
##
## <items — grammar productions with dot markers>
##
## The known suffix of the stack is as follows:
## <suffix>
##

<the message>

<blank line>
```

To replace a PLACEHOLDER message:
1. Find the state via `grep -n "state: <N>\." lib/parser.messages`.
2. The PLACEHOLDER line follows the blank line after `## The known suffix ... ##`.
3. Replace the PLACEHOLDER line — and only the PLACEHOLDER line — with the new message.
4. The message can span multiple lines; Menhir treats blank-line-to-blank-line as one message.

Use state-specific anchors for the `Edit` tool: the entire `## Ends in an error in state: N. ##` block plus the items is unique across the file; the PLACEHOLDER line alone is often duplicated across states with similar suffixes (e.g., multiple states have suffix `LPAREN`).
