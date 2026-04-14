# SMT encoding 

## Building the S-expression library 

### SMT-LIB syntax

#### Lexical syntax

Source files are Unicode in any 8-bit encoding (e.g. UTF-8). Most tokens are
restricted to printable US-ASCII (32–126); non-English letters from 128 up are
permitted only inside string literals, quoted symbols, and comments.

Whitespace and comments separate tokens but carry no semantic content:

    whitespace ::= [ \t\n\r]+
    comment    ::= ; [^\n\r]*

Numeric and string literals:

    digit       ::= [0-9]
    letter      ::= [a-zA-Z]
    numeral     ::= 0 | [1-9][0-9]*
    decimal     ::= <numeral> \. [0-9]* <numeral>
    hexadecimal ::= #x [0-9a-fA-F]+
    binary      ::= #b [01]+
    string      ::= " ( [^"] | "" )* "

Inside a `<string>` the only escape sequence is `""`, which denotes a single
`"`. Sequences like `\n` are *not* escapes — they stand for their literal
characters. String literals may contain raw line breaks.

Reserved words are tokenised as themselves and may not appear as simple
symbols:

    reserved ::= BINARY | DECIMAL | HEXADECIMAL | NUMERAL | STRING
               | _ | ! | as | lambda | let | exists | forall | match | par
               | <command_name>

where `<command_name>` ranges over every command in the script language. Per
Figure 3.7 of the standard, the full set is:

    assert                  declare-sort-parameter   get-assignment       get-value
    check-sat               define-const             get-info             pop
    check-sat-assuming      define-fun               get-model            push
    declare-const           define-fun-rec           get-option           reset
    declare-datatype        define-funs-rec          get-proof            reset-assertions
    declare-datatypes       define-sort              get-unsat-assumptions set-info
    declare-fun             echo                     get-unsat-core       set-logic
    declare-sort            exit                     get-assertions       set-option

Symbols come in two flavours. A simple symbol is a non-empty sequence of
letters, digits, and the punctuation characters `~ ! @ $ % ^ & * _ - + = < > . ? /`,
that does not start with a digit and is not a reserved word. A quoted symbol
is any run of printable/whitespace characters delimited by `|`, containing
neither `|` nor `\`.

    sym_punct     ::= [~!@$%^&*_+=<>.?/-]
    sym_char      ::= <letter> | <digit> | <sym_punct>
    simple_symbol ::= <sym_char>+   ; not starting with a digit, not a reserved word
    quoted_symbol ::= \| [^|\\]* \|
    symbol        ::= <simple_symbol> | <quoted_symbol>

A keyword is a colon followed by a simple symbol; it lives in its own
namespace, used for attribute and option names:

    keyword ::= : <simple_symbol>

Punctuation tokens:

    lparen ::= (
    rparen ::= )

#### S-expression grammar

Every SMT-LIB expression is an S-expression. Special constants and the
five token classes (symbol, reserved, keyword, plus the parenthesised form)
make up the whole grammar:

    spec_constant ::= <numeral> | <decimal> | <hexadecimal> | <binary> | <string>

    s_expr ::= <spec_constant>
             | <symbol>
             | <reserved>
             | <keyword>
             | ( <s_expr>* )

### An OCaml s-exp library

1. Define an AST for S-expressions. The atomic leaves of an s-expression
   correspond one-for-one to the non-paren token classes from §3.1, so we
   model them as a single sum type. The reserved-word class becomes its own
   enumeration (no payload — each reserved word is a distinct constructor)
   so that pattern matches over the SMT-LIB surface forms cannot mistype a
   keyword.

   ```ocaml
   (* Reserved words from §3.1 of the SMT-LIB 2.7 standard. *)
   type reserved =
     (* meta-spec constants *)
     | R_BINARY | R_DECIMAL | R_HEXADECIMAL | R_NUMERAL | R_STRING
     (* punctuation / binders / qualifiers *)
     | R_underscore       (* _ *)
     | R_bang             (* ! *)
     | R_as
     | R_lambda
     | R_let
     | R_exists
     | R_forall
     | R_match
     | R_par
     (* script command names from Figure 3.7 *)
     | R_assert
     | R_check_sat
     | R_check_sat_assuming
     | R_declare_const
     | R_declare_datatype
     | R_declare_datatypes
     | R_declare_fun
     | R_declare_sort
     | R_declare_sort_parameter
     | R_define_const
     | R_define_fun
     | R_define_fun_rec
     | R_define_funs_rec
     | R_define_sort
     | R_echo
     | R_exit
     | R_get_assertions
     | R_get_assignment
     | R_get_info
     | R_get_model
     | R_get_option
     | R_get_proof
     | R_get_unsat_assumptions
     | R_get_unsat_core
     | R_get_value
     | R_pop
     | R_push
     | R_reset
     | R_reset_assertions
     | R_set_info
     | R_set_logic
     | R_set_option

   (* Atoms — the leaves of an s-expression. Numeric literals are kept as
      verbatim digit strings (no leading #x / #b for the based variants),
      so the lexer commits to no numeric representation; clients can parse
      them into Z.t / Q.t on demand.

      For [Symbol], simple and quoted symbols are unified into a single
      constructor holding the canonical contents (per §3.1, |abc| ≡ abc).
      For [Keyword], the leading colon is stripped. For [String], the
      contents are stored unescaped (the doubled-quote escape ""→" has
      already been applied). *)
   type atom =
     | Numeral     of string
     | Decimal     of string
     | Hexadecimal of string
     | Binary      of string
     | String      of string
     | Symbol      of string
     | Keyword     of string
     | Reserved    of reserved

   type sexp =
     | S of atom
     | Nil
     | Cons of sexp * sexp
   ```

   The `Atom` module exposes `type t = atom`, a `compare : t -> t -> int`,
   and `print : Format.formatter -> t -> unit` that round-trips back to
   valid SMT-LIB concrete syntax (re-quoting symbols whose contents are not
   a legal `<simple_symbol>`, re-escaping `"` inside strings, re-prefixing
   `#x` / `#b` on based numerals, and re-prefixing `:` on keywords).

2. Define a Sedlex lexer for the lexical syntax above, producing symbols, open
   parens, and close parens. Accept smt-lib comments, but elide them.

3. Define a Menhir grammar following the above grammar. 

