/**
 * @file nanoCN grammar for tree-sitter
 * @author nanoCN contributors
 * @license MIT
 *
 * Translation of `lib/parser.mly`. One unified grammar accepts both
 * `.cn` (surface) and `.rcn` (refined) programs — the LSP backend is
 * the authority on which forms are valid in which file.
 *
 * Design notes:
 *   - The precedence cascade (or → and → eq → cmp → add → mul → app)
 *     is preserved structurally as nested rules, mirroring parser.mly.
 *     This is more verbose than using `prec.left` with a flat
 *     expression rule, but gives a direct 1:1 mapping that is easy to
 *     verify against the Menhir grammar.
 *   - Reserved lowercase keywords (`let`, `case`, `iter`, …) are
 *     extracted by tree-sitter's `word:` mechanism via `lower_ident`.
 *   - Reserved uppercase names (`Int`, `Bool`, `Ptr`, `Pred`, `Eq`,
 *     `Set`, `Get`, `New`, `Del`, `Own`) compete with the general
 *     `upper_label` regex at the lexer level. tree-sitter's
 *     "string literal > regex" rule at equal length resolves this.
 *   - Hyphenated keywords (`open-ret`, `open-take`, `make-ret`,
 *     `make-take`) are ordinary string tokens — tree-sitter's default
 *     tokenizer handles them as single tokens because the grammar
 *     explicitly mentions them.
 *   - Every decl start keyword (`fun`, `rfun`, `sort`, `type`, `main`)
 *     is reserved and cannot appear inside an expression — so a
 *     keyword-split error-recovery strategy is always sound. The
 *     compiler's Phase-2 parser refactor (a separate piece of work)
 *     relies on this property, but this grammar does not depend on it.
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  annot: 1,    // e : sort   (loosest — applied last)
  or:    2,    // ||
  and:   3,    // &&
  eq:    4,    // ==
  cmp:   5,    // <  <=  >  >=
  add:   6,    // +  -
  mul:   7,    // *  /
  inject:8,    // Label expr  (right-assoc, prefix)
  app:   9,    // f arg
  atom:  10,
};

// Helpers for repetition with separator.
function sepBy1(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)));
}
function sepBy(sep, rule) {
  return optional(sepBy1(sep, rule));
}

export default grammar({
  name: "nanocn",

  word: $ => $.lower_ident,

  extras: $ => [
    /\s/,
    $.comment,
  ],

  conflicts: $ => [
    // main_decl has a surface form (`: _sort`) and a refined form
    // (`: pf_sort`). A bare `()` is a unit_sort, and an empty
    // `pf_sort` (empty separated_list) is also a valid pf_sort, so
    // `main : ()` is ambiguous. GLR explores both; downstream body
    // usually disambiguates. When the body is also ambiguous (e.g.
    // `main : () [impure] = ()` in a `.rcn` file), either parse is
    // syntactically valid — the semantic layer decides.
    [$.unit_sort, $.pf_sort],
  ],

  rules: {

    // ======================================================================
    // Entry
    // ======================================================================

    source_file: $ => seq(
      repeat($._decl),
      $.main_decl,
    ),

    // ======================================================================
    // Declarations
    // ======================================================================

    _decl: $ => choice(
      $.fun_decl,
      $.rfun_decl,
      $.sort_decl,
      $.type_decl,
    ),

    // `fun` covers two forms: the surface fun-decl (with branches) and
    // the refined surface-embedded fun-decl (with single-param header
    // and a body expression). They disambiguate on the first token
    // after the name.
    fun_decl: $ => choice(
      seq(
        'fun',
        field('name', $.lower_ident),
        ':',
        field('arg_sort', $._sort),
        '->',
        field('ret_sort', $._sort),
        '[', field('eff', $.eff_level), ']',
        '=',
        '{',
        field('branches', sepBy1('|', $.branch)),
        '}',
      ),
      seq(
        'fun',
        field('name', $.lower_ident),
        '(',
        field('param', $.lower_ident),
        ':',
        field('arg_sort', $._sort),
        ')',
        '->',
        field('ret_sort', $._sort),
        '[', field('eff', $.eff_level), ']',
        '=',
        field('body', $._expr),
      ),
    ),

    rfun_decl: $ => seq(
      'rfun',
      field('name', $.lower_ident),
      field('domain', $.pf_domain),
      '->',
      field('codomain', $.pf_sort),
      '[', field('eff', $.eff_level), ']',
      '=',
      field('body', $._crt_expr),
    ),

    sort_decl: $ => seq(
      'sort',
      field('name', $.upper_label),
      optional(seq(
        '(',
        field('params', sepBy1(',', $.lower_ident)),
        ')',
      )),
      '=',
      '{',
      field('ctors', sepBy1('|', $.ctor_decl)),
      '}',
    ),

    type_decl: $ => seq(
      'type',
      field('name', $.upper_label),
      optional(seq(
        '(',
        field('params', sepBy1(',', $.lower_ident)),
        ')',
      )),
      '=',
      '{',
      field('ctors', sepBy1('|', $.ctor_decl)),
      '}',
    ),

    ctor_decl: $ => seq(
      field('label', $.upper_label),
      ':',
      field('sort', $._sort),
    ),

    branch: $ => seq(
      field('pat', $._pat),
      '->',
      field('body', $._expr),
    ),

    main_decl: $ => choice(
      // surface main
      seq(
        'main', ':',
        field('ret_sort', $._sort),
        '[', field('eff', $.eff_level), ']',
        '=',
        field('body', $._expr),
      ),
      // refined main
      seq(
        'main', ':',
        field('pf', $.pf_sort),
        '[', field('eff', $.eff_level), ']',
        '=',
        field('body', $._crt_expr),
      ),
    ),

    eff_level: _ => choice('pure', 'impure', 'spec'),

    // ======================================================================
    // Sorts
    // ======================================================================

    _sort: $ => choice(
      $.pred_sort,
      $.ptr_sort,
      $._atomic_sort,
    ),

    pred_sort: $ => seq('Pred', field('inner', $._atomic_sort)),
    ptr_sort:  $ => seq('Ptr',  field('inner', $._atomic_sort)),

    _atomic_sort: $ => choice(
      $.unit_sort,
      $.paren_sort,
      $.tuple_sort,
      $.int_sort,
      $.bool_sort,
      $.app_sort,
      $.name_sort,
      $.tvar_sort,
    ),

    unit_sort:  _ => seq('(', ')'),
    paren_sort: $ => seq('(', $._sort, ')'),
    tuple_sort: $ => seq(
      '(',
      $._sort,
      '*',
      sepBy1('*', $._sort),
      ')',
    ),
    int_sort:   _ => 'Int',
    bool_sort:  _ => 'Bool',
    app_sort:   $ => seq(
      field('name', $.upper_label),
      '(',
      field('args', sepBy1(',', $._sort)),
      ')',
    ),
    name_sort:  $ => $.upper_label,
    tvar_sort:  $ => $.lower_ident,

    // ======================================================================
    // Patterns
    // ======================================================================

    _pat: $ => choice($.ctor_pat, $._atomic_pat),

    ctor_pat: $ => seq(
      field('label', $.upper_label),
      field('arg', $._atomic_pat),
    ),

    _atomic_pat: $ => choice(
      $.var_pat,
      $.unit_pat,
      $.paren_pat,
      $.tuple_pat,
    ),

    var_pat:   $ => $.lower_ident,
    unit_pat:  _ => seq('(', ')'),
    paren_pat: $ => seq('(', $._pat, ')'),
    tuple_pat: $ => seq(
      '(', $._pat, ',', sepBy1(',', $._pat), ')',
    ),

    // ======================================================================
    // Surface expressions (expr → seq_expr → or_expr → … → simple_expr)
    // ======================================================================

    _expr: $ => choice(
      $.annot_expr,
      $._seq_expr,
    ),

    annot_expr: $ => prec.right(PREC.annot, seq(
      field('inner', $._seq_expr),
      ':',
      field('sort', $._sort),
    )),

    _seq_expr: $ => choice(
      $.let_expr,
      $.take_expr,
      $.return_expr,
      $.fail_expr,
      $.case_expr,
      $.iter_expr,
      $.if_expr,
      $._or_expr,
    ),

    let_expr: $ => choice(
      seq(
        'let', field('pat', $._pat),
        ':', field('sort', $._sort),
        '=', field('value', $._expr),
        ';', field('body', $._seq_expr),
      ),
      seq(
        'let', field('pat', $._pat),
        '=', field('value', $._expr),
        ';', field('body', $._seq_expr),
      ),
    ),

    take_expr: $ => choice(
      seq(
        'take', field('pat', $._pat),
        ':', field('sort', $._sort),
        '=', field('value', $._expr),
        ';', field('body', $._seq_expr),
      ),
      seq(
        'take', field('pat', $._pat),
        '=', field('value', $._expr),
        ';', field('body', $._seq_expr),
      ),
    ),

    return_expr: $ => seq('return', field('value', $._app_expr)),
    fail_expr:   _ => 'fail',

    case_expr: $ => seq(
      'case', field('scrutinee', $._app_expr), 'of',
      '{',
      field('branches', sepBy1('|', $.case_branch)),
      '}',
    ),

    case_branch: $ => seq(
      field('pat', $._pat),
      '->',
      field('body', $._expr),
    ),

    iter_expr: $ => seq(
      'iter', '(',
      field('binder', $._pat), '=', field('init', $._expr),
      ')',
      '{', field('body', $._expr), '}',
    ),

    if_expr: $ => seq(
      'if',   field('cond', $._expr),
      'then', field('then', $._expr),
      'else', field('else', $._seq_expr),
    ),

    // Binary-op precedence cascade. Left-associative at every level.
    _or_expr: $ => choice(
      $.or_expr,
      $._and_expr,
    ),
    or_expr: $ => prec.left(PREC.or, seq(
      field('left',  $._or_expr),
      field('op',    '||'),
      field('right', $._and_expr),
    )),

    _and_expr: $ => choice(
      $.and_expr,
      $._eq_expr,
    ),
    and_expr: $ => prec.left(PREC.and, seq(
      field('left',  $._and_expr),
      field('op',    '&&'),
      field('right', $._eq_expr),
    )),

    _eq_expr: $ => choice(
      $.eq_expr,
      $._cmp_expr,
    ),
    eq_expr: $ => prec.left(PREC.eq, seq(
      field('left',  $._cmp_expr),
      field('op',    '=='),
      field('right', $._cmp_expr),
    )),

    _cmp_expr: $ => choice(
      $.cmp_expr,
      $._add_expr,
    ),
    cmp_expr: $ => prec.left(PREC.cmp, seq(
      field('left',  $._add_expr),
      field('op',    choice('<', '<=', '>', '>=')),
      field('right', $._add_expr),
    )),

    _add_expr: $ => choice(
      $.add_expr,
      $._mul_expr,
    ),
    add_expr: $ => prec.left(PREC.add, seq(
      field('left',  $._add_expr),
      field('op',    choice('+', '-')),
      field('right', $._mul_expr),
    )),

    _mul_expr: $ => choice(
      $.mul_expr,
      $._app_expr,
    ),
    mul_expr: $ => prec.left(PREC.mul, seq(
      field('left',  $._mul_expr),
      field('op',    choice('*', '/')),
      field('right', $._app_expr),
    )),

    // app_expr: Label-inject | state_prim[ty] e | not e | f e | simple
    _app_expr: $ => choice(
      $.inject_expr,
      $.state_prim_app,
      $.not_expr,
      $.fun_call,
      $._simple_expr,
    ),

    inject_expr: $ => prec.right(PREC.inject, seq(
      field('label', $.upper_label),
      field('arg',   $._app_expr),
    )),

    state_prim_app: $ => seq(
      field('prim', $.state_prim),
      '[', field('ty', $._sort), ']',
      field('arg', $._simple_expr),
    ),

    state_prim: _ => choice('Eq', 'Set', 'Get', 'New', 'Del', 'Own'),

    not_expr: $ => seq('not', field('arg', $._simple_expr)),

    fun_call: $ => prec.left(PREC.app, seq(
      field('func', $.lower_ident),
      field('arg',  $._simple_expr),
    )),

    _simple_expr: $ => choice(
      $.var_expr,
      $.int_lit,
      $.bool_lit,
      $.unit_expr,
      $.paren_expr,
      $.tuple_expr,
    ),

    var_expr:   $ => $.lower_ident,
    int_lit:    _ => /\d+/,
    bool_lit:   _ => choice('true', 'false'),
    unit_expr:  _ => seq('(', ')'),
    paren_expr: $ => seq('(', $._expr, ')'),
    tuple_expr: $ => seq(
      '(', $._expr, ',', sepBy1(',', $._expr), ')',
    ),

    // ======================================================================
    // Refined: proof sorts (pf_sort — used for codomains and annotations)
    // ======================================================================

    pf_sort: $ => seq(
      '(', sepBy(',', $.pf_entry), ')',
    ),

    pf_entry: $ => choice(
      // x : s
      seq(field('var', $.lower_ident),
          ':', field('sort', $._sort)),
      // [eff] x : s
      seq('[', field('eff', $.eff_level), ']',
          field('var', $.lower_ident),
          ':', field('sort', $._sort)),
      // [log] prop
      seq('[', 'log', ']', field('prop', $._app_expr)),
      // [res] pred @ value
      seq('[', 'res', ']',
          field('pred', $._app_expr), '@',
          field('value', $._app_expr)),
      // [res] (do y : s = e)
      seq('[', 'res', ']', '(',
          'do', field('bound_var', $.lower_ident),
          ':', field('bound_sort', $._sort),
          '=', field('pred', $._app_expr),
          ')'),
      // [res] (do y = e)
      seq('[', 'res', ']', '(',
          'do', field('bound_var', $.lower_ident),
          '=', field('pred', $._app_expr),
          ')'),
    ),

    // ======================================================================
    // Refined: proof domain (rfun param lists; every entry binds)
    // ======================================================================

    pf_domain: $ => seq(
      '(', sepBy(',', $.pf_domain_entry), ')',
    ),

    pf_domain_entry: $ => choice(
      seq(field('var', $.lower_ident),
          ':', field('sort', $._sort)),
      seq('[', field('eff', $.eff_level), ']',
          field('var', $.lower_ident),
          ':', field('sort', $._sort)),
      seq('[', 'log', ']', field('var', $.lower_ident),
          ':', field('prop', $._app_expr)),
      seq('[', 'res', ']', field('var', $.lower_ident), ':',
          field('pred', $._app_expr), '@',
          field('value', $._app_expr)),
      seq('[', 'res', ']', field('var', $.lower_ident), ':',
          '(',
          'do', field('bound_var', $.lower_ident),
          ':', field('bound_sort', $._sort),
          '=', field('pred', $._app_expr),
          ')'),
      seq('[', 'res', ']', field('var', $.lower_ident), ':',
          '(',
          'do', field('bound_var', $.lower_ident),
          '=', field('pred', $._app_expr),
          ')'),
    ),

    // ======================================================================
    // Core refined terms (crt)
    // ======================================================================

    _crt_expr: $ => choice($.crt_annot, $._crt_seq_expr),

    crt_annot: $ => prec.right(PREC.annot, seq(
      field('inner', $._crt_seq_expr),
      ':', field('pf', $.pf_sort),
    )),

    _crt_seq_expr: $ => choice(
      $.crt_let,
      $.crt_let_log,
      $.crt_let_log_annot,
      $.crt_let_res,
      $.crt_let_res_annot,
      $.crt_let_core,
      $.crt_iter,
      $.crt_if,
      $.crt_case,
      $.crt_exfalso,
      $.crt_open_take,
      $.crt_spine,
      $.crt_call,
      $.crt_prim_app,
    ),

    crt_let: $ => seq(
      'let', field('pat', $.rpat),
      '=', field('value', $._crt_expr),
      ';', field('body', $._crt_seq_expr),
    ),

    crt_let_log: $ => seq(
      'let', 'log', field('var', $.lower_ident),
      '=', field('value', $.lpf_expr),
      ';', field('body', $._crt_seq_expr),
    ),

    crt_let_log_annot: $ => seq(
      'let', 'log', field('var', $.lower_ident),
      ':', field('prop', $._app_expr),
      '=', field('value', $.lpf_expr),
      ';', field('body', $._crt_seq_expr),
    ),

    crt_let_res: $ => seq(
      'let', 'res', field('var', $.lower_ident),
      '=', field('value', $.rpf_expr),
      ';', field('body', $._crt_seq_expr),
    ),

    crt_let_res_annot: $ => seq(
      'let', 'res', field('var', $.lower_ident),
      ':',
      field('pred', $._app_expr), '@',
      field('value_expr', $._app_expr),
      '=', field('value', $.rpf_expr),
      ';', field('body', $._crt_seq_expr),
    ),

    crt_let_core: $ => seq(
      'let', 'core',
      '[', field('proof', $.lower_ident), ']',
      choice(
        field('pat', $.lower_ident),
        seq('(', field('pats', sepBy1(',', $.lower_ident)), ')'),
      ),
      '=', field('value', $._expr),
      ';', field('body', $._crt_seq_expr),
    ),

    crt_iter: $ => seq(
      'iter', '[', field('ce', $._expr), ']',
      '(',
      field('pat', $.rpat), '=', field('init', $._crt_expr),
      ')',
      '{', field('body', $._crt_expr), '}',
    ),

    crt_if: $ => seq(
      'if', '[', field('witness', $.lower_ident), ']',
      field('cond', $._expr),
      'then', field('then', $._crt_expr),
      'else', field('else', $._crt_seq_expr),
    ),

    crt_case: $ => seq(
      'case', '[', field('witness', $.lower_ident), ']',
      field('scrutinee', $._expr), 'of',
      '{',
      field('branches', sepBy1('|', $.crt_case_branch)),
      '}',
    ),

    crt_case_branch: $ => seq(
      field('label', $.upper_label),
      field('var', $.lower_ident),
      '->',
      field('body', $._crt_expr),
    ),

    crt_exfalso:  _ => 'exfalso',
    crt_open_take: $ => seq(
      'open-take', '(', field('arg', $.rpf_expr), ')',
    ),

    // Bare parenthesized spine as a tuple form.
    crt_spine: $ => seq(
      '(', sepBy(',', $.spine_arg), ')',
    ),

    crt_call: $ => prec.left(PREC.app, seq(
      field('func', $.lower_ident),
      field('spine', $.spine),
    )),

    crt_prim_app: $ => seq(
      field('prim', $.state_prim),
      '[', field('ty', $._sort), ']',
      field('spine', $.spine),
    ),

    // ======================================================================
    // Refined: spines and spine args
    // ======================================================================

    spine: $ => seq(
      '(', sepBy(',', $.spine_arg), ')',
    ),

    spine_arg: $ => choice(
      seq('log', field('value', $.lpf_expr)),
      seq('res', field('value', $.rpf_expr)),
      field('value', $._expr),
    ),

    // ======================================================================
    // Refined: rpat
    // ======================================================================

    rpat_elem: $ => choice(
      $.lower_ident,
      seq('(',
          field('x', $.lower_ident), ',',
          field('y', $.lower_ident),
          ')'),
    ),

    rpat: $ => choice(
      seq('(', ')'),
      seq('(', sepBy1(',', $.rpat_elem), ')'),
      $.lower_ident,
    ),

    // ======================================================================
    // Refined: logical proof facts (lpf)
    // ======================================================================

    lpf_expr: $ => choice($.lpf_annot, $._lpf_atom),

    lpf_annot: $ => prec.right(PREC.annot, seq(
      field('inner', $._lpf_atom),
      ':',
      field('prop', $._expr),
    )),

    _lpf_atom: $ => choice(
      $.lpf_var,
      $.lpf_auto,
      $.lpf_unfold,
      $.lpf_open_ret,
    ),

    lpf_var:  $ => $.lower_ident,
    lpf_auto: _ => 'auto',
    lpf_unfold: $ => seq(
      'unfold', field('func', $.lower_ident),
      '(', field('arg', $._expr), ')',
    ),
    lpf_open_ret: $ => seq(
      'open-ret', '(', field('arg', $.rpf_expr), ')',
    ),

    // ======================================================================
    // Refined: resource proof facts (rpf)
    // ======================================================================

    rpf_expr: $ => choice($.rpf_annot, $._rpf_atom),

    rpf_annot: $ => prec.right(PREC.annot, seq(
      field('inner', $._rpf_atom),
      ':',
      field('pred', $._simple_expr),
      '@',
      field('value', $._simple_expr),
    )),

    _rpf_atom: $ => choice(
      $.rpf_var,
      $.rpf_make_ret,
      $.rpf_make_take,
    ),

    rpf_var: $ => $.lower_ident,
    rpf_make_ret: $ => seq(
      'make-ret', '(', field('arg', $.lpf_expr), ')',
    ),
    rpf_make_take: $ => seq(
      'make-take', '(', field('arg', $._crt_expr), ')',
    ),

    // ======================================================================
    // Terminals
    // ======================================================================

    lower_ident: _ => /[a-z][a-zA-Z0-9_']*/,
    upper_label: _ => /[A-Z][a-zA-Z0-9_']*/,

    comment: _ => token(seq('//', /[^\n]*/)),
  },
});
