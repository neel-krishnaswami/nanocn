(** SMT-LIB 2.7 atoms: the leaves of an s-expression.

    Atoms are the non-paren token classes of §3.1 of the standard:
    spec-constants ([Numeral], [Decimal], [Hexadecimal], [Binary],
    [String]), symbols (simple or quoted, unified), keywords, and the
    reserved-word class. *)

(** Reserved words from §3.1 and the command names of Figure 3.7. Each
    constructor stands for exactly one SMT-LIB spelling; the mapping is
    bijective via [reserved_to_string] / [reserved_of_string]. *)
type reserved =
  (* meta-spec constant tags *)
  | R_BINARY | R_DECIMAL | R_HEXADECIMAL | R_NUMERAL | R_STRING
  (* punctuation / binders / qualifiers *)
  | R_underscore | R_bang
  | R_as | R_lambda | R_let | R_exists | R_forall | R_match | R_par
  (* script command names from Figure 3.7 *)
  | R_assert | R_check_sat | R_check_sat_assuming
  | R_declare_const | R_declare_datatype | R_declare_datatypes
  | R_declare_fun | R_declare_sort | R_declare_sort_parameter
  | R_define_const | R_define_fun | R_define_fun_rec | R_define_funs_rec
  | R_define_sort
  | R_echo | R_exit
  | R_get_assertions | R_get_assignment | R_get_info | R_get_model
  | R_get_option | R_get_proof | R_get_unsat_assumptions
  | R_get_unsat_core | R_get_value
  | R_pop | R_push | R_reset | R_reset_assertions
  | R_set_info | R_set_logic | R_set_option

val reserved_compare : reserved -> reserved -> int
val reserved_to_string : reserved -> string
val reserved_of_string : string -> reserved option

(** Atoms. Numeric literal contents are kept as verbatim digit strings
    without the [#x] / [#b] prefix. [Symbol] unifies simple and quoted
    symbols into a single constructor holding the canonical contents.
    [Keyword] contents omit the leading colon. [String] contents are
    unescaped: the doubled-quote sequence already has each pair of
    adjacent quote characters collapsed to a single quote character. *)
type t =
  | Numeral     of string
  | Decimal     of string
  | Hexadecimal of string
  | Binary      of string
  | String      of string
  | Symbol      of string
  | Keyword     of string
  | Reserved    of reserved

val compare   : t -> t -> int
val print     : Format.formatter -> t -> unit
(** Round-trips to valid SMT-LIB concrete syntax:
    - [Numeral n] / [Decimal d] print as their contents;
    - [Hexadecimal h] prints as [#x] then [h]; [Binary b] as [#b]
      then [b];
    - [String s] prints the contents wrapped in double quotes with
      each embedded double quote doubled;
    - [Symbol s] prints bare iff [s] is a legal simple-symbol and not
      a reserved-word spelling; otherwise quoted as [|s|];
    - [Keyword k] prints as a colon followed by [k];
    - [Reserved r] prints as [reserved_to_string r]. *)

val to_string : t -> string

val is_valid_symbol : string -> bool
(** Symbol contents are storable iff they contain neither the pipe
    character nor a backslash (forbidden inside quoted symbols by
    SMT-LIB §3.1). Smart constructors in [SmtSexp] do not enforce
    this; callers feeding untrusted input can pre-validate. *)

module Test : sig
  val gen_reserved : reserved QCheck.Gen.t
  val gen          : t QCheck.Gen.t
  val test         : QCheck.Test.t list
end
