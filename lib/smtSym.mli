(** Mangling of nanoCN names into SMT-LIB simple symbols.
    Produces {b flat} names for use as identifiers in declarations and
    references. Structured sort terms (e.g. [(Tuple-2 Int Bool)]) are
    built by [SmtExpr.of_sort], not here. *)

val of_var     : Var.t -> string
val of_label   : Label.t -> string
val of_dsort   : Dsort.t -> string
val of_tvar    : Tvar.t -> string
val of_funname : string -> string

val ctor_selector : Label.t -> string
(** [ctor_selector L] returns ["get-L"] — the SMT selector name for
    the unique payload of constructor [L]. *)

val tuple_sort : int -> string
(** [tuple_sort n] returns ["Tuple-n"] — the datatype constructor
    for n-ary tuples. Used for n = 0 and n >= 2. *)

val tuple_ctor : int -> string
(** [tuple_ctor n] returns ["tuple-n"] — the value constructor for
    n-ary tuples. *)

val tuple_proj : int -> int -> string
(** [tuple_proj n k] returns ["prj-n-k"] — the selector for the k-th
    field of an n-ary tuple (1-indexed). *)

val sort_tag : Sort.sort -> string
(** Flat identifier derived from a sort, for embedding inside per-sort
    monad-op symbol names like [fail-X], [return-X], [bind-X-Y]. The
    tag is not a sort term; the corresponding structured sort term is
    built by [SmtExpr.of_sort]. See [doc/smt-encoding-plan.md].

    Invariant: [Sort.compare s1 s2 = 0] iff
    [String.equal (sort_tag s1) (sort_tag s2)].

    The resulting string is always a legal SMT-LIB simple symbol. *)

val return_sym : Sort.sort -> string
(** [return_sym τ = "return-" ^ sort_tag τ]. *)

val fail_sym : Sort.sort -> string
(** [fail_sym τ = "fail-" ^ sort_tag τ]. *)

val bind_sym : Sort.sort -> Sort.sort -> string
(** [bind_sym τ σ = "bind-" ^ sort_tag τ ^ "-" ^ sort_tag σ]. *)

val own_sym : Sort.sort -> string
(** [own_sym τ = "own-" ^ sort_tag τ] — the uninterpreted function
    name for the pointee-sort-indexed ownership predicate. *)

module Test : sig
  val test : QCheck.Test.t list
end
