(** A View on [CoreExpr.t] for shape-extraction and reconstruction
    inside the typechecker.

    [type 'a t = 'a option] — every extractor and builder works in
    option-land.  [Get.foo] takes an optioned typed_ce and returns
    optioned sub-components (or [None] when the shape doesn't match
    or the input was already [None]).  [Build.foo] takes optioned
    sub-components and an info object and produces an optioned
    typed_ce (or [None] when any sub-component was [None]).

    The View carries no error vocabulary.  Consumers (typechecker
    clauses) define small *local* wrappers that lift the option
    output to a [(_, Error.t) result] using the appropriate
    [K_wrong_pred_shape] error kind with the call-site's
    [construct] string.  This keeps the "no branching on result
    inside clauses" invariant — the [Option.to_result] conversion
    lives inside the wrapper helper, not the clause body.

    The polymorphic ['b] is the info parameter on [CoreExpr.t]; the
    View doesn't constrain it. *)

type 'a t = 'a option

module Get : sig
  (** Per-component shape extractors.  When the input shape doesn't
      match (or the input is [None]), each output component is [None];
      callers consume them individually so a missing component flows
      naturally to its first downstream consumer. *)

  val return : 'b CoreExpr.t t -> 'b CoreExpr.t t
  val fail   : 'b CoreExpr.t t -> unit t

  val take : 'b CoreExpr.t t
          -> Var.t t * 'b CoreExpr.t t * 'b CoreExpr.t t

  val let_ : 'b CoreExpr.t t
          -> Var.t t * 'b CoreExpr.t t * 'b CoreExpr.t t

  val let_tuple : 'b CoreExpr.t t
               -> Var.t list t * 'b CoreExpr.t t * 'b CoreExpr.t t

  val if_ : 'b CoreExpr.t t
         -> 'b CoreExpr.t t * 'b CoreExpr.t t * 'b CoreExpr.t t

  val case : 'b CoreExpr.t t
          -> 'b CoreExpr.t t
           * (Label.t * Var.t * 'b CoreExpr.t * 'b) list t

  val call : 'b CoreExpr.t t -> string t * 'b CoreExpr.t t

  val inject : 'b CoreExpr.t t -> Label.t t * 'b CoreExpr.t t
end

module Build : sig
  val return : 'b -> 'b CoreExpr.t t -> 'b CoreExpr.t t
  val eq     : 'b -> 'b CoreExpr.t t -> 'b CoreExpr.t t -> 'b CoreExpr.t t
  val tuple  : 'b -> 'b CoreExpr.t t list -> 'b CoreExpr.t t
end

module Test : sig
  val test : QCheck.Test.t list
end
