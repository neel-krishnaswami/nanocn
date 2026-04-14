(** Translate core sorts and typed core expressions to SMT
    s-expressions. Sort translation shares this module because
    [CoreExpr.Annot] embeds a [Sort.sort], producing mutual recursion
    with [of_ce]. *)

val of_sort : Sort.sort -> SmtSexp.sexp
(** Structural translation of a sort to an SMT sort term:
    - [Int] / [Bool] / [TVar a] emit the atomic symbol;
    - [Ptr τ]  → [(Ptr <of_sort τ>)];
    - [Pred τ] → [(Pred <of_sort τ>)];
    - [Record []] → [Tuple-0];
    - [Record [τ]] → [of_sort τ] (unboxed);
    - [Record τ₁..τₙ] (n ≥ 2) → [(Tuple-n <of_sort τ₁> … <of_sort τₙ>)];
    - [App D [τ₁..τₙ]] with n = 0 → [D], with n ≥ 1 → [(D <of_sort τ₁> …)]. *)

val of_ce : CoreExpr.typed_ce -> (SmtSexp.sexp, string) result
(** Per the SMT(ce) rules in [doc/smt-encoding.md]. Returns [Error]
    for forms that are not SMT-expressible:
    - [Iter] (unbounded iteration);
    - state primitives [New]/[Del]/[Get]/[Set]/[Own];
    - binary primitives whose argument is not a 2-tuple. *)

module Test : sig
  val test : QCheck.Test.t list
end
