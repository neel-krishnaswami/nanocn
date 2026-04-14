(** Collect the sets [R], [F], [T], [S] from a refined signature and
    a constraint tree. [S] is the set of all monadic payload sorts
    used in the program; the prelude emits per-sort [return]/[fail]
    declarations for [S] and per-pair [bind] declarations for [S × S]
    (see [doc/smt-encoding.md]). *)

module SortSet     : Set.S with type elt = Sort.sort
module SortPairSet : Set.S with type elt = Sort.sort * Sort.sort

type t = {
  s : SortSet.t;
  (** All monadic payload sorts appearing in the program.
      [S = R ∪ F ∪ {τ | (τ,_) ∈ T} ∪ {σ | (_,σ) ∈ T} ∪ O]. *)

  pairs : SortPairSet.t;
  (** Pairs [(τ, σ)] for every [take x = (_ : Pred τ); (_ : Pred σ)]
      in the program. These drive the [bind-τ-σ] declarations. *)

  own_sorts : SortSet.t;
  (** Pointee sorts τ such that [Own[τ]] appears in the program. Each
      one gets a [(declare-fun own-τ ((Ptr τ)) (Pred τ))] in the
      prelude. Also merged into [s] so the corresponding [return]/
      [fail]/[bind] ops exist. *)
}

val collect : RSig.t -> Constraint.typed_ct -> t
