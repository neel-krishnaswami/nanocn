(** A View on [Sort.t] for destructuring and rebuilding sorts in the
    typechecker.

    [type 'a t = 'a option] — every extractor and builder works in
    option-land. [Get] consumes a wrapped sort and returns wrapped
    sub-sorts; [Build] takes wrapped sub-sorts and produces a wrapped
    sort.  When the input is [None], the output is [None] too.

    The View carries no error vocabulary: it just reports whether a
    shape matched or didn't.  Consumers (typechecker clauses) define
    small *local* wrappers that lift the option output to a
    [(_, Error.kind) result] using the appropriate per-call-site error
    kind (typically [K_construct_sort_mismatch] with a [construct]
    argument).  This keeps the "no branching on result inside clauses"
    invariant — the [Option.to_result] conversion lives inside the
    wrapper helper, not the clause body.

    The View is polymorphic over the info parameter ['info]: as long
    as ['info] has a [loc : SourcePos.t] method (so error-reporting
    helpers in the wrapping consumer can project a [Sort.sort] for
    the diagnostic), the View works on any ['info Sort.t].  *)

type 'a t = 'a option

(** [project s] extracts a [Sort.sort] (info erased to bare [loc]) from
    an arbitrary ['info Sort.t].  Used by consumers' wrapper helpers to
    fill in a [K_construct_sort_mismatch.got] field with a sort the
    error machinery can render. *)
val project : (< loc : SourcePos.t; .. > as 'info) Sort.t -> Sort.sort

module Get : sig
  val int    : (< loc : SourcePos.t; .. >) Sort.t t -> unit t
  val bool   : (< loc : SourcePos.t; .. >) Sort.t t -> unit t
  val ptr    : (< loc : SourcePos.t; .. > as 'info) Sort.t t -> 'info Sort.t t
  val pred   : (< loc : SourcePos.t; .. > as 'info) Sort.t t -> 'info Sort.t t
  val record : int -> (< loc : SourcePos.t; .. > as 'info) Sort.t t -> 'info Sort.t t list
  (** [record n s] returns a list of *exactly* [n] sub-sort options.
      [Some ts] when [s = Some (Record ts)] with [List.length ts = n];
      [List.init n (fun _ -> None)] otherwise (wrong shape, wrong
      arity, or [s = None]).  Callers iterating by tuple shape don't
      need to check length. *)
  val app    : (< loc : SourcePos.t; .. > as 'info) Sort.t t -> Dsort.t t * 'info Sort.t t list
  val tvar   : (< loc : SourcePos.t; .. >) Sort.t t -> Tvar.t t
end

module Build : sig
  val int    : 'info -> unit t -> 'info Sort.t t
  val bool   : 'info -> unit t -> 'info Sort.t t
  val ptr    : 'info -> 'info Sort.t t -> 'info Sort.t t
  val pred   : 'info -> 'info Sort.t t -> 'info Sort.t t
  val record : 'info -> 'info Sort.t t list -> 'info Sort.t t
  val app    : 'info -> Dsort.t t -> 'info Sort.t t list -> 'info Sort.t t
  val tvar   : 'info -> Tvar.t t -> 'info Sort.t t
end

module Test : sig
  val test : QCheck.Test.t list
end
