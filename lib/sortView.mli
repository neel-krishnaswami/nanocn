(** A View on [Sort.t] that lets typechecker clauses destructure and
    rebuild sorts while threading errors through, without ever
    pattern-matching on a result.

    [type 'a t = ('a, Error.kind) result] — every extractor and builder
    works in result-land.  [Get] consumes a wrapped sort and returns
    wrapped sub-sorts; [Build] takes wrapped sub-sorts and produces a
    wrapped sort.  When the input is [Error], the output is [Error]
    too — error propagation is automatic.

    The View is polymorphic over the info parameter ['info]: as long
    as ['info] has a [loc : SourcePos.t] method (so error reporting
    can project a [Sort.sort] for the diagnostic), the View works on
    any ['info Sort.t].  Callers using concrete [Sort.sort] (=
    [< loc : SourcePos.t > Sort.t]) instantiate ['info] to that;
    elaboration- and refined-context sorts that carry richer info do
    the same, since they already include [loc].  [Build]'s first
    argument is the [info] value to attach at the constructed node. *)

type 'a t = ('a, Error.kind) result

module Get : sig
  val int    : construct:string -> (< loc : SourcePos.t; .. > as 'info) Sort.t t -> unit t
  val bool   : construct:string -> (< loc : SourcePos.t; .. > as 'info) Sort.t t -> unit t
  val ptr    : construct:string -> (< loc : SourcePos.t; .. > as 'info) Sort.t t -> 'info Sort.t t
  val pred   : construct:string -> (< loc : SourcePos.t; .. > as 'info) Sort.t t -> 'info Sort.t t
  val record : construct:string -> int -> (< loc : SourcePos.t; .. > as 'info) Sort.t t -> 'info Sort.t t list
  (** [record ~construct n s] returns a list of *exactly* [n]
      sub-sort results.  When [s] is [Ok (Record ts)] with
      [List.length ts = n], the result is [List.map Ok ts].  In every
      other case (wrong shape, wrong arity, [s] itself an [Error]),
      the result is a list of [n] copies of an appropriate [Error] —
      callers can then iterate by tuple shape without checking
      length. *)
  val app    : construct:string -> (< loc : SourcePos.t; .. > as 'info) Sort.t t -> Dsort.t t * 'info Sort.t t list
  val tvar   : construct:string -> (< loc : SourcePos.t; .. > as 'info) Sort.t t -> Tvar.t t
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
